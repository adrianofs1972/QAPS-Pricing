FUNCTION /qaps/fm_lista_custo_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_DATA) TYPE  /QAPS/S_LISTA_HEADER OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_LISTA_HEADER
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 84.
  lv_y2 = lv_y1 + 9.

  gv_tiltle = 'Lista de Custo'.
  CLEAR gv_loaded_edit.

  gv_action = iv_action.

  IF gv_action = 'E'.
    gs_data = CORRESPONDING #( is_data ).
  ELSE.
    CLEAR gs_data.
    "set default
    gs_data-moeda_calculo = 'BRL'.
  ENDIF.

  PERFORM fill_range_moeda.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      SELECT SINGLE *
        FROM /qaps/fonte_cmb
        WHERE descricao = @gs_data-dsc_fonte
        INTO @DATA(ls_fonte).

      es_data = CORRESPONDING #( gs_data ).
      es_data-status = 'G'.
      es_data-id_fonte = ls_fonte-id_fonte.
      es_data-metodo_custeio = gs_data-metodo_custeio.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
