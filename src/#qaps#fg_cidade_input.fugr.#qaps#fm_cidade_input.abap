FUNCTION /qaps/fm_cidade_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IS_DATA) TYPE  /QAPS/S_CIDADE OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_CIDADE
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 15.

  lv_x2 = lv_x1 + 55.
  lv_y2 = lv_y1 + 7.

*  CLEAR gv_loaded.
  gv_action = iv_action.

  IF gv_action = 'E'.
    gs_cidade = CORRESPONDING #( is_data ).
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_cidade ).
      es_data-uf = VALUE #( gt_uf[ index = gs_uf-index ]-uf OPTIONAL ).
      es_data-id_regiao = VALUE #( gt_regiao[ index = gs_regiao-index ]-id_regiao OPTIONAL ).

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
