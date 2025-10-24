FUNCTION /QAPS/FM_CLIENTE_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IS_DATA) TYPE  /QAPS/S_CLIENTE OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_CLIENTE
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 8.

  lv_x2 = lv_x1 + 70.
  lv_y2 = lv_y1 + 4.

  CLEAR: gv_loaded,
         gv_loaded_edit.
  gv_action = iv_action.

  IF gv_action = 'E'.
    gs_data = CORRESPONDING #( is_data ).
  ELSE.
    CLEAR gs_data.
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).

      es_data-id_grp_cliente = VALUE #( gt_grp_cliente[ index = gs_grp_cliente-index ]-id_grp_cliente OPTIONAL ).
      es_data-id_cidade = VALUE #( gt_cidade[ index = gs_cidade-index ]-id_cidade OPTIONAL ).
    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
