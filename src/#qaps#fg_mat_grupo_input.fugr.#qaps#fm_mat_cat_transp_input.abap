FUNCTION /qaps/fm_mat_cat_transp_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EV_ID_CATEGORIA) TYPE  /QAPS/ID_CATEGORIA
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_ok      TYPE abap_bool,
        lv_message TYPE bapi_msg.

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 10.
  lv_y1 = 10.

  lv_x2 = lv_x1 + 50.
  lv_y2 = lv_y1 + 2.

  PERFORM fill_categ_transp CHANGING lv_ok lv_message.

  IF lv_ok = abap_false.
    CLEAR: ev_id_categoria.
    es_message = VALUE #( type = 'E' message = lv_message ).
    RETURN.
  ENDIF.

  gv_tiltle = 'Categoria de Transporte'.
  clear /qaps/categ_trns.

  CALL SCREEN 2000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      data(lv_id_categoria) = value #( gt_listbox[ index = gs_listbox-index ]-id_categoria OPTIONAL ).
      ev_id_categoria = lv_id_categoria.
    WHEN 'CANCEL'.
      CLEAR: ev_id_categoria.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.

ENDFUNCTION.
