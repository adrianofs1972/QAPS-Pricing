FUNCTION /qaps/fm_std_prod_sel_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EV_ID_STD_PRODUCAO) TYPE  /QAPS/ED_ID_STD_PRODUCAO
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

  PERFORM fill_std_producao CHANGING lv_ok lv_message.

  IF lv_ok = abap_false.
    CLEAR: ev_id_std_producao.
    es_message = VALUE #( type = 'E' message = lv_message ).
    RETURN.
  ENDIF.

  gv_tiltle = 'Std de Produção'.
  CLEAR /qaps/std_prd_h.

  CALL SCREEN 2000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      DATA(lv_id_std_producao) = VALUE #( gt_listbox[ index = gs_listbox-index ]-id_std_producao OPTIONAL ).
      ev_id_std_producao = lv_id_std_producao.
    WHEN 'CANCEL'.
      CLEAR: ev_id_std_producao.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.

ENDFUNCTION.
