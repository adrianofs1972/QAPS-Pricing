FUNCTION /QAPS/FM_AREA_INPUT .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1
*"     REFERENCE(IV_ID_TP_LISTA) TYPE  /QAPS/ED_TP_LISTA
*"  EXPORTING
*"     REFERENCE(ET_DATA) TYPE  /QAPS/T_AREA
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"--------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = 80.
  lv_y2 = 18.

  clear gv_loaded.

  FREE: go_cont_source,
        go_cont_target,
        go_alv_source,
        go_alv_target.

  PERFORM fill_source_target USING iv_id_tp_lista.

  CALL SCREEN 2000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      et_data = CORRESPONDING #( gt_target ).
    WHEN 'CANCEL'.
      REFRESH: et_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

  FREE: go_cont_source,
        go_cont_target,
        go_alv_source,
        go_alv_target.

ENDFUNCTION.
