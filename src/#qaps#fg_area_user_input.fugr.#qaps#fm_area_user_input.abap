FUNCTION /QAPS/FM_AREA_USER_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IV_ID_TP_LISTA) TYPE  /QAPS/ED_TP_LISTA
*"     VALUE(IV_ID_AREA) TYPE  /QAPS/ED_ID_AREA
*"  EXPORTING
*"     VALUE(ET_DATA) TYPE  /QAPS/T_AREA_USER
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = 123.
  lv_y2 = 18.

  PERFORM fill_target USING iv_id_tp_lista iv_id_area.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      et_data = CORRESPONDING #( gt_target ).

      LOOP at et_data ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-id_tp_lista = iv_id_tp_lista.
        <fs>-id_area     = iv_id_area.
      ENDLOOP.

    WHEN 'CANCEL'.
      REFRESH: et_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

*  FREE: go_cont_source,
*        go_cont_target,
*        go_alv_source,
*        go_alv_target.

ENDFUNCTION.
