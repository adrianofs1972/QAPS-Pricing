FUNCTION /qaps/fm_periodo_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_PERIODO
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 8.

  lv_x2 = lv_x1 + 30.
  lv_y2 = lv_y1 + 5.

  DATA(lv_actual_month) = sy-datum(6).

  PERFORM fill_p100_fields USING ''
                                 ''
                                 lv_actual_month
                                 sy-langu.

  gs_periodo-year = sy-datum(4).
  gs_periodo-month = sy-datum+4(2).

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = gs_periodo.
    WHEN 'CANCEL'.
      CLEAR es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.

ENDFUNCTION.
