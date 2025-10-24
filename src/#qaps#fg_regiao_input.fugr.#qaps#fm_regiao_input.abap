FUNCTION /qaps/fm_regiao_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1
*"     REFERENCE(IS_DATA) TYPE  /QAPS/S_REGIAO OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_REGIAO
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 8.

  lv_x2 = lv_x1 + 55.
  lv_y2 = lv_y1 + 2.

  CLEAR gv_loaded.

  gv_action = iv_action.

  IF iv_action = 'E'.
    gs_regiao = CORRESPONDING #( is_data ).
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_regiao ).
    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
