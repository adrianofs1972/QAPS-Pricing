FUNCTION /QAPS/FM_TIPO_LISTA_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_DATA) TYPE  /QAPS/S_TP_LISTA
*"     REFERENCE(IV_ACTION) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_TP_LISTA
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 60.
  lv_y2 = lv_y1 + 3.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_tipo_lista ).
    WHEN 'CANCEL'.
      CLEAR: gs_tipo_lista,
             es_data.

      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
