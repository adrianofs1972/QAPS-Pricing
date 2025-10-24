FUNCTION /QAPS/FM_APROVADORES_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1
*"     REFERENCE(IV_ID_TP_LISTA) TYPE  /QAPS/ED_TP_LISTA
*"  EXPORTING
*"     REFERENCE(ET_DATA) TYPE  /QAPS/T_LISTA_APROV
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = 123.
  lv_y2 = 18.

*  clear gv_loaded.

  PERFORM fill_target USING iv_id_tp_lista.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      et_data = CORRESPONDING #( gt_target ).

      LOOP at et_data ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-id_tp_lista = iv_id_tp_lista.
      ENDLOOP.

    WHEN 'CANCEL'.
      REFRESH: et_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
