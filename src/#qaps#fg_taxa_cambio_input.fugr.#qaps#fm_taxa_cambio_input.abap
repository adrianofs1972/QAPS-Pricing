FUNCTION /qaps/fm_taxa_cambio_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_ITEM) TYPE  /QAPS/S_TAXA_CAMBIO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_TAXA_CAMBIO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 73.
  lv_y2 = lv_y1 + 3.

  gv_tiltle = 'Taxa de Câmbio'.
  IF iv_action = 'C'.
    CLEAR gs_data.
  ELSEIF iv_action = 'E'.
    gs_data = is_item.
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).
      es_data-id_fonte = VALUE #( gt_fonte[ index = gs_fonte-index ]-id_fonte ).
    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
