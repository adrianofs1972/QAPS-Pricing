FUNCTION /qaps/fm_std_prd_cp_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_ITEM) TYPE  /QAPS/S_STD_PRODUCAO_CP OPTIONAL
*"     VALUE(IS_PA) TYPE  /QAPS/S_STD_PRODUCAO_PA OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_STD_PRODUCAO_CP
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 73.
  lv_y2 = lv_y1 + 7.

  gv_tiltle = 'Std de Produção - Componentes'.
  gv_action = iv_action.
  IF iv_action = 'C'.
    CLEAR gs_data.
  ELSEIF iv_action = 'E'.
    gs_data = is_item.
  ENDIF.

  gs_pa = is_pa.

  PERFORM fill_not_allowed USING is_pa.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).
    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
