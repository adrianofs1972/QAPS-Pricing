FUNCTION /qaps/fm_var_elementar_select .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_PARENT) TYPE  /QAPS/S_CALCULATION_NODE
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_CUSTO_ELEMENTAR
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------


  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 120.
  lv_y2 = lv_y1 + 18.

  gv_tiltle = 'Variáveis de Custo Elementar'.
*  CLEAR gv_loaded.

  PERFORM fill_data USING is_parent.

  IF lines( gt_data ) = 0.
    CLEAR: es_data.
    es_message = VALUE #( type = 'E' message = 'Não há variáveis disponíveis para este item' ).
    RETURN.
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).
    WHEN 'CANCEL' OR ''.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
