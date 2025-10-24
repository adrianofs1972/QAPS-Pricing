FUNCTION /qaps/fm_trecho_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IS_DATA) TYPE  /QAPS/S_TRECHO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_TRECHO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 85.
  lv_y2 = lv_y1 + 17.

*  CLEAR gv_loaded.
  gv_action = iv_action.

  IF gv_action = 'E'.
    gs_data = CORRESPONDING #( is_data ).
  ELSE.
    CLEAR gs_data.
    gs_data-id_modal = 'R'.
    gs_data-dsc_modal = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_MODAL'
                                                             iv_value  = conv #( gs_data-id_modal ) ).
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).

      DATA(ls_origem) = VALUE #( gt_tipo_origem[ index =  gs_tipo_origem-index ] OPTIONAL ).

      IF NOT ls_origem IS INITIAL.
        es_data-tipo_origem = ls_origem-tipo_ponto.
        es_data-id_origem = ls_origem-id_ponto.
      ENDIF.

      DATA(ls_destino) = VALUE #( gt_tipo_destino[ index = gs_tipo_destino-index ] OPTIONAL ).

      IF NOT ls_destino IS INITIAL.
        es_data-tipo_destino = ls_destino-tipo_ponto.
        es_data-id_destino = ls_destino-id_ponto.
      ENDIF.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
