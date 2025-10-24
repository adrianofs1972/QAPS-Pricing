FUNCTION /qaps/fm_efetivacao_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IS_DATA) TYPE  /QAPS/S_LISTA_HEADER
*"     VALUE(IS_SIMULACAO) TYPE  /QAPS/S_SIMULACAO
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_EFETIVACAO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 8.

  lv_x2 = lv_x1 + 55.
  lv_y2 = lv_y1 + 16.

  CLEAR: gv_loaded,
         gv_loaded_edit.

  gv_action = iv_action.
  gs_header = is_data.
  gs_simulacao = is_simulacao.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data-/qaps/grkey = gt_grupo_aps[ gs_grupo_aps-index ]-/qaps/grkey.
      es_data-t_periodos = gt_periodo_versao.
      es_message = VALUE #( type = 'S' ).
    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.

ENDFUNCTION.
