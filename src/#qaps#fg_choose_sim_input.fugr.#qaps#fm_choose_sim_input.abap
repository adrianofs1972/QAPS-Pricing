FUNCTION /QAPS/FM_CHOOSE_SIM_INPUT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EV_ID_SIMULACAO) TYPE  /QAPS/ED_ID_SIMULACAO
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

  CLEAR: gv_loaded.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      ev_id_simulacao = gt_simulacao[ gs_simulacao-index ]-id_simulacao.
*      es_data-t_periodos = gt_periodo_versao.
*      es_message = VALUE #( type = 'S' ).
    WHEN 'CANCEL'.
      CLEAR: ev_id_simulacao.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.


ENDFUNCTION.
