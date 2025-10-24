FUNCTION /qaps/fm_simulacao_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     REFERENCE(IV_ID_TP_LISTA) TYPE  /QAPS/ED_TP_LISTA
*"     REFERENCE(IS_DATA) TYPE  /QAPS/S_SIMULACAO OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_SIMULACAO
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 73.
  lv_y2 = lv_y1 + 11.

  gv_tiltle = 'Simulação'.
  gv_action = iv_action.

  CASE iv_action.
    WHEN 'C'. "Criação

      gs_data-id_tp_lista = iv_id_tp_lista.

      DATA(lv_actual_month) = sy-datum(6).

      PERFORM fill_p100_fields USING ''
                                     ''
                                     lv_actual_month
                                     sy-langu.



      gs_data-id_original = is_data-id_original.
      gs_periodo_inicial-year = sy-datum(4).
      gs_periodo_inicial-month = sy-datum+4(2).
      gs_data-status = 'A'.

    WHEN 'P'. "criar por cópia
      gs_data = CORRESPONDING #( is_data ).
      gs_data-id_original = gs_data-id_simulacao.
      gs_data-dsc_referencia = gs_data-descricao.

      CLEAR: gs_data-id_simulacao,
             gs_data-descricao.

      lv_actual_month = sy-datum(6).

      PERFORM fill_p100_fields USING ''
                                     ''
                                     lv_actual_month
                                     sy-langu.



*      gs_data-id_original = is_data-id_original.
*      gs_periodo_inicial-year = sy-datum(4).
*      gs_periodo_inicial-month = sy-datum+4(2).
*      gs_data-status = 'A'.

*      gs_periodo_inicial-year  = gs_data-periodo_inicial(4).
*      gs_periodo_inicial-month =  gs_data-periodo_inicial+4(2).
*
*      gs_periodo_final-year  = gs_data-periodo_final(4).
*      gs_periodo_final-month =  gs_data-periodo_final+4(2).
    WHEN 'U'.
      gs_data = CORRESPONDING #( is_data ).

      gs_periodo_inicial-year  = gs_data-periodo_inicial(4).
      gs_periodo_inicial-month =  gs_data-periodo_inicial+4(2).

      gs_periodo_final-year  = gs_data-periodo_final(4).
      gs_periodo_final-month =  gs_data-periodo_final+4(2).
  ENDCASE.

  DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
  DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( iv_id_tipo_lista = iv_id_tp_lista ).

  gs_data-dsc_tp_lista = VALUE #( lt_tp_lista[ 1 ]-descricao ).
  gs_data-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_SIMUL'
                                                              iv_value  = CONV #( gs_data-status ) ).

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.

      es_data = CORRESPONDING #( gs_data ).
      es_data-id_tp_lista = iv_id_tp_lista.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
