FUNCTION /qaps/fm_var_elem_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     REFERENCE(IS_CUSTO_ELEMENTAR) TYPE  /QAPS/S_CUSTO_ELEMENTAR
*"       OPTIONAL
*"     REFERENCE(IS_SIMULACAO) TYPE  /QAPS/S_SIMULACAO
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_VAR_INPUT
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 85.
  lv_y2 = lv_y1 + 27.

  gv_tiltle = 'Input de Custo Elementar'.
  gs_custo_elementar = is_custo_elementar.

  CLEAR gv_loaded.

  CLEAR: es_data.

  PERFORM reset_options.
*  if iv_action = 'C'.
*    gv_destino_geral = 'X'.
*  endif.

*  DATA(lo_simulacao) = NEW /qaps/cl_mdl_simulacao( iv_action = 'C' ).
*
*  DATA(lt_simulacao) = lo_simulacao->get_simulacao( is_custo_elementar-id_tp_lista ).
  gs_data-dsc_simulacao = is_simulacao-descricao.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

*  break c060863.

  CASE sy-ucomm.
    WHEN 'OK'.

      es_data-id_simulacao       = is_simulacao-id_simulacao.
      es_data-id_custo_elementar = is_custo_elementar-id_custo_elementar.
      "Dados de tipo de regra
      es_data-tipo_regra         = COND #( WHEN gv_tr_geral = 'X' THEN 'GE'
                                           WHEN gv_tr_grupo_produto = 'X' THEN 'GP'
                                           WHEN gv_tr_mat_planejado  = 'X' THEN 'MP'
                                           WHEN gv_tr_agregador  = 'X' THEN 'AG'
                                           WHEN gv_tr_material  = 'X'THEN 'MA').


      CASE es_data-tipo_regra.
        WHEN 'GE'.
        WHEN 'GP'.
          es_data-id_grupo_produto   = gt_list_gp[ gs_list_gp-index ]-id_grupo_produto.
        WHEN 'MP'.
          es_data-mat_planejado      = gt_list_mp[ gs_list_mp-index ]-mat_planejado.
        WHEN 'AG'.
          es_data-agregador          = gt_list_ag[ gs_list_ag-index ]-agregador.
        WHEN 'MA'.
          es_data-matnr              = |{ gs_data-matnr ALPHA = IN WIDTH = 18 }|.
      ENDCASE.

      IF gs_custo_elementar-tipo_variavel = 'G' OR gs_custo_elementar-tipo_variavel = 'L'
        OR gs_custo_elementar-tipo_variavel = 'C'.

        DATA(ls_origem) = VALUE #( gt_tipo_origem[ index =  gs_tipo_origem-index ] OPTIONAL ).

        IF NOT ls_origem IS INITIAL.
          es_data-tipo_origem = ls_origem-tipo_ponto.
          es_data-id_origem = ls_origem-id_ponto.
        ENDIF.

      ENDIF.

      IF gs_custo_elementar-tipo_variavel = 'G' OR gs_custo_elementar-tipo_variavel = 'L'
        OR gs_custo_elementar-tipo_variavel = 'P' OR gs_custo_elementar-tipo_variavel = 'C' .

        DATA(ls_destino) = VALUE #( gt_tipo_destino[ index = gs_tipo_destino-index ] OPTIONAL ).

        IF NOT ls_destino IS INITIAL.
          es_data-tipo_destino = ls_destino-tipo_ponto.
          es_data-id_destino = ls_destino-id_ponto.
        ENDIF.

      ENDIF.

      IF gs_custo_elementar-tipo_variavel = 'L'.
*      "Transporte
        es_data-id_modal           = VALUE #( gt_list_mod[ gs_list_mod-index ]-id_modal OPTIONAL ).
        es_data-id_categoria       = VALUE #( gt_list_cattr[ gs_list_cattr-index ]-id_categoria OPTIONAL ).
      ENDIF.

      IF gs_custo_elementar-tipo_variavel = 'P'.
        "Processo
        es_data-id_processo        = VALUE #( gt_list_proc[ gs_list_proc-index ]-id_processo OPTIONAL ).
      ENDIF.

      IF gs_custo_elementar-tipo_variavel = 'F'.
        "Trecho
        es_data-id_trecho        = VALUE #( gt_trecho[ gs_trecho-index ]-id_trecho OPTIONAL ).

        data(lt_trecho) = new /qaps/cl_mdl_logistica( )->get_trechos( ).
        data(ls_trecho) = value #( lt_trecho[ id_trecho = es_data-id_trecho ] OPTIONAL ).

        if not ls_trecho is INITIAL.
          es_data-tipo_origem = ls_trecho-tipo_origem.
          es_data-id_origem = ls_trecho-id_origem.
          es_data-tipo_destino = ls_trecho-tipo_destino.
          es_data-id_destino = ls_trecho-id_destino.
          es_data-id_modal = ls_trecho-id_modal.
        endif.

      ENDIF.


    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
