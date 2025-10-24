**----------------------------------------------------------------------*
****INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  SET_TIPO_REGRA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM set_tipo_regra .
*
**  IF gv_tr_geral = abap_true.
**    gv_tr_option = 1.
**  ELSEIF gv_tr_grupo_produto = abap_true.
**    gv_tr_option = 2.
**  ELSEIF gv_tr_mat_planejado = abap_true.
**    gv_tr_option = 3.
**  ELSEIF gv_tr_agregador = abap_true.
**    gv_tr_option = 4.
**  ELSEIF gv_tr_material = abap_true.
**    gv_tr_option = 5.
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SET_ORIGEM
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM set_origem .
*
**  IF gv_origem_ponto = abap_true.
**    gv_origem_option = 1.
**  ELSEIF gv_origem_regiao = abap_true.
**    gv_origem_option = 2.
**  ELSEIF gv_origem_grp_planta = abap_true.
**    gv_origem_option = 3.
**  ELSEIF gv_origem_porto = abap_true.
**    gv_origem_option = 4.
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SET_DESTINO
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM set_destino .
*
**  IF gv_destino_ponto = abap_true.
**    gv_destino_option = 1.
**  ELSEIF gv_destino_regiao = abap_true.
**    gv_destino_option = 2.
**  ELSEIF gv_destino_grp_planta = abap_true.
**    gv_destino_option = 3.
**  ELSEIF gv_destino_porto = abap_true.
**    gv_destino_option = 4.
**  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SCREEN_GENERAL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM screen_general .
*
*  SET PF-STATUS 'GENERAL'.
*  SET TITLEBAR '1000' WITH gv_tiltle.
**  PERFORM fill_listbox.
**  PERFORM fill_listarea.
**
*
*  LOOP AT SCREEN.
*
*    CHECK NOT screen-group1 IS INITIAL.
*    CASE gs_custo_elementar-tipo_variavel.
*      WHEN 'G'. "Geral
*        IF screen-group1 = 'TRN' OR screen-group1 = 'PRD' OR screen-group1 = 'PRC'.
*          screen-invisible = 1.
*          screen-active = 0.
*        ENDIF.
*      WHEN 'L'. "Logística
*        IF screen-group1 = 'PRD' OR screen-group1 = 'PRC'.
*          screen-invisible = 1.
*          screen-active = 0.
*        ENDIF.
*      WHEN 'P'. "Produtiva
*        IF screen-group1 = 'TRN' OR screen-group1 = 'ORI'.
*          screen-invisible = 1.
*          screen-active = 0.
*        ENDIF.
*    ENDCASE.
*
*    MODIFY SCREEN.
*
*  ENDLOOP.
*
*ENDFORM.
*FORM screen_tipo_regra.
*
*  LOOP AT SCREEN.
*
*    IF screen-name = 'GS_LIST_GP-INDEX' AND gv_tr_option = 2.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_GP-INDEX' AND gv_tr_option <> 2.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_MP-INDEX' AND gv_tr_option = 3.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_MP-INDEX' AND gv_tr_option <> 3.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option = 4.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option <> 4.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_DATA-MATNR' AND gv_tr_option = 5.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_DATA-MATNR' AND gv_tr_option <> 5.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
*FORM screen_origem.
*
*  LOOP AT SCREEN.
*
*    IF screen-name = 'GS_LIST_ORP-INDEX' AND gv_origem_option = 1.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_ORP-INDEX' AND gv_origem_option <> 1.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_ORR-INDEX' AND gv_origem_option = 2.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_ORR-INDEX' AND gv_origem_option <> 2.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_ORG-INDEX' AND gv_origem_option = 3.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_ORG-INDEX' AND gv_origem_option <> 3.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_ORPP-INDEX' AND gv_origem_option = 4.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_ORPP-INDEX' AND gv_origem_option <> 4.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
*FORM screen_destino.
*
*  LOOP AT SCREEN.
*
*    IF screen-name = 'GS_LIST_DSP-INDEX' AND gv_destino_option = 1.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_DSP-INDEX' AND gv_destino_option <> 1.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_DSR-INDEX' AND gv_destino_option = 2.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_DSR-INDEX' AND gv_destino_option <> 2.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_DSG-INDEX' AND gv_destino_option = 3.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_DSG-INDEX' AND gv_destino_option <> 3.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_LIST_DSPP-INDEX' AND gv_destino_option = 4.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ELSEIF screen-name = 'GS_LIST_DSPP-INDEX' AND gv_destino_option <> 4.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  FILL_LIST_BOXES
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM fill_list_boxes .
*
*  CHECK gv_loaded = abap_false.
*
*  "Grupo de Produtos
*  PERFORM fill_list_grupo_produtos.
*
*  "Material Planejado
*  PERFORM fill_list_mat_planejado.
*
*  "Agregador
*  PERFORM fill_list_agregador.
*
*  "Ponto
*  PERFORM fill_list_ponto.
*
*  "Região
*  PERFORM fill_list_regiao.
*
*  "Grupo Plantas
*  PERFORM fill_list_grupo_plantas.
*
*  "Grupo Plantas
*  PERFORM fill_list_porto.
*
*  "Modal
*  PERFORM fill_list_modal.
*
*  "Categ_Transp
*  PERFORM fill_list_categ_transp.
*
*  "Custo Processo
*  PERFORM fill_list_custo_processo.
*
*
*  gv_loaded = abap_true.
*
*ENDFORM.
*FORM fill_list_grupo_produtos.
*
*  SELECT id_grupo_produto, descricao
*    FROM /qaps/grp_prod
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_gp.
*
*  APPEND INITIAL LINE TO gt_list_grupo_produtos ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_gp ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_grupo_produtos ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_GP-INDEX'
*      values = gt_list_grupo_produtos.
*
*ENDFORM.
*FORM fill_list_mat_planejado.
*
*  SELECT DISTINCT mat_planejado
*    FROM /qaps/material
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_mp.
*
*  APPEND INITIAL LINE TO gt_list_mat_planejado ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_mp ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_mat_planejado ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-mat_planejado.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_MP-INDEX'
*      values = gt_list_mat_planejado.
*
*ENDFORM.
*FORM fill_list_agregador.
*
*  SELECT DISTINCT agregador
*    FROM /qaps/material
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_ag.
*
*  APPEND INITIAL LINE TO gt_list_agregador ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_ag ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_agregador ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-agregador.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_AG-INDEX'
*      values = gt_list_agregador.
*
*ENDFORM.
*FORM fill_list_ponto.
*
*  SELECT id_ponto,tipo_ponto,werks,kunnr,lifnr",cod_porto,porto
*    FROM /qaps/ponto
*    WHERE tipo_ponto <> 'P'
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_orp.
*
*  gt_list_dsp = gt_list_orp.
*
*  APPEND INITIAL LINE TO gt_list_origem_ponto ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
*  <fs_value_origem>-key = 0.
*  <fs_value_origem>-text = '(Selecionar)'.
*
*  APPEND INITIAL LINE TO gt_list_destino_ponto ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
*  <fs_value_destino>-key = 0.
*  <fs_value_destino>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_orp ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_origem_ponto ASSIGNING <fs_value_origem>.
*    APPEND INITIAL LINE TO gt_list_destino_ponto ASSIGNING <fs_value_destino>.
*    <fs_value_origem>-key = <fs>-index.
*    <fs_value_destino>-key = <fs>-index.
*
*    CASE <fs>-tipo_ponto.
*      WHEN 'W'.
*        <fs_value_origem>-text = <fs_value_destino>-text = <fs>-werks.
*      WHEN 'C'.
*        <fs_value_origem>-text = <fs_value_destino>-text = <fs>-kunnr.
*      WHEN 'F'.
*        <fs_value_origem>-text = <fs_value_destino>-text = <fs>-lifnr.
**      WHEN 'P'.
**        <fs_value_origem>-text = <fs_value_destino>-text = <fs>-cod_porto && ` ` && <fs>-porto.
*    ENDCASE.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_ORP-INDEX'
*      values = gt_list_origem_ponto.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_DSP-INDEX'
*      values = gt_list_destino_ponto.
*
*ENDFORM.
*FORM fill_list_regiao.
*
*  SELECT id_regiao,descricao
*    FROM /qaps/regiao_prc
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_orr.
*
*  gt_list_dsr = gt_list_orr.
*
*  APPEND INITIAL LINE TO gt_list_origem_regiao ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
*  <fs_value_origem>-key = 0.
*  <fs_value_origem>-text = '(Selecionar)'.
*
*  APPEND INITIAL LINE TO gt_list_destino_regiao ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
*  <fs_value_destino>-key = 0.
*  <fs_value_destino>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_orr ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_origem_regiao ASSIGNING <fs_value_origem>.
*    APPEND INITIAL LINE TO gt_list_destino_regiao ASSIGNING <fs_value_destino>.
*    <fs_value_origem>-key = <fs_value_destino>-key = <fs>-index.
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_ORR-INDEX'
*      values = gt_list_origem_regiao.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_DSR-INDEX'
*      values = gt_list_destino_regiao.
*
*ENDFORM.
*FORM fill_list_grupo_plantas.
*
*  SELECT DISTINCT id_grp_planta,descricao
*    FROM /qaps/grp_planta
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_org.
*
*  gt_list_dsg = gt_list_org.
*
*  APPEND INITIAL LINE TO gt_list_origem_grp_plan ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
*  <fs_value_origem>-key = 0.
*  <fs_value_origem>-text = '(Selecionar)'.
*
*  APPEND INITIAL LINE TO gt_list_destino_grp_plan ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
*  <fs_value_destino>-key = 0.
*  <fs_value_destino>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_org ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_origem_grp_plan ASSIGNING <fs_value_origem>.
*    APPEND INITIAL LINE TO gt_list_destino_grp_plan ASSIGNING <fs_value_destino>.
*    <fs_value_origem>-key = <fs_value_destino>-key = <fs>-index.
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_ORG-INDEX'
*      values = gt_list_origem_grp_plan.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_DSG-INDEX'
*      values = gt_list_destino_grp_plan.
*
*ENDFORM.
*FORM fill_list_porto.
*
*  SELECT *
*    FROM /qaps/ponto
*    WHERE tipo_ponto = 'P'
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_orpp.
*
*  gt_list_dspp = gt_list_orpp.
*
*  APPEND INITIAL LINE TO gt_list_origem_porto ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
*  <fs_value_origem>-key = 0.
*  <fs_value_origem>-text = '(Selecionar)'.
*
*  APPEND INITIAL LINE TO gt_list_destino_porto ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
*  <fs_value_destino>-key = 0.
*  <fs_value_destino>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_orpp ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_origem_porto ASSIGNING <fs_value_origem>.
*    APPEND INITIAL LINE TO gt_list_destino_porto ASSIGNING <fs_value_destino>.
*    <fs_value_origem>-key = <fs>-index.
*    <fs_value_destino>-key = <fs>-index.
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-cod_porto && ` ` && <fs>-porto.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_ORPP-INDEX'
*      values = gt_list_origem_porto.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_DSPP-INDEX'
*      values = gt_list_destino_porto.
*
*ENDFORM.
*FORM fill_list_modal.
*
*  DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
*
*  gt_list_mod = VALUE #( FOR wa IN lt_modal
*                                 ( id_modal = wa-domvalue_l
*                                   modal = wa-ddtext ) ).
*
*  APPEND INITIAL LINE TO gt_list_modal ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_mod ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_modal ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-modal.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_MOD-INDEX'
*      values = gt_list_modal.
*
*ENDFORM.
*FORM fill_list_categ_transp.
*
*  SELECT id_categoria, descricao
*      FROM /qaps/categ_trns
*      INTO CORRESPONDING FIELDS OF TABLE @gt_list_cattr.
*
*  APPEND INITIAL LINE TO gt_list_categ ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_cattr ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_categ ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_CATTR-INDEX'
*      values = gt_list_categ.
*
*ENDFORM.
*FORM fill_list_custo_processo.
*
*  SELECT id_processo, descricao
*      FROM /qaps/custo_prc
*      INTO CORRESPONDING FIELDS OF TABLE @gt_list_proc.
*
*  APPEND INITIAL LINE TO gt_list_processo ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_proc ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_processo ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_PROC-INDEX'
*      values = gt_list_processo.
*
*ENDFORM.
