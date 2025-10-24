*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_TIPO_REGRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_tipo_regra .

  IF gv_tr_geral = abap_true.
    gv_tr_option = 1.
  ELSEIF gv_tr_grupo_produto = abap_true.
    gv_tr_option = 2.
  ELSEIF gv_tr_mat_planejado = abap_true.
    gv_tr_option = 3.
  ELSEIF gv_tr_agregador = abap_true.
    gv_tr_option = 4.
  ELSEIF gv_tr_material = abap_true.
    gv_tr_option = 5.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_ORIGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_origem .

  PERFORM fill_origem.
*  IF gv_origem_ponto = abap_true.
*    gv_origem_option = 1.
*  ELSEIF gv_origem_regiao = abap_true.
*    gv_origem_option = 2.
*  ELSEIF gv_origem_grp_planta = abap_true.
*    gv_origem_option = 3.
*  ELSEIF gv_origem_porto = abap_true.
*    gv_origem_option = 4.
*  ELSEIF gv_origem_cidade = abap_true.
*    gv_origem_option = 5.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_DESTINO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_destino .

  PERFORM fill_destino.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SCREEN_GENERAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_general .

  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000' WITH gv_tiltle.
*  PERFORM fill_listbox.
*  PERFORM fill_listarea.
*

  LOOP AT SCREEN.

    CHECK NOT screen-group1 IS INITIAL.
    CASE gs_custo_elementar-tipo_variavel.
      WHEN 'G'. "Geral
        IF screen-group1 = 'TRN' OR screen-group1 = 'PRD'
          OR screen-group1 = 'PRC' OR screen-group1 = 'FRE'.
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
      WHEN 'C'. "Compra
        IF screen-group1 = 'TRN' OR screen-group1 = 'PRD'
          OR screen-group1 = 'PRC' OR screen-group1 = 'FRE'.
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
      WHEN 'L'. "Logística
        IF screen-group1 = 'PRD' OR screen-group1 = 'PRC'
            OR screen-group1 = 'FRE'.
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
      WHEN 'P'. "Produtiva
        IF screen-group1 = 'TRN' OR screen-group1 = 'ORI'
            OR screen-group1 = 'FRE'.
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
      WHEN 'F'. "Frete
        IF screen-group1 = 'TRN' OR screen-group1 = 'ORI'
            OR screen-group1 = 'DES' OR screen-group1 = 'PRC' .
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
FORM screen_tipo_regra.

  LOOP AT SCREEN.

    IF screen-name = 'GS_LIST_GP-INDEX' AND gv_tr_option = 2.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_LIST_GP-INDEX' AND gv_tr_option <> 2.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'GS_LIST_MP-INDEX' AND gv_tr_option = 3.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_LIST_MP-INDEX' AND gv_tr_option <> 3.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option = 4.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option <> 4.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'GS_DATA-MATNR' AND gv_tr_option = 5.
      screen-input = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_DATA-MATNR' AND gv_tr_option <> 5.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM screen_origem.

  LOOP AT SCREEN.

    CHECK screen-name = 'GS_TIPO_ORIGEM-INDEX' OR screen-name = 'LBL_ORIGEM'.

    IF gv_origem_geral = abap_true.
      screen-active = 0.
      screen-input = 0.
      MODIFY SCREEN.
    ELSE.
      screen-active = 1.
      screen-input = 1.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM screen_destino.

  LOOP AT SCREEN.

    CHECK screen-name = 'GS_TIPO_DESTINO-INDEX' OR screen-name = 'LBL_DESTINO'.

    IF gv_destino_geral = abap_true.
      screen-active = 0.
      screen-input = 0.
      MODIFY SCREEN.
    ELSE.
      screen-active = 1.
      screen-input = 1.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LIST_BOXES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes .

  DATA: lt_ids TYPE vrm_ids,
        lv_id  TYPE vrm_id.

  CHECK gv_loaded = abap_false.

  REFRESH: gt_list_grupo_produtos  ,
           gt_list_mat_planejado   ,
           gt_list_agregador       ,
           gt_list_modal     ,
           gt_list_categ    ,
           gt_list_processo.

  "Grupo de Produtos
  PERFORM fill_list_grupo_produtos.

  "Material Planejado
  PERFORM fill_list_mat_planejado.

  "Agregador
  PERFORM fill_list_agregador.

  "Modal
  PERFORM fill_list_modal.

  "Categ_Transp
  PERFORM fill_list_categ_transp.

  "Custo Processo
  PERFORM fill_list_custo_processo.

  "Custo Processo
  PERFORM fill_list_trecho.

  gv_loaded = abap_true.

ENDFORM.
FORM fill_list_grupo_produtos.

  REFRESH gt_list_grupo_produtos.

  SELECT id_grupo_produto, descricao
    FROM /qaps/grp_prod
    INTO CORRESPONDING FIELDS OF TABLE @gt_list_gp.

  APPEND INITIAL LINE TO gt_list_grupo_produtos ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_gp ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_grupo_produtos ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-descricao.

  ENDLOOP.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_GP-INDEX'
      values = gt_list_grupo_produtos.

ENDFORM.
FORM fill_list_mat_planejado.

  REFRESH gt_list_mat_planejado.

  SELECT DISTINCT mat_planejado
    FROM /qaps/material
    INTO CORRESPONDING FIELDS OF TABLE @gt_list_mp.

  APPEND INITIAL LINE TO gt_list_mat_planejado ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_mp ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_mat_planejado ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-mat_planejado.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_MP-INDEX'
      values = gt_list_mat_planejado.

ENDFORM.
FORM fill_list_agregador.

  REFRESH gt_list_agregador.

  SELECT DISTINCT agregador
    FROM /qaps/material
    INTO CORRESPONDING FIELDS OF TABLE @gt_list_ag.

  APPEND INITIAL LINE TO gt_list_agregador ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_ag ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_agregador ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-agregador.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_AG-INDEX'
      values = gt_list_agregador.

ENDFORM.
FORM fill_origem.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.

  REFRESH gt_list_tipo_origem.

  IF gv_origem_geral = abap_true.
    CLEAR lv_tipo.
  ELSEIF gv_origem_regiao = abap_true.
    lv_tipo = 'R'.
  ELSEIF gv_origem_cidade = abap_true.
    lv_tipo = 'C'.
  ELSEIF gv_origem_grp_planta = abap_true.
    lv_tipo = 'G'.
  ELSEIF gv_origem_centro = abap_true.
    lv_tipo = 'W'.
  ELSEIF gv_origem_cliente = abap_true.
    lv_tipo = 'K'.
  ELSEIF gv_origem_fornec = abap_true.
    lv_tipo = 'F'.
  ELSEIF gv_origem_porto = abap_true.
    lv_tipo = 'P'.
  ELSEIF gv_origem_term_port = abap_true.
    lv_tipo = 'I'.
  ENDIF.

  SELECT *
    FROM /qaps/v_ponto
    WHERE tipo_ponto = @lv_tipo
    INTO CORRESPONDING FIELDS OF TABLE @gt_tipo_origem.

  APPEND INITIAL LINE TO gt_list_tipo_origem ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
  <fs_value_origem>-key = 0.
  <fs_value_origem>-text = '(Selecionar)'.

  LOOP AT gt_tipo_origem ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_tipo_origem ASSIGNING <fs_value_origem>.

    <fs_value_origem>-key = <fs>-index.
    DATA(lv_codigo) = |{ <fs>-codigo ALPHA = OUT }|.
    CONDENSE lv_codigo.
    lv_text =  |{ lv_codigo } - { <fs>-descricao }|.
    <fs_value_origem>-text = lv_text.

  ENDLOOP.

  SORT: gt_list_tipo_origem BY text.

  gs_tipo_origem-index = 0.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TIPO_ORIGEM-INDEX'
      values = gt_list_tipo_origem.



ENDFORM.
FORM reset_options.

  gv_tr_option = '1'.
  gv_tr_geral = abap_true.

  REFRESH: gt_list_gp     ,
           gt_list_mp     ,
           gt_list_ag     ,
           gt_tipo_origem ,
           gt_tipo_destino,
           gt_list_mod    ,
           gt_list_cattr  ,
           gt_list_proc   ,
           gt_trecho      .

  CLEAR: gs_list_gp     ,
         gs_list_mp     ,
         gs_list_ag     ,
         gs_tipo_origem ,
         gs_tipo_destino,
         gs_list_mod    ,
         gs_list_cattr  ,
         gs_list_proc   ,
         gs_trecho      .

  CLEAR: gs_list_gp,
         gs_list_mp,
         gs_list_mt,
         gs_list_ag.

  CLEAR: gv_tr_grupo_produto,
         gv_tr_mat_planejado,
         gv_tr_agregador,
         gv_tr_material.

  gv_origem_geral = abap_true.
  CLEAR: gv_origem_regiao,
         gv_origem_cidade,
         gv_origem_grp_planta,
         gv_origem_centro,
         gv_origem_cliente,
         gv_origem_fornec,
         gv_origem_porto,
         gv_origem_term_port.

  gv_destino_geral = abap_true.
  CLEAR: gv_destino_regiao,
         gv_destino_cidade,
         gv_destino_grp_planta,
         gv_destino_centro,
         gv_destino_cliente,
         gv_destino_fornec,
         gv_destino_porto,
         gv_destino_term_port.

ENDFORM.
FORM fill_destino.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.

  REFRESH gt_list_tipo_destino.

  IF gv_destino_geral = abap_true.
    CLEAR lv_tipo.
  ELSEIF gv_destino_regiao = abap_true.
    lv_tipo = 'R'.
  ELSEIF gv_destino_cidade = abap_true.
    lv_tipo = 'C'.
  ELSEIF gv_destino_grp_planta = abap_true.
    lv_tipo = 'G'.
  ELSEIF gv_destino_centro = abap_true.
    lv_tipo = 'W'.
  ELSEIF gv_destino_cliente = abap_true.
    lv_tipo = 'K'.
  ELSEIF gv_destino_fornec = abap_true.
    lv_tipo = 'F'.
  ELSEIF gv_destino_porto = abap_true.
    lv_tipo = 'P'.
  ELSEIF gv_destino_term_port = abap_true.
    lv_tipo = 'I'.
  ENDIF.

  SELECT *
    FROM /qaps/v_ponto
    WHERE tipo_ponto = @lv_tipo
    INTO CORRESPONDING FIELDS OF TABLE @gt_tipo_destino.

*  gt_list_dsp = gt_list_orp.

  APPEND INITIAL LINE TO gt_list_tipo_destino ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
  <fs_value_destino>-key = 0.
  <fs_value_destino>-text = '(Selecionar)'.

  LOOP AT gt_tipo_destino ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_tipo_destino ASSIGNING <fs_value_destino>.

    <fs_value_destino>-key = <fs>-index.
    DATA(lv_codigo) = |{ <fs>-codigo ALPHA = OUT }|.
    CONDENSE lv_codigo.
    lv_text =  |{ lv_codigo } - { <fs>-descricao }|.
    <fs_value_destino>-text = lv_text.

  ENDLOOP.

  SORT: gt_list_tipo_destino BY text.

  gs_tipo_destino-index = 0.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TIPO_DESTINO-INDEX'
      values = gt_list_tipo_destino.



ENDFORM.
*FORM fill_list_regiao.
*
*  REFRESH gt_list_origem_regiao.
*
*  SELECT id_regiao,codigo,descricao
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
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-codigo && ` - ` && <fs>-descricao.
*
*  ENDLOOP.
*
*  SORT: gt_list_origem_regiao BY text ASCENDING,
*        gt_list_destino_regiao BY text ASCENDING.
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
*  REFRESH gt_list_origem_grp_plan.
*
*  SELECT DISTINCT id_grp_planta,codigo, descricao
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
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-codigo && ` - ` && <fs>-descricao.
*
*  ENDLOOP.
*
*  SORT: gt_list_origem_grp_plan BY text ASCENDING,
*        gt_list_destino_grp_plan BY text ASCENDING.
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
*  REFRESH gt_list_origem_porto.
*
*  SELECT *
*    FROM /qaps/porto
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
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-cod_porto && ` - ` && <fs>-porto.
*
*  ENDLOOP.
*
*  SORT: gt_list_origem_porto BY text ASCENDING,
*        gt_list_destino_porto BY text ASCENDING.
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
*FORM fill_list_cidade.
*
*  REFRESH gt_list_origem_cidade.
*
*  SELECT *
*    FROM /qaps/cidade
*    INTO CORRESPONDING FIELDS OF TABLE @gt_list_orpc.
*
*  gt_list_dspc = gt_list_orpc.
*
*  APPEND INITIAL LINE TO gt_list_origem_cidade ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
*  <fs_value_origem>-key = 0.
*  <fs_value_origem>-text = '(Selecionar)'.
*
*  APPEND INITIAL LINE TO gt_list_destino_cidade ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
*  <fs_value_destino>-key = 0.
*  <fs_value_destino>-text = '(Selecionar)'.
*
*  LOOP AT gt_list_orpc ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_origem_cidade ASSIGNING <fs_value_origem>.
*    APPEND INITIAL LINE TO gt_list_destino_cidade ASSIGNING <fs_value_destino>.
*    <fs_value_origem>-key = <fs>-index.
*    <fs_value_destino>-key = <fs>-index.
*    <fs_value_origem>-text = <fs_value_destino>-text = <fs>-cidade && ` - ` && <fs>-uf.
*
*  ENDLOOP.
*
*  SORT: gt_list_origem_cidade BY text ASCENDING,
*        gt_list_destino_cidade BY text ASCENDING.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_ORPC-INDEX'
*      values = gt_list_origem_cidade.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_DSPC-INDEX'
*      values = gt_list_destino_cidade.
*
*ENDFORM.
FORM fill_list_modal.

  REFRESH gt_list_modal.

  DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).

  gt_list_mod = VALUE #( FOR wa IN lt_modal
                                 ( id_modal = wa-domvalue_l
                                   modal = wa-ddtext ) ).

  APPEND INITIAL LINE TO gt_list_modal ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_mod ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_modal ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-modal.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_MOD-INDEX'
      values = gt_list_modal.

ENDFORM.
FORM fill_list_categ_transp.

  REFRESH gt_list_categ.

  SELECT id_categoria, descricao
      FROM /qaps/categ_trns
      INTO CORRESPONDING FIELDS OF TABLE @gt_list_cattr.

  APPEND INITIAL LINE TO gt_list_categ ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_cattr ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_categ ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-descricao.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_CATTR-INDEX'
      values = gt_list_categ.

ENDFORM.
FORM fill_list_custo_processo.

  REFRESH gt_list_processo.

  SELECT id_processo, descricao
      FROM /qaps/custo_prc
      INTO CORRESPONDING FIELDS OF TABLE @gt_list_proc.

  APPEND INITIAL LINE TO gt_list_processo ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_list_proc ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_processo ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-descricao.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_PROC-INDEX'
      values = gt_list_processo.

ENDFORM.
FORM fill_list_trecho.

  DATA lv_trecho TYPE /qaps/trecho-cod_trecho.

  REFRESH: gt_trecho,
           gt_list_trecho.

  DATA(lt_trechos) = NEW /qaps/cl_mdl_logistica( )->get_trechos( ).

  SORT lt_trechos BY cod_trecho.

  LOOP AT lt_trechos INTO DATA(ls_trecho).

    lv_trecho = ls_trecho-cod_trecho.

    SHIFT lv_trecho LEFT DELETING LEADING '0'.

    DATA(lv_descricao) = lv_trecho && ` [` && ls_trecho-dsc_tipo_origem && `] ` && ls_trecho-dsc_origem.
    lv_descricao = lv_descricao && ` -> [` && ls_trecho-dsc_tipo_destino && `] ` && ls_trecho-dsc_destino.
    lv_descricao = lv_descricao && ` - [` && ls_trecho-dsc_modal && `]`.
    APPEND VALUE ts_trecho(
        id_trecho = ls_trecho-id_trecho
        descricao = lv_descricao
*        index     = sy-tabix
    ) TO gt_trecho.
  ENDLOOP.
*  gt_trecho = CORRESPONDING #( lt_trechos ).

  APPEND INITIAL LINE TO gt_list_processo ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_trecho ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_trecho ASSIGNING <fs_value>.
    <fs_value>-key = <fs>-index.
    <fs_value>-text = <fs>-descricao.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TRECHO-INDEX'
      values = gt_list_trecho.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  IF gv_origem_geral = abap_false AND gs_tipo_origem-index = 0.
    MESSAGE 'Origem é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_destino_geral = abap_false AND gs_tipo_destino-index = 0.
    MESSAGE 'Destino é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.
