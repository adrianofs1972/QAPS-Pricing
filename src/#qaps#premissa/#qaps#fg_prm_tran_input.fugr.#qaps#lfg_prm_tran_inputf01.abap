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

*  LOOP AT SCREEN.
*
*    CHECK NOT screen-group1 IS INITIAL.
**    CASE gs_custo_elementar-tipo_variavel.
**      WHEN 'G'. "Geral
**        IF screen-group1 = 'TRN' OR screen-group1 = 'PRD'
**          OR screen-group1 = 'PRC' OR screen-group1 = 'FRE'.
**          screen-invisible = 1.
**          screen-active = 0.
**        ENDIF.
**      WHEN 'L'. "Logística
**        IF screen-group1 = 'PRD' OR screen-group1 = 'PRC'
**            OR screen-group1 = 'FRE'.
**          screen-invisible = 1.
**          screen-active = 0.
**        ENDIF.
**      WHEN 'P'. "Produtiva
**        IF screen-group1 = 'TRN' OR screen-group1 = 'ORI'
**            OR screen-group1 = 'FRE'.
**          screen-invisible = 1.
**          screen-active = 0.
**        ENDIF.
**      WHEN 'F'. "Frete
**        IF screen-group1 = 'TRN' OR screen-group1 = 'ORI'
**            OR screen-group1 = 'DES' OR screen-group1 = 'PRC' .
**          screen-invisible = 1.
**          screen-active = 0.
**        ENDIF.
**    ENDCASE.
*
*    MODIFY SCREEN.
*
*  ENDLOOP.

ENDFORM.
FORM screen_tipo_regra.

  LOOP AT SCREEN.

    IF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option = 4.
      screen-input = 1.
*      screen-required = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_LIST_AG-INDEX' AND gv_tr_option <> 4.
      screen-input = 0.
*      screen-required = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'GS_DATA-MATNR' AND gv_tr_option = 5.
      screen-input = 1.
*      screen-required = 1.
      MODIFY SCREEN.
    ELSEIF screen-name = 'GS_DATA-MATNR' AND gv_tr_option <> 5.
      screen-input = 0.
*      screen-required = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.
FORM screen_origem.

*  LOOP AT SCREEN.
*
*    CHECK screen-name = 'GS_TIPO_ORIGEM-INDEX' OR screen-name = 'LBL_ORIGEM'.
*
*    IF gv_origem_geral = abap_true.
*      screen-active = 0.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ELSE.
*      screen-active = 1.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ENDIF.
*
*  ENDLOOP.

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
*  PERFORM fill_list_grupo_produtos.
*
*  "Material Planejado
*  PERFORM fill_list_mat_planejado.

  "Agregador
*  PERFORM fill_list_agregador.

*  "Modal
*  PERFORM fill_list_modal.
*
*  "Categ_Transp
*  PERFORM fill_list_categ_transp.
*
*  "Custo Processo
*  PERFORM fill_list_custo_processo.
*
*  "Custo Processo
*  PERFORM fill_list_trecho.

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

  REFRESH: gt_list_agregador,
           gt_list_ag.

  TRY.
      DATA(ls_origem) = VALUE #( gt_tipo_origem[ index =  gs_tipo_origem-index ]  ).
*      BREAK c060863.
      IF NOT gv_origem_grp_planta IS INITIAL.

        SELECT DISTINCT
*          hdr~id_premissa,hdr~id_grp_planta,hdr~id_centro,
          itm~agregador
          FROM /qaps/prem_hdr AS hdr
          INNER JOIN /qaps/prem_item AS itm
          ON hdr~id_premissa = itm~id_premissa
          WHERE itm~tipo_regra = 'AG'
          AND   hdr~id_grp_planta = @ls_origem-id_externo
          AND   hdr~id_centro     = @gc_guid_null
          and   hdr~id_simulacao  = @gs_simulacao-id_simulacao
          INTO CORRESPONDING FIELDS OF TABLE @gt_list_ag.

      ELSE.

        SELECT DISTINCT
*          hdr~id_premissa,hdr~id_grp_planta,hdr~id_centro,
          itm~agregador
          FROM /qaps/prem_hdr AS hdr
          INNER JOIN /qaps/prem_item AS itm
          ON hdr~id_premissa = itm~id_premissa
          WHERE itm~tipo_regra = 'AG'
          AND   hdr~id_centro = @ls_origem-id_externo
*          AND   hdr~id_centro     = @gc_guid_null
          and   hdr~id_simulacao  = @gs_simulacao-id_simulacao
          INTO CORRESPONDING FIELDS OF TABLE @gt_list_ag.

      ENDIF.

      APPEND INITIAL LINE TO gt_list_agregador ASSIGNING FIELD-SYMBOL(<fs_value>).
      <fs_value>-key = 0.
      <fs_value>-text = '(Selecionar)'.

      LOOP AT gt_list_ag ASSIGNING FIELD-SYMBOL(<fs>).

        <fs>-index = sy-tabix.

        APPEND INITIAL LINE TO gt_list_agregador ASSIGNING <fs_value>.
        <fs_value>-key = <fs>-index.
        <fs_value>-text = <fs>-agregador.

      ENDLOOP.

    CATCH cx_sy_itab_line_not_found.

  ENDTRY.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_AG-INDEX'
      values = gt_list_agregador.

ENDFORM.
FORM fill_origem.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.
  DATA lr_id_ponto TYPE RANGE OF /qaps/v_ponto-id_ponto.

  REFRESH gt_list_tipo_origem.

  IF gv_origem_grp_planta = abap_true.
    lv_tipo = 'G'.

    SELECT *
      FROM /qaps/prem_hdr
      WHERE id_simulacao = @gs_simulacao-id_simulacao
      INTO TABLE @DATA(lt_header).

    LOOP AT lt_header INTO DATA(ls_header).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_header-id_grp_planta ) TO lr_id_ponto.
    ENDLOOP.

  ELSEIF gv_origem_centro = abap_true.
    lv_tipo = 'W'.

    SELECT *
      FROM /qaps/prem_hdr
      WHERE id_simulacao = @gs_simulacao-id_simulacao
      INTO TABLE @lt_header.

    SELECT *
      FROM /qaps/centro
      FOR ALL ENTRIES IN @lt_header
      WHERE id_grp_planta = @lt_header-id_grp_planta
      INTO TABLE @DATA(lt_centro).

    LOOP AT lt_centro INTO DATA(ls_centro).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_centro-id_centro ) TO lr_id_ponto.
    ENDLOOP.

  ENDIF.

  SELECT *
    FROM /qaps/v_ponto
    WHERE tipo_ponto = @lv_tipo
    AND   id_externo IN @lr_id_ponto
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
FORM fill_destino.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.

  REFRESH gt_list_tipo_destino.

  IF gv_destino_grp_planta = abap_true.
    lv_tipo = 'G'.
  ELSEIF gv_destino_centro = abap_true.
    lv_tipo = 'W'.
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

  gt_list_mod = VALUE #( FOR wa1 IN lt_modal
                                 ( id_modal = wa1-domvalue_l
                                   modal = wa1-ddtext ) ).

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
*FORM fill_list_categ_transp.
*
*  REFRESH gt_list_categ.
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
*  REFRESH gt_list_processo.
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
*FORM fill_list_trecho.
*
*  DATA lv_trecho TYPE /qaps/trecho-cod_trecho.
*
*  REFRESH: gt_trecho,
*           gt_list_trecho.
*
*  DATA(lt_trechos) = NEW /qaps/cl_mdl_logistica( )->get_trechos( ).
*
*  SORT lt_trechos BY cod_trecho.
*
*  LOOP AT lt_trechos INTO DATA(ls_trecho).
*
*    lv_trecho = ls_trecho-cod_trecho.
*
*    SHIFT lv_trecho LEFT DELETING LEADING '0'.
*
*    DATA(lv_descricao) = lv_trecho && ` [` && ls_trecho-dsc_tipo_origem && `] ` && ls_trecho-dsc_origem.
*    lv_descricao = lv_descricao && ` -> [` && ls_trecho-dsc_tipo_destino && `] ` && ls_trecho-dsc_destino.
*    lv_descricao = lv_descricao && ` - [` && ls_trecho-dsc_modal && `]`.
*    APPEND VALUE ts_trecho(
*        id_trecho = ls_trecho-id_trecho
*        descricao = lv_descricao
**        index     = sy-tabix
*    ) TO gt_trecho.
*  ENDLOOP.
**  gt_trecho = CORRESPONDING #( lt_trechos ).
*
*  APPEND INITIAL LINE TO gt_list_processo ASSIGNING FIELD-SYMBOL(<fs_value>).
*  <fs_value>-key = 0.
*  <fs_value>-text = '(Selecionar)'.
*
*  LOOP AT gt_trecho ASSIGNING FIELD-SYMBOL(<fs>).
*
*    <fs>-index = sy-tabix.
*
*    APPEND INITIAL LINE TO gt_list_trecho ASSIGNING <fs_value>.
*    <fs_value>-key = <fs>-index.
*    <fs_value>-text = <fs>-descricao.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_TRECHO-INDEX'
*      values = gt_list_trecho.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  IF gv_tr_agregador = 'X' AND gs_list_ag-index = 0.
    MESSAGE 'Agregador é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_tr_material = 'X' AND gs_data-matnr IS INITIAL.
    MESSAGE 'Material é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gs_tipo_origem-index = 0.
    MESSAGE 'Origem é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_destino_geral = abap_false AND gs_tipo_destino-index = 0.
    MESSAGE 'Destino é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(ls_origem) = VALUE #( gt_tipo_origem[ index =  gs_tipo_origem-index ] OPTIONAL ).
  DATA(ls_destino) = VALUE #( gt_tipo_destino[ index =  gs_tipo_destino-index ] OPTIONAL ).

  IF ls_origem-tipo_ponto = ls_destino-tipo_ponto.
    IF ls_origem-codigo     = ls_destino-codigo.
      MESSAGE 'Origem e Destino devem ser diferentes' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ELSE.

    IF ls_origem-tipo_ponto = 'G'.
      SELECT * FROM /qaps/v_centro WHERE id_grp_planta = @ls_origem-id_externo
        INTO TABLE @DATA(lt_origem).

      IF line_exists( lt_origem[ werks = ls_destino-codigo ] ).
        MESSAGE 'Origem ou Destino não podem estar contido no outro' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    IF ls_destino-tipo_ponto = 'G'.
      SELECT * FROM /qaps/v_centro WHERE id_grp_planta = @ls_destino-id_externo
        INTO TABLE @DATA(lt_destino).

      IF line_exists( lt_destino[ werks = ls_origem-codigo ] ).
        MESSAGE 'Origem ou Destino não podem estar contido no outro' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.
