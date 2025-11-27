class /QAPS/CL_RULE_SINCRONIZACAO definition
  public
  final
  create public .

public section.

  methods EXECUTE
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
      !IV_ID_SIMUL_REF type /QAPS/ED_ID_SIMULACAO optional .
  methods CONSTRUCTOR .
  methods SINCRONIZAR_INPUT_CREATE
    importing
      !IS_DATA type /QAPS/VAR_INPUT .
  methods SINCRONIZAR_INPUT_DELETE
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IR_ID_VAR_INPUT type /QAPS/R_ID_VAR_INPUT
    returning
      value(RETURN) type ABAP_BOOL .
  methods SINCRONIZAR_STD_PRD_ASSIGN
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IS_DATA type /QAPS/SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods SINCRONIZAR_STD_PRD_CHANGE .
  methods SINCRONIZAR_STD_PRD_UNASSIGN
    importing
      !IS_DATA type /QAPS/SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods SINCRONIZAR_TRAJETO_DELETE
    importing
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO
      !IV_CHECK type ABAP_BOOL
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_TRAJETO_BY_ID
    importing
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO
    returning
      value(RETURN) type /QAPS/TRAJETO .
  methods SINCRONIZAR_TRAJETO_CREATE
    importing
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO
    returning
      value(RETURN) type ABAP_BOOL .
protected section.
private section.

  data MO_MATERIAL type ref to /QAPS/CL_MDL_MATERIAL .
  data MS_SIMULACAO type /QAPS/S_SIMULACAO .
  data MT_TRANSFERENCIAS type /QAPS/T_PRM_FULL .
  constants MC_GUID_NULL type GUID16 value '00000000000000000000000000000000' ##NO_TEXT.
  data MO_CUSTO_ELEMENTAR type ref to /QAPS/CL_MDL_CUSTO_ELEMENTAR .
  data MO_SIMULACAO type ref to /QAPS/CL_MDL_SIMULACAO .
  data MS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR .

  methods CHECK_RELATION
    changing
      !CS_DATA type /QAPS/S_RELATION_STD_PRD .
  methods EXPAND_GRP_PLANTA_TO_WERKS_DST
    importing
      !IT_DATA type /QAPS/T_RELATION_STD_PRD_HDR
    returning
      value(RETURN) type /QAPS/T_RELATION_STD_PRD_HDR .
  methods EXPAND_GRP_PLANTA_TO_WERKS_MAT
    importing
      !IS_DATA type /QAPS/S_RELATION_STD_PRD_ITEM
    returning
      value(RETURN) type /QAPS/T_RELATION_STD_PRD_ITEM .
  methods SEND_TRANSFER_TO_MEMORY
    importing
      !IV_ID_SIMUL_REF type /QAPS/ED_ID_SIMULACAO optional .
  methods SHOW_PROGRESS
    importing
      !IV_MESSAGE type STRING
      !IV_PERCENTUAL type I .
  methods REDO_TRANSFER .
  methods CREATE_STD_PRODUCAO .
  methods UPDATE_PERCENTUAL_TRAJETO .
  methods UPDATE_PERCENTUAL_PREMISSA .
  methods UPDATE_PERCENTUAL .
  methods GET_PONTO_BY_ID
    importing
      !IV_ID_PONTO type /QAPS/ID_PONTO optional
      !IV_ID_EXTERNO type /QAPS/ID_EXTERNO optional
    returning
      value(RETURN) type /QAPS/V_PONTO .
  methods CREATE_IMPORTACAO .
  methods CREATE_COMPRA_NACIONAL .
  methods CREATE_COMPRA_IMPORTADA .
  methods CLEAN_DATA_PREMISSA .
  methods CLEAN_DATA_MATRIZ .
  methods CLEAN_DATA .
  methods CHOOSE_SIMULACAO
    returning
      value(RETURN) type /QAPS/ED_ID_SIMULACAO
    raising
      /QAPS/CX_PRICING_ERROR .
  methods APPEND_NEW_TRAJETO
    importing
      value(IS_TEMPLATE) type /QAPS/V_PRM_FULL
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO .
  methods GET_PREMISSA_HEADER_ITEM
    returning
      value(RETURN) type /QAPS/T_V_PRM_HD_I .
  methods GET_PREMISSA_FULL
    importing
      !IV_FILL_ZEROS type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_PRM_FULL .
  methods GET_HIERARQUIA_DESTINO
    changing
      !CT_HIERARQUIA_DESTINO type /QAPS/T_HIERARQUIA_DESTINO .
  methods GET_HIERARQUIA_ORIGEM
    changing
      !CT_HIERARQUIA_ORIGEM type /QAPS/T_HIERARQUIA_ORIGEM .
  methods HERDAR_ORIGEM_FROM_MATRIZ_ABS
    importing
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_ORIGEM type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods UPDATE_LINK_MATRIZ_PREM_DISTR
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
      !IV_ID_DISTR_MATRIZ type /QAPS/ED_ID_MATRIZ_DISTR .
  methods UPDATE_LINK_MATRIZ_PREM_ITEM
    importing
      !IV_ID_ITEM type /QAPS/ED_ID_ITEM
      !IV_ID_ITEM_MATRIZ type /QAPS/ED_ID_ITEM .
  methods UPDATE_LINK_MATRIZ_PREM_HDR
    importing
      !IV_ID_PREMISSA type /QAPS/ED_ID_PREMISSA
      !IV_ID_MATRIZ_ABAST type /QAPS/ED_ID_MATRIZ_ABAST .
  methods UPDATE_PERCENTUAL_AUTO_PREM .
  methods UPDATE_PERCENTUAL_AUTOMATICO .
  methods CHECK_RELATION_MATRIZ
    importing
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IS_DATA type /QAPS/V_COST_INP
      !IS_DESTINO type /QAPS/V_DIST_LOG
    returning
      value(RETURN) type /QAPS/S_RELATION_MATRIZ_PREM .
  methods CHECK_RELATION_PREMISSA
    importing
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
    returning
      value(RETURN) type /QAPS/S_RELATION .
  methods CHECK_RELATION_STD_PRD
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IS_DATA type /QAPS/S_SIMULACAO
    returning
      value(RETURN) type /QAPS/S_RELATION_STD_PRD .
  methods APPEND_MATRIZ
    importing
      !IS_RELATION type /QAPS/S_RELATION_MATRIZ_PREM
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE .
  methods CREATE_MATRIZ
    importing
      !IS_RELATION type /QAPS/S_RELATION_MATRIZ_PREM
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE .
  methods CREATE_MATRIZ_DESTINO
    importing
      !IS_RELATION type /QAPS/S_RELATION_MATRIZ_PREM
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IT_PARENT_MATRIZ type /QAPS/T_CUSTO_MATRIZ optional
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods CREATE_MATRIZ_MATERIAL
    importing
      !IS_RELATION type /QAPS/S_RELATION_MATRIZ_PREM
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IS_DATA type /QAPS/V_COST_INP
      !IT_PARENT_MATRIZ type /QAPS/T_CUSTO_MATRIZ optional
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods CREATE_MATRIZ_ORIGEM
    importing
      !IS_RELATION type /QAPS/S_RELATION_MATRIZ_PREM
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IT_PARENT_MATRIZ type /QAPS/T_CUSTO_MATRIZ optional
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods CREATE_MATRIZ_RELATION
    importing
      !IS_DESTINO type /QAPS/V_DIST_LOG
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_ORIGEM type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CREATE_PREMISSA
    importing
      !IS_RELATION type /QAPS/S_RELATION
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE .
  methods CREATE_PREMISSA_DESTINO
    importing
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods CREATE_PREMISSA_MATERIAL
    importing
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_DATA type /QAPS/V_COST_INP
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/S_PAIR_MATRIZ_PREMISSA .
  methods CREATE_PREMISSA_ORIGEM
    importing
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IT_PARENT_PREMISSA type /QAPS/T_CUSTO_PREMISSA optional
      !IV_WITH_PARENT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_PAIR_MATRIZ_PREMISSA .
  methods CREATE_PREMISSA_RELATION_MTZ
    importing
      !IS_DATA type /QAPS/V_COST_INP .
  methods CREATE_PREM_RELAT_HERANCA_SKU
    importing
      !IS_ITEM type /QAPS/PREM_ITEM .
  methods CREATE_PREMISSA_RELATION
    importing
      !IS_DATA type /QAPS/V_COST_INP
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IT_ID_ORIGEM type /QAPS/T_PAIR_MATRIZ_PREMISSA
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CREATE_PREMISSA_TRAJETO
    importing
      !IS_ORIGEM type /QAPS/PREM_DISTR
      !IS_PARENT_PREMISSA type /QAPS/CUSTO_PRM optional .
  methods CREATE_PRM_AGREGADOR_TO_SKU
    importing
      !IS_DESTINO type /QAPS/V_DEST_ATV
      !IS_ID_DESTINO type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IS_ID_MATERIAL type /QAPS/S_PAIR_MATRIZ_PREMISSA
      !IT_ID_ORIGEM type /QAPS/T_PAIR_MATRIZ_PREMISSA
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CREATE_STD_PRD_ASSIGN_DESTINO
    importing
      value(IT_DESTINO) type /QAPS/T_RELATION_STD_PRD_HDR
    returning
      value(RETURN) type /QAPS/T_RELATION_STD_PRD_HDR .
  methods CREATE_STD_PRD_ASSIGN_MATERIAL
    importing
      value(IT_DESTINO) type /QAPS/T_RELATION_STD_PRD_HDR
      value(IT_MATERIAL) type /QAPS/T_RELATION_STD_PRD_ITEM
    returning
      value(RETURN) type /QAPS/T_RELATION_STD_PRD_ITEM .
  methods CREATE_STD_PRD_ASSIGN_ORIGEM
    importing
      !IT_DESTINO type /QAPS/T_RELATION_STD_PRD_HDR
      !IT_MATERIAL type /QAPS/T_RELATION_STD_PRD_ITEM
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_STD_PRD_DISTRIBUICAO
    importing
      !IS_DATA type /QAPS/S_RELATION_STD_PRD_ITEM .
  methods CREATE_TRAJETO
    importing
      !IS_TRAJETO type /QAPS/S_TRAJETO
      !IS_ORIGEM_DESTINO type /QAPS/V_PRM_ALL .
  methods DELETE_MATRIZ_ABASTECIMENTO
    importing
      !IS_DATA type /QAPS/V_CST_INP .
  methods DELETE_PREMISSA_MATRIZ
    importing
      !IS_DATA type /QAPS/V_CST_INP
      !IV_FORCE type ABAP_BOOL default ABAP_FALSE .
  methods DELETE_PREMISSA
    importing
      !IS_DATA type /QAPS/V_CST_INP .
  methods GET_CUSTO_ELEMENTAR
    importing
      !IV_ID_CUSTO_ELEMENTAR type /QAPS/ID_CUSTO_ELEMENTAR .
  methods GET_CUSTO_MATRIZ
    importing
      !IV_GERAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_CUSTO_MATRIZ .
  methods GET_CUSTO_MATRIZ_GERAL
    importing
      !IV_GERAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_CUSTO_MATRIZ .
  methods GET_CUSTO_PREMISSA
    importing
      !IV_GERAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_CUSTO_PREMISSA .
  methods GET_DESTINOS_ATIVOS
    importing
      !IV_GERAL type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_DESTINOS_ATIVOS .
  methods GET_DISTRIBUICAO_LOGISTICA
    importing
      !IS_DATA type /QAPS/V_COST_INP
    returning
      value(RETURN) type /QAPS/T_DISTRIB_LOGISTICA .
  methods GET_PREMISSA_DISTR_EXCLUSAO
    importing
      !IT_DATA type /QAPS/T_CUSTO_PREMISSA
      !IT_ITEMS type /QAPS/T_PREM_ITEM
    returning
      value(RETURN) type /QAPS/T_PREM_DISTR .
  methods GET_PREMISSA_ITENS_EXCLUSAO
    importing
      !IT_DATA type /QAPS/T_CUSTO_PREMISSA
    returning
      value(RETURN) type /QAPS/T_PREM_ITEM .
  methods GET_PREMISSA_STD_PRD
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_PREMISSA_STD_PRD .
  methods GET_SIMULACAO
    importing
      !IV_SIMULACAO type /QAPS/ED_ID_SIMULACAO .
  methods GET_TRAJETO
    importing
      !IS_DATA type /QAPS/V_PRM_ALL
    returning
      value(RETURN) type /QAPS/T_TRAJETO .
  methods HERDAR_ORIGEM
    importing
      !IS_DATA type /QAPS/PREM_DISTR
    returning
      value(RETURN) type /QAPS/T_PARENT_ORIGEM .
  methods HERDAR_ORIGEM_MATRIZ_PREMISSA
    importing
      !IS_DATA type /QAPS/PREM_DISTR
    returning
      value(RETURN) type /QAPS/T_PARENT_ORIGEM .
  methods INPUT_CREATE_COMPRAS
    importing
      !IS_DATA type /QAPS/V_COST_INP
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods INPUT_CREATE_FRETE
    importing
      !IS_DATA type /QAPS/VAR_INPUT .
  methods INPUT_CREATE_IMPORTACAO
    importing
      !IS_DATA type /QAPS/V_COST_INP .
  methods PREENCHER_DADOS_CONTROLE
    changing
      !CR_DATA type ref to DATA .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods REPLY_TO_CHILDREN
    importing
      !IS_ITEM_PARENT type /QAPS/PREM_ITEM
      !IT_DISTRIB_TEMPLATE type /QAPS/T_PREM_DISTR .
  methods SINCRONIZAR_MATRIZ_PREMISSA
    importing
      !IV_ID_ITEM type /QAPS/ED_ID_ITEM
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
      !IS_DATA type /QAPS/V_COST_INP .
ENDCLASS.



CLASS /QAPS/CL_RULE_SINCRONIZACAO IMPLEMENTATION.


  METHOD APPEND_MATRIZ.

    DATA(lt_custo_matriz) = get_custo_matriz( ).
    APPEND LINES OF get_custo_matriz_geral( ) TO lt_custo_matriz.

    DATA(lt_custo_premissa) = get_custo_premissa( ).
    APPEND LINES OF get_custo_premissa( abap_true ) TO lt_custo_premissa.

    "Criar Destino em Matriz Abastecimento
*    break c060863.
    IF is_relation-destino_matriz IS INITIAL.

      DATA(ls_id_destino) = create_matriz_destino( is_relation        = is_relation
                                                   is_destino         = is_destino
                                                   iv_with_parent     = iv_with_parent
                                                   it_parent_matriz   = lt_custo_matriz
                                                   it_parent_premissa = lt_custo_premissa ).

    ELSE.
      ls_id_destino-id_matriz = lt_custo_matriz[ id_grp_planta = is_destino-id_grp_planta
                                                 id_centro     = is_destino-id_centro ]-id_matriz_abast.
      ls_id_destino-id_premissa = lt_custo_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                     id_centro     = is_destino-id_centro ]-id_premissa.

    ENDIF.

    "Criar material em Matriz Abastecimento
    IF is_relation-material_matriz IS INITIAL.

      DATA(ls_id_material) = create_matriz_material( is_relation        = is_relation
                                                     is_id_destino = ls_id_destino
                                                     is_destino    = is_destino
                                                     is_data       = is_data
                                                     iv_with_parent     = iv_with_parent
                                                     it_parent_matriz   = lt_custo_matriz
                                                     it_parent_premissa = lt_custo_premissa ).
    ELSE.
      ls_id_material-id_matriz = lt_custo_matriz[ id_grp_planta    = is_destino-id_grp_planta
                                                  id_centro        = is_destino-id_centro
                                                  tipo_regra       = is_data-tipo_regra
                                                  matnr            = is_data-matnr
                                                  id_grupo_produto = is_data-id_grupo_produto
                                                  agregador        = is_data-agregador
                                                  mat_planejado    = is_data-mat_planejado ]-id_item.
      ls_id_material-id_premissa = lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                                      id_centro        = is_destino-id_centro
                                                      tipo_regra       = is_data-tipo_regra
                                                      matnr            = is_data-matnr
                                                      id_grupo_produto = is_data-id_grupo_produto
                                                      agregador        = is_data-agregador
                                                      mat_planejado    = is_data-mat_planejado ]-id_item.

    ENDIF.

    "Criar Origem em Matriz Abastecimento
    IF is_relation-origem_matriz IS INITIAL.
      DATA(ls_id_origem) = create_matriz_origem( is_relation        = is_relation
                                                 is_id_destino  = ls_id_destino
                                                 is_id_material = ls_id_material
                                                 is_data        = is_data
                                                 is_destino     = is_destino
                                                 iv_modalidade  = iv_modalidade
                                                 iv_with_parent     = iv_with_parent
                                                 it_parent_matriz   = lt_custo_matriz
                                                 it_parent_premissa = lt_custo_premissa ).
    else.
      ls_id_origem-id_matriz = lt_custo_matriz[ id_grp_planta    = is_destino-id_grp_planta
                                                  id_centro        = is_destino-id_centro
                                                  tipo_regra       = is_data-tipo_regra
                                                  matnr            = is_data-matnr
                                                  id_grupo_produto = is_data-id_grupo_produto
                                                  agregador        = is_data-agregador
                                                  mat_planejado    = is_data-mat_planejado
                                                  id_origem        = is_data-id_origem  ]-id_distribuicao.
      ls_id_origem-id_premissa = lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                                      id_centro        = is_destino-id_centro
                                                      tipo_regra       = is_data-tipo_regra
                                                      matnr            = is_data-matnr
                                                      id_grupo_produto = is_data-id_grupo_produto
                                                      agregador        = is_data-agregador
                                                      mat_planejado    = is_data-mat_planejado
                                                      id_origem        = is_data-id_origem  ]-id_distribuicao.
    ENDIF.

    "Criar entrada em Relação Var Input x Matriz Abastecimento
    sincronizar_matriz_premissa( iv_id_item = ls_id_material-id_premissa
                                 iv_id_distribuicao = ls_id_origem-id_premissa
                                 is_data            = is_data ).

    "Criar entrada em Relação Var Input x Matriz Abastecimento
    create_matriz_relation( is_destino     = is_destino
                            is_data        = is_data
                            is_id_destino  = ls_id_destino
                            is_id_material = ls_id_material
                            is_id_origem   = ls_id_origem
                            iv_modalidade  = iv_modalidade  ).

     update_percentual_automatico( ).


  ENDMETHOD.


  METHOD append_new_trajeto.

    DATA lt_data TYPE TABLE OF /qaps/prem_traj.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @is_template-id_simulacao
      INTO @DATA(ls_simulacao).

    DATA(lv_periodo_inicial) = ls_simulacao-periodo_inicial.

    ls_periodo-year = ls_simulacao-periodo_inicial(4).
    ls_periodo-month = ls_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = ls_simulacao-periodo_inicial
        final   = ls_simulacao-periodo_final    ).

    DATA(ls_data) = VALUE /qaps/prem_traj(
        id_prem_trajeto        = cl_system_uuid=>create_uuid_x16_static( )
*        periodo                =
        id_trajeto             = iv_id_trajeto
        id_premissa            = is_template-id_premissa
        id_item                = is_template-id_item
        id_distribuicao        = is_template-id_distribuicao
        percentual             = 0 ).

    lr_data = REF #( ls_data ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial <= ls_per-final.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      check lv_periodo_inicial <= ls_per-final.

      ls_data-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_data TO lt_data.

    ENDWHILE.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_traj FROM TABLE lt_data.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD check_relation.

    DATA ls_return TYPE /qaps/s_relation_std_prd.

    SELECT *
      FROM /qaps/v_stp_full
      WHERE id_std_producao = @ms_simulacao-id_std_producao
      INTO TABLE @DATA(lt_std_full).

    LOOP AT cs_data-t_destino INTO DATA(ls_destino).
      IF ls_destino-werks IS INITIAL.
        CHECK line_exists( lt_std_full[ cod_grp_planta = ls_destino-cod_grp_planta
                                        werks = '' ] ).
      ELSEIF NOT ls_destino-werks IS INITIAL.
        CHECK line_exists( lt_std_full[ cod_grp_planta = ls_destino-cod_grp_planta
                                        werks = ls_destino-werks ] ).
      ENDIF.

      APPEND ls_destino TO ls_return-t_destino.

    ENDLOOP.

    LOOP AT cs_data-t_item INTO DATA(ls_item).

      IF ls_item-werks IS INITIAL.
        CHECK line_exists( lt_std_full[ cod_grp_planta = ls_item-cod_grp_planta
                                        werks = ''
                                        matnr = ls_item-matnr ] ).
      ELSEIF NOT ls_item-werks IS INITIAL.
        CHECK line_exists( lt_std_full[ cod_grp_planta = ls_item-cod_grp_planta
                                        werks          = ls_item-werks
                                        matnr = ls_item-matnr ] ).
      ENDIF.

      APPEND ls_item TO ls_return-t_item.

    ENDLOOP.

    cs_data = ls_return.

  ENDMETHOD.


  METHOD check_relation_matriz.

    DATA(lt_custo_matriz) = get_custo_matriz( ).
    APPEND LINES OF get_custo_matriz_geral( ) TO lt_custo_matriz.

    DATA(lt_custo_premissa) = get_custo_premissa( ).
    APPEND LINES OF get_custo_premissa( abap_true ) TO lt_custo_premissa.

    DATA(lt_prm_full) = get_premissa_full( ).

    IF lines( lt_custo_matriz ) = 0 AND lines( lt_custo_premissa ) = 0
      AND lines( lt_prm_full ) = 0.
      RETURN.
    ENDIF.

    "Destino
    IF line_exists( lt_custo_matriz[ id_grp_planta = is_destino-id_grp_planta
                                     id_centro     = is_destino-id_centro ] ).
      return-destino_matriz = 'X'.
    ENDIF.

    IF line_exists( lt_custo_premissa[ id_grp_planta = is_destino-id_grp_planta
                                     id_centro     = is_destino-id_centro ] )
     OR line_exists( lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                     id_centro     = is_destino-id_centro ] ).
      return-destino_premissa = 'X'.
    ENDIF.

    "Material
    IF line_exists( lt_custo_matriz[ id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto  ] ).

      return-material_matriz = 'X'.
    ENDIF.

    IF line_exists( lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto  ] )
      or line_exists( lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto  ] ).

      return-material_premissa = 'X'.
    ENDIF.

    IF iv_modalidade = 'I'.
      DELETE lt_custo_matriz WHERE importacao IS INITIAL.
      DELETE lt_custo_premissa WHERE importacao IS INITIAL.
    ELSE.
      DELETE lt_custo_matriz WHERE nacional IS INITIAL.
      DELETE lt_custo_premissa WHERE nacional IS INITIAL.
    ENDIF.

    IF line_exists( lt_custo_matriz[ id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto
                                     tipo_origem      = is_data-tipo_origem
                                     id_origem        = is_data-id_origem
                                     importacao       = COND #( WHEN iv_modalidade = 'I'
                                                                THEN abap_true )
                                     nacional         = COND #( WHEN iv_modalidade = 'N'
                                                                THEN abap_true )
                                      ] ).

      return-origem_matriz = 'X'.
    ENDIF.

    IF line_exists( lt_custo_premissa[
                                     id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto
                                     tipo_origem      = is_data-tipo_origem
                                     id_origem        = is_data-id_origem
                                     importacao       = COND #( WHEN iv_modalidade = 'I'
                                                                THEN abap_true )
                                     nacional         = COND #( WHEN iv_modalidade = 'N'
                                                                THEN abap_true )
                                      ] )
      or line_exists( lt_prm_full[
                                     id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto
                                     tipo_origem      = is_data-tipo_origem
                                     id_origem        = is_data-id_origem
                                     modalidade       = iv_modalidade
                                      ] ).

      return-origem_premissa = 'X'.
    ENDIF.



  ENDMETHOD.


  METHOD check_relation_premissa.

    DATA(lt_custo_premissa) = get_custo_premissa( ).
    APPEND LINES OF get_custo_premissa( abap_true ) TO lt_custo_premissa.
    DATA(lt_custo_geral) = get_custo_premissa( iv_geral = abap_true ).
    DATA(lt_prm_full) = get_premissa_full( iv_fill_zeros = abap_true ).

    IF lines( lt_custo_premissa ) = 0 AND lines( lt_custo_geral ) = 0
      AND lines( lt_prm_full ) = 0.
      RETURN.
    ENDIF.

    IF line_exists( lt_custo_premissa[ id_grp_planta = is_destino-id_grp_planta
                                       id_centro     = is_destino-id_centro ] )
       OR line_exists( lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                       id_centro  = is_destino-id_centro ] ).
      return-destino = 'X'.
    ELSEIF   line_exists( lt_custo_geral[ id_grp_planta = is_destino-id_grp_planta
                                          id_centro     = is_destino-id_centro ] )
      OR line_exists( lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                       id_centro     = is_destino-id_centro ] ).
      return-destino = 'C'.
    ENDIF.


    IF line_exists( lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                       id_centro        = is_destino-id_centro
                                       tipo_regra       = is_data-tipo_regra
                                       matnr            = is_data-matnr
                                       mat_planejado    = is_data-mat_planejado
                                       agregador        = is_data-agregador
                                       id_grupo_produto = is_data-id_grupo_produto  ] )
      OR line_exists( lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                   id_centro        = is_destino-id_centro
                                   tipo_regra       = is_data-tipo_regra
                                   matnr            = is_data-matnr
                                   mat_planejado    = is_data-mat_planejado
                                   agregador        = is_data-agregador
                                   id_grupo_produto = is_data-id_grupo_produto ] ).
      return-material = 'X'.
    ELSEIF line_exists( lt_custo_geral[ id_grp_planta    = is_destino-id_grp_planta
                                        id_centro        = is_destino-id_centro
                                        tipo_regra       = is_data-tipo_regra
                                        matnr            = is_data-matnr
                                        mat_planejado    = is_data-mat_planejado
                                        agregador        = is_data-agregador
                                        id_grupo_produto = is_data-id_grupo_produto  ] )
       OR line_exists( lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                        id_centro        = is_destino-id_centro
                                        tipo_regra       = is_data-tipo_regra
                                        matnr            = is_data-matnr
                                        mat_planejado    = is_data-mat_planejado
                                        agregador        = is_data-agregador
                                        id_grupo_produto = is_data-id_grupo_produto ] ).

      return-material = 'C'.
    ENDIF.

    IF iv_modalidade = 'I'.
      DELETE lt_custo_premissa WHERE importacao IS INITIAL.
    ELSE.
      DELETE lt_custo_premissa WHERE nacional IS INITIAL.
    ENDIF.

    IF line_exists( lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                     id_centro        = is_destino-id_centro
                                     tipo_regra       = is_data-tipo_regra
                                     matnr            = is_data-matnr
                                     mat_planejado    = is_data-mat_planejado
                                     agregador        = is_data-agregador
                                     id_grupo_produto = is_data-id_grupo_produto
                                     tipo_origem      = is_data-tipo_origem
                                     id_origem        = is_data-id_origem
                                     importacao       = COND #( WHEN iv_modalidade = 'I'
                                                                THEN abap_true )
                                     nacional         = COND #( WHEN iv_modalidade = 'N'
                                                                THEN abap_true )
                                      ] ).
      return-origem = 'X'.
    ELSEIF line_exists( lt_custo_geral[ id_grp_planta    = is_destino-id_grp_planta
                                 id_centro        = is_destino-id_centro
                                 tipo_regra       = is_data-tipo_regra
                                 matnr            = is_data-matnr
                                 mat_planejado    = is_data-mat_planejado
                                 agregador        = is_data-agregador
                                 id_grupo_produto = is_data-id_grupo_produto
                                 tipo_origem      = is_data-tipo_origem
                                 id_origem        = is_data-id_origem
                                 importacao       = COND #( WHEN iv_modalidade = 'I'
                                                            THEN abap_true )
                                 nacional         = COND #( WHEN iv_modalidade = 'N'
                                                            THEN abap_true )
                                  ] ).

      return-origem = 'C'.
    ELSEIF line_exists( lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                 id_centro        = is_destino-id_centro
                                 tipo_regra       = is_data-tipo_regra
                                 matnr            = is_data-matnr
                                 mat_planejado    = is_data-mat_planejado
                                 agregador        = is_data-agregador
                                 id_grupo_produto = is_data-id_grupo_produto
                                 tipo_origem      = is_data-tipo_origem
                                 id_origem        = is_data-id_origem
                                 modalidade       = iv_modalidade
                                  ] ).

      return-origem = 'C'.
    ELSEIF line_exists( lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                 id_centro        = is_destino-id_centro
                                 tipo_regra       = is_data-tipo_regra
                                 matnr            = is_data-matnr
                                 mat_planejado    = is_data-mat_planejado
                                 agregador        = is_data-agregador
                                 id_grupo_produto = is_data-id_grupo_produto
*                                 tipo_origem      = is_data-tipo_origem
*                                 id_origem        = is_data-id_origem
                                 modalidade       = iv_modalidade
                                  ] ).

      return-origem = 'C'.
    ENDIF.

  ENDMETHOD.


  METHOD check_relation_std_prd.

    DATA lt_item TYPE /qaps/t_relation_std_prd_item.
    DATA ls_item TYPE /qaps/s_relation_std_prd_item.
    DATA: lr_matnr_out     TYPE RANGE OF matnr,
          lr_matnr_in      TYPE RANGE OF matnr,
          lr_id_grp_planta TYPE RANGE OF /qaps/std_prd_pa-id_grp_planta,
          lr_werks         TYPE RANGE OF /qaps/std_prd_pa-werks.

    SELECT *
      FROM /qaps/centro
      INTO TABLE @DATA(lt_centro).

    SELECT *
        FROM /qaps/v_prm_hdr
        WHERE id_simulacao = @is_data-id_simulacao
        INTO TABLE @DATA(lt_header).

    SELECT DISTINCT id_regiao,id_grp_planta,werks
      FROM /qaps/std_prd_pa
      WHERE id_std_producao = @is_data-id_std_producao
      INTO TABLE @DATA(lt_std_prd_destino).

    IF lines( lt_std_prd_destino ) > 0.

      lr_id_grp_planta = VALUE #( FOR wa_grp_planta IN lt_std_prd_destino
                                  WHERE ( id_grp_planta <> mc_guid_null )
                                  ( sign = 'I' option = 'EQ' low = wa_grp_planta-id_grp_planta ) ).

      lr_werks = VALUE #( FOR wa_grp_planta IN lt_std_prd_destino
                          WHERE ( werks <> '' )
                          ( sign = 'I' option = 'EQ' low = wa_grp_planta-werks ) ).

      IF lines( lr_id_grp_planta ) > 0.
        SELECT *
          FROM /qaps/v_centro
          WHERE id_grp_planta IN @lr_id_grp_planta
          INTO TABLE @DATA(lt_children).
      ENDIF.

      IF lines( lr_werks ) > 0.
        SELECT *
          FROM /qaps/v_centro
          WHERE werks IN @lr_werks
          APPENDING TABLE @lt_children.
      ENDIF.

    ENDIF.

    SELECT *
      FROM /qaps/v_ponto
      WHERE tipo_ponto = 'G'
      INTO TABLE @DATA(lt_grp_planta).

    SELECT *
      FROM /qaps/v_ponto
      WHERE tipo_ponto = 'W'
      INTO TABLE @DATA(lt_werks).

    "Destino
    LOOP AT lt_std_prd_destino INTO DATA(ls_std_producao).

      IF ls_std_producao-id_grp_planta <> mc_guid_null.
        DATA(ls_destino) = VALUE /qaps/s_relation_std_prd_hdr(
                                          id_simulacao = is_data-id_simulacao
                                          tipo_origem   = 'G'
                                          id_grp_planta = ls_std_producao-id_grp_planta
                                          cod_grp_planta = VALUE #( lt_grp_planta[ id_externo = ls_std_producao-id_grp_planta ]-codigo OPTIONAL ) ).

        ls_destino-id_premissa = VALUE #( lt_header[ id_grp_planta = ls_std_producao-id_grp_planta
                                                     id_centro = mc_guid_null ]-id_premissa OPTIONAL ).

        APPEND ls_destino TO return-t_destino.

        LOOP AT lt_children INTO DATA(ls_children).
          CHECK ls_children-id_grp_planta = ls_std_producao-id_grp_planta.
          ls_destino-id_centro = ls_children-id_centro.
          ls_destino-id_premissa = VALUE #( lt_header[ id_grp_planta = ls_std_producao-id_grp_planta
                                                       id_centro = ls_children-id_centro ]-id_premissa OPTIONAL ).
          ls_destino-tipo_origem   = 'W'.
          ls_destino-werks         = ls_children-werks.
          APPEND ls_destino TO return-t_destino.
        ENDLOOP.

      ELSEIF NOT ls_std_producao-werks IS INITIAL.
        ls_destino = VALUE /qaps/s_relation_std_prd_hdr(
                              id_simulacao = is_data-id_simulacao
                              tipo_origem   = 'W'
                              id_centro     = lt_centro[ werks = ls_std_producao-werks ]-id_centro
                              werks         = ls_std_producao-werks ).

        ls_destino-id_premissa = VALUE #( lt_header[ id_centro = ls_destino-id_centro ]-id_premissa OPTIONAL ).

        APPEND ls_destino TO return-t_destino.

      ENDIF.

    ENDLOOP.

    "Material
    IF lines( lr_id_grp_planta ) > 0.
      SELECT /qaps/std_prd_pa~id_grp_planta,grp_planta~codigo, id_centro,centro~werks,matnr
        FROM /qaps/std_prd_pa
        LEFT OUTER JOIN /qaps/grp_planta AS grp_planta
        ON /qaps/std_prd_pa~id_grp_planta = grp_planta~id_grp_planta
        LEFT OUTER JOIN /qaps/centro AS centro
        ON /qaps/std_prd_pa~werks = centro~werks
        WHERE /qaps/std_prd_pa~id_grp_planta IN @lr_id_grp_planta
        INTO TABLE @DATA(lt_material).
    ENDIF.

    IF lines( lr_werks ) > 0.
      SELECT /qaps/std_prd_pa~id_grp_planta,grp_planta~codigo, id_centro,centro~werks,matnr
        FROM /qaps/std_prd_pa
        LEFT OUTER JOIN /qaps/grp_planta AS grp_planta
        ON /qaps/std_prd_pa~id_grp_planta = grp_planta~id_grp_planta
        LEFT OUTER JOIN /qaps/centro AS centro
        ON /qaps/std_prd_pa~werks = centro~werks
        WHERE /qaps/std_prd_pa~werks IN @lr_werks
        APPENDING TABLE @lt_material.
    ENDIF.

    lr_matnr_out = VALUE #( FOR wa IN lt_material
                        ( sign = 'I' option = 'EQ'
                          low = |{ wa-matnr ALPHA = out  }| ) ).

    lr_matnr_in = VALUE #( FOR wa IN lt_material
                        ( sign = 'I' option = 'EQ'
                          low = |{ wa-matnr ALPHA = in WIDTH = 18 }| ) ).

    "Dados existentes na premissa
    SELECT DISTINCT id_simulacao,id_premissa,id_grp_planta,id_centro,id_item,matnr
        FROM /qaps/v_prm_full
        WHERE id_simulacao = @is_data-id_simulacao
        AND   id_grp_planta IN @lr_id_grp_planta
        AND   tipo_regra = 'MA'
        AND   ( matnr    IN @lr_matnr_out or matnr    IN @lr_matnr_in )
        INTO TABLE @DATA(lt_destino_item).

    LOOP AT lt_destino_item ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    LOOP AT lt_material INTO DATA(ls_material).

      "Por grupo de planta - expandir para centros
      IF ls_material-codigo <> '' AND ls_material-werks = ''.

        CLEAR ls_item.

        ls_item = VALUE /qaps/s_relation_std_prd_item( tipo_regra = 'MA'
                                                       matnr      = ls_material-matnr
                                                       id_grp_planta = ls_material-id_grp_planta
                                                       cod_grp_planta = VALUE #( lt_grp_planta[ id_externo = ls_material-id_grp_planta ]-codigo ) ).

        DATA(ls_destino_item) = VALUE #( lt_destino_item[ id_grp_planta = ls_material-id_grp_planta
                                                          id_centro     = mc_guid_null
                                                          matnr      = ls_material-matnr  ] OPTIONAL ).

        ls_item-id_premissa = ls_destino_item-id_premissa.
        ls_item-id_item = ls_destino_item-id_item.
        APPEND ls_item TO lt_item.

        "Insere nos filhos
        LOOP AT lt_children INTO ls_children.
          CHECK ls_children-id_grp_planta = ls_material-id_grp_planta.

          ls_item = VALUE /qaps/s_relation_std_prd_item( tipo_regra = 'MA'
                                                         matnr      = ls_material-matnr
                                                         id_grp_planta = ls_material-id_grp_planta
                                                         cod_grp_planta = VALUE #( lt_grp_planta[ id_externo = ls_material-id_grp_planta ]-codigo )
                                                         id_centro     = ls_children-id_centro
                                                         werks = ls_children-werks ).

          ls_destino_item = VALUE #( lt_destino_item[ id_grp_planta = ls_material-id_grp_planta
                                                  id_centro     = ls_children-id_centro
                                                  matnr      = ls_material-matnr ] OPTIONAL ).

          ls_item-id_premissa = ls_destino_item-id_premissa.
          ls_item-id_item = ls_destino_item-id_item.
          APPEND ls_item TO lt_item.

        ENDLOOP.

      ELSEIF ls_material-codigo <> '' AND ls_material-werks <> ''.

        ls_item = VALUE /qaps/s_relation_std_prd_item( tipo_regra = 'MA'
                                                       matnr      = ls_material-matnr
                                                       id_grp_planta = ls_material-id_grp_planta
                                                       id_centro     = ls_children-id_centro ).

        ls_destino_item = VALUE #( lt_destino_item[ id_grp_planta = ls_material-id_grp_planta
                                                    id_centro     = ls_children-id_centro
                                                    matnr      = ls_material-matnr ] OPTIONAL ).

        ls_item-id_premissa = ls_destino_item-id_premissa.
        ls_item-id_item = ls_destino_item-id_item.
        APPEND ls_item TO lt_item.

      ELSEIF ls_material-codigo = '' AND ls_material-werks <> ''.

        ls_item = VALUE /qaps/s_relation_std_prd_item( tipo_regra = 'MA'
                                                       matnr      = ls_material-matnr
*                                                       id_grp_planta = ls_material-id_grp_planta
                                                       id_centro     = ls_material-id_centro
                                                       werks         = ls_material-werks ).

        ls_destino_item = VALUE #( lt_destino_item[
*                                                    id_grp_planta = ls_material-id_grp_planta
                                                    id_centro     = ls_material-id_centro
                                                    matnr         = ls_material-matnr ] OPTIONAL ).

        ls_item-id_premissa = ls_destino_item-id_premissa.
        ls_item-id_item = ls_destino_item-id_item.
        APPEND ls_item TO lt_item.

      ENDIF.

    ENDLOOP.

    return-t_item = lt_item.

  ENDMETHOD.


  METHOD choose_simulacao.

    DATA ls_message TYPE bapiret2.

    CALL FUNCTION '/QAPS/FM_CHOOSE_SIM_INPUT'
      IMPORTING
        ev_id_simulacao = return
        es_message      = ls_message.

    IF ls_message-type = 'E'.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = ls_message.
    ENDIF.

  ENDMETHOD.


  METHOD clean_data.

    clean_data_premissa( ).
    clean_data_matriz( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD clean_data_matriz.

    DATA: lr_id_item         TYPE RANGE OF /qaps/ed_id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/ed_id_distribuicao.

    "Matriz
    SELECT *
      FROM /qaps/v_mtz_full
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_matriz).

    CHECK lines( lt_matriz ) > 0.

    lr_id_item = VALUE #( FOR ls IN lt_matriz
                          ( sign = 'I' option = 'EQ' low = ls-id_item ) ).

    lr_id_distribuicao = VALUE #( FOR ls IN lt_matriz
                          ( sign = 'I' option = 'EQ' low = ls-id_distribuicao ) ).

    "Delete trajeto e Distribuicao
    IF lines( lr_id_distribuicao ) > 0.
      DELETE FROM /qaps/matriz_dst WHERE id_distribuicao IN lr_id_distribuicao.
    ENDIF.

    "Delete items/materiais
    IF lines( lr_id_item ) > 0.
      DELETE FROM /qaps/matriz_itm WHERE id_item IN lr_id_item.
    ENDIF.

    DELETE FROM /qaps/matriz_hdr WHERE id_simulacao = ms_simulacao-id_simulacao.
    DELETE FROM /qaps/custo_mtz WHERE id_simulacao = ms_simulacao-id_simulacao.

  ENDMETHOD.


  METHOD clean_data_premissa.

    DATA: lr_id_item         TYPE RANGE OF /qaps/ed_id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/ed_id_distribuicao.
    "Premissa
    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_premissa).

    CHECK lines( lt_premissa ) > 0.

    lr_id_item = VALUE #( FOR ls IN lt_premissa
                          ( sign = 'I' option = 'EQ' low = ls-id_item ) ).

    lr_id_distribuicao = VALUE #( FOR ls IN lt_premissa
                          ( sign = 'I' option = 'EQ' low = ls-id_distribuicao ) ).

    "Delete trajeto e Distribuicao
    IF lines( lr_id_distribuicao ) > 0.
      DELETE FROM /qaps/prem_traj WHERE id_distribuicao IN lr_id_distribuicao.
      DELETE FROM /qaps/prem_distr WHERE id_distribuicao IN lr_id_distribuicao.
    ENDIF.

    "Delete items/materiais
    IF lines( lr_id_item ) > 0.
      DELETE FROM /qaps/prem_item WHERE id_item IN lr_id_item.
    ENDIF.

    DELETE FROM /qaps/prem_hdr WHERE id_simulacao = ms_simulacao-id_simulacao.
    DELETE FROM /qaps/custo_prm WHERE id_simulacao = ms_simulacao-id_simulacao.

  ENDMETHOD.


  METHOD constructor.
    mo_custo_elementar = NEW /qaps/cl_mdl_custo_elementar( ).
    mo_simulacao = NEW /qaps/cl_mdl_simulacao( ).
    mo_material = NEW /qaps/cl_mdl_material( ).
  ENDMETHOD.


  METHOD create_compra_importada.

    DATA: lt_var_custo TYPE TABLE OF /qaps/v_cost_inp,
          ls_data      TYPE /qaps/v_cost_inp.

    SELECT *
      FROM /qaps/v_cost_inp
      WHERE importacao = 'X'
      AND   tipo_variavel = 'C'
      AND id_simulacao = @ms_simulacao-id_simulacao
      and tipo_regra = 'MA'
      INTO CORRESPONDING FIELDS OF TABLE @lt_var_custo.


    LOOP AT lt_var_custo INTO DATA(ls_var_custo).
      input_create_compras( is_data = ls_var_custo
                            iv_modalidade = 'I' ).
    ENDLOOP.


    SELECT *
      FROM /qaps/v_cost_inp
      WHERE importacao = 'X'
      AND   tipo_variavel = 'C'
      AND id_simulacao = @ms_simulacao-id_simulacao
      and tipo_regra = 'AG'
      INTO CORRESPONDING FIELDS OF TABLE @lt_var_custo.

*    break c060863.
    LOOP AT lt_var_custo INTO ls_var_custo.
      input_create_compras( is_data = ls_var_custo
                            iv_modalidade = 'I' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD CREATE_COMPRA_NACIONAL.

    DATA: lt_var_custo TYPE TABLE OF /qaps/v_cost_inp,
          ls_data      TYPE /qaps/v_cost_inp.

    SELECT *
      FROM /qaps/v_cost_inp
      WHERE nacional = 'X'
      AND   tipo_variavel = 'C'
      AND id_simulacao = @ms_simulacao-id_simulacao
      INTO CORRESPONDING FIELDS OF TABLE @lt_var_custo.

    LOOP AT lt_var_custo INTO DATA(ls_var_custo).
      input_create_compras( is_data = ls_var_custo
                            iv_modalidade = 'N' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_importacao.

    DATA: lt_var_custo TYPE TABLE OF /qaps/v_cost_inp,
          ls_data      TYPE /qaps/v_cost_inp.

    SELECT *
      FROM /qaps/v_cost_inp
      WHERE importacao = 'X'
      AND ( tipo_origem = 'I' OR tipo_origem = 'P' )
      AND tipo_variavel <> 'F'
      AND id_simulacao = @ms_simulacao-id_simulacao
      INTO CORRESPONDING FIELDS OF TABLE @lt_var_custo.

    LOOP AT lt_var_custo INTO DATA(ls_var_custo).
      input_create_importacao( ls_var_custo ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_matriz.

    DATA(lt_custo_matriz) = get_custo_matriz( ).
    APPEND LINES OF get_custo_matriz_geral( ) TO lt_custo_matriz.

    DATA(lt_custo_premissa) = get_custo_premissa( ).
    APPEND LINES OF get_custo_premissa( abap_true ) TO lt_custo_premissa.

    DATA(lt_prm_full) = get_premissa_full( ).

*    break c060863.
    "Criar Destino em Matriz Abastecimento
    DATA(ls_id_destino) = create_matriz_destino( is_relation        = is_relation
                                                 is_destino         = is_destino
                                                 iv_with_parent     = iv_with_parent
                                                 it_parent_matriz   = lt_custo_matriz
                                                 it_parent_premissa = lt_custo_premissa ).

    "Criar material em Matriz Abastecimento
    DATA(ls_id_material) = create_matriz_material( is_relation        = is_relation
                                                   is_id_destino = ls_id_destino
                                                   is_destino    = is_destino
                                                   is_data       = is_data
                                                   iv_with_parent     = iv_with_parent
                                                   it_parent_matriz   = lt_custo_matriz
                                                   it_parent_premissa = lt_custo_premissa ).

    "Criar Origem em Matriz Abastecimento
    IF is_relation-origem_matriz IS INITIAL.
      DATA(ls_id_origem) = create_matriz_origem( is_relation        = is_relation
                                                 is_id_destino  = ls_id_destino
                                                 is_id_material = ls_id_material
                                                 is_data        = is_data
                                                 is_destino     = is_destino
                                                 iv_modalidade  = iv_modalidade
                                                 iv_with_parent     = iv_with_parent
                                                 it_parent_matriz   = lt_custo_matriz
                                                 it_parent_premissa = lt_custo_premissa ).
    ENDIF.

    "Criar entrada em Relação Var Input x Matriz Abastecimento
    sincronizar_matriz_premissa( iv_id_item = ls_id_material-id_premissa
                                 iv_id_distribuicao = ls_id_origem-id_premissa
                                 is_data            = is_data ).

    "Criar entrada em Relação Var Input x Matriz Abastecimento
    create_matriz_relation( is_destino     = is_destino
                            is_data        = is_data
                            is_id_destino  = ls_id_destino
                            is_id_material = ls_id_material
                            is_id_origem   = ls_id_origem
                            iv_modalidade  = iv_modalidade  ).


    herdar_origem_from_matriz_abs( is_id_destino  = ls_id_destino
                                   is_id_material = ls_id_material
                                   is_id_origem   = ls_id_origem ).

    update_percentual_automatico( ).


  ENDMETHOD.


  METHOD create_matriz_destino.

    DATA: lr_data  TYPE REF TO data,
          lr_ponto TYPE RANGE OF /qaps/id_ponto.

    DATA(lt_prm_full) = get_premissa_full( ).

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.

      DATA(ls_parent_matriz) = VALUE #( it_parent_matriz[ id_grp_planta = is_destino-id_grp_planta
                                                           id_centro     = mc_guid_null ] OPTIONAL ).
      DATA(ls_parent_premissa) = VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                             id_centro     = mc_guid_null ] OPTIONAL  ).

    ENDIF.

    "Matriz
    IF is_relation-destino_matriz IS INITIAL.
      DATA(ls_matriz_header) = VALUE /qaps/matriz_hdr( id_matriz_abast  = cl_system_uuid=>create_uuid_x16_static( )
                                                       id_simulacao     = ms_simulacao-id_simulacao
                                                       id_grp_planta    = is_destino-id_grp_planta
                                                       id_centro        = is_destino-id_centro
                                                       id_parent        = ls_parent_matriz-id_matriz_abast ).

      lr_data = REF #( ls_matriz_header ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/matriz_hdr FROM ls_matriz_header.
      COMMIT WORK AND WAIT.

    ELSE.
      ls_matriz_header-id_matriz_abast = it_parent_matriz[ id_grp_planta = is_destino-id_grp_planta
                                                 id_centro     = is_destino-id_centro ]-id_matriz_abast.
    ENDIF.

    IF is_relation-destino_premissa IS INITIAL.
      "Premissa
      DATA(ls_premissa_header) = VALUE /qaps/prem_hdr(
          id_premissa     = cl_system_uuid=>create_uuid_x16_static( )
          id_simulacao    = ms_simulacao-id_simulacao
          id_grp_planta   = is_destino-id_grp_planta
          id_centro       = is_destino-id_centro
          id_matriz_abast = ls_matriz_header-id_matriz_abast
          id_parent       = ls_parent_premissa-id_premissa
      ).

      lr_data = REF #( ls_premissa_header ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/prem_hdr FROM ls_premissa_header.
      COMMIT WORK AND WAIT.

    ELSE.
      ls_premissa_header-id_premissa = value #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                           id_centro     = is_destino-id_centro ]-id_premissa OPTIONAL ).

      if ls_premissa_header-id_premissa is INITIAL.
        ls_premissa_header-id_premissa = lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                                      id_centro     = is_destino-id_centro ]-id_premissa.
      endif.

      IF is_relation-destino_matriz IS INITIAL.
        update_link_matriz_prem_hdr( iv_id_premissa     = ls_premissa_header-id_premissa
                                     iv_id_matriz_abast = ls_matriz_header-id_matriz_abast ).
      ENDIF.

    ENDIF.

    return-id_matriz = ls_matriz_header-id_matriz_abast.
    return-id_premissa = ls_premissa_header-id_premissa.

  ENDMETHOD.


  METHOD create_matriz_material.

    DATA: lr_data TYPE REF TO data.

*    DATA(lt_prm_full) = get_premissa_header_item( ).
    DATA(lt_prm_full) = get_premissa_full( ).

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.

      DATA(ls_parent_matriz) = VALUE #( it_parent_matriz[ id_grp_planta = is_destino-id_grp_planta
                                                          id_centro     = mc_guid_null
                                                          tipo_regra       = is_data-tipo_regra
                                                          matnr            = is_data-matnr
                                                          id_grupo_produto = is_data-id_grupo_produto
                                                          agregador        = is_data-agregador
                                                          mat_planejado    = is_data-mat_planejado ] OPTIONAL ).
      DATA(ls_parent_premissa) = VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                              id_centro     = mc_guid_null
                                                              tipo_regra       = is_data-tipo_regra
                                                              matnr            = is_data-matnr
                                                              id_grupo_produto = is_data-id_grupo_produto
                                                              agregador        = is_data-agregador
                                                              mat_planejado    = is_data-mat_planejado ] OPTIONAL  ).

    ENDIF.

    "Matriz
    IF is_relation-material_matriz IS INITIAL.
      DATA(ls_matriz_item) =  VALUE /qaps/matriz_itm( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                         id_matriz_abast  = is_id_destino-id_matriz
                                         tipo_regra       = is_data-tipo_regra
                                         matnr            = is_data-matnr
                                         id_grupo_produto = is_data-id_grupo_produto
                                         agregador        = is_data-agregador
                                         mat_planejado    = is_data-mat_planejado
                                         id_parent        = ls_parent_matriz-id_item ).

      lr_data = REF #( ls_matriz_item ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      MODIFY /qaps/matriz_itm FROM ls_matriz_item.
      COMMIT WORK AND WAIT.

    ELSE.
      ls_matriz_item-id_item = it_parent_matriz[ id_grp_planta    = is_destino-id_grp_planta
                                                  id_centro        = is_destino-id_centro
                                                  tipo_regra       = is_data-tipo_regra
                                                  matnr            = is_data-matnr
                                                  id_grupo_produto = is_data-id_grupo_produto
                                                  agregador        = is_data-agregador
                                                  mat_planejado    = is_data-mat_planejado ]-id_item.

    ENDIF.

    "Premissa
    IF is_relation-material_premissa IS INITIAL.
      DATA(ls_premissa_item) =  VALUE /qaps/prem_item(
          id_item          = cl_system_uuid=>create_uuid_x16_static( )
          id_premissa      = is_id_destino-id_premissa
          id_item_matriz   = ls_matriz_item-id_item
          tipo_regra       = is_data-tipo_regra
          matnr            = /qaps/cl_helper_data=>material_to_input( is_data-matnr )
          id_grupo_produto = is_data-id_grupo_produto
          agregador        = is_data-agregador
          mat_planejado    = /qaps/cl_helper_data=>material_to_input( is_data-mat_planejado ) ).

      lr_data = REF #( ls_premissa_item ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      MODIFY /qaps/prem_item FROM ls_premissa_item.
      COMMIT WORK AND WAIT.
    ELSE.
      ls_premissa_item-id_item = VALUE #( it_parent_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                                      id_centro        = is_destino-id_centro
                                                      tipo_regra       = is_data-tipo_regra
                                                      matnr            = /qaps/cl_helper_data=>material_to_input( is_data-matnr )
                                                      id_grupo_produto = is_data-id_grupo_produto
                                                      agregador        = is_data-agregador
                                                      mat_planejado    = /qaps/cl_helper_data=>material_to_input( is_data-mat_planejado ) ]-id_item OPTIONAL ).

      IF ls_premissa_item-id_premissa IS INITIAL.
        ls_premissa_item-id_item = lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                                    id_centro        = is_destino-id_centro
                                                    tipo_regra       = is_data-tipo_regra
                                                    matnr            = /qaps/cl_helper_data=>material_to_input( is_data-matnr )
                                                    id_grupo_produto = is_data-id_grupo_produto
                                                    agregador        = is_data-agregador
                                                    mat_planejado    = /qaps/cl_helper_data=>material_to_input( is_data-mat_planejado ) ]-id_item.
      ENDIF.

      IF is_relation-material_matriz IS INITIAL.
        update_link_matriz_prem_item( iv_id_item        = ls_premissa_item-id_item
                                      iv_id_item_matriz = ls_matriz_item-id_item ).
      ENDIF.

    ENDIF.

    return-id_matriz = ls_matriz_item-id_item.
    return-id_premissa = ls_premissa_item-id_item.

  ENDMETHOD.


  METHOD create_matriz_origem.

    DATA: lt_matriz_distr   TYPE TABLE OF /qaps/matriz_dst,
          lt_entry          TYPE TABLE OF /qaps/prem_distr,
          lt_premissa_distr TYPE TABLE OF /qaps/prem_distr.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/matriz_dst,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_data-id_origem
      INTO @DATA(ls_origem).

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.

      DATA(ls_parent_matriz) = VALUE #( it_parent_matriz[ id_grp_planta = is_destino-id_grp_planta
                                                          id_centro     = mc_guid_null
                                                          tipo_regra       = is_data-tipo_regra
                                                          matnr            = is_data-matnr
                                                          id_grupo_produto = is_data-id_grupo_produto
                                                          agregador        = is_data-agregador
                                                          mat_planejado    = is_data-mat_planejado
                                                          importacao      = 'X'
                                                          tipo_origem     = ls_origem-tipo_ponto
                                                          id_origem       = ls_origem-id_ponto ] OPTIONAL ).
      DATA(ls_parent_premissa) = VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                              id_centro     = mc_guid_null
                                                              tipo_regra       = is_data-tipo_regra
                                                              matnr            = is_data-matnr
                                                              id_grupo_produto = is_data-id_grupo_produto
                                                              agregador        = is_data-agregador
                                                              mat_planejado    = is_data-mat_planejado
                                                              importacao      = 'X'
                                                              tipo_origem     = ls_origem-tipo_ponto
                                                              id_origem       = ls_origem-id_ponto ] OPTIONAL  ).

    ENDIF.

    DATA(ls_matriz_distr) = VALUE /qaps/matriz_dst(
        id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
        id_matriz_abast = is_id_destino-id_matriz
        id_item         = is_id_material-id_matriz
        modalidade      =  'I'
        tipo_origem     = ls_origem-tipo_ponto
        id_origem       = ls_origem-id_ponto
        id_parent       = ls_parent_matriz-id_distribuicao
*        percentual      = COND #( WHEN iv_full = abap_true THEN 100 ELSE 0 )
    ).

    DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

    ls_periodo-year = ms_simulacao-periodo_inicial(4).
    ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = ms_simulacao-periodo_inicial
        final   = ms_simulacao-periodo_final    ).

    lr_data = REF #( ls_matriz_distr ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial <= ls_per-final.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      check lv_periodo_inicial <= ls_per-final.

      ls_matriz_distr-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_matriz_distr TO lt_matriz_distr.

    ENDWHILE.

    IF lines( lt_matriz_distr ) > 0.
      MODIFY /qaps/matriz_dst FROM TABLE lt_matriz_distr.
      COMMIT WORK.
      return-id_matriz = ls_matriz_distr-id_distribuicao.
    ENDIF.

    "Premissa
    DATA(ls_premissa_distr) = VALUE /qaps/prem_distr(
        id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
        id_premissa     = is_id_destino-id_premissa
        id_item         = is_id_material-id_premissa
*        id_item_matriz  = is_id_material-id_matriz
        id_distr_matriz = ls_matriz_distr-id_distribuicao
        modalidade      =  'I'
        tipo_origem     = ls_origem-tipo_ponto
        id_origem       = ls_origem-id_ponto
        id_parent       = ls_parent_premissa-id_distribuicao
*        percentual      = COND #( WHEN iv_full = abap_true THEN 100 ELSE 0 )
    ).

    lv_periodo_inicial = ms_simulacao-periodo_inicial.

    ls_periodo-year = ms_simulacao-periodo_inicial(4).
    ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = ms_simulacao-periodo_inicial
        final   = ms_simulacao-periodo_final    ).

    lr_data = REF #( ls_premissa_distr ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial <= ls_per-final.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      check lv_periodo_inicial <= ls_per-final.

      ls_premissa_distr-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_premissa_distr TO lt_premissa_distr.

    ENDWHILE.

    IF lines( lt_premissa_distr ) > 0.
*      BREAK c060863.
      MODIFY /qaps/prem_distr FROM TABLE lt_premissa_distr.
      COMMIT WORK AND WAIT.
      return-id_premissa = ls_premissa_distr-id_distribuicao.

    ENDIF.

    "Vincular Trajetos
    create_premissa_trajeto( is_origem  = ls_premissa_distr
                             is_parent_premissa = ls_parent_premissa ).


  ENDMETHOD.


  METHOD create_matriz_relation.

    DATA lr_data TYPE REF TO data.

    "Matriz
    DATA(ls_custo_matriz) = VALUE /qaps/custo_mtz(
        id_custo_matriz    = cl_system_uuid=>create_uuid_x16_static( )
        id_custo_elementar = ms_custo_elementar-id_custo_elementar
        id_var_input       = is_data-id_var_input
        id_simulacao       = ms_simulacao-id_simulacao
        id_matriz_abast    = is_id_destino-id_matriz
        id_grp_planta      = is_destino-id_grp_planta
        id_centro          = is_destino-id_centro
        id_item            = is_id_material-id_matriz
        tipo_regra         = is_data-tipo_regra
        matnr              = is_data-matnr
        id_grupo_produto   = is_data-id_grupo_produto
        agregador          = is_data-agregador
        mat_planejado      = is_data-mat_planejado
        importacao         = COND #( WHEN iv_modalidade = 'I' THEN abap_true )
        nacional           = COND #( WHEN iv_modalidade = 'N' THEN abap_true )
        id_distribuicao    = is_id_origem-id_matriz
        tipo_origem        = is_data-tipo_origem
        id_origem          = is_data-id_origem
    ).

    lr_data = REF #( ls_custo_matriz ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/custo_mtz FROM ls_custo_matriz.
    COMMIT WORK AND WAIT.

    "Premissa
    DATA(ls_custo_premissa) = VALUE /qaps/custo_prm(
        id_custo_premisa   = cl_system_uuid=>create_uuid_x16_static( )
        id_custo_elementar = ms_custo_elementar-id_custo_elementar
        id_simulacao       = ms_simulacao-id_simulacao
        id_var_input       = is_data-id_var_input
        id_premissa        = is_id_destino-id_premissa
        id_grp_planta      = is_destino-id_grp_planta
        id_centro          = is_destino-id_centro
        id_item            = is_id_material-id_premissa
        tipo_regra         = is_data-tipo_regra
        matnr              = is_data-matnr
        id_grupo_produto   = is_data-id_grupo_produto
        agregador          = is_data-agregador
        mat_planejado      = is_data-mat_planejado
        importacao         = COND #( WHEN iv_modalidade = 'I' THEN abap_true )
        nacional           = COND #( WHEN iv_modalidade = 'N' THEN abap_true )
        id_distribuicao    = is_id_origem-id_premissa
        tipo_origem        = is_data-tipo_origem
        id_origem          = is_data-id_origem    ).

    lr_data = REF #( ls_custo_premissa ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/custo_prm FROM ls_custo_premissa.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD create_premissa.

    DATA ls_id_origem TYPE /qaps/s_pair_matriz_premissa.

    DATA(lt_custo_premissa) = get_custo_premissa( ).
    APPEND LINES OF get_custo_premissa( abap_true ) TO lt_custo_premissa.
    DATA(lt_custo_geral)    = get_custo_premissa( iv_geral = abap_true ).
    DATA(lt_prm_full) = get_premissa_full( ).

    "Criar Destino em Matriz Abastecimento
*    break c060863.
    IF is_relation-destino IS INITIAL.

      DATA(ls_id_destino) = create_premissa_destino( is_destino         = is_destino
                                                     iv_with_parent     = iv_with_parent
                                                     it_parent_premissa = lt_custo_premissa ).

    ELSEIF is_relation-destino = 'X'.

      ls_id_destino-id_premissa = VALUE #( lt_custo_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                              id_centro     = is_destino-id_centro ]-id_premissa OPTIONAL ).

      IF ls_id_destino-id_premissa IS INITIAL.
        ls_id_destino-id_premissa = lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                                     id_centro     = is_destino-id_centro ]-id_premissa.
      ENDIF.

    ELSEIF is_relation-destino = 'C'.

      ls_id_destino-id_premissa = VALUE #( lt_custo_geral[ id_grp_planta = is_destino-id_grp_planta
                                                  id_centro     = is_destino-id_centro ]-id_premissa OPTIONAL ).

      IF ls_id_destino-id_premissa IS INITIAL.
        ls_id_destino-id_premissa = lt_prm_full[ id_grp_planta = is_destino-id_grp_planta
                                                     id_centro     = is_destino-id_centro ]-id_premissa.
      ENDIF.

    ENDIF.

    "Criar material em Matriz Abastecimento
    IF is_relation-material IS INITIAL.

      DATA(ls_id_material) = create_premissa_material( is_id_destino    = ls_id_destino
                                                     is_destino         = is_destino
                                                     is_data            = is_data
                                                     iv_with_parent     = iv_with_parent
                                                     it_parent_premissa = lt_custo_premissa ).
    ELSEIF is_relation-destino = 'X'.
      ls_id_material-id_premissa =
              VALUE #( lt_custo_premissa[ id_grp_planta    = is_destino-id_grp_planta
                                          id_centro        = is_destino-id_centro
                                          tipo_regra       = is_data-tipo_regra
                                          matnr            = is_data-matnr
                                          id_grupo_produto = is_data-id_grupo_produto
                                          agregador        = is_data-agregador
                                          mat_planejado    = is_data-mat_planejado ]-id_item OPTIONAL ).

      IF ls_id_material-id_premissa IS INITIAL.
        ls_id_material-id_premissa = lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                                      id_centro        = is_destino-id_centro
                                                      tipo_regra       = is_data-tipo_regra
                                                      matnr            = is_data-matnr ]-id_item.
      ENDIF.


    ELSEIF is_relation-destino = 'C'.
      ls_id_material-id_premissa =
        VALUE #( lt_custo_geral[ id_grp_planta    = is_destino-id_grp_planta
                                id_centro        = is_destino-id_centro
                                tipo_regra       = is_data-tipo_regra
                                matnr            = is_data-matnr
                                id_grupo_produto = is_data-id_grupo_produto
                                agregador        = is_data-agregador
                                mat_planejado    = is_data-mat_planejado ]-id_item OPTIONAL ).

      IF ls_id_material-id_premissa IS INITIAL.
        ls_id_material-id_premissa = lt_prm_full[ id_grp_planta    = is_destino-id_grp_planta
                                                      id_centro        = is_destino-id_centro
                                                      tipo_regra       = is_data-tipo_regra
                                                      matnr            = is_data-matnr ]-id_item.
      ENDIF.

    ENDIF.

    "Criar Origem em Matriz Abastecimento
    IF is_relation-origem IS INITIAL.
      DATA(lt_id_origem) = create_premissa_origem( is_id_destino  = ls_id_destino
                                                   is_id_material = ls_id_material
                                                   is_data        = is_data
                                                   is_destino     = is_destino
                                                   iv_modalidade  = iv_modalidade
                                                   iv_with_parent     = iv_with_parent
                                                   it_parent_premissa = lt_custo_premissa ).
    ELSE.

      ls_id_origem-id_premissa = VALUE #( lt_custo_geral[ id_grp_planta    = is_destino-id_grp_planta
                                id_centro        = is_destino-id_centro
                                tipo_regra       = is_data-tipo_regra
                                matnr            = is_data-matnr
                                id_grupo_produto = is_data-id_grupo_produto
                                agregador        = is_data-agregador
                                mat_planejado    = is_data-mat_planejado ]-id_distribuicao OPTIONAL ).
      APPEND ls_id_origem TO lt_id_origem.

    ENDIF.

    create_premissa_relation( is_data        = is_data
                              is_destino     = is_destino
                              is_id_destino  = ls_id_destino
                              is_id_material = ls_id_material
                              it_id_origem   = lt_id_origem
                              iv_modalidade  = iv_modalidade ).

    create_prm_agregador_to_sku( is_destino     = is_destino
                                 is_data        = is_data
                                 is_id_destino  = ls_id_destino
                                 is_id_material = ls_id_material
                                 it_id_origem   = lt_id_origem
                                 iv_modalidade  = iv_modalidade  ).


    update_percentual_auto_prem( ).


  ENDMETHOD.


  METHOD create_premissa_destino.

    DATA: lr_data  TYPE REF TO data,
          lr_ponto TYPE RANGE OF /qaps/id_ponto.

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.

      DATA(ls_parent_premissa) = VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                              id_centro     = mc_guid_null ] OPTIONAL  ).

    ENDIF.

    "Premissa
    DATA(ls_premissa_header) = VALUE /qaps/prem_hdr(
        id_premissa     = cl_system_uuid=>create_uuid_x16_static( )
        id_simulacao    = ms_simulacao-id_simulacao
        id_grp_planta   = is_destino-id_grp_planta
        id_centro       = is_destino-id_centro
        id_parent       = ls_parent_premissa-id_premissa ).

    lr_data = REF #( ls_premissa_header ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/prem_hdr FROM ls_premissa_header.
    COMMIT WORK AND WAIT.

*    return-id_matriz = ls_matriz_header-id_matriz_abast.
    return-id_premissa = ls_premissa_header-id_premissa.

  ENDMETHOD.


  METHOD create_premissa_material.

    DATA: lr_data TYPE REF TO data.

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.
*      break c060863.
      DATA(ls_parent_premissa) = VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                                              id_centro     = mc_guid_null
                                                              tipo_regra       = is_data-tipo_regra
                                                              matnr            = is_data-matnr
                                                              id_grupo_produto = is_data-id_grupo_produto
                                                              agregador        = is_data-agregador
                                                              mat_planejado    = is_data-mat_planejado ] OPTIONAL  ).

    ENDIF.

    "Premissa
    DATA(ls_premissa_item) =  VALUE /qaps/prem_item(
        id_item          = cl_system_uuid=>create_uuid_x16_static( )
        id_premissa      = is_id_destino-id_premissa
        id_parent        = ls_parent_premissa-id_item
        tipo_regra       = is_data-tipo_regra
        matnr            = /qaps/cl_helper_data=>material_to_input( is_data-matnr )
        id_grupo_produto = is_data-id_grupo_produto
        agregador        = is_data-agregador
        mat_planejado    = /qaps/cl_helper_data=>material_to_input( is_data-mat_planejado ) ).

    lr_data = REF #( ls_premissa_item ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/prem_item FROM ls_premissa_item.
    COMMIT WORK AND WAIT.

*    return-id_matriz = ls_matriz_item-id_item.
    return-id_premissa = ls_premissa_item-id_item.

  ENDMETHOD.


  METHOD create_premissa_origem.

    DATA: lt_matriz_distr   TYPE TABLE OF /qaps/matriz_dst,
          lt_entry          TYPE TABLE OF /qaps/prem_distr,
          lt_premissa_distr TYPE TABLE OF /qaps/prem_distr.

    DATA: lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
*          ls_entry      TYPE /qaps/matriz_dst,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_data-id_origem
      INTO @DATA(ls_origem).

    IF iv_with_parent = abap_true AND is_destino-id_centro <> mc_guid_null.
*      break c060863.
      IF NOT ls_origem-id_ponto IS INITIAL.
        DATA(ls_parent_premissa) =
              VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                           id_centro     = mc_guid_null
                                           tipo_regra       = is_data-tipo_regra
                                           matnr            = is_data-matnr
                                           id_grupo_produto = is_data-id_grupo_produto
                                           agregador        = is_data-agregador
                                           mat_planejado    = is_data-mat_planejado
                                           importacao      = COND #( WHEN iv_modalidade = 'I'
                                                                     THEN 'X' )
                                           nacional        = COND #( WHEN iv_modalidade = 'N'
                                                                     THEN 'X' )
                                           tipo_origem     = ls_origem-tipo_ponto
                                           id_origem       = ls_origem-id_ponto ] OPTIONAL  ).
      ELSE.
        ls_parent_premissa =
            VALUE #( it_parent_premissa[ id_grp_planta = is_destino-id_grp_planta
                                         id_centro     = mc_guid_null
                                         tipo_regra       = is_data-tipo_regra
                                         matnr            = is_data-matnr
                                         id_grupo_produto = is_data-id_grupo_produto
                                         agregador        = is_data-agregador
                                         mat_planejado    = is_data-mat_planejado
                                         importacao      = COND #( WHEN iv_modalidade = 'I'
                                                                   THEN 'X' )
                                         nacional        = COND #( WHEN iv_modalidade = 'N'
                                                                   THEN 'X' ) ] OPTIONAL  ).
      ENDIF.

    ENDIF.

    "Premissa
    DATA(ls_premissa_distr) = VALUE /qaps/prem_distr(
        id_premissa     = is_id_destino-id_premissa
        id_item         = is_id_material-id_premissa
        modalidade      =  iv_modalidade
        tipo_origem     = ls_origem-tipo_ponto
        id_origem       = ls_origem-id_ponto
        id_parent       = ls_parent_premissa-id_distribuicao
    ).

    IF ls_premissa_distr-id_origem IS INITIAL.

      DATA(lt_heranca) = herdar_origem( ls_premissa_distr ).

      IF lines( lt_heranca ) > 0.

        LOOP AT lt_heranca INTO DATA(ls_heranca).
          DATA(ls_new) = ls_premissa_distr.
          ls_new-tipo_origem = ls_heranca-tipo_origem.
          ls_new-id_origem = ls_heranca-id_origem.
          ls_new-id_parent = ls_heranca-id_parent.
          ls_new-percentual = ls_heranca-is_distrib_origem-percentual.
          APPEND ls_new TO lt_entry.

          "Hierarquia entre pai e filho em nível de material
          UPDATE /qaps/prem_item
          SET id_parent = ls_heranca-id_item_parent
          WHERE id_item = ls_heranca-id_item.

        ENDLOOP.

      ELSE.
        APPEND ls_premissa_distr TO lt_entry.
      ENDIF.

    ELSE."IF lines( lt_entry ) = 0.
      APPEND ls_premissa_distr TO lt_entry.
    ENDIF.

    LOOP AT lt_entry INTO DATA(ls_entry).

      REFRESH: lt_premissa_distr.
      CLEAR ls_premissa_distr.
      ls_premissa_distr = ls_entry.
      ls_premissa_distr-id_distribuicao = cl_system_uuid=>create_uuid_x16_static( ).


      DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

      ls_periodo-year = ms_simulacao-periodo_inicial(4).
      ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = ms_simulacao-periodo_inicial
          final   = ms_simulacao-periodo_final    ).

      lr_data = REF #( ls_premissa_distr ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      WHILE lv_periodo_inicial <= ls_per-final.

        lv_id = sy-index.

        lv_pos = lv_pos + 1.

        IF lv_id = 1.
          lv_mes = ls_periodo-month.
          lv_ano = ls_periodo-year.
        ELSE.
          lv_mes =  lv_mes + 1.

          IF lv_mes > 12.
            lv_mes = '01'.
            lv_ano = lv_ano + 1.
          ENDIF.

          lv_periodo_inicial = lv_ano && lv_mes.

        ENDIF.

        check lv_periodo_inicial <= ls_per-final.

        ls_premissa_distr-periodo = lv_periodo = lv_ano && lv_mes.
        APPEND ls_premissa_distr TO lt_premissa_distr.

      ENDWHILE.

      IF lines( lt_premissa_distr ) > 0.
        MODIFY /qaps/prem_distr FROM TABLE lt_premissa_distr.
        COMMIT WORK AND WAIT.
        APPEND VALUE /qaps/s_pair_matriz_premissa(
            id_premissa = ls_premissa_distr-id_distribuicao ) TO return.

      ENDIF.

      "Vincular Trajetos
      create_premissa_trajeto( is_origem  = ls_premissa_distr
                               is_parent_premissa = ls_parent_premissa ).

    ENDLOOP.


  ENDMETHOD.


  METHOD create_premissa_relation.

    DATA lr_data TYPE REF TO data.


    LOOP AT it_id_origem INTO DATA(ls_id_origem).

      DATA(ls_custo_premissa) = VALUE /qaps/custo_prm(
          id_custo_premisa   = cl_system_uuid=>create_uuid_x16_static( )
          id_custo_elementar = ms_custo_elementar-id_custo_elementar
          id_simulacao       = ms_simulacao-id_simulacao
          id_var_input       = is_data-id_var_input
          id_premissa        = is_id_destino-id_premissa
          id_grp_planta      = is_destino-id_grp_planta
          id_centro          = is_destino-id_centro
          id_item            = is_id_material-id_premissa
          tipo_regra         = is_data-tipo_regra
          matnr              = is_data-matnr
          id_grupo_produto   = is_data-id_grupo_produto
          agregador          = is_data-agregador
          mat_planejado      = is_data-mat_planejado
          importacao         = COND #( WHEN iv_modalidade = 'I' THEN abap_true )
          nacional           = COND #( WHEN iv_modalidade = 'N' THEN abap_true )
          id_distribuicao    = ls_id_origem-id_premissa
          tipo_origem        = is_data-tipo_origem
          id_origem          = is_data-id_origem ).

      lr_data = REF #( ls_custo_premissa ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/custo_prm FROM ls_custo_premissa.
      COMMIT WORK AND WAIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD CREATE_PREMISSA_RELATION_MTZ.

    DATA lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/custo_prm
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_custo_premissa).

    SELECT *
      FROM /qaps/v_prm_all
      where id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_premissa_all).

    "Premissa
    LOOP AT lt_premissa_all INTO DATA(ls_premissa_all).

      CHECK NOT line_exists( lt_custo_premissa[ id_custo_elementar = ms_custo_elementar-id_custo_elementar
                                                id_simulacao       = ms_simulacao-id_simulacao
                                                id_var_input       = is_data-id_var_input
                                                id_premissa        = ls_premissa_all-id_premissa
                                                id_grp_planta      = ls_premissa_all-id_grp_planta
                                                id_centro          = ls_premissa_all-id_centro
                                                id_item            = ls_premissa_all-id_item
                                                tipo_regra         = ls_premissa_all-tipo_regra
                                                matnr              = ls_premissa_all-matnr
                                                id_grupo_produto   = ls_premissa_all-id_grupo_produto
                                                agregador          = ls_premissa_all-agregador
                                                mat_planejado      = ls_premissa_all-mat_planejado
                                                importacao         = COND #( WHEN ls_premissa_all-modalidade = 'I' THEN abap_true )
                                                nacional           = COND #( WHEN ls_premissa_all-modalidade = 'N' THEN abap_true )
                                                id_distribuicao    = ls_premissa_all-id_distribuicao
                                                tipo_origem        = ls_premissa_all-tipo_origem
                                                id_origem          = ls_premissa_all-id_origem   ] ).

      DATA(ls_custo_premissa) = VALUE /qaps/custo_prm(
          id_custo_premisa   = cl_system_uuid=>create_uuid_x16_static( )
          id_custo_elementar = ms_custo_elementar-id_custo_elementar
          id_simulacao       = ms_simulacao-id_simulacao
          id_var_input       = is_data-id_var_input
          id_premissa        = ls_premissa_all-id_premissa
          id_grp_planta      = ls_premissa_all-id_grp_planta
          id_centro          = ls_premissa_all-id_centro
          id_item            = ls_premissa_all-id_item
          tipo_regra         = ls_premissa_all-tipo_regra
          matnr              = ls_premissa_all-matnr
          id_grupo_produto   = ls_premissa_all-id_grupo_produto
          agregador          = ls_premissa_all-agregador
          mat_planejado      = ls_premissa_all-mat_planejado
          importacao         = COND #( WHEN ls_premissa_all-modalidade = 'I' THEN abap_true )
          nacional           = COND #( WHEN ls_premissa_all-modalidade = 'N' THEN abap_true )
          id_distribuicao    = ls_premissa_all-id_distribuicao
          tipo_origem        = ls_premissa_all-tipo_origem
          id_origem          = ls_premissa_all-id_origem ).

      lr_data = REF #( ls_custo_premissa ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/custo_prm FROM ls_custo_premissa.
      COMMIT WORK AND WAIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_premissa_trajeto.

    TYPES: BEGIN OF ts_mapping,
             id_prem_trajeto TYPE /qaps/prem_traj-id_prem_trajeto,
             id_new          TYPE /qaps/prem_traj-id_prem_trajeto,
           END OF ts_mapping.

    DATA lt_mapping TYPE TABLE OF ts_mapping.

    IF not is_parent_premissa IS INITIAL.
      SELECT *
        FROM /qaps/prem_traj
        WHERE id_distribuicao = @is_parent_premissa-id_distribuicao
        INTO TABLE @DATA(lt_prem_traj).
    endif.

    if lines( lt_prem_traj ) = 0.
      SELECT SINGLE *
        FROM /qaps/v_prm_all
        WHERE id_distribuicao = @is_origem-id_distribuicao
        and   id_simulacao    = @ms_simulacao-id_simulacao
        INTO @DATA(ls_distrib).

      DATA(lt_trajeto) = get_trajeto( ls_distrib ).

      CHECK lines( lt_trajeto ) > 0.

      LOOP AT lt_trajeto INTO DATA(ls_trajeto).

        create_trajeto( is_trajeto        = ls_trajeto
                        is_origem_destino = ls_distrib ).
      ENDLOOP.

    ELSE.

      SORT lt_prem_traj BY id_prem_trajeto periodo.

      LOOP AT lt_prem_traj ASSIGNING FIELD-SYMBOL(<fs>).

        IF NOT line_exists( lt_mapping[ id_prem_trajeto = <fs>-id_prem_trajeto ] ).
          DATA(ls_mapping) = VALUE ts_mapping(
              id_prem_trajeto = <fs>-id_prem_trajeto
              id_new          = cl_system_uuid=>create_uuid_x16_static( ) ).
          APPEND ls_mapping TO lt_mapping.
        ELSE.
          ls_mapping = lt_mapping[ id_prem_trajeto = <fs>-id_prem_trajeto ].
        ENDIF.

        <fs>-id_distribuicao = is_origem-id_distribuicao.
        <fs>-id_prem_trajeto = ls_mapping-id_new.
        <fs>-id_parent       = ls_mapping-id_prem_trajeto.

      ENDLOOP.

      MODIFY /qaps/prem_traj FROM TABLE lt_prem_traj.

    ENDIF.

  ENDMETHOD.


  METHOD create_prem_relat_heranca_sku.

    DATA lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/custo_prm
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      AND id_item = @is_item-id_item
      INTO @DATA(ls_source).

    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_item_parent = @is_item-id_item
      and   id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_premissa_all).
    "Premissa
*    BREAK c060863.
    LOOP AT lt_premissa_all INTO DATA(ls_premissa_all).

      DATA(ls_custo_premissa) = VALUE /qaps/custo_prm(
          id_custo_premisa   = cl_system_uuid=>create_uuid_x16_static( )
          id_custo_elementar = ms_custo_elementar-id_custo_elementar
          id_simulacao       = ms_simulacao-id_simulacao
          id_var_input       = ls_source-id_var_input
          id_premissa        = ls_premissa_all-id_premissa
          id_grp_planta      = ls_premissa_all-id_grp_planta
          id_centro          = ls_premissa_all-id_centro
          id_item            = ls_premissa_all-id_item
          tipo_regra         = ls_premissa_all-tipo_regra
          matnr              = ls_premissa_all-matnr
          id_grupo_produto   = ls_premissa_all-id_grupo_produto
          agregador          = ls_premissa_all-agregador
          mat_planejado      = ls_premissa_all-mat_planejado
          importacao         = COND #( WHEN ls_premissa_all-modalidade = 'I' THEN abap_true )
          nacional           = COND #( WHEN ls_premissa_all-modalidade = 'N' THEN abap_true )
          id_distribuicao    = ls_premissa_all-id_distribuicao
          tipo_origem        = ls_premissa_all-tipo_origem
          id_origem          = ls_premissa_all-id_origem ).

      lr_data = REF #( ls_custo_premissa ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/custo_prm FROM ls_custo_premissa.
      COMMIT WORK AND WAIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_prm_agregador_to_sku.

    TYPES: BEGIN OF ts_mapping,
             id_distribuicao TYPE /qaps/prem_distr-id_distribuicao,
             id_new          TYPE /qaps/prem_distr-id_distribuicao,
           END OF ts_mapping.

    TYPES: BEGIN OF ts_check,
             id_distribuicao TYPE /qaps/prem_distr-id_distribuicao,
             id_prem_trajeto TYPE /qaps/prem_traj-id_prem_trajeto,
             id_new          TYPE /qaps/prem_traj-id_prem_trajeto,
           END OF ts_check.

    DATA: lt_mapping TYPE TABLE OF ts_mapping,
          lt_check   TYPE TABLE OF ts_check.

    DATA: lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/prem_item
      WHERE id_item = @is_id_material-id_premissa
      INTO @DATA(ls_item).

    CHECK ls_item-tipo_regra = 'AG'.

    break c060863.
    LOOP AT it_id_origem INTO DATA(ls_id_origem).

      SELECT *
        FROM /qaps/prem_distr
        WHERE id_item = @is_id_material-id_premissa
        AND  id_distribuicao = @ls_id_origem-id_premissa
        INTO TABLE @DATA(lt_distrib).

      IF lines( lt_distrib ) > 0.

        SELECT *
          FROM /qaps/prem_traj
          FOR ALL ENTRIES IN @lt_distrib
          WHERE id_distribuicao = @lt_distrib-id_distribuicao
          INTO TABLE @DATA(lt_prem_trajeto).

      ENDIF.

*    BREAK c060863.

      SELECT *
        FROM /qaps/v_prm_full
        WHERE id_premissa = @is_id_destino-id_premissa
        AND tipo_regra = 'MA'
        INTO TABLE @DATA(lt_material_check).

      LOOP AT lt_material_check ASSIGNING FIELD-SYMBOL(<fs_material_check>).
        <fs_material_check>-matnr = |{ <fs_material_check>-matnr ALPHA = IN WIDTH = 18 }|.
      ENDLOOP.

      DATA(lt_material) = mo_material->get_materiais_by_agregador( ls_item-agregador ).

      LOOP AT lt_material INTO DATA(ls_material).

        IF NOT line_exists( lt_material_check[ matnr = ls_material-matnr ] ).

          DATA(ls_template) = ls_item.
          CLEAR: ls_template-agregador.

          ls_template-id_item = cl_system_uuid=>create_uuid_x16_static( ).
          ls_template-tipo_regra = 'MA'.
          ls_template-matnr = |{ ls_material-matnr ALPHA = OUT WIDTH = 18 }|.
          ls_template-id_parent = ls_item-id_item.
          ls_template-oculto = abap_true.
          CLEAR: ls_template-created_by,
                 ls_template-created_on,
                 ls_template-created_in.
          lr_data = REF #( ls_template ).
          preencher_dados_controle( CHANGING cr_data = lr_data  ).
          MODIFY /qaps/prem_item FROM ls_template.
        ELSE.
          ls_template-id_item = lt_material_check[ matnr = ls_material-matnr ]-id_item.
          CONTINUE.
*        update /qaps/prem_item
*        set id_parent = ls_item-id_item
*        where id_item = ls_template-id_item.
*        commit work.
        ENDIF.

        REFRESH lt_mapping.

        LOOP AT lt_distrib ASSIGNING FIELD-SYMBOL(<fs>).

          IF line_exists( lt_mapping[ id_distribuicao = <fs>-id_distribuicao ] ).
            DATA(ls_mapping) = lt_mapping[ id_distribuicao = <fs>-id_distribuicao ].
            DATA(lv_key_distrib) = ls_mapping-id_new.
          ELSE.
            lv_key_distrib = cl_system_uuid=>create_uuid_x16_static( ).
            APPEND VALUE ts_mapping( id_distribuicao = <fs>-id_distribuicao
                                     id_new          = lv_key_distrib ) TO lt_mapping.
          ENDIF.

          <fs>-id_parent = <fs>-id_distribuicao.
          <fs>-id_item   = ls_template-id_item.
          <fs>-id_distribuicao = lv_key_distrib.

        ENDLOOP.

        MODIFY /qaps/prem_distr FROM TABLE lt_distrib.
        COMMIT WORK AND WAIT.

        "Herdando Trajetos
        LOOP AT lt_mapping INTO ls_mapping.

          DATA(lt_traj) = lt_prem_trajeto.
*          DELETE lt_traj WHERE id_distribuicao <> ls_mapping-id_distribuicao.

          SORT lt_traj BY id_prem_trajeto.
          LOOP AT lt_traj ASSIGNING FIELD-SYMBOL(<fs_traj>).

            IF NOT line_exists( lt_check[ id_distribuicao = ls_mapping-id_new
                                          id_prem_trajeto = <fs_traj>-id_prem_trajeto ] ).

              DATA(lv_key_prem_traj) = cl_system_uuid=>create_uuid_x16_static( ).

              DATA(ls_check) = VALUE ts_check(
                  id_distribuicao = ls_mapping-id_new
                  id_prem_trajeto = <fs_traj>-id_prem_trajeto
                  id_new          = lv_key_prem_traj ).
              APPEND ls_check TO lt_check.
            ELSE.
              ls_check = lt_check[ id_distribuicao = ls_mapping-id_new
                                   id_prem_trajeto = <fs_traj>-id_prem_trajeto ].

            ENDIF.

            <fs_traj>-id_prem_trajeto = ls_check-id_new.
            <fs_traj>-id_parent       = ls_check-id_prem_trajeto.
            <fs_traj>-id_distribuicao = ls_check-id_distribuicao.

          ENDLOOP.

          MODIFY /qaps/prem_traj FROM TABLE lt_traj.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    COMMIT WORK AND WAIT.

    create_prem_relat_heranca_sku( ls_item ).

  ENDMETHOD.


  METHOD create_std_prd_assign_destino.

    DATA lr_data TYPE REF TO data.
    DATA lt_entry TYPE TABLE OF /qaps/prem_hdr.

    data(lt_destino) = expand_grp_planta_to_werks_dst( it_destino ).

    LOOP AT lt_destino ASSIGNING FIELD-SYMBOL(<ls_destino>).

      CHECK <ls_destino>-id_premissa IS INITIAL.

      DATA(ls_entry) = VALUE /qaps/prem_hdr(
          id_premissa     = cl_system_uuid=>create_uuid_x16_static( )
          id_simulacao    = <ls_destino>-id_simulacao
          id_grp_planta   = <ls_destino>-id_grp_planta
          id_centro       = <ls_destino>-id_centro ).

      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_entry TO lt_entry.

      <ls_destino>-id_premissa = ls_entry-id_premissa.

    ENDLOOP.

    IF lines( lt_entry ) > 0.
      MODIFY /qaps/prem_hdr FROM TABLE lt_entry.
      COMMIT WORK AND WAIT.
    ENDIF.

    return = lt_destino.

  ENDMETHOD.


  METHOD create_std_prd_assign_material.

    DATA lr_data TYPE REF TO data.
    DATA lt_entry TYPE TABLE OF /qaps/prem_item.
    DATA lt_material TYPE /qaps/t_relation_std_prd_item.
    DATA lv_matnr TYPE matnr.

    DATA(lt_prm_full) = get_premissa_full( ).

    DELETE lt_prm_full WHERE tipo_regra <> 'MA'.
    LOOP AT lt_prm_full ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/v_stp_full
      WHERE id_std_producao = @ms_simulacao-id_std_producao
      INTO TABLE @DATA(lt_std_prd).

    SELECT *
      FROM /qaps/v_ponto
      WHERE tipo_ponto = 'W'
      INTO TABLE @DATA(lt_centro).

    DATA(lt_destino) = expand_grp_planta_to_werks_dst( it_destino ).

    LOOP AT it_material INTO DATA(ls_material_main).
      DATA(lt_mat_partial) = expand_grp_planta_to_werks_mat( ls_material_main ).
      APPEND LINES OF lt_mat_partial TO lt_material.
    ENDLOOP.

    LOOP AT lt_destino INTO DATA(ls_destino).

      CASE ls_destino-tipo_origem.

        WHEN 'G'.

          LOOP AT lt_material INTO DATA(ls_material)
              WHERE cod_grp_planta = ls_destino-cod_grp_planta
              AND   werks          = ls_destino-werks.

            IF ls_destino-werks = mc_guid_null OR ls_destino-werks IS INITIAL.

              CHECK line_exists( lt_std_prd[ cod_grp_planta = ls_destino-cod_grp_planta
                                             werks          = ''
                                             matnr          = ls_material-matnr ] ).

              IF line_exists( lt_prm_full[ id_premissa = ls_destino-id_premissa
                                           id_grp_planta = ls_destino-id_grp_planta
                                           id_centro     = mc_guid_null
                                           matnr       = ls_material-matnr ] ).

                DATA(ls_item) = lt_prm_full[ id_premissa = ls_destino-id_premissa
                                             id_grp_planta = ls_destino-id_grp_planta
                                             id_centro     = mc_guid_null
                                             matnr       = ls_material-matnr ].

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa     = ls_destino-id_premissa
                    id_grp_planta   = ls_destino-id_grp_planta
                    cod_grp_planta  = ls_destino-cod_grp_planta
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra      = 'MA'
                    matnr           = ls_material-matnr
                    id_item         = ls_item-id_item
                    tipo_origem     = ls_destino-tipo_origem
                    id_origem       = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

                UPDATE /qaps/prem_item
                SET std_prd = 'X'
                    oculto  = ''
                WHERE id_item = ls_item-id_item.

              ELSE.

                DATA(ls_entry) = VALUE /qaps/prem_item(
                    id_item          = cl_system_uuid=>create_uuid_x16_static( )
                    id_premissa      = ls_destino-id_premissa
                    tipo_regra       = 'MA'
                    std_prd          = 'X'
                    matnr            = /qaps/cl_helper_data=>material_to_input( ls_material-matnr  ) ).

                lr_data = REF #( ls_entry ).
                preencher_dados_controle( CHANGING cr_data = lr_data ).
                APPEND ls_entry TO lt_entry.

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa = ls_destino-id_premissa
                    id_grp_planta   = ls_destino-id_grp_planta
                    cod_grp_planta  = ls_destino-cod_grp_planta
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra  = 'MA'
                    matnr       = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
                    id_item     = ls_entry-id_item
                    tipo_origem = ls_destino-tipo_origem
                    id_origem   = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

              ENDIF.

            ELSE.

              CHECK line_exists( lt_std_prd[ cod_grp_planta = ls_destino-cod_grp_planta
                                             matnr          = ls_material-matnr ] )
                 OR line_exists( lt_std_prd[ werks          = ls_destino-werks
                                             matnr          = ls_material-matnr ] ).

              IF line_exists( lt_prm_full[ id_premissa = ls_destino-id_premissa
                                           id_grp_planta = ls_destino-id_grp_planta
                                           id_centro     = ls_destino-id_centro
                                           matnr       = ls_material-matnr ] ).

                ls_item = lt_prm_full[ id_premissa = ls_destino-id_premissa
                                       id_grp_planta = ls_destino-id_grp_planta
                                       id_centro     = ls_destino-id_centro
                                       matnr       = ls_material-matnr ].

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa     = ls_destino-id_premissa
                    id_grp_planta   = ls_destino-id_grp_planta
                    cod_grp_planta  = ls_destino-cod_grp_planta
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra      = 'MA'
                    matnr           = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
                    id_item         = ls_item-id_item
                    tipo_origem     = ls_destino-tipo_origem
                    id_origem       = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

                UPDATE /qaps/prem_item
                SET std_prd = 'X'
                    oculto  = ''
                WHERE id_item = ls_item-id_item.

              ELSE.

                ls_entry = VALUE /qaps/prem_item(
                    id_item          = cl_system_uuid=>create_uuid_x16_static( )
                    id_premissa      = ls_destino-id_premissa
                    tipo_regra       = 'MA'
                    std_prd          = 'X'
                    matnr            = /qaps/cl_helper_data=>material_to_input( ls_material-matnr  ) ).

                lr_data = REF #( ls_entry ).
                preencher_dados_controle( CHANGING cr_data = lr_data ).
                APPEND ls_entry TO lt_entry.

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa = ls_destino-id_premissa
                    id_grp_planta   = ls_destino-id_grp_planta
                    cod_grp_planta  = ls_destino-cod_grp_planta
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra  = 'MA'
                    matnr       = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
                    id_item     = ls_entry-id_item
                    tipo_origem = ls_destino-tipo_origem
                    id_origem   = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

              ENDIF.

            ENDIF.

          ENDLOOP.

        WHEN 'W'.

          LOOP AT lt_material INTO ls_material
              WHERE werks          = ls_destino-werks.

*            IF ls_destino-werks = mc_guid_null OR ls_destino-werks IS INITIAL.
*
*              CHECK line_exists( lt_std_prd[ cod_grp_planta = ls_destino-cod_grp_planta
*                                             werks          = ''
*                                             matnr          = ls_material-matnr ] ).
*
*              IF line_exists( lt_prm_full[ id_premissa = ls_destino-id_premissa
*                                           id_grp_planta = ls_destino-id_grp_planta
*                                           id_centro     = mc_guid_null
*                                           matnr       = ls_material-matnr ] ).
*
*                DATA(ls_item) = lt_prm_full[ id_premissa = ls_destino-id_premissa
*                                             id_grp_planta = ls_destino-id_grp_planta
*                                             id_centro     = mc_guid_null
*                                             matnr       = ls_material-matnr ].
*
*                APPEND VALUE /qaps/s_relation_std_prd_item(
*                    id_premissa     = ls_destino-id_premissa
*                    id_grp_planta   = ls_destino-id_grp_planta
*                    cod_grp_planta  = ls_destino-cod_grp_planta
*                    id_centro       = ls_destino-id_centro
*                    werks           = ls_destino-werks
*                    tipo_regra      = 'MA'
*                    matnr           = ls_material-matnr
*                    id_item         = ls_item-id_item
*                    tipo_origem     = ls_destino-tipo_origem
*                    id_origem       = COND #( WHEN ls_destino-tipo_origem = 'G'
*                                           THEN ls_destino-id_grp_planta
*                                           ELSE ls_destino-id_centro ) ) TO return.
*
*                UPDATE /qaps/prem_item
*                SET std_prd = 'X'
*                    oculto  = ''
*                WHERE id_item = ls_item-id_item.
*
*              ELSE.
*
*                DATA(ls_entry) = VALUE /qaps/prem_item(
*                    id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                    id_premissa      = ls_destino-id_premissa
*                    tipo_regra       = 'MA'
*                    std_prd          = 'X'
*                    matnr            = /qaps/cl_helper_data=>material_to_input( ls_material-matnr  ) ).
*
*                lr_data = REF #( ls_entry ).
*                preencher_dados_controle( CHANGING cr_data = lr_data ).
*                APPEND ls_entry TO lt_entry.
*
*                APPEND VALUE /qaps/s_relation_std_prd_item(
*                    id_premissa = ls_destino-id_premissa
*                    id_grp_planta   = ls_destino-id_grp_planta
*                    cod_grp_planta  = ls_destino-cod_grp_planta
*                    id_centro       = ls_destino-id_centro
*                    werks           = ls_destino-werks
*                    tipo_regra  = 'MA'
*                    matnr       = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
*                    id_item     = ls_entry-id_item
*                    tipo_origem = ls_destino-tipo_origem
*                    id_origem   = COND #( WHEN ls_destino-tipo_origem = 'G'
*                                           THEN ls_destino-id_grp_planta
*                                           ELSE ls_destino-id_centro ) ) TO return.
*
*              ENDIF.
*
*            ELSE.

              CHECK line_exists( lt_std_prd[ werks          = ls_destino-werks
                                             matnr          = ls_material-matnr ] ).

              IF line_exists( lt_prm_full[ id_premissa = ls_destino-id_premissa
                                           id_centro     = ls_destino-id_centro
                                           matnr       = ls_material-matnr ] ).

                ls_item = lt_prm_full[ id_premissa = ls_destino-id_premissa
                                       id_centro     = ls_destino-id_centro
                                       matnr       = ls_material-matnr ].

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa     = ls_destino-id_premissa
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra      = 'MA'
                    matnr           = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
                    id_item         = ls_item-id_item
                    tipo_origem     = ls_destino-tipo_origem
                    id_origem       = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

                UPDATE /qaps/prem_item
                SET std_prd = 'X'
                    oculto  = ''
                WHERE id_item = ls_item-id_item.

              ELSE.

                ls_entry = VALUE /qaps/prem_item(
                    id_item          = cl_system_uuid=>create_uuid_x16_static( )
                    id_premissa      = ls_destino-id_premissa
                    tipo_regra       = 'MA'
                    std_prd          = 'X'
                    matnr            = /qaps/cl_helper_data=>material_to_input( ls_material-matnr  ) ).

                lr_data = REF #( ls_entry ).
                preencher_dados_controle( CHANGING cr_data = lr_data ).
                APPEND ls_entry TO lt_entry.

                APPEND VALUE /qaps/s_relation_std_prd_item(
                    id_premissa = ls_destino-id_premissa
                    id_centro       = ls_destino-id_centro
                    werks           = ls_destino-werks
                    tipo_regra  = 'MA'
                    matnr       = /qaps/cl_helper_data=>material_to_input( ls_material-matnr )
                    id_item     = ls_entry-id_item
                    tipo_origem = ls_destino-tipo_origem
                    id_origem   = COND #( WHEN ls_destino-tipo_origem = 'G'
                                           THEN ls_destino-id_grp_planta
                                           ELSE ls_destino-id_centro ) ) TO return.

              ENDIF.

*            ENDIF.

          ENDLOOP.

      ENDCASE.

    ENDLOOP.


    IF lines( lt_entry ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_entry.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD create_std_prd_assign_origem.

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @it_material
      WHERE id_premissa = @it_material-id_premissa
      INTO TABLE @DATA(lt_prm_full).

    SELECT *
      FROM /qaps/v_stp_full
      WHERE id_std_producao = @ms_simulacao-id_std_producao
      INTO TABLE @DATA(lt_std_prd).

    LOOP AT it_material INTO DATA(ls_material).

*      DATA(ls_destino) = lt_prm_full[ id_premissa = ls_material-id_premissa ].
      DATA(ls_destino) = it_destino[ id_premissa = ls_material-id_premissa ].

      IF ls_destino-id_centro = mc_guid_null.

        CHECK line_exists( lt_std_prd[ matnr = ls_material-matnr
                                       id_grp_planta = ls_material-id_grp_planta ] ).

        DATA(lt_material) = expand_grp_planta_to_werks_mat( ls_material ).

        LOOP AT lt_material INTO DATA(ls_exp_material).
          create_std_prd_distribuicao( is_data    = ls_exp_material ).
        ENDLOOP.

      ELSEIF ls_destino-id_centro <> mc_guid_null.

        CHECK line_exists( lt_std_prd[ matnr = ls_material-matnr
                                       id_grp_planta = ls_material-id_grp_planta ] )
           or line_exists( lt_std_prd[ matnr = ls_material-matnr
                                       werks     = ls_material-werks ] ).

        create_std_prd_distribuicao( is_data    = ls_material ).

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD create_std_prd_distribuicao.

    DATA: lt_entry          TYPE TABLE OF /qaps/prem_distr,
          lt_premissa_distr TYPE TABLE OF /qaps/prem_distr.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,

          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_externo = @is_data-id_origem
      INTO @DATA(ls_origem).

    "Premissa
    DATA(ls_premissa_distr) = VALUE /qaps/prem_distr(
        id_premissa     = is_data-id_premissa
        id_item         = is_data-id_item
        modalidade      = 'P'
        tipo_origem     = ls_origem-tipo_ponto
        id_origem       = ls_origem-id_ponto
*        id_parent       = ls_parent_premissa-id_distribuicao
    ).

    APPEND ls_premissa_distr TO lt_entry.

    LOOP AT lt_entry INTO DATA(ls_entry).

      REFRESH: lt_premissa_distr.
      CLEAR ls_premissa_distr.
      ls_premissa_distr = ls_entry.
      ls_premissa_distr-id_distribuicao = cl_system_uuid=>create_uuid_x16_static( ).

      DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

      ls_periodo-year = ms_simulacao-periodo_inicial(4).
      ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = ms_simulacao-periodo_inicial
          final   = ms_simulacao-periodo_final    ).

      lr_data = REF #( ls_premissa_distr ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      WHILE lv_periodo_inicial <= ls_per-final.

        lv_id = sy-index.

        lv_pos = lv_pos + 1.

        IF lv_id = 1.
          lv_mes = ls_periodo-month.
          lv_ano = ls_periodo-year.
        ELSE.
          lv_mes =  lv_mes + 1.

          IF lv_mes > 12.
            lv_mes = '01'.
            lv_ano = lv_ano + 1.
          ENDIF.

          lv_periodo_inicial = lv_ano && lv_mes.

        ENDIF.

        check lv_periodo_inicial <= ls_per-final.

        ls_premissa_distr-periodo = lv_periodo = lv_ano && lv_mes.
        ls_premissa_distr-percentual = '100'.
        APPEND ls_premissa_distr TO lt_premissa_distr.

      ENDWHILE.

      IF lines( lt_premissa_distr ) > 0.
        MODIFY /qaps/prem_distr FROM TABLE lt_premissa_distr.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_std_producao.

    CHECK NOT ms_simulacao-id_std_producao IS INITIAL.
    break abap.
    DATA(ls_relation) = check_relation_std_prd( iv_id_tp_lista = ms_simulacao-id_tp_lista
                                                is_data        = ms_simulacao ).

    check_relation( changing cs_data = ls_relation ).

    DATA(lt_destino) = create_std_prd_assign_destino( ls_relation-t_destino ).

    DATA(lt_material) = create_std_prd_assign_material( it_destino = lt_destino
                                                        it_material = ls_relation-t_item ).

    create_std_prd_assign_origem( it_destino  = lt_destino
                                  it_material = lt_material ).

    update_percentual_auto_prem( ).

  ENDMETHOD.


  METHOD create_trajeto.

    DATA lt_data TYPE TABLE OF /qaps/prem_traj.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @is_origem_destino-id_simulacao
      INTO @DATA(ls_simulacao).

    DATA(lv_periodo_inicial) = ls_simulacao-periodo_inicial.

    ls_periodo-year = ls_simulacao-periodo_inicial(4).
    ls_periodo-month = ls_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = ls_simulacao-periodo_inicial
        final   = ls_simulacao-periodo_final    ).

    DATA(ls_data) = VALUE /qaps/prem_traj(
        id_prem_trajeto        = cl_system_uuid=>create_uuid_x16_static( )
*        periodo                =
        id_trajeto             = is_trajeto-id_trajeto
        id_premissa            = is_origem_destino-id_premissa
        id_item                = is_origem_destino-id_item
        id_distribuicao        = is_origem_destino-id_distribuicao
        percentual             = 0 ).

    lr_data = REF #( ls_data ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial <= ls_per-final.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      check lv_periodo_inicial <= ls_per-final.

      ls_data-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_data TO lt_data.

    ENDWHILE.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_traj FROM TABLE lt_data.
      COMMIT WORK.
    ENDIF.


  ENDMETHOD.


  METHOD delete_matriz_abastecimento.

    DATA lr_id_custo_matriz TYPE RANGE OF /qaps/custo_mtz-id_custo_matriz.

    DATA: lr_id_matriz_abast TYPE RANGE OF /qaps/matriz_hdr-id_matriz_abast,
          lr_id_item         TYPE RANGE OF /qaps/matriz_itm-id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/matriz_dst-id_distribuicao.

    SELECT *
      FROM /qaps/custo_mtz
      WHERE id_custo_elementar = @is_data-id_custo_elementar
      AND   id_var_input       = @is_data-id_var_input
      and   id_simulacao       = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_custo_matriz).

    CHECK lines( lt_custo_matriz ) > 0.

    lr_id_custo_matriz = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_custo_matriz ) ).

    lr_id_matriz_abast = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_matriz_abast ) ).
    lr_id_item  = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_item ) ).
    lr_id_distribuicao = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).

    SORT: lr_id_custo_matriz BY low,
          lr_id_matriz_abast BY low,
          lr_id_item         BY low,
          lr_id_distribuicao  BY low.

    DELETE: lr_id_custo_matriz WHERE low IS INITIAL,
            lr_id_matriz_abast WHERE low IS INITIAL,
            lr_id_item WHERE low IS INITIAL       ,
            lr_id_distribuicao WHERE low IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM: lr_id_custo_matriz,
                                     lr_id_matriz_abast,
                                     lr_id_item        ,
                                     lr_id_distribuicao.

    DELETE FROM /qaps/custo_mtz
    WHERE id_simulacao = @ms_simulacao-id_simulacao
    and   id_custo_matriz IN @lr_id_custo_matriz.
    COMMIT WORK AND WAIT.

    SELECT COUNT( * ) AS qty
      FROM /qaps/custo_mtz "/qaps/matriz_dst
      WHERE id_distribuicao IN @lr_id_distribuicao
      and   id_simulacao = @ms_simulacao-id_simulacao
      INTO @DATA(lv_qty_dst).

    CHECK lv_qty_dst = 0.

    DELETE FROM /qaps/matriz_dst
    WHERE id_distribuicao IN lr_id_distribuicao.
    COMMIT WORK AND WAIT.

    SELECT COUNT( * ) AS qty
      FROM /qaps/custo_mtz "/qaps/matriz_itm
      WHERE id_item IN @lr_id_item
      and   id_simulacao = @ms_simulacao-id_simulacao
        INTO @DATA(lv_qty_item).

    CHECK lv_qty_item = 0.
    DELETE FROM /qaps/matriz_itm WHERE id_item IN @lr_id_item.
    COMMIT WORK AND WAIT.

    SELECT COUNT( * ) AS qty
      FROM /qaps/custo_mtz "/qaps/matriz_abs
      WHERE id_matriz_abast IN @lr_id_matriz_abast
      and   id_simulacao = @ms_simulacao-id_simulacao
      INTO @DATA(lv_qty_abast).

    CHECK lv_qty_abast = 0.

    DELETE FROM /qaps/matriz_hdr WHERE id_matriz_abast IN lr_id_matriz_abast
                                  AND  id_simulacao = ms_simulacao-id_simulacao.
    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD delete_premissa.

    data lv_where type string.

    DATA: lr_id_custo_premissa TYPE RANGE OF /qaps/custo_prm-id_custo_premisa,
          lr_id_premissa       TYPE RANGE OF /qaps/prem_hdr-id_premissa,
          lr_id_item           TYPE RANGE OF /qaps/prem_item-id_item,
          lr_id_distribuicao   TYPE RANGE OF /qaps/prem_distr-id_distribuicao,
          lr_id_prem_trajeto   TYPE RANGE OF /qaps/prem_traj-id_prem_trajeto.

    DATA lr_modalidade TYPE RANGE OF /qaps/prem_distr-modalidade.

    IF is_data-importacao = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'I' ) TO lr_modalidade.
    ENDIF.

    IF is_data-nacional = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'N' ) TO lr_modalidade.
    ENDIF.

    SELECT *
      FROM /qaps/custo_prm
      WHERE id_custo_elementar = @is_data-id_custo_elementar
      AND   id_var_input       = @is_data-id_var_input
      and   id_simulacao       = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_custo_premissa).

    CHECK lines( lt_custo_premissa ) > 0.

    lr_id_custo_premissa = VALUE #( FOR wa IN lt_custo_premissa
                                  ( sign = 'I' option = 'EQ' low = wa-id_custo_premisa ) ).

    lr_id_premissa = VALUE #( FOR wa IN lt_custo_premissa
                                  ( sign = 'I' option = 'EQ' low = wa-id_premissa ) ).
    lr_id_item  = VALUE #( FOR wa IN lt_custo_premissa
                                  ( sign = 'I' option = 'EQ' low = wa-id_item ) ).
    lr_id_distribuicao = VALUE #( FOR wa IN lt_custo_premissa
                                  ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).

    SORT: lr_id_custo_premissa BY low,
          lr_id_premissa BY low,
          lr_id_item         BY low,
          lr_id_distribuicao  BY low.

    DELETE: lr_id_custo_premissa WHERE low IS INITIAL,
            lr_id_premissa WHERE low IS INITIAL,
            lr_id_item WHERE low IS INITIAL,
            lr_id_distribuicao WHERE low IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM: lr_id_custo_premissa,
                                     lr_id_premissa,
                                     lr_id_item        ,
                                     lr_id_distribuicao.

    DELETE FROM /qaps/custo_prm WHERE id_custo_premisa IN lr_id_custo_premissa.
    COMMIT WORK AND WAIT.

    case is_data-tipo_regra.
      when 'GE'. lv_where = ` tipo_regra = 'GE' `.
      when 'GP'. lv_where = ` tipo_regra = 'GP' and id_grupo_produto = @is_data-id_grupo_produto `.
      when 'MP'. lv_where = ` tipo_regra = 'MP' and mat_planej = @is_data-mat_planej `.
      when 'AG'. lv_where = ` tipo_regra = 'AG' and agregador = @is_data-agregador `.
      when 'MA'. lv_where = ` tipo_regra = 'MA' and matnr = @is_data-matnr `.
    endcase.

    SELECT COUNT( * ) AS qty
      FROM /qaps/custo_prm
      WHERE id_distribuicao IN @lr_id_distribuicao
      and   (lv_where)
      INTO @DATA(lv_qty_dst).

    CHECK lv_qty_dst = 0.

    DELETE FROM /qaps/prem_traj WHERE id_distribuicao IN lr_id_distribuicao.
    DELETE FROM /qaps/prem_distr WHERE id_parent IN lr_id_distribuicao
                                 AND modalidade IN lr_modalidade.
    DELETE FROM /qaps/prem_distr WHERE id_distribuicao IN lr_id_distribuicao
                                 AND modalidade IN lr_modalidade.
    COMMIT WORK AND WAIT.



    SELECT id_item, COUNT( * ) AS qty
      FROM /qaps/prem_distr
      WHERE id_item IN @lr_id_item
      GROUP BY id_item
      INTO TABLE @DATA(lt_prem_distr).

    SELECT *
      FROM /qaps/prem_item
      WHERE id_item IN @lr_id_item
      INTO TABLE @DATA(lt_prem_item).

    LOOP AT lt_prem_item ASSIGNING FIELD-SYMBOL(<fs_prem_item>).

      IF NOT line_exists( lt_prem_distr[ id_item = <fs_prem_item>-id_item ] ).
        DELETE FROM /qaps/prem_item WHERE id_item = <fs_prem_item>-id_item.
      ENDIF.

    ENDLOOP.

    COMMIT WORK.

    SELECT COUNT( * ) AS qty
      FROM /qaps/v_prm_full
      WHERE id_premissa IN @lr_id_premissa
      INTO @DATA(lv_qty_premissa).

    IF lv_qty_premissa = 0.

      DELETE FROM /qaps/prem_hdr WHERE id_premissa IN lr_id_premissa.
      COMMIT WORK AND WAIT.

    ENDIF.

    update_percentual_auto_prem( ).

*    SELECT COUNT( * ) AS qty
*      FROM /qaps/custo_prm
*      WHERE id_item IN @lr_id_item
*        INTO @DATA(lv_qty_item).
*
*    SELECT COUNT( * ) AS qty
*      FROM /qaps/v_prm_full
*      WHERE id_item IN @lr_id_item
*        INTO @DATA(lv_qty_prm_full).
*
*    CHECK lv_qty_item = 0 and lv_qty_prm_full = 0.
*    DELETE FROM /qaps/prem_item WHERE id_item IN @lr_id_item.
*    COMMIT WORK AND WAIT.
*
*    SELECT COUNT( * ) AS qty
*      FROM /qaps/custo_prm"/qaps/prem_hdr
*      WHERE id_premissa IN @lr_id_premissa
*      INTO @DATA(lv_qty_premissa).
*
*    CHECK lv_qty_premissa = 0.
*
*    DELETE FROM /qaps/prem_hdr WHERE id_premissa IN lr_id_premissa.
*    COMMIT WORK AND WAIT.



  ENDMETHOD.


  METHOD delete_premissa_matriz.

    DATA: lr_id_custo_premissa TYPE RANGE OF /qaps/custo_prm-id_custo_premisa,
          lr_id_premissa       TYPE RANGE OF /qaps/prem_hdr-id_premissa.

    DATA: lr_id_item         TYPE RANGE OF /qaps/matriz_itm-id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/matriz_dst-id_distribuicao.

    SELECT *
      FROM /qaps/custo_mtz
      WHERE id_custo_elementar = @is_data-id_custo_elementar
      AND   id_var_input       = @is_data-id_var_input
      AND   id_simulacao       = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_custo_matriz).

    CHECK lines( lt_custo_matriz ) > 0.

    lr_id_item  = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_item ) ).
    lr_id_distribuicao = VALUE #( FOR wa IN lt_custo_matriz
                                  ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).

    SORT: lr_id_item         BY low,
          lr_id_distribuicao  BY low.

    DELETE: lr_id_item WHERE low IS INITIAL       ,
            lr_id_distribuicao WHERE low IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM: lr_id_item        ,
                                     lr_id_distribuicao.

    "Distribuição
    SELECT DISTINCT id_distribuicao,tipo_origem,id_origem
      FROM /qaps/prem_distr
      WHERE id_distr_matriz IN @lr_id_distribuicao
      INTO TABLE @DATA(lt_distrib).

    LOOP AT lt_distrib INTO DATA(ls_distrib).

      "TODO - avalisar se deve limpar origem
      UPDATE /qaps/prem_distr
      SET id_parent = ''
      WHERE id_parent = ls_distrib-id_distribuicao.

      DELETE FROM /qaps/prem_traj WHERE id_distribuicao = ls_distrib-id_distribuicao.
      DELETE FROM /qaps/prem_distr WHERE id_distribuicao = ls_distrib-id_distribuicao.

    ENDLOOP.

    COMMIT WORK.

    "Item
    SELECT *
      FROM /qaps/prem_item
      WHERE id_item_matriz IN @lr_id_item
      INTO TABLE @DATA(lt_item).

    LOOP AT lt_item INTO DATA(ls_item).

      UPDATE /qaps/prem_item
      SET id_parent = ''
      WHERE id_item = ls_item-id_item.

      DELETE FROM /qaps/prem_item WHERE id_item = ls_item-id_item.

    ENDLOOP.
*    break c060863.
*
    SELECT *
      FROM /qaps/custo_prm
      WHERE id_custo_elementar = @is_data-id_custo_elementar
      AND   id_var_input       = @is_data-id_var_input
      AND   id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_custo_premissa).
*
    CHECK lines( lt_custo_premissa ) > 0.

    lr_id_custo_premissa = VALUE #( FOR wa_premissa IN lt_custo_premissa
                                  ( sign = 'I'
                                    option = 'EQ'
                                    low = wa_premissa-id_custo_premisa ) ).

    lr_id_premissa = VALUE #( FOR wa_premissa IN lt_custo_premissa
                                  ( sign = 'I'
                                    option = 'EQ'
                                    low = wa_premissa-id_premissa ) ).

    SORT: lr_id_premissa BY low.

    DELETE:  lr_id_premissa WHERE low IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM:  lr_id_premissa.

    DELETE FROM /qaps/custo_prm WHERE id_custo_premisa IN lr_id_custo_premissa.
    COMMIT WORK AND WAIT.

    SELECT COUNT( * ) AS qty
      FROM /qaps/custo_prm"/qaps/prem_hdr
      WHERE id_premissa IN @lr_id_premissa
      AND   id_simulacao = @ms_simulacao-id_simulacao
      INTO @DATA(lv_qty_premissa).

    CHECK lv_qty_premissa = 0.

    DELETE FROM /qaps/prem_hdr WHERE id_premissa IN lr_id_premissa
                               AND   id_simulacao = ms_simulacao-id_simulacao.
    COMMIT WORK AND WAIT.



  ENDMETHOD.


  METHOD execute.

    IF iv_id_simulacao IS INITIAL.
      TRY.
          DATA(lv_id_simulacao) = choose_simulacao(  ).
        CATCH /qaps/cx_pricing_error.    "
          RETURN.
      ENDTRY.
      get_simulacao( lv_id_simulacao ).
    ELSE.
      get_simulacao( iv_id_simulacao ).
    ENDIF.

    "apaga dados de Matriz Abast e Premissa desta simulação
    show_progress( iv_message    = |Iniciando atualização....| iv_percentual = 10 ).
    send_transfer_to_memory( iv_id_simul_ref ).
    clean_data( ).

    "Criar Importacao
    show_progress( iv_message    = |Etapa 1 de 6 - Matriz de Abastecimento...| iv_percentual = 20 ).
    create_importacao( ).

*    "Criar Compra Importada
    show_progress( iv_message    = |Etapa 2 de 6 - Compra Importada...| iv_percentual = 40 ).
    create_compra_importada( ).

    "Criar Compra Nacional
    show_progress( iv_message    = |Etapa 3 de 6 - Compra Nacional...| iv_percentual = 60 ).
    create_compra_nacional( ).

    "Std Produção
    show_progress( iv_message    = |Etapa 4 de 6 - Std de Produção...| iv_percentual = 80 ).
    create_std_producao( ).

    "Refazer Transferências
    show_progress( iv_message    = |Etapa 5 de 6 - Transferência...| iv_percentual = 90 ).
    redo_transfer( ).

    "Set 100%
    show_progress( iv_message    = |Etapa 6 de 6 - Finalizando...| iv_percentual = 100 ).
    update_percentual( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD expand_grp_planta_to_werks_dst.

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @it_data
      WHERE id_grp_planta = @it_data-id_grp_planta
      AND   id_simulacao  = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_prm_full).

    SELECT *
      FROM /qaps/v_centro
      FOR ALL ENTRIES IN @it_data
      WHERE id_grp_planta = @it_data-id_grp_planta
      INTO TABLE @DATA(lt_centro).

    "Grp Planta
    APPEND LINES OF it_data TO return.

    LOOP AT lt_centro INTO DATA(ls_centro).

      DATA(ls_template) = it_data[ id_grp_planta = ls_centro-id_grp_planta ].

      ls_template-id_premissa =
          VALUE #( lt_prm_full[ id_grp_planta = ls_centro-id_grp_planta
                                id_centro     = ls_centro-id_centro ]-id_premissa OPTIONAL )   .


      ls_template-id_centro = ls_centro-id_centro.
      ls_template-werks     = ls_centro-werks.
      APPEND ls_template TO return.

    ENDLOOP.

    sort return by cod_grp_planta werks.

  ENDMETHOD.


  METHOD expand_grp_planta_to_werks_mat.

    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_grp_planta = @is_data-id_grp_planta
      AND   id_simulacao  = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_prm_full).

    LOOP AT lt_prm_full ASSIGNING FIELD-SYMBOL(<fs_prm_full>).
      <fs_prm_full>-matnr = |{ <fs_prm_full>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/v_centro
      WHERE id_grp_planta = @is_data-id_grp_planta
      INTO TABLE @DATA(lt_centro).

    "Grp Planta
    APPEND is_data TO return.

    LOOP AT lt_centro INTO DATA(ls_centro).

      DATA(ls_template) = is_data.

      ls_template-id_premissa =
          VALUE #( lt_prm_full[ id_grp_planta = ls_centro-id_grp_planta
                                id_centro     = ls_centro-id_centro ]-id_premissa OPTIONAL )   .

      ls_template-id_item =
          VALUE #( lt_prm_full[ id_grp_planta = ls_centro-id_grp_planta
                                id_centro     = ls_centro-id_centro
                                tipo_regra = 'MA'
                                matnr      = is_data-matnr ]-id_item OPTIONAL )   .


      ls_template-id_centro = ls_centro-id_centro.
      ls_template-werks     = ls_centro-werks.
      APPEND ls_template TO return.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_custo_elementar.
    DATA(lt_custo_elm) = mo_custo_elementar->get_variavel( ).
    ms_custo_elementar = lt_custo_elm[ id_custo_elementar = iv_id_custo_elementar ].
  ENDMETHOD.


  METHOD GET_CUSTO_MATRIZ.

    SELECT *
      FROM /qaps/custo_mtz
      WHERE id_custo_elementar = @ms_custo_elementar-id_custo_elementar
      and   id_simulacao       = @ms_simulacao-id_simulacao
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_custo_matriz_geral.

    SELECT *
      FROM /qaps/custo_mtz
      WHERE id_simulacao       = @ms_simulacao-id_simulacao
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_custo_premissa.

    IF iv_geral = abap_false.
      SELECT *
        FROM /qaps/custo_prm
        WHERE id_custo_elementar = @ms_custo_elementar-id_custo_elementar
        and   id_simulacao       = @ms_simulacao-id_simulacao
        INTO TABLE @return.
    ELSE.
      SELECT *
        FROM /qaps/custo_prm
        where  id_simulacao       = @ms_simulacao-id_simulacao
        INTO TABLE @return.
    ENDIF.

     LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      check not <fs>-matnr is INITIAL.
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_destinos_ativos.

    SELECT *
      FROM /qaps/v_dest_atv
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_distribuicao_logistica.

*    BREAK abap.
    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_data-id_origem
      INTO @DATA(ls_origem).

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_data-id_destino
      INTO @DATA(ls_destino).

    CASE is_data-tipo_origem.
      WHEN 'P'. "Porto

        IF NOT ls_destino IS INITIAL.

          SELECT DISTINCT id_grp_planta,id_centro,werks,id_porto,cod_porto,porto,ativo
            FROM /qaps/v_dst_log
            WHERE ativo = 'X'
            AND id_porto = @ls_origem-id_externo
            AND ( id_grp_planta = @ls_destino-id_externo OR id_centro = @ls_destino-id_externo )
            INTO CORRESPONDING FIELDS OF TABLE @return.

        ELSE.

          SELECT DISTINCT id_grp_planta,id_centro,werks,id_porto,cod_porto,porto,ativo
            FROM /qaps/v_dst_log"/qaps/v_dist_log
            WHERE ativo = 'X'
            AND id_porto = @ls_origem-id_externo
            INTO CORRESPONDING FIELDS OF TABLE @return.

        ENDIF.

      WHEN 'I'.

        IF NOT ls_destino IS INITIAL.
          SELECT *
            FROM /qaps/v_dst_log "/qaps/v_dist_log
            WHERE ativo = 'X'
            AND id_cais = @ls_origem-id_externo
            AND ( id_grp_planta = @ls_destino-id_externo OR id_centro = @ls_destino-id_externo )
            INTO CORRESPONDING FIELDS OF TABLE @return.
        ELSE.
          SELECT *
            FROM /qaps/v_dst_log "/qaps/v_dist_log
            WHERE ativo = 'X'
            AND id_cais = @ls_origem-id_externo
            INTO CORRESPONDING FIELDS OF TABLE @return.
        ENDIF.
    ENDCASE.

    SORT return BY werks cod_porto cod_cais.

  ENDMETHOD.


  METHOD get_hierarquia_destino.

    DATA lt_append TYPE /qaps/t_hierarquia_destino.

    SELECT *
      FROM /QAPS/V_CENTRO
      INTO TABLE @DATA(lt_grupo_centro).

    LOOP AT ct_hierarquia_destino INTO DATA(ls_hierarquia_destino).

      CHECK ls_hierarquia_destino-tipo_destino = 'G'.
      DATA(lt_grupo_centro_partial) = lt_grupo_centro.

      DELETE lt_grupo_centro_partial WHERE codigo <> ls_hierarquia_destino-codigo.

      CHECK lines( lt_grupo_centro_partial ) > 0.

      LOOP AT lt_grupo_centro_partial INTO DATA(ls_grupo_centro_partial).
        APPEND VALUE /qaps/s_hierarquia_destino(
            tipo_destino = 'W'
            id_destino   = ls_grupo_centro_partial-id_centro
            codigo       = ls_grupo_centro_partial-werks ) TO lt_append.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF lt_append TO ct_hierarquia_destino.

  ENDMETHOD.


  METHOD get_hierarquia_origem.

    DATA lt_append TYPE /qaps/t_hierarquia_origem.

    SELECT *
      FROM /qaps/v_prt_cais
      INTO TABLE @DATA(lt_porto_cais).

    LOOP AT ct_hierarquia_origem INTO DATA(ls_hierarquia_origem).

      CHECK ls_hierarquia_origem-tipo_origem = 'P'.
      DATA(lt_porto_cais_partial) = lt_porto_cais.

      DELETE lt_porto_cais_partial WHERE cod_porto <> ls_hierarquia_origem-codigo.
      DELETE lt_porto_cais_partial WHERE cod_cais IS INITIAL.

      CHECK lines( lt_porto_cais_partial ) > 0.

      LOOP AT lt_porto_cais_partial INTO DATA(ls_porto_cais_partial).
        APPEND VALUE /qaps/s_hierarquia_origem(
            tipo_origem = 'I'
            id_origem   = ls_porto_cais_partial-id_cais ) TO lt_append.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF lt_append TO ct_hierarquia_origem.

  ENDMETHOD.


  METHOD get_ponto_by_id.

    IF NOT iv_id_ponto IS INITIAL.
      select SINGLE *
        from /qaps/v_ponto
        where id_ponto = @iv_id_ponto
        into @return.
    ELSEIF NOT iv_id_externo IS INITIAL.
      select SINGLE *
        from /qaps/v_ponto
        where id_externo = @iv_id_externo
        into @return.
    ENDIF.

  ENDMETHOD.


  METHOD get_premissa_distr_exclusao.

    DATA: lv_exit TYPE abap_bool,
          lt_base TYPE /qaps/t_prem_distr.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_data
      WHERE id_premissa = @it_data-id_premissa
      AND   id_item     = @it_data-id_item
      AND   id_distribuicao = @it_data-id_distribuicao
      AND   tipo_origem  = @it_data-tipo_origem
      AND   id_origem  = @it_data-id_origem
      INTO TABLE @return.

    LOOP AT it_data INTO DATA(ls_data).

      SELECT *
       FROM /qaps/prem_distr
       FOR ALL ENTRIES IN @it_items
       WHERE id_premissa = @it_items-id_premissa
       AND   id_item     = @it_items-id_item
       AND   id_distribuicao = @ls_data-id_distribuicao
       AND   tipo_origem  = @ls_data-tipo_origem
       AND   id_origem  = @ls_data-id_origem
       APPENDING TABLE @return.

    ENDLOOP.

    CHECK lines( return ) > 0.

    lt_base = return.

    WHILE lv_exit = abap_false.

      SELECT *
        FROM /qaps/prem_distr
        FOR ALL ENTRIES IN @lt_base
        WHERE id_parent = @lt_base-id_distribuicao
        AND   tipo_origem  = @lt_base-tipo_origem
        AND   id_origem  = @lt_base-id_origem
        INTO TABLE @DATA(lt_distr).

      IF sy-subrc EQ 0.
        APPEND LINES OF lt_distr TO return.
        lt_base = lt_distr.
      ELSE.
        lv_exit = abap_true.
      ENDIF.

    ENDWHILE.

    SORT return BY id_distribuicao periodo.
    DELETE ADJACENT DUPLICATES FROM return COMPARING ALL FIELDS.

  ENDMETHOD.


  METHOD get_premissa_full.

    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @return.

    CHECK iv_fill_zeros = abap_true.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_premissa_header_item.

    SELECT *
      FROM /qaps/v_prm_hd_i
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_premissa_itens_exclusao.

    DATA: lv_exit TYPE abap_bool,
          lt_base TYPE /qaps/t_prem_item.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @it_data
      WHERE id_premissa = @it_data-id_premissa
      AND   id_item = @it_data-id_item
      INTO TABLE @return.

    CHECK lines( return ) > 0.

    lt_base = return.

    WHILE lv_exit = abap_false.

      SELECT *
        FROM /qaps/prem_item
        FOR ALL ENTRIES IN @lt_base
        WHERE id_parent = @lt_base-id_item
        INTO TABLE @DATA(lt_item).

      IF sy-subrc EQ 0.
        APPEND LINES OF lt_item TO return.
        lt_base = lt_item.
      ELSE.
        lv_exit = abap_true.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD get_premissa_std_prd.

    IF iv_id_simulacao IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_stdp
        INTO TABLE @return.
    ELSE.
      SELECT *
        FROM /qaps/v_prm_stdp
        WHERE id_simulacao = @iv_id_simulacao
        INTO TABLE @return.
    ENDIF.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_simulacao.
    ms_simulacao = mo_simulacao->get_simulacao_by_id( iv_simulacao  ).
  ENDMETHOD.


  METHOD get_trajeto.

    DATA lt_trajeto TYPE /qaps/t_trajeto.

    SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @is_data-id_centro
        INTO @DATA(ls_ponto_centro).

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_externo = @is_data-id_grp_planta
      INTO @DATA(ls_ponto_grp_planta).

    IF is_data-tipo_origem = 'I'.

      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_ponto = @is_data-id_origem
        INTO @DATA(ls_origem).

      SELECT SINGLE *
        FROM /qaps/cais
        WHERE id_cais = @ls_origem-id_externo
        INTO @DATA(ls_cais).

      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @ls_cais-id_porto
        INTO @DATA(ls_porto).

    ENDIF.

    IF is_data-id_centro <> mc_guid_null.

      SELECT *
          FROM /qaps/trajeto
          WHERE ( id_origem = @is_data-id_origem or id_origem = @ls_porto-id_ponto )
          AND   id_destino = @ls_ponto_centro-id_ponto
          INTO CORRESPONDING FIELDS OF TABLE @lt_trajeto.

      IF sy-subrc NE 0.
        SELECT *
          FROM /qaps/trajeto
          WHERE ( id_origem = @is_data-id_origem or id_origem = @ls_porto-id_ponto )
          AND   id_destino = @ls_ponto_grp_planta-id_ponto
          INTO CORRESPONDING FIELDS OF TABLE @lt_trajeto.
      ENDIF.

    ELSE.
      SELECT *
          FROM /qaps/trajeto
          WHERE ( id_origem = @is_data-id_origem or id_origem = @ls_porto-id_ponto )
          AND   id_destino = @ls_ponto_grp_planta-id_ponto
          INTO CORRESPONDING FIELDS OF TABLE @lt_trajeto.
    ENDIF.

    CHECK lines( lt_trajeto ) > 0.

    SELECT *
      FROM /qaps/v_trj_trc
      FOR ALL ENTRIES IN @lt_trajeto
      WHERE id_trajeto = @lt_trajeto-id_trajeto
      INTO TABLE @DATA(lt_traj_trecho).

    SELECT *
      FROM /qaps/v_trc_cst
      FOR ALL ENTRIES IN @lt_traj_trecho
      WHERE id_trecho = @lt_traj_trecho-id_trecho
      INTO TABLE @DATA(lt_check).

    DATA lv_insert TYPE abap_bool.

    LOOP AT lt_trajeto INTO DATA(ls_trajeto).

      DATA(lt_traj_trecho_filter) = lt_traj_trecho.
      DELETE lt_traj_trecho_filter WHERE id_trajeto <> ls_trajeto-id_trajeto.

*      lv_insert = abap_true.
*      LOOP AT lt_traj_trecho_filter INTO DATA(ls_traj_trecho_filter).
*
*        TRY.
*            DATA(ls_check) = lt_check[ cod_trecho = ls_traj_trecho_filter-cod_trecho ].
*
*            IF ls_check-valor = 0.
*              lv_insert = abap_false.
*              EXIT.
*            ENDIF.
*
*          CATCH cx_sy_itab_line_not_found.
*            lv_insert = abap_false.
*            EXIT.
*        ENDTRY.
*
*      ENDLOOP.
*
*      CHECK lv_insert = abap_true.
      APPEND ls_trajeto TO return.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_trajeto_by_id.

    SELECT SINGLE *
      FROM /qaps/trajeto
      WHERE id_trajeto = @iv_id_trajeto
      INTO @return.

  ENDMETHOD.


  METHOD herdar_origem.

    DATA: lt_entry   TYPE TABLE OF /qaps/prem_distr,
          ls_entry   TYPE /qaps/s_parent_origem,
          ls_distrib TYPE /qaps/prem_distr.

    SELECT *
      FROM /qaps/v_prm_all
      WHERE id_premissa = @is_data-id_premissa
      AND   id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_heranca).

    LOOP AT lt_heranca ASSIGNING FIELD-SYMBOL(<fs_heranca>).
      CHECK NOT <fs_heranca>-matnr IS INITIAL.
      <fs_heranca>-matnr = |{ <fs_heranca>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT SINGLE *
      FROM /qaps/prem_item
      WHERE id_item = @is_data-id_item
      INTO @DATA(ls_item).

    DEFINE set_value_and_return.

      SELECT SINGLE *
        FROM /qaps/prem_distr
        WHERE id_distribuicao = @&1-id_distribuicao
        INTO @ls_distrib.

      CLEAR ls_entry.
      ls_entry-tipo_origem = &1-tipo_origem.
      ls_entry-id_origem = &1-id_origem.
      ls_entry-id_parent = &1-id_distribuicao.
      ls_entry-id_item = is_data-id_item.
      ls_entry-id_item_parent = &1-id_item.
      ls_entry-is_distrib_origem = ls_distrib.
      APPEND ls_entry TO return.
    END-OF-DEFINITION.

    DATA(ls_data) = is_data.
*    break abap.
    CASE ls_item-tipo_regra.
      WHEN 'MA'.
        DATA(lt_material) = mo_material->get_materiais_by_material( ls_item-matnr  ).

        LOOP AT lt_material INTO DATA(ls_material).
          LOOP AT lt_heranca INTO DATA(ls_heranca).
            IF ls_heranca-tipo_regra = 'AG' AND ls_heranca-agregador = ls_material-agregador.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GE'.
              set_value_and_return ls_heranca.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

      WHEN 'MP'.
        lt_material = mo_material->get_materiais_by_mat_planejado( ls_item-mat_planejado ).

        LOOP AT lt_material INTO ls_material.
          LOOP AT lt_heranca INTO ls_heranca.
            IF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.
          ENDLOOP.

        ENDLOOP.

      WHEN 'AG'.
        lt_material = mo_material->get_materiais_by_agregador( ls_item-agregador ).

        LOOP AT lt_material INTO ls_material.
          LOOP AT lt_heranca INTO ls_heranca.
            IF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

      WHEN 'GP'.
        lt_material = mo_material->get_materiais_by_grp_produto( ls_item-id_grupo_produto ).

        LOOP AT lt_material INTO ls_material.
          LOOP AT lt_heranca INTO ls_heranca.
            IF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

    ENDCASE.

  ENDMETHOD.


  METHOD herdar_origem_from_matriz_abs.


    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_premissa = @is_id_destino-id_premissa
      AND   tipo_origem = ''
      AND   id_origem   = @mc_guid_null
      AND   id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_prm_full).

    IF lines( lt_prm_full ) > 0.

      SELECT SINGLE *
        FROM /qaps/prem_distr
        WHERE id_distribuicao = @is_id_origem-id_premissa
        INTO @DATA(ls_prem_distr).

      SELECT *
        FROM /qaps/prem_traj
        WHERE id_distribuicao = @is_id_origem-id_premissa
        INTO TABLE @DATA(lt_prem_traj).

      LOOP AT lt_prm_full INTO DATA(ls_prm_full).

        UPDATE /qaps/prem_distr
        SET    tipo_origem = ls_prem_distr-tipo_origem
               id_origem   = ls_prem_distr-id_origem
               id_parent   = ls_prem_distr-id_distribuicao
*               percentual  = ls_prem_distr-percentual
        WHERE id_distribuicao = ls_prm_full-id_distribuicao.

        DATA(lt_template_prm_traj) = lt_prem_traj.

        DATA(lv_key) = cl_system_uuid=>create_uuid_x16_static( ).

        LOOP AT lt_template_prm_traj ASSIGNING FIELD-SYMBOL(<ls_template_prm_traj>).
          <ls_template_prm_traj>-id_parent = <ls_template_prm_traj>-id_distribuicao.
          <ls_template_prm_traj>-id_prem_trajeto = lv_key.
          <ls_template_prm_traj>-id_distribuicao = ls_prm_full-id_distribuicao.
        ENDLOOP.

        MODIFY /qaps/prem_traj FROM TABLE lt_template_prm_traj.

      ENDLOOP.


      COMMIT WORK.

    ENDIF.

  ENDMETHOD.


  METHOD herdar_origem_matriz_premissa.

    DATA: lt_entry TYPE TABLE OF /qaps/prem_distr,
          ls_entry TYPE /qaps/s_parent_origem.

    SELECT *
      FROM /qaps/v_prm_all
      WHERE id_premissa = @is_data-id_premissa
      and   id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_heranca).

    SELECT SINGLE *
      FROM /qaps/prem_item
      WHERE id_parent = @is_data-id_item
      INTO @DATA(ls_item).

    DEFINE set_value_and_return.
      CLEAR ls_entry.
      ls_entry-tipo_origem = &1-tipo_origem.
      ls_entry-id_origem = &1-id_origem.
      ls_entry-id_parent = &1-id_distribuicao.
      ls_entry-id_item   = ls_item-id_item.
      ls_entry-id_item_parent = ls_item-id_parent.
      APPEND ls_entry TO return.
    END-OF-DEFINITION.

    DATA(ls_data) = is_data.

    CASE ls_item-tipo_regra.
      WHEN 'MA'.
        DATA(lt_material) = mo_material->get_materiais_by_material( ls_item-matnr  ).

        LOOP AT lt_material INTO DATA(ls_material).
          LOOP AT lt_heranca INTO DATA(ls_heranca).
            IF ls_heranca-tipo_regra = 'AG' AND ls_heranca-agregador = ls_material-agregador.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

      WHEN 'MP'.
        lt_material = mo_material->get_materiais_by_mat_planejado( ls_item-mat_planejado ).

        LOOP AT lt_material INTO ls_material.
          LOOP AT lt_heranca INTO ls_heranca.
            IF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.
          ENDLOOP.

        ENDLOOP.

      WHEN 'AG'.
        lt_material = mo_material->get_materiais_by_agregador( ls_item-agregador ).

        LOOP AT lt_material INTO ls_material.
          LOOP AT lt_heranca INTO ls_heranca.
            IF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
              set_value_and_return ls_heranca.
            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
              set_value_and_return ls_heranca.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

*      WHEN 'GP'.
*        lt_material = mo_material->get_materiais_by_grp_produto( ls_item-id_grupo_produto ).
*
*        LOOP AT lt_material INTO ls_material.
*          LOOP AT lt_heranca INTO ls_heranca.
*            IF ls_heranca-tipo_regra = 'MP' AND ls_heranca-mat_planejado = ls_material-mat_planejado.
*              set_value_and_return ls_heranca.
*            ELSEIF ls_heranca-tipo_regra = 'GP' AND ls_heranca-id_grupo_produto = ls_material-id_grupo_produto.
*              set_value_and_return ls_heranca.
*            ENDIF.
*
*          ENDLOOP.
*
*        ENDLOOP.

    ENDCASE.

  ENDMETHOD.


  METHOD input_create_compras.

    DATA(lt_destinos_ativos) = get_destinos_ativos( ).

    "Possui Destino
    IF is_data-id_destino <> mc_guid_null.

      CASE is_data-tipo_destino.
        WHEN 'G'.

          DELETE lt_destinos_ativos WHERE id_ponto <> is_data-id_destino.

          DATA(lv_lines) = lines( lt_destinos_ativos ) + 1.
          DO lv_lines TIMES.

            IF sy-index = 1.
              TRY.
                  DATA(ls_destinos_ativos) = lt_destinos_ativos[ 1 ].
                CATCH cx_sy_itab_line_not_found.
                  RETURN.
              ENDTRY.
              DATA(ls_destino) = ls_destinos_ativos.
              CLEAR: ls_destino-id_centro,
                     ls_destino-werks.
            ELSE.
              ls_destinos_ativos = lt_destinos_ativos[ sy-index - 1 ].
              ls_destino = ls_destinos_ativos.
            ENDIF.

            DATA(ls_relation) = check_relation_premissa( is_destino    = ls_destino
                                                         is_data       = is_data
                                                         iv_modalidade = iv_modalidade ).

            "Todos marcados
            IF ls_relation <> 'XXX'.
              "rever esta chave

              create_premissa( is_relation    = ls_relation
                               is_destino     = ls_destino
                               is_data        = is_data
                               iv_modalidade  = iv_modalidade
                               iv_with_parent = abap_true ).
            ENDIF.

          ENDDO.

        WHEN 'W'.
*          break abap.
          ls_destinos_ativos = lt_destinos_ativos[ id_ponto_centro = is_data-id_destino ].

          ls_relation = check_relation_premissa( is_destino    = ls_destinos_ativos
                                                 is_data       = is_data
                                                 iv_modalidade = iv_modalidade ).

          "Todos marcados
          IF ls_relation <> 'XXX'.
            "rever esta chave

            create_premissa( is_relation   = ls_relation
                             is_destino    = ls_destinos_ativos
                             is_data       = is_data
                             iv_modalidade = iv_modalidade ).
          ENDIF.

      ENDCASE.

      "Não possui Destino
    ELSEIF is_data-id_destino = mc_guid_null.

      LOOP AT lt_destinos_ativos INTO ls_destinos_ativos.

        IF NOT ls_destinos_ativos-id_grp_planta IS INITIAL.

          DO 2 TIMES.

            "Com e Sem Centro
            CASE sy-index.
              WHEN 1.
                ls_destino = ls_destinos_ativos.
                CLEAR ls_destino-id_centro.
              WHEN 2.
                ls_destino = ls_destinos_ativos.
            ENDCASE.

            ls_relation = check_relation_premissa( is_destino    = ls_destino
                                                   is_data       = is_data
                                                   iv_modalidade = 'N' ).

            IF ls_relation <> 'XXX'.
              create_premissa( is_relation    = ls_relation
                               is_destino     = ls_destino
                               is_data        = is_data
                               iv_modalidade  = iv_modalidade
                               iv_with_parent = abap_true ).
            ENDIF.
*
          ENDDO.

        ELSE.

          ls_destino = ls_destinos_ativos.

          ls_relation = check_relation_premissa( is_destino    = ls_destino
                                                 is_data       = is_data
                                                 iv_modalidade = 'N' ).

          IF ls_relation <> 'XXX'.
            create_premissa( is_relation    = ls_relation
                             is_destino     = ls_destino
                             is_data        = is_data
                             iv_modalidade  = iv_modalidade
                             iv_with_parent = abap_true ).
          ENDIF.

        ENDIF.
*
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD input_create_frete.

    DATA: lt_origem   TYPE /qaps/t_hierarquia_origem,
          lt_destino  TYPE /qaps/t_hierarquia_destino,
          lt_prm_full TYPE TABLE OF /qaps/v_prm_full.


    SELECT *
      FROM /qaps/v_trj_trc
      WHERE id_trecho = @is_data-id_trecho
      INTO TABLE @DATA(lt_traj_trecho).

    CHECK lines( lt_traj_trecho ) > 0.

    LOOP AT lt_traj_trecho INTO DATA(ls_traj_trecho).

      IF NOT line_exists( lt_origem[ id_origem = ls_traj_trecho-id_origem ] ).
        APPEND VALUE /qaps/s_hierarquia_origem(
            tipo_origem = ls_traj_trecho-tipo_origem
            id_origem   = ls_traj_trecho-id_origem
            codigo      = ls_traj_trecho-cod_origem ) TO lt_origem.
      ENDIF.

      IF NOT line_exists( lt_destino[ id_destino = ls_traj_trecho-id_destino ] ).
        APPEND VALUE /qaps/s_hierarquia_destino(
            tipo_destino = ls_traj_trecho-tipo_destino
            id_destino   = ls_traj_trecho-id_destino
            codigo      = ls_traj_trecho-cod_destino ) TO lt_destino.
      ENDIF.

    ENDLOOP.

    get_hierarquia_origem( CHANGING ct_hierarquia_origem = lt_origem ).
    get_hierarquia_destino( CHANGING ct_hierarquia_destino = lt_destino ).

    LOOP AT lt_destino INTO DATA(ls_destino).

      CASE ls_destino-tipo_destino.
        WHEN 'G'.
          SELECT  *
            FROM /qaps/v_prm_full
            WHERE grp_planta = @ls_destino-codigo
            AND id_centro = @mc_guid_null
            APPENDING TABLE @lt_prm_full.
        WHEN 'W'.
          SELECT  *
            FROM /qaps/v_prm_full
            WHERE werks = @ls_destino-codigo
            APPENDING TABLE @lt_prm_full.
      ENDCASE.

    ENDLOOP.

    "Excluir Origens não contidas na inclusão.
    DATA(lt_prm_full_temp) = lt_prm_full.

    REFRESH lt_prm_full.

    LOOP AT lt_prm_full_temp INTO DATA(ls_prm_full_temp).

      CHECK line_exists( lt_origem[ tipo_origem = ls_prm_full_temp-tipo_origem
                                    id_origem   = ls_prm_full_temp-id_origem ] ).
      APPEND ls_prm_full_temp TO lt_prm_full.

    ENDLOOP.

    CHECK lines( lt_prm_full ) > 0.

    SELECT *
      FROM /qaps/dist_traj
      FOR ALL ENTRIES IN @lt_prm_full
      WHERE id_distribuicao = @lt_prm_full-id_distribuicao
      INTO TABLE @DATA(lt_prem_traj).

  ENDMETHOD.


  METHOD input_create_importacao.

    DATA(lt_distr_logist) = get_distribuicao_logistica( is_data ).

    "Possui Destino
    IF is_data-id_destino <> mc_guid_null.

      CASE is_data-tipo_destino.
        WHEN 'G'.
*          BREAK c060863.
          LOOP AT lt_distr_logist INTO DATA(ls_distr_logist).

            DO 2 TIMES.

              "Com e Sem Centro
              CASE sy-index.
                WHEN 1.
                  DATA(ls_destino) = ls_distr_logist.
                  CLEAR: ls_destino-id_centro,ls_destino-werks.
                WHEN 2.
                  ls_destino = ls_distr_logist.
              ENDCASE.


              DATA(ls_relation) = check_relation_matriz( is_destino    = ls_destino
                                                         is_data       = is_data
                                                         iv_modalidade = 'I' ).

              "Todos marcados
              IF ls_relation <> 'XXXXXX'.
                create_matriz( is_relation    = ls_relation
                               is_destino     = ls_destino
                               is_data        = is_data
                               iv_modalidade  = 'I'
                               iv_with_parent = abap_true
                               ).
              ELSE.

                append_matriz( is_relation    = ls_relation
                               is_destino     = ls_destino
                               is_data        = is_data
                               iv_modalidade  = 'I'
                               iv_with_parent = abap_true
                               ).
              ENDIF.
            ENDDO.

          ENDLOOP.
        WHEN 'W'.
*          BREAK c060863.
          LOOP AT lt_distr_logist INTO ls_distr_logist.
            ls_relation = check_relation_matriz( is_destino    = ls_distr_logist
                                                 is_data       = is_data
                                                 iv_modalidade = 'I' ).

            "Todos marcados
            IF ls_relation <> 'XXXXXX'.
              create_matriz( is_relation   = ls_relation
                             is_destino    = ls_distr_logist
                             is_data       = is_data
                             iv_modalidade = 'I' ).
            ELSE.

              append_matriz( is_relation   = ls_relation
                             is_destino    = ls_distr_logist
                             is_data       = is_data
                             iv_modalidade = 'I' ).
            ENDIF.
          ENDLOOP.
      ENDCASE.

      "Não possui Destino
    ELSEIF is_data-id_destino = mc_guid_null.
*      BREAK abap.
      LOOP AT lt_distr_logist INTO ls_distr_logist.

        IF NOT ls_distr_logist-id_grp_planta IS INITIAL.

          DO 2 TIMES.

            "Com e Sem Centro
            CASE sy-index.
              WHEN 1.
                ls_destino = ls_distr_logist.
                CLEAR ls_destino-id_centro.
              WHEN 2.
                ls_destino = ls_distr_logist.
            ENDCASE.

            ls_relation = check_relation_matriz( is_destino    = ls_destino
                                                 is_data       = is_data
                                                 iv_modalidade = 'I' ).

            IF ls_relation <> 'XXXXXX'.
              create_matriz( is_relation    = ls_relation
                             is_destino     = ls_destino
                             is_data        = is_data
                             iv_modalidade  = 'I'
                             iv_with_parent = abap_true ).
            ELSE.

              append_matriz( is_relation    = ls_relation
                             is_destino     = ls_destino
                             is_data        = is_data
                             iv_modalidade  = 'I'
                             iv_with_parent = abap_true
                             ).
            ENDIF.

          ENDDO.

        ELSE.

          ls_destino = ls_distr_logist.
*          IF ls_destino-werks = '1600'.
*            BREAK abap.
*          ENDIF.
          ls_relation = check_relation_matriz( is_destino    = ls_destino
                                               is_data       = is_data
                                               iv_modalidade = 'I' ).

          IF ls_relation <> 'XXXXXX'.
            create_matriz( is_relation   = ls_relation
                           is_destino    = ls_destino
                           is_data       = is_data
                           iv_modalidade = 'I' ).
*                             iv_with_parent = abap_true ).
          ELSE.

            append_matriz( is_relation   = ls_relation
                           is_destino    = ls_destino
                           is_data       = is_data
                           iv_modalidade = 'I' ).
*                             iv_with_parent = abap_true
*                             ).
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.


  METHOD preencher_dados_controle.
    /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = cr_data ).
  ENDMETHOD.


  METHOD question.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR      = ' '
*       DIAGNOSE_OBJECT             = ' '
        text_question = iv_message
*       TEXT_BUTTON_1 = 'Ja'(001)
*       ICON_BUTTON_1 = ' '
*       TEXT_BUTTON_2 = 'Nein'(002)
*       ICON_BUTTON_2 = ' '
*       DEFAULT_BUTTON              = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN  = 25
*       START_ROW     = 6
*       POPUP_TYPE    =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer        = lv_answer
*     TABLES
*       PARAMETER     =
*     EXCEPTIONS
*       TEXT_NOT_FOUND              = 1
*       OTHERS        = 2
      .

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD redo_transfer.

    DATA: lt_src        TYPE TABLE OF /qaps/v_prm_full,
          lv_grp_planta TYPE /qaps/ed_cod_grp_planta.
************************************************
*    Provisório p/ não perder cenário
************************************************
*    delete from /qaps/prem_distr where id_simulacao = ms_simulacao-id_simulacao
*                                   and modalidade   = 'T'.

************************************************
*    Fim Provisório p/ não perder cenário
************************************************

    CHECK lines( mt_transferencias ) > 0.

    DATA(lo_premissa) = NEW /qaps/cl_mdl_premissa( ).

    DATA(lt_transfer_grp_planta) = mt_transferencias.
    DATA(lt_transfer_centros) = mt_transferencias.

    DELETE lt_transfer_grp_planta WHERE id_centro <> mc_guid_null.
    DELETE lt_transfer_centros WHERE id_centro = mc_guid_null.

    LOOP AT lt_transfer_grp_planta INTO DATA(ls_transfer_grp_planta).

      lv_grp_planta = ls_transfer_grp_planta-cod_origem.

      SELECT *
        FROM /qaps/v_prm_full
        WHERE grp_planta = @lv_grp_planta
        AND   id_centro  = @mc_guid_null
        AND   id_simulacao = @ms_simulacao-id_simulacao
        AND modalidade <> 'T'
        APPENDING TABLE @lt_src.

    ENDLOOP.

    LOOP AT lt_src ASSIGNING FIELD-SYMBOL(<fs_src>).
      <fs_src>-matnr = |{ <fs_src>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    LOOP AT lt_transfer_grp_planta INTO ls_transfer_grp_planta.

      TRY.
          CASE ls_transfer_grp_planta-tipo_regra.
            WHEN 'AG'.
              DATA(ls_prm_full) = lt_src[ grp_planta = ls_transfer_grp_planta-cod_origem
                                           tipo_regra = 'AG'
                                           agregador  =  ls_transfer_grp_planta-agregador ].
            WHEN 'MA'.
              ls_prm_full = lt_src[ grp_planta = ls_transfer_grp_planta-cod_origem
                                    tipo_regra = 'MA'
                                    matnr      =  ls_transfer_grp_planta-matnr ].
          ENDCASE.

          lo_premissa->add_transfer_without_dialog( is_transfer = ls_transfer_grp_planta
                                                    is_premissa = ls_prm_full ).

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDLOOP.

****    Por Centro

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @lt_transfer_centros
      WHERE grp_planta = @lt_transfer_centros-grp_planta
      AND   id_simulacao = @ms_simulacao-id_simulacao
      AND modalidade = 'T'
      INTO TABLE @DATA(lt_check).

    LOOP AT lt_check ASSIGNING <fs_src>.
      <fs_src>-matnr = |{ <fs_src>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @lt_transfer_centros
      WHERE grp_planta = @lt_transfer_centros-grp_planta
      AND   werks  = @lt_transfer_centros-werks
      AND   id_simulacao = @ms_simulacao-id_simulacao
      AND modalidade <> 'T'
      INTO TABLE @lt_src.

    LOOP AT lt_src ASSIGNING <fs_src>.
      <fs_src>-matnr = |{ <fs_src>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    LOOP AT lt_transfer_centros INTO DATA(ls_transfer_centros).

      CHECK NOT line_exists( lt_check[ werks = ls_transfer_centros-cod_origem
                                       tipo_regra = 'AG'
                                       agregador  =  ls_transfer_centros-agregador ] ).

      TRY.
          CASE ls_transfer_centros-tipo_regra.
            WHEN 'AG'.
              ls_prm_full = lt_src[ werks = ls_transfer_centros-cod_origem
                                    tipo_regra = 'AG'
                                    agregador  =  ls_transfer_centros-agregador ].
            WHEN 'MA'.
              ls_prm_full = lt_src[ werks = ls_transfer_centros-cod_origem
                                    tipo_regra = 'MA'
                                    agregador  =  ls_transfer_centros-matnr ].
          ENDCASE.

          lo_premissa->add_transfer_without_dialog( is_transfer = ls_transfer_centros
                                                    is_premissa = ls_prm_full ).

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.


  METHOD reply_to_children.

    TYPES: BEGIN OF ts_distrib,
             modalidade  TYPE /qaps/prem_distr-modalidade,
             tipo_origem TYPE /qaps/prem_distr-tipo_origem,
             id_origem   TYPE /qaps/prem_distr-id_origem,
           END OF ts_distrib.

    TYPES: BEGIN OF ts_distr_traj,
             id_distribuicao TYPE /qaps/prem_distr-id_distribuicao,
             id_prem_trajeto TYPE /qaps/prem_traj-id_prem_trajeto,
             id_new          TYPE /qaps/prem_traj-id_prem_trajeto,
           END OF ts_distr_traj.

    DATA: lt_distrib         TYPE TABLE OF ts_distrib,
          lv_id_distribuicao TYPE /qaps/prem_distr-id_distribuicao,
          lr_data            TYPE REF TO data.

    DATA: lt_distr_traj TYPE TABLE OF ts_distr_traj.

*    BREAK C060863.

    lt_distrib = VALUE #( FOR wa IN it_distrib_template
                            ( modalidade = wa-modalidade
                              tipo_origem = wa-tipo_origem
                              id_origem =  wa-id_origem ) ).

    SORT lt_distrib BY modalidade tipo_origem id_origem.
    DELETE ADJACENT DUPLICATES FROM lt_distrib COMPARING ALL FIELDS.

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_item = @is_item_parent-id_item
      INTO TABLE @DATA(lt_validacao).

    SELECT *
      FROM /qaps/prem_item
      WHERE id_parent = @is_item_parent-id_item
      INTO TABLE @DATA(lt_item_parent).

    LOOP AT lt_distrib INTO DATA(ls_distrib).

      CHECK NOT line_exists( lt_validacao[ modalidade  = ls_distrib-modalidade
                                           tipo_origem = ls_distrib-tipo_origem
                                           id_origem   = ls_distrib-id_origem ] ).

      DATA(lt_distrib_new) = it_distrib_template.
      DELETE lt_distrib_new WHERE modalidade  <> ls_distrib-modalidade
                               OR  tipo_origem <> ls_distrib-tipo_origem
                               OR  id_origem   <> ls_distrib-id_origem.

      "Template para buscar trajetos
      DATA(ls_distrib_template) = lt_distrib_new[ 1 ].

      "Distribuições
      lv_id_distribuicao = cl_system_uuid=>create_uuid_x16_static( ).
      LOOP AT lt_distrib_new ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-id_parent = <fs>-id_distribuicao.
        <fs>-id_distribuicao = lv_id_distribuicao.
        <fs>-id_item = is_item_parent-id_item.

        lr_data = REF #( <fs> ).
        preencher_dados_controle( CHANGING cr_data = lr_data  ).

      ENDLOOP.

      MODIFY /qaps/prem_distr FROM TABLE lt_distrib_new.

      "Trajeto
      SELECT *
        FROM /qaps/prem_traj
        WHERE id_distribuicao = @ls_distrib_template-id_distribuicao
        INTO TABLE @DATA(lt_trajeto_new).


      LOOP AT lt_trajeto_new ASSIGNING FIELD-SYMBOL(<ls_trajeto_new>).

        IF NOT line_exists( lt_distr_traj[ id_distribuicao = lv_id_distribuicao
                                           id_prem_trajeto = <ls_trajeto_new>-id_prem_trajeto ] ).
          DATA(ls_distr_traj) = VALUE ts_distr_traj(
              id_distribuicao = lv_id_distribuicao
              id_prem_trajeto = <ls_trajeto_new>-id_prem_trajeto
              id_new          = cl_system_uuid=>create_uuid_x16_static( ) ).
          APPEND ls_distr_traj TO lt_distr_traj.
        ELSE.
          ls_distr_traj = lt_distr_traj[ id_distribuicao = lv_id_distribuicao
                                         id_prem_trajeto = <ls_trajeto_new>-id_prem_trajeto ].
        ENDIF.

        <ls_trajeto_new>-id_parent = ls_distr_traj-id_prem_trajeto.
        <ls_trajeto_new>-id_prem_trajeto = ls_distr_traj-id_new.
        <ls_trajeto_new>-id_distribuicao = ls_distr_traj-id_distribuicao.

        lr_data = REF #( <ls_trajeto_new> ).
        preencher_dados_controle( CHANGING cr_data = lr_data  ).

      ENDLOOP.

      MODIFY /qaps/prem_traj FROM TABLE lt_trajeto_new.

      "Recursividade
      LOOP AT lt_item_parent INTO DATA(ls_item_parent).
        reply_to_children( is_item_parent = ls_item_parent
                           it_distrib_template = lt_distrib_new  ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD send_transfer_to_memory.

    IF iv_id_simul_ref IS INITIAL.

      SELECT *
        FROM /qaps/v_prm_full
        WHERE id_simulacao = @ms_simulacao-id_simulacao
        AND modalidade = 'T'
        AND id_item_parent = @mc_guid_null
        INTO TABLE @mt_transferencias.

    ELSE.

      SELECT *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @iv_id_simul_ref
      AND modalidade = 'T'
      AND id_item_parent = @mc_guid_null
      INTO TABLE @mt_transferencias.

    ENDIF.

  ENDMETHOD.


  METHOD show_progress.

    DATA: lv_total TYPE i,
          lv_idx   TYPE i,
          lv_sent  TYPE abap_bool.

      CALL METHOD cl_progress_indicator=>progress_indicate(
        EXPORTING
          i_text               = iv_message
          i_processed          = iv_percentual
          i_total              = 100
          i_output_immediately = abap_true
        IMPORTING
          e_progress_sent      = lv_sent ).

  ENDMETHOD.


  METHOD sincronizar_input_create.

*    get_simulacao( is_data-id_simulacao ).
*
*    CHECK ms_simulacao-status = 'A'.
*
*    get_custo_elementar( is_data-id_custo_elementar ).
*
*    CASE ms_custo_elementar-tipo_variavel.
*
*      WHEN 'F'.
*        input_create_frete( is_data ).
*      WHEN OTHERS.
*
*        "Identificar cenário - Destino Cais ou Porto
*        IF ( is_data-tipo_origem = 'I' OR is_data-tipo_origem = 'P' ).
*
*          "Importado
*          IF ms_custo_elementar-importacao = 'X'.
*            input_create_importacao( is_data ).
*          ENDIF.
*
*        ENDIF.
*
*        "Identificar cenário - Premissa - Nacional
*        "Importado
*        IF ms_custo_elementar-tipo_variavel = 'C'.
*          IF ms_custo_elementar-importacao = 'X'.
*            input_create_compras( is_data = is_data  iv_modalidade = 'I' ).
*          ENDIF.
*
*          IF ms_custo_elementar-nacional = 'X'.
*            input_create_compras( is_data = is_data iv_modalidade = 'N' ).
*          ENDIF.
*
*        ENDIF.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD sincronizar_input_delete.

*    DATA lv_message TYPE string.
*    DATA lv_first_time TYPE abap_bool VALUE abap_true.
*
*    DEFINE question.
*
*      IF lv_first_time = abap_true.
*        IF question( lv_message ) = abap_false.
*          RETURN.
*        ENDIF.
*        lv_first_time = abap_false.
*      ENDIF.
*    END-OF-DEFINITION.
*
*    get_simulacao( iv_id_simulacao ).
*
*    SELECT *
*      FROM /qaps/v_cst_inp
*      WHERE id_var_input IN @ir_id_var_input
*      AND   id_simulacao = @ms_simulacao-id_simulacao
*      AND   status = 'A'
*      INTO TABLE @DATA(lt_input).
*
*    IF lines( lt_input ) = 0.
*      return = abap_true.
*      RETURN.
*    ENDIF.
*
*    lv_message = TEXT-m01.
*
*    LOOP AT lt_input INTO DATA(ls_input).
*
*      IF ( ls_input-tipo_origem = 'I' OR ls_input-tipo_origem = 'P' )
*          AND ls_input-importacao = 'X'.
*        question."Macro
*        delete_premissa_matriz( ls_input ).
*        delete_matriz_abastecimento( ls_input ).
*      ENDIF.
*
*      IF ls_input-tipo_variavel = 'C'.
*        IF ls_input-importacao = 'X' OR ls_input-nacional = 'X'..
*          question. "Macro
*          delete_premissa( ls_input ).
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.

    return = abap_true.

  ENDMETHOD.


  METHOD sincronizar_matriz_premissa.

    DATA lt_distr_template TYPE /qaps/t_prem_distr.

    SELECT *
      FROM /qaps/prem_item
      WHERE id_parent = @iv_id_item
      INTO TABLE @DATA(lt_item_parent).

    IF lines( lt_item_parent ) > 0.

      "Distribuição do Pai
      SELECT *
        FROM /qaps/prem_distr
        WHERE id_item = @iv_id_item
        INTO TABLE @lt_distr_template.

      LOOP AT lt_item_parent INTO DATA(ls_item_parent).
        reply_to_children( is_item_parent      = ls_item_parent
                           it_distrib_template = lt_distr_template ).
      ENDLOOP.

    ENDIF.

    create_premissa_relation_mtz( is_data ).

  ENDMETHOD.


  METHOD sincronizar_std_prd_assign.

*    get_simulacao( is_data-id_simulacao ).
*
*    DATA(ls_relation) = check_relation_std_prd( iv_id_tp_lista = iv_id_tp_lista
*                                                is_data        = is_data ).
*
*
*    DATA(lt_destino) = create_std_prd_assign_destino( ls_relation-t_destino ).
*
*    DATA(lt_material) = create_std_prd_assign_material( it_destino = lt_destino
*                                                        it_material = ls_relation-t_item ).
*
*    create_std_prd_assign_origem( it_destino  = lt_destino
*                                  it_material = lt_material ).
*
*    update_percentual_auto_prem( ).


  ENDMETHOD.


  method SINCRONIZAR_STD_PRD_CHANGE.
  endmethod.


  METHOD sincronizar_std_prd_unassign.

*    DATA: lr_id_premissa     TYPE RANGE OF /qaps/prem_hdr-id_premissa,
*          lr_id_item         TYPE RANGE OF /qaps/prem_item-id_item,
*          lr_id_distribuicao TYPE RANGE OF /qaps/prem_distr-id_distribuicao.
*
*    DATA: lr_data  TYPE REF TO data,
*          lt_entry TYPE TABLE OF /qaps/prem_item.
*
*    return = abap_true.
*
*    get_simulacao( is_data-id_simulacao ).
*
*    SELECT *
*      FROM /qaps/v_prm_full
*      WHERE id_simulacao = @is_data-id_simulacao
*      AND modalidade = 'P'
*      INTO TABLE @DATA(lt_prm_full).
*
*    CHECK lines( lt_prm_full ) > 0.
*
*    lr_id_premissa = VALUE #( FOR wa IN lt_prm_full
*                                  ( sign = 'I' option = 'EQ' low = wa-id_premissa ) ).
*    lr_id_item  = VALUE #( FOR wa IN lt_prm_full
*                                  ( sign = 'I' option = 'EQ' low = wa-id_item ) ).
*    lr_id_distribuicao = VALUE #( FOR wa IN lt_prm_full
*                                  ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).
*
*    SORT: lr_id_premissa BY low,
*          lr_id_item         BY low,
*          lr_id_distribuicao  BY low.
*
*    DELETE: lr_id_premissa WHERE low IS INITIAL,
*            lr_id_item WHERE low IS INITIAL       ,
*            lr_id_distribuicao WHERE low IS INITIAL.
*
*    DELETE ADJACENT DUPLICATES FROM: lr_id_premissa,
*                                     lr_id_item        ,
*                                     lr_id_distribuicao.
*
*    DELETE FROM /qaps/prem_distr WHERE id_distribuicao IN lr_id_distribuicao.
*    COMMIT WORK AND WAIT.
*
*    SELECT id_item, COUNT( * ) AS qty
*      FROM /qaps/prem_distr
*      WHERE id_item IN @lr_id_item
*      GROUP BY id_item
*      INTO TABLE @DATA(lt_prem_distr).
*
*    SELECT *
*      FROM /qaps/prem_item
*      WHERE id_item IN @lr_id_item
*      INTO TABLE @DATA(lt_prem_item).
*
*    LOOP AT lt_prem_item ASSIGNING FIELD-SYMBOL(<fs_prem_item>).
*
*      IF NOT line_exists( lt_prem_distr[ id_item = <fs_prem_item>-id_item ] ).
*        DELETE FROM /qaps/prem_item WHERE id_item = <fs_prem_item>-id_item.
*      ELSE.
*
*        DATA(ls_entry) = <fs_prem_item>.
*
*        CLEAR ls_entry-std_prd.
*
*        IF NOT ls_entry-id_parent IS INITIAL.
*          ls_entry-oculto = 'X'.
*        ENDIF.
*
*        lr_data = REF #( ls_entry ).
*        preencher_dados_controle( CHANGING cr_data = lr_data ).
*
*        APPEND ls_entry TO lt_entry.
*
*      ENDIF.
*
*    ENDLOOP.
*
*    MODIFY /qaps/prem_item FROM TABLE lt_entry.
*
*    COMMIT WORK.
*
*    SELECT COUNT( * ) AS qty
*      FROM /qaps/v_prm_full
*      WHERE id_premissa IN @lr_id_premissa
*      INTO @DATA(lv_qty_premissa).
*
*    IF lv_qty_premissa = 0.
*
*      DELETE FROM /qaps/prem_hdr WHERE id_premissa IN lr_id_premissa.
*      COMMIT WORK AND WAIT.
*
*    ENDIF.
*
*    update_percentual_auto_prem( ).


  ENDMETHOD.


  METHOD sincronizar_trajeto_create.

*    DATA: lt_origem  TYPE /qaps/t_hierarquia_origem,
*          lt_destino TYPE /qaps/t_hierarquia_destino,
*          lt_create  type /QAPS/T_PRM_FULL.
*
*    DATA(ls_trajeto) = get_trajeto_by_id( iv_id_trajeto ).
*
*    data(ls_ponto_origem) = get_ponto_by_id( iv_id_ponto = ls_trajeto-id_origem  ).
*    data(ls_ponto_destino) = get_ponto_by_id( iv_id_ponto = ls_trajeto-id_destino  ).
*
*    append value /qaps/s_hierarquia_origem(
*        tipo_origem = ls_trajeto-tipo_origem
*        id_origem   = ls_trajeto-id_origem
*        codigo      = ls_ponto_origem-codigo ) to lt_origem.
*
*    append value /qaps/s_hierarquia_destino(
*        tipo_destino = ls_trajeto-tipo_destino
*        id_destino   = ls_trajeto-id_destino
*        codigo       = ls_ponto_destino-codigo ) to lt_destino.
*
*    get_hierarquia_origem( CHANGING ct_hierarquia_origem = lt_origem ).
*    get_hierarquia_destino( CHANGING ct_hierarquia_destino = lt_destino ).
*
*    loop at lt_destino ASSIGNING FIELD-SYMBOL(<fs_destino>).
*      check <fs_destino>-tipo_destino = 'G'.
*      <fs_destino>-id_destino = ls_ponto_destino-id_externo.
*    ENDLOOP.
*
*    data(lt_prm_full) = get_premissa_full( ).
*    delete lt_prm_full where status <> 'A'.
*
*    loop at lt_prm_full into data(ls_prm_full).
*
*      check line_exists( lt_origem[ tipo_origem = ls_prm_full-tipo_origem
*                                    id_origem   = ls_prm_full-id_origem ] )
*        and ( line_exists( lt_destino[ tipo_destino = 'G'
*                                       id_destino   = ls_prm_full-id_grp_planta ] ) or
*              line_exists( lt_destino[ tipo_destino = 'W'
*                                       id_destino   = ls_prm_full-id_centro ] )
*                                       ).
*
*        append ls_prm_full to lt_create.
*
*    endloop.
*
*    loop at lt_create into data(ls_create).
*      append_new_trajeto( is_template   = ls_create
*                          iv_id_trajeto = iv_id_trajeto ).
*    endloop.

    return = abap_true.

  ENDMETHOD.


  METHOD sincronizar_trajeto_delete.

*    DATA lv_message TYPE string.
*
*    DEFINE question.
*      IF question( lv_message ) = abap_false.
*        RETURN.
*      ENDIF.
*    END-OF-DEFINITION.
*
*    DATA(ls_trajeto) = get_trajeto_by_id( iv_id_trajeto ).
*
*    SELECT DISTINCT id_prem_trajeto,id_distribuicao
*      FROM /qaps/prem_traj
*      WHERE id_trajeto = @iv_id_trajeto
*      INTO TABLE @DATA(lt_prem_traj).
*
*    IF lines( lt_prem_traj ) > 0.
*
*      IF iv_check = abap_true.
*        lv_message = TEXT-m02.
*        question."Macro
*      ENDIF.
*
*      DATA(lt_prm_full) = get_premissa_full( ).
*      DELETE lt_prm_full WHERE status <> 'A'.
*
*      LOOP AT lt_prem_traj INTO DATA(ls_prem_traj).
*
*        CHECK line_exists( lt_prm_full[ id_distribuicao = ls_prem_traj-id_distribuicao ] ).
*        DELETE FROM /qaps/prem_traj WHERE id_prem_trajeto = ls_prem_traj-id_prem_trajeto.
*
*      ENDLOOP.
*
*      COMMIT WORK.
*
*    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD update_link_matriz_prem_distr.

    UPDATE /qaps/prem_distr
    SET id_distr_matriz = iv_id_distr_matriz
    WHERE id_distribuicao = iv_id_distribuicao.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD update_link_matriz_prem_hdr.

    UPDATE /qaps/prem_hdr
    SET id_matriz_abast = iv_id_matriz_abast
    WHERE id_premissa = iv_id_premissa.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD update_link_matriz_prem_item.

    UPDATE /qaps/prem_item
    SET id_item_matriz = iv_id_item_matriz
    WHERE id_item = iv_id_item.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD update_percentual.

    "Distribuição
    update_percentual_premissa( ).

    "Trajeto
    update_percentual_trajeto( ).

  ENDMETHOD.


  METHOD update_percentual_automatico.

    SELECT *
      FROM /qaps/v_mtz_res
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_data).

    CHECK lines( lt_data ) >  0.

    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @lt_data
      WHERE id_item = @lt_data-id_item
      INTO TABLE @DATA(lt_distr).


    LOOP AT lt_distr INTO DATA(ls_distr).

      DATA(ls_data) = lt_data[ id_item = ls_distr-id_item ].

      IF ls_data-qty = 1.
        UPDATE /qaps/matriz_dst
        SET percentual = 100
        WHERE id_distribuicao = @ls_distr-id_distribuicao.

        UPDATE /qaps/prem_distr
        SET percentual = 100
        WHERE id_distr_matriz = @ls_distr-id_distribuicao.

      ELSE.
        UPDATE /qaps/matriz_dst
        SET percentual = 0
        WHERE id_distribuicao = @ls_distr-id_distribuicao
        AND percentual = 100.

        UPDATE /qaps/prem_distr
        SET percentual = 0
        WHERE id_distr_matriz = @ls_distr-id_distribuicao
        AND percentual = 100.

      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  METHOD update_percentual_auto_prem.

    SELECT *
      FROM /qaps/v_prm_res
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_data).

    CHECK lines( lt_data ) >  0.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_data
      WHERE id_item = @lt_data-id_item
      INTO TABLE @DATA(lt_distr).


    LOOP AT lt_distr INTO DATA(ls_distr).

      DATA(ls_data) = lt_data[ id_item = ls_distr-id_item ].

      IF ls_data-qty = 1.

        UPDATE /qaps/prem_distr
        SET percentual = 100
        WHERE id_distribuicao = @ls_distr-id_distribuicao.

      ELSE.

        UPDATE /qaps/prem_distr
        SET percentual = 0
        WHERE id_distribuicao = @ls_distr-id_distribuicao.

      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  method UPDATE_PERCENTUAL_PREMISSA.
  endmethod.


  METHOD update_percentual_trajeto.

    SELECT *
      FROM /qaps/v_prt_cnt
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_data).

    CHECK lines( lt_data ) >  0.

    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @lt_data
      WHERE id_distribuicao = @lt_data-id_distribuicao
      INTO TABLE @DATA(lt_traj).


    LOOP AT lt_traj INTO DATA(ls_traj).

      DATA(ls_data) = lt_data[ id_distribuicao = ls_traj-id_distribuicao ].

      IF ls_data-qty = 1.

        UPDATE /qaps/prem_traj
        SET percentual = 100
        WHERE id_distribuicao = ls_traj-id_distribuicao.

      ELSE.

        UPDATE /qaps/prem_traj
        SET percentual = 0
        WHERE id_distribuicao = ls_traj-id_distribuicao.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
