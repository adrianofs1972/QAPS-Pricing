class /QAPS/CL_MDL_PREMISSA definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods ADD_STD_PRODUCAO
    importing
      !IS_SIMULACAO type /QAPS/S_PREMISSA_SIMULACAO
      !IS_PREMISSA_HEADER type /QAPS/S_MATRIZ_ABAST_HEADER
      !IS_TREE_DESTINO type /QAPS/S_DESTINO
    returning
      value(RETURN) type ABAP_BOOL .
  methods ADD_TRANSFER_WITHOUT_DIALOG
    importing
      !IS_PREMISSA type /QAPS/V_PRM_FULL
      !IS_TRANSFER type /QAPS/V_PRM_FULL optional
    returning
      value(RETURN) type ABAP_BOOL .
  methods ADD_TRANSFER
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_HEADER type /QAPS/S_PREMISSA_HEADER
      !IS_ITEM type /QAPS/S_PREMISSA_ITEM optional
    returning
      value(RETURN) type ABAP_BOOL .
  methods CONSTRUCTOR
    importing
      !IV_ACTION type /QAPS/PROCESS_ACTION optional .
  methods CREATE_INPUT
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_DATA type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_INPUT
    importing
      !IT_DATA type /QAPS/T_PRM_INPUT_SELECTED
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_DISTRIBUICAO
    importing
      !IS_DATA type /QAPS/S_PREMISSA_ITEM
    returning
      value(RETURN) type /QAPS/T_PREMISSA_DISTRIB .
  methods GET_HEADER
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_PREMISSA_HEADER .
  methods GET_ITEMS
    importing
      !IS_DATA type /QAPS/S_PREMISSA_HEADER
    returning
      value(RETURN) type /QAPS/T_PREMISSA_ITEM .
  methods GET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_PREMISSA_SIMULACAO .
  methods GET_TRAJETO_BY_DISTRIBUICAO
    importing
      !IS_DATA type /QAPS/S_PREMISSA_DISTRIB
    returning
      value(RETURN) type /QAPS/T_PREMISSA_TRAJETO .
  methods INITIALIZE_PREMISSA .
  methods SET_PERIODO
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  methods UPDATE_INPUT
    importing
      !IT_CHANGED_DATA type /QAPS/T_PREMISSA_CHANGED_DATA .
  methods UPDATE_TRAJETO
    importing
      !IT_CHANGED_DATA type /QAPS/T_PREM_TRAJ_CHANGED_DATA .
  PROTECTED SECTION.
private section.

  data MS_SIMULACAO type /QAPS/SIMULACAO .
  data MT_CAIS type /QAPS/T_CAIS .
  data MT_PONTO type /QAPS/V_PONTO .
  data MT_CUSTO_ELM type /QAPS/T_CUSTO_ELEMENTAR .
  data MV_FIRST_TIME type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data MV_TEXT_DOMAIN_LOADED type ABAP_BOOL .
  data MO_MATERIAL type ref to /QAPS/CL_MDL_MATERIAL .
  data MO_CUSTO_ELEM type ref to /QAPS/CL_MDL_CUSTO_ELEMENTAR .
  data MT_STD_PRODUCAO type /QAPS/T_STD_PRD_PA .
  data MO_INPUT_ELEM type ref to /QAPS/CL_MDL_INPUT_CUSTO_ELEM .
  data MO_LOGISTICA type ref to /QAPS/CL_MDL_LOGISTICA .
  data MT_CATALOG type LVC_T_FCAT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MT_DATA type ref to DATA .
  data MS_DATA type ref to DATA .
  data MT_MODALIDADE type DD07VTAB .
  data MT_ORIGEM_DESTINO type DD07VTAB .
  data MT_TIPO_REGRA type DD07VTAB .

  methods ADD_TRANSFER_DESTINO
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_DESTINO type /QAPS/PONTO
    returning
      value(RETURN) type /QAPS/T_PREM_DESTINO .
  methods ADD_TRANSFER_DISTRIB
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_HEADER type /QAPS/S_PREMISSA_HEADER
      !IS_MATERIAL type /QAPS/PREM_ITEM
      !IS_ORIGEM type /QAPS/PONTO
      !IV_ID_PARENT type /QAPS/ED_ID_DISTRIBUICAO optional
      !IV_PERCENTUAL type /QAPS/PERCENTUAL
    returning
      value(RETURN) type /QAPS/ED_ID_DISTRIBUICAO .
  methods ADD_TRANSFER_MATERIAL
    importing
      !IV_ID_PREMISSA type /QAPS/ED_ID_PREMISSA
      !IV_TIPO_REGRA type /QAPS/TIPO_REGRA
      !IV_AGREGADOR type /QAPS/AGREGADOR optional
      !IV_MATNR type MATNR optional
    returning
      value(RETURN) type /QAPS/PREM_ITEM .
  methods DELETE_TRANSFERENCIAS
    importing
      !IR_ID_DISTRIBUICAO type /QAPS/R_ID_DISTRIBUICAO
      !IS_SIMULACAO type /QAPS/S_SIMULACAO .
  methods GET_DESTINO_MATERIAL
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_DESTINO type /QAPS/PONTO
      !IS_MATERIAL type /QAPS/PREM_ITEM
    exporting
      !ET_DESTINO type /QAPS/T_PREM_DESTINO
      !ET_MATERIAL type /QAPS/T_PREM_MATERIAL .
  methods GET_TRANSFER_HIERARQUIA
    importing
      !IS_MATERIAL type /QAPS/PREM_ITEM
    returning
      value(RETURN) type /QAPS/T_PREM_MATERIAL .
  methods GET_MATERIAL_TRANSFER_ITEM
    importing
      !IS_ITEM type /QAPS/PREM_ITEM
      !IS_HEADER type /QAPS/S_PREMISSA_HEADER
    returning
      value(RETURN) type /QAPS/S_PREMISSA_ITEM .
  methods MATERIAL_TRANSFER_EXISTS
    importing
      !IS_ITEM type /QAPS/PREM_ITEM
      !IS_HEADER type /QAPS/S_PREMISSA_HEADER
    returning
      value(RETURN) type ABAP_BOOL .
  methods CONVERT_CAIS_TO_PORTO
    importing
      !IT_DATA type /QAPS/T_VAR_FULL
    returning
      value(RETURN) type /QAPS/T_VAR_FULL .
  methods CHECK_DISTRIB_EXISTS
    importing
      !IS_DATA type /QAPS/V_PRM_DBAS
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_DISTRIBUICAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      value(IS_DATA) type /QAPS/PREM_DISTR .
  methods CREATE_DISTRIBUICAO_BY_MATRIZ
    importing
      !IS_DATA type /QAPS/V_PRM_DBAS .
  methods CREATE_DYNAMIC_CATALOG
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type LVC_T_FCAT .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT .
  methods CREATE_TRAJETO
    importing
      !IS_TRAJETO type /QAPS/S_TRAJETO
      !IS_ORIGEM_DESTINO type /QAPS/V_PRM_ALL
      !IV_PERCENTUAL type /QAPS/PERCENTUAL default 0 .
  methods CRIAR_PREMISSA
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_TRAJETO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_DISTRIBUICAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_DISTR_BY_DEST
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IS_DESTINO type /QAPS/V_PRM_HDR
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CRIAR_PREMISSA_DISTR_C_DEST
    importing
      !IS_ITEM type /QAPS/V_PRM_DBAS
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CRIAR_PREMISSA_DISTR_STD_PRD
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IS_DESTINO type /QAPS/V_PRM_HDR
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CRIAR_PREMISSA_DISTR_S_DEST
    importing
      !IS_ITEM type /QAPS/V_PRM_DBAS
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE .
  methods CRIAR_PREMISSA_HEADER
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_ITEM_NACIONAL
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_ITEM_IMPORTACAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_ITEM
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods CRIAR_PREMISSA_ITEM_STD_PRD
    importing
      !IS_HEADER type /QAPS/V_PRM_HDR
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_DATA type /QAPS/T_INP_MAT .
  methods CRIAR_PREMISSA_ITEM_IMPORTADO
    importing
      !IS_HEADER type /QAPS/V_PRM_HDR optional
      !IS_SIMULACAO type /QAPS/SIMULACAO optional
      !IT_DATA type /QAPS/T_INP_MAT optional .
  methods CRIAR_PREMISSA_ITEM_C_DEST
    importing
      !IS_HEADER type /QAPS/V_PRM_HDR
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_DATA type /QAPS/T_INP_MAT .
  methods CRIAR_PREMISSA_ITEM_S_DEST
    importing
      !IS_HEADER type /QAPS/V_PRM_HDR
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_DATA type /QAPS/T_INP_MAT .
  methods EXECUTE_DISTRIBUICAO
    importing
      !IS_ORIGEM type /QAPS/V_PRM_IBAS
      !IS_DESTINO type /QAPS/V_PRM_DBAS
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods EXECUTE_TRAJETO_TRANSFER
    importing
      !IS_DESTINO type /QAPS/PONTO
      !IS_DATA type /QAPS/V_PRM_ALL .
  methods EXECUTE_TRAJETO
    importing
      !IS_DATA type /QAPS/V_PRM_ALL .
  methods FILL_DESCRIPTION
    changing
      !CT_DATA type /QAPS/T_PREMISSA_INPUT .
  methods FILL_DOMAIN_TEXT .
  methods FILL_STD_PRODUCAO
    importing
      !IS_SIMULACAO type /QAPS/S_PREMISSA_SIMULACAO
      !IS_HEADER type /QAPS/S_PREMISSA_HEADER
    changing
      !CT_DATA type /QAPS/T_PREMISSA_INPUT .
  methods GET_CUSTOS_ELEMENTARES
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_DISTRIBUICAO_NACIONAL
    importing
      !IS_DATA type /QAPS/S_PREMISSA_ITEM
    returning
      value(RETURN) type /QAPS/T_PREMISSA_DISTRIB .
  methods GET_INPUT_BY_SIMULACAO
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/T_FILE_VAR_INPUT .
  methods GET_REFERENCIAS
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods GET_SINGLE_HEADER
    importing
      !IV_ID_PREMISSA type /QAPS/ED_ID_PREMISSA
    returning
      value(RETURN) type /QAPS/PREM_HDR .
  methods GET_STATUS
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_TIPO_DESTINO type /QAPS/ED_TIPO_DESTINO
      !IS_MATRIZ_HEADER type /QAPS/S_MATRIZ_ABAST_HEADER
      !IV_WERKS type WERKS_D
      !IV_ID_DESTINO type GUID16
    returning
      value(RETURN) type ICON_D .
  methods GET_STD_PRODUCAO
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/T_STD_PRD_PA .
  methods GET_TRAJETO_TRANSFER
    importing
      !IS_DESTINO type /QAPS/PONTO
      !IS_DATA type /QAPS/V_PRM_ALL
    returning
      value(RETURN) type /QAPS/T_TRAJETO .
  methods GET_TRAJETO
    importing
      !IS_DATA type /QAPS/V_PRM_ALL
    returning
      value(RETURN) type /QAPS/T_TRAJETO .
  methods INITIALIZE .
  methods INITIALIZE_DISTRIBUICAO .
  methods INITIALIZE_HEADER .
  methods INITIALIZE_ITEM .
  methods INITIALIZE_ITEM_COM_DESTINO .
  methods INITIALIZE_ITEM_IMPORTACAO .
  methods INITIALIZE_ITEM_SEM_DESTINO .
  methods INITIALIZE_ITEM_STD_PRODUCAO .
  methods INITIALIZE_TRAJETO .
  methods INIT_DISTRIB_COM_DEST .
  methods INIT_DISTRIB_IMPORTACAO .
  methods INIT_DISTRIB_SEM_DEST .
  methods INIT_DISTRIB_STD_PRODUCAO .
  methods MATERIAL_EXISTS
    importing
      !IS_HEADER type /QAPS/V_PRM_HDR
      !IT_ITEM type /QAPS/T_PRM_DBAS
      !IS_DATA type /QAPS/V_INP_MAT
      !IT_BUFFER type /QAPS/T_PREM_ITEM
    returning
      value(RETURN) type ABAP_BOOL .
  methods MERGE_DATA
    importing
      !IT_DATA type /QAPS/T_MATRIZ_ABASTECIMENTO
      !IT_FCAT type LVC_T_FCAT .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods SET_DISTRIBUICAO_INICIAL .
ENDCLASS.



CLASS /QAPS/CL_MDL_PREMISSA IMPLEMENTATION.


  METHOD add_std_producao.

    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
*          ls_ponto      TYPE /qaps/s_ponto,
          ls_message    TYPE bapiret2,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_data       TYPE /qaps/premissa,
          lt_data       TYPE TABLE OF /qaps/premissa,
          lr_data       TYPE REF TO data.

    SELECT SINGLE COUNT( * ) AS qty
      FROM /qaps/premissa
      WHERE id_simulacao  = @is_simulacao-id_simulacao
      AND   modalidade   = 'P'
      AND tipo_regra    = @is_premissa_header-tipo_regra
      AND key_input     = @is_premissa_header-key_input
      AND id_destino    = @is_tree_destino-id_destino
      AND id_grp_planta = @is_tree_destino-id_grp_planta
      AND werks         = @is_tree_destino-werks
      INTO @DATA(lv_qty).

    CHECK lv_qty = 0.
*    BREAK-POINT.

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    REFRESH lt_data.

    DATA(lv_key) = cl_system_uuid=>create_uuid_x16_static( ).

    WHILE lv_periodo_inicial < is_simulacao-periodo_final.

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

      ls_data = VALUE /qaps/premissa(
          id_premissa   = lv_key
          periodo       = lv_ano && lv_mes
          modalidade    = 'P'
          id_simulacao  = is_simulacao-id_simulacao
          tipo_origem   = is_tree_destino-tipo_destino
          id_origem     = is_tree_destino-id_destino
          tipo_regra    = is_premissa_header-tipo_regra
          key_input     = is_premissa_header-key_input
          id_destino    = is_tree_destino-id_destino
          id_grp_planta = is_tree_destino-id_grp_planta
          werks         = is_tree_destino-werks
*                percentual    =
      ).

      lr_data = REF #( ls_data ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_data TO lt_data.

    ENDWHILE.

    SORT lt_data BY id_premissa periodo.
    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING ALL FIELDS.

*    BREAK-POINT.
    MODIFY /qaps/premissa FROM TABLE lt_data.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD add_transfer.

    DATA: lv_id(2)             TYPE n,
          lv_mes(2)            TYPE n,
          lv_ano(4)            TYPE n,
          lv_periodo(7)        TYPE c,
          lv_pos(2)            TYPE n,
          ls_ponto             TYPE /qaps/s_ponto_premissa,
          ls_message           TYPE bapiret2,
          ls_periodo           TYPE /qaps/s_periodo,
          lt_item              TYPE TABLE OF /qaps/prem_item,
          ls_data              TYPE /qaps/prem_distr,
          lt_data              TYPE TABLE OF /qaps/prem_distr,
          ls_ponto_item        TYPE  /qaps/prem_item,
          ls_ponto_origem      TYPE  /qaps/ponto,
          ls_ponto_destino     TYPE  /qaps/ponto,
          lr_data              TYPE REF TO data,
          lv_material_exists   TYPE abap_bool,
          lv_id_distrib_parent TYPE /qaps/prem_distr-id_distribuicao,
          lv_percentual        TYPE /qaps/percentual.
*    BREAK c060863.
    IF is_simulacao IS INITIAL.
      MESSAGE 'Selecionar uma simulação' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_PRM_TRAN_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        is_header    = is_header
        is_simulacao = is_simulacao
      IMPORTING
        es_item      = ls_ponto_item
        es_origem    = ls_ponto_origem
        es_destino   = ls_ponto_destino
        es_message   = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    get_destino_material( EXPORTING is_simulacao = is_simulacao
                                    is_destino  = ls_ponto_destino
                                    is_material = ls_ponto_item
                          IMPORTING et_destino = DATA(lt_destino)
                                    et_material = DATA(lt_material) ).

    LOOP AT lt_destino INTO DATA(ls_destino).

      CASE ls_ponto_item-tipo_regra.
        WHEN 'AG'.
          TRY.
              DATA(ls_material) = lt_material[ id_premissa = ls_destino-id_premissa
                                               tipo_regra = 'AG'
                                               agregador = ls_ponto_item-agregador ].
            CATCH cx_sy_itab_line_not_found.
              ls_material = add_transfer_material( iv_id_premissa = ls_destino-id_premissa
                                                   iv_tipo_regra  = 'AG'
                                                   iv_agregador   = ls_ponto_item-agregador ).
          ENDTRY.
        WHEN 'MA'.
          TRY.
              ls_material = lt_material[ id_premissa = ls_destino-id_premissa
                                         tipo_regra = 'MA'
                                         matnr = ls_ponto_item-matnr ].
            CATCH cx_sy_itab_line_not_found.
              ls_material = add_transfer_material( iv_id_premissa = ls_destino-id_premissa
                                                   iv_tipo_regra  = 'MA'
                                                   iv_matnr   = ls_ponto_item-matnr ).
          ENDTRY.
      ENDCASE.

      DATA(lt_hierarquia) = get_transfer_hierarquia( ls_material ).

      SELECT SINGLE COUNT( * ) AS qty
        FROM /qaps/prem_distr
        WHERE id_item = @ls_material-id_item
        INTO @DATA(lv_qty).

      IF lv_qty = 0.
        lv_percentual = 100.
      ENDIF.

      LOOP AT lt_hierarquia INTO ls_material.

        DATA(lv_id_distrib) = add_transfer_distrib( is_simulacao  = is_simulacao
                                                    is_header     = is_header
                                                    is_material   = ls_material
                                                    is_origem     = ls_ponto_origem
                                                    iv_percentual = lv_percentual
                                                    iv_id_parent  = lv_id_distrib_parent ).

        IF ls_material-tipo_regra = 'AG'.
          lv_id_distrib_parent = lv_id_distrib.
        ELSE.
          CLEAR lv_id_distrib_parent.
        ENDIF.

      ENDLOOP.

    ENDLOOP.



    return = abap_true.

  ENDMETHOD.


  METHOD add_transfer_destino.

    DATA lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_destino-id_ponto
      INTO @DATA(ls_ponto).

    CASE is_destino-tipo_ponto.
      WHEN 'G'.

        DATA(lv_key) = cl_system_uuid=>create_uuid_x16_static( ).

        DATA(ls_entry) = VALUE /qaps/prem_hdr(
            id_premissa     = lv_key
            id_simulacao    = is_simulacao-id_simulacao
            id_grp_planta   = ls_ponto-id_externo ).

        lr_data = REF #( ls_entry ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        MODIFY /qaps/prem_hdr FROM ls_entry.

        APPEND ls_entry TO return.

        SELECT *
          FROM /qaps/centro
          WHERE id_grp_planta = @ls_ponto-id_externo
          INTO TABLE @DATA(lt_centro).

        LOOP AT lt_centro INTO DATA(ls_centro).

          ls_entry = VALUE /qaps/prem_hdr(
            id_premissa     = cl_system_uuid=>create_uuid_x16_static( )
            id_simulacao    = is_simulacao-id_simulacao
            id_grp_planta   = ls_ponto-id_externo
            id_centro       = ls_centro-id_centro
            id_parent       = lv_key ).

          lr_data = REF #( ls_entry ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).
          MODIFY /qaps/prem_hdr FROM ls_entry.

          APPEND ls_entry TO return.

        ENDLOOP.

        COMMIT WORK AND WAIT.

      WHEN 'W'.

        lv_key = cl_system_uuid=>create_uuid_x16_static( ).

        SELECT SINGLE *
          FROM /qaps/centro
          WHERE id_centro       = @ls_ponto-id_externo
          INTO @ls_centro.

        ls_entry = VALUE /qaps/prem_hdr(
            id_premissa     = lv_key
            id_simulacao    = is_simulacao-id_simulacao
            id_grp_planta   = ls_centro-id_grp_planta
*            id_centro       = ls_ponto-id_externo
             ).

        lr_data = REF #( ls_entry ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        MODIFY /qaps/prem_hdr FROM ls_entry.

        APPEND ls_entry TO return.

        SELECT *
          FROM /qaps/centro
          WHERE id_grp_planta = @ls_centro-id_grp_planta
          INTO TABLE @lt_centro.

        LOOP AT lt_centro INTO ls_centro.

          ls_entry = VALUE /qaps/prem_hdr(
            id_premissa     = cl_system_uuid=>create_uuid_x16_static( )
            id_simulacao    = is_simulacao-id_simulacao
            id_grp_planta   = ls_centro-id_grp_planta
            id_centro       = ls_centro-id_centro
            id_parent       = lv_key ).

          lr_data = REF #( ls_entry ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).
          MODIFY /qaps/prem_hdr FROM ls_entry.

          APPEND ls_entry TO return.

        ENDLOOP.

        COMMIT WORK AND WAIT.

    ENDCASE.

  ENDMETHOD.


  METHOD add_transfer_distrib.

    DATA: lv_id(2)           TYPE n,
          lv_mes(2)          TYPE n,
          lv_ano(4)          TYPE n,
          lv_periodo(7)      TYPE c,
          lv_pos(2)          TYPE n,
          ls_ponto           TYPE /qaps/s_ponto_premissa,
          ls_message         TYPE bapiret2,
          ls_periodo         TYPE /qaps/s_periodo,
          lt_item            TYPE TABLE OF /qaps/prem_item,
          ls_data            TYPE /qaps/prem_distr,
          lt_data            TYPE TABLE OF /qaps/prem_distr,
          ls_ponto_item      TYPE  /qaps/prem_item,
          ls_ponto_origem    TYPE  /qaps/ponto,
          ls_ponto_destino   TYPE  /qaps/ponto,
          lr_data            TYPE REF TO data,
          lv_material_exists TYPE abap_bool.

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    REFRESH lt_data.

    DATA(lv_key) = cl_system_uuid=>create_uuid_x16_static( ).

    WHILE lv_periodo_inicial <= is_simulacao-periodo_final.

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

      check lv_periodo_inicial <= is_simulacao-periodo_final.

      ls_data = VALUE /qaps/prem_distr(
          id_distribuicao = lv_key
          periodo         = lv_ano && lv_mes
          id_premissa     = is_material-id_premissa
          id_item         = is_material-id_item
          modalidade      = 'T'
          percentual      = iv_percentual
          tipo_origem     = is_origem-tipo_ponto
          id_origem       = is_origem-id_ponto
          id_parent       = iv_id_parent ).

      lr_data = REF #( ls_data ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_data TO lt_data.

    ENDWHILE.

    SORT lt_data BY id_premissa periodo.
    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING ALL FIELDS.

*    BREAK-POINT.
    MODIFY /qaps/prem_distr FROM TABLE lt_data.
    COMMIT WORK AND WAIT.

    "---------------------------------------------
    "Trajeto
*    break c060863.
    SELECT *
      FROM /qaps/v_prm_all
      WHERE id_distribuicao = @lv_key
      INTO TABLE @DATA(lt_distrib).

    LOOP AT lt_distrib INTO DATA(ls_distrib).
      execute_trajeto_transfer( is_destino = is_origem
                                is_data = ls_distrib ).
    ENDLOOP.

    return = lv_key.

  ENDMETHOD.


  METHOD add_transfer_material.

    DATA: lr_data TYPE REF TO data.

    return =  VALUE /qaps/prem_item( id_item            = cl_system_uuid=>create_uuid_x16_static( )
                                     id_premissa      = iv_id_premissa
                                     tipo_regra       = iv_tipo_regra
                                     matnr            = iv_matnr
                                     agregador        = iv_agregador
                                       ).

    lr_data = REF #( return ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/prem_item FROM return.

    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD add_transfer_without_dialog.

    DATA: lv_id(2)             TYPE n,
          lv_mes(2)            TYPE n,
          lv_ano(4)            TYPE n,
          lv_periodo(7)        TYPE c,
          lv_pos(2)            TYPE n,
          ls_ponto             TYPE /qaps/s_ponto_premissa,
          ls_message           TYPE bapiret2,
          ls_periodo           TYPE /qaps/s_periodo,
          ls_header            TYPE /qaps/s_premissa_header,
          lt_item              TYPE TABLE OF /qaps/prem_item,
          ls_data              TYPE /qaps/prem_distr,
          lt_data              TYPE TABLE OF /qaps/prem_distr,
          ls_ponto_item        TYPE  /qaps/prem_item,
          ls_ponto_origem      TYPE  /qaps/ponto,
          ls_ponto_destino     TYPE  /qaps/ponto,
          lr_data              TYPE REF TO data,
          lv_material_exists   TYPE abap_bool,
          lv_id_distrib_parent TYPE /qaps/prem_distr-id_distribuicao,
          lv_percentual        TYPE /qaps/percentual.

    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao(
                         )->get_simulacao_by_id( is_premissa-id_simulacao ).

    SELECT SINGLE *
      FROM /qaps/ponto
      WHERE tipo_ponto = @is_transfer-tipo_origem
      AND   id_ponto   = @is_transfer-id_origem
      INTO @ls_ponto_origem.

    IF is_premissa-id_centro = mc_guid_null.
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE tipo_ponto = 'G'
        AND   codigo = @is_transfer-grp_planta
        INTO CORRESPONDING FIELDS OF @ls_ponto_destino.
    ELSE.
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE tipo_ponto = 'W'
        AND   codigo = @is_transfer-werks
        INTO CORRESPONDING FIELDS OF @ls_ponto_destino.
    ENDIF.

    SELECT SINGLE *
      FROM /qaps/prem_hdr
      WHERE id_premissa = @is_premissa-id_premissa
      INTO CORRESPONDING FIELDS OF @ls_header.

    SELECT SINGLE *
      FROM /qaps/prem_item
      WHERE id_premissa = @is_premissa-id_premissa
      AND   id_item     = @is_premissa-id_item
      INTO @ls_ponto_item.

    get_destino_material( EXPORTING is_simulacao = ls_simulacao
                                    is_destino  = ls_ponto_destino
                                    is_material = ls_ponto_item
                          IMPORTING et_destino = DATA(lt_destino)
                                    et_material = DATA(lt_material) ).

    LOOP AT lt_destino INTO DATA(ls_destino).

      CASE ls_ponto_item-tipo_regra.
        WHEN 'AG'.
          TRY.
              DATA(ls_material) = lt_material[ id_premissa = ls_destino-id_premissa
                                               tipo_regra = 'AG'
                                               agregador = ls_ponto_item-agregador ].
            CATCH cx_sy_itab_line_not_found.
              ls_material = add_transfer_material( iv_id_premissa = ls_destino-id_premissa
                                                   iv_tipo_regra  = 'AG'
                                                   iv_agregador   = ls_ponto_item-agregador ).
          ENDTRY.
        WHEN 'MA'.
          TRY.
              ls_material = lt_material[ id_premissa = ls_destino-id_premissa
                                         tipo_regra = 'MA'
                                         matnr = ls_ponto_item-matnr ].
            CATCH cx_sy_itab_line_not_found.
              ls_material = add_transfer_material( iv_id_premissa = ls_destino-id_premissa
                                                   iv_tipo_regra  = 'MA'
                                                   iv_matnr   = ls_ponto_item-matnr ).
          ENDTRY.
      ENDCASE.

      DATA(lt_hierarquia) = get_transfer_hierarquia( ls_material ).

      SELECT SINGLE COUNT( * ) AS qty
        FROM /qaps/prem_distr
        WHERE id_item = @ls_material-id_item
        INTO @DATA(lv_qty).

      IF lv_qty = 0.
        lv_percentual = 100.
      ENDIF.

      LOOP AT lt_hierarquia INTO ls_material.

        DATA(lv_id_distrib) = add_transfer_distrib( is_simulacao  = ls_simulacao
                                                    is_header     = ls_header
                                                    is_material   = ls_material
                                                    is_origem     = ls_ponto_origem
                                                    iv_percentual = lv_percentual
                                                    iv_id_parent  = lv_id_distrib_parent ).

        IF ls_material-tipo_regra = 'AG'.
          lv_id_distrib_parent = lv_id_distrib.
        ELSE.
          CLEAR lv_id_distrib_parent.
        ENDIF.

      ENDLOOP.

    ENDLOOP.



    return = abap_true.

  ENDMETHOD.


  METHOD check_distrib_exists.

    IF is_data-tipo_regra <> 'GE'.
      SELECT *
        FROM /qaps/v_prm_ibas
        WHERE ( ( tipo_regra = 'GP' AND id_grupo_produto = @is_data-id_grupo_produto ) OR
              ( tipo_regra = 'AG'   AND agregador = @is_data-agregador ) OR
              ( tipo_regra = 'MP'   AND mat_planejado = @is_data-mat_planejado ) OR
              ( tipo_regra = 'MA'   AND matnr = @is_data-matnr ) )
        AND id_simulacao = @is_data-id_simulacao
        AND has_destino = 'X'
        AND cod_destino = @is_data-codigo
        INTO TABLE @DATA(lt_origem).
    ELSE.
      SELECT *
      FROM /qaps/v_prm_ibas
      WHERE (  tipo_regra = 'GE' )
      AND id_simulacao = @is_data-id_simulacao
      AND has_destino = 'X'
      AND cod_destino = @is_data-codigo
      INTO TABLE @lt_origem.
    ENDIF.

    SELECT *
      FROM /qaps/v_prm_hier ""/qaps/prem_distr
      FOR ALL ENTRIES IN @lt_origem
      WHERE id_origem = @lt_origem-id_origem
      and   id_item = @is_data-id_item
      AND   id_premissa = @is_data-id_premissa
      AND   tipo_regra  = @is_data-tipo_regra
      INTO TABLE @DATA(lt_distrib).

*    if is_data-tipo_regra = 'MA'.
*      BREAK-POINT.
*    ENDIF.

    IF sy-subrc NE 0.
      return = abap_true.
    ELSE.
      return = abap_false.
*      BREAK-POINT.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mo_logistica = NEW /qaps/cl_mdl_logistica( ).
    mo_input_elem = NEW /qaps/cl_mdl_input_custo_elem( ).
    mo_custo_elem = NEW /qaps/cl_mdl_custo_elementar( ).
    mo_material = NEW /qaps/cl_mdl_material( ).
  ENDMETHOD.


  METHOD convert_cais_to_porto.

    IF mt_cais IS INITIAL.
      mt_cais = mo_logistica->get_cais( ).
    ENDIF.

    DATA(lt_pontos) = mo_logistica->get_pontos( ).


    LOOP AT it_data INTO DATA(ls_data).

      IF ls_data-tipo_origem = 'I'.

        DATA(ls_ponto) = VALUE #( lt_pontos[ id_ponto = ls_data-id_origem ] OPTIONAL ).
        DATA(ls_cais) = VALUE #( mt_cais[ id_cais = ls_ponto-id_externo ] OPTIONAL ).


        IF NOT line_exists( return[ id_origem = ls_cais-id_porto ] ).
          ls_data-id_origem = lt_pontos[ id_externo = ls_cais-id_porto ]-id_ponto.
          ls_data-tipo_origem = 'P'.
          APPEND ls_data TO return.
        ENDIF.
      ELSE.
        APPEND ls_data TO return.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_distribuicao.

    DATA lt_data TYPE TABLE OF /qaps/prem_distr.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

*    BREAK-POINT.
    DATA(ls_header) = get_single_header( is_data-id_premissa ).

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = is_simulacao-periodo_inicial
        final   = is_simulacao-periodo_final    ).

    lr_data = REF #( is_data ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial < ls_per-final.

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

      is_data-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND is_data TO lt_data.

    ENDWHILE.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_distr FROM TABLE lt_data.
      COMMIT WORK.
    ENDIF.


  ENDMETHOD.


  METHOD create_distribuicao_by_matriz.

*    DATA lt_data TYPE TABLE OF /qaps/prem_distr.
*    DATA lr_data TYPE REF TO data.
*    DATA lv_key TYPE /qaps/prem_distr-id_distribuicao.
*
**    "Origem
**    SELECT *
**      FROM /qaps/matriz_dst
**      WHERE id_item = @is_data-id_item_matriz
**      INTO TABLE @DATA(lt_source).
*
*    SELECT *
*      FROM /qaps/v_mt_pr_ds
*      WHERE id_item = @is_data-id_item_matriz
*      INTO TABLE @DATA(lt_porto).
*
*    "Destino
*    SELECT *
*      FROM /qaps/prem_distr
*      WHERE id_item_matriz = @is_data-id_item_matriz
*      INTO TABLE @DATA(lt_target).
*
*    LOOP AT lt_porto INTO DATA(ls_porto).
*
*      IF not line_exists( lt_target[ id_item_matriz = ls_porto-id_item
*                                     id_origem      = ls_porto-id_porto
*                                     periodo        = ls_porto-periodo ] ).
*        CLEAR lv_key.
*        lv_key = VALUE #( lt_data[ id_item_matriz = ls_porto-id_item
*                                   id_origem      = ls_porto-id_porto ]-id_distribuicao
*                                      OPTIONAL ).
*        IF lv_key IS INITIAL.
*          lv_key = cl_system_uuid=>create_uuid_x16_static( ).
*        ENDIF.
*
*        "insere
*        DATA(ls_data) = VALUE /qaps/prem_distr(
*            id_distribuicao = lv_key
*            periodo         = ls_porto-periodo
*            id_premissa     = is_data-id_premissa
*            id_item         = is_data-id_item
*            id_item_matriz  = ls_porto-id_item
*            modalidade      = 'I'
*            tipo_origem     = 'P'
*            id_origem       = ls_porto-id_porto
*            percentual      = ls_porto-percentual ).
*
*        lr_data = REF #( ls_data ).
*        preencher_dados_controle( CHANGING cr_data = lr_data ).
*
*        APPEND ls_data TO lt_data.
*
*      ELSE. "atualiza
*        ls_data = lt_target[ id_item_matriz = ls_porto-id_item
*                             id_origem      = ls_porto-id_porto
*                             periodo        = ls_porto-periodo ].
*
*        ls_data-percentual = ls_porto-percentual.
*        lr_data = REF #( ls_data ).
*        preencher_dados_controle( CHANGING cr_data = lr_data ).
*        append ls_data to lt_data.
*
*      ENDIF.
*
*    ENDLOOP.
*
*    IF lines( lt_data ) > 0.
*      MODIFY /qaps/prem_distr FROM TABLE lt_data.
*      COMMIT WORK.
*    ENDIF.

  ENDMETHOD.


  METHOD create_dynamic_catalog.

    DATA: lt_fcat       TYPE lvc_t_fcat,
          lt_fcat_comp  TYPE lvc_t_fcat,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_VAR_INPUT'
      CHANGING
        ct_fieldcat      = lt_fcat.

    DATA(ls_valor_template)      = lt_fcat[ fieldname = 'VALOR' ].

    CASE is_custo_elementar-tipo_variavel.
      WHEN 'G'."Geral
        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
                        AND  fieldname <> 'TIPO_ORIGEM'
                        AND  fieldname <> 'DSC_ORIGEM'
                        AND  fieldname <> 'TIPO_DESTINO'
                        AND  fieldname <> 'DSC_DESTINO'.

        LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'TIPO_ORIGEM'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_ORIGEM'. <fs_fcat>-col_pos = 5.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 6.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 7.
          ENDCASE.
        ENDLOOP.

      WHEN 'L'."Logística
        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
                        AND  fieldname <> 'DSC_MODAL'
                        AND  fieldname <> 'DSC_CATEGORIA'
                        AND  fieldname <> 'TIPO_ORIGEM'
                        AND  fieldname <> 'DSC_ORIGEM'
                        AND  fieldname <> 'TIPO_DESTINO'
                        AND  fieldname <> 'DSC_DESTINO'.

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'MODAL'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_CATEGORIA'. <fs_fcat>-col_pos = 5.
            WHEN 'TIPO_ORIGEM'. <fs_fcat>-col_pos = 6.
            WHEN 'DSC_ORIGEM'. <fs_fcat>-col_pos = 7.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 8.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 9.
          ENDCASE.
        ENDLOOP.

      WHEN 'P'."Produtiva
        DELETE lt_fcat WHERE fieldname  <> 'TIPO_REGRA'
                       AND  fieldname  <> 'KEY_INPUT'
                       AND  fieldname  <> 'TIPO_DESTINO'
                       AND  fieldname  <> 'DSC_DESTINO'
                       AND  fieldname  <> 'DSC_PROCESSO'.

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 5.
            WHEN 'DSC_PROCESSO'. <fs_fcat>-col_pos = 6.
          ENDCASE.
        ENDLOOP.

    ENDCASE.

    lv_pos = lines( lt_fcat ) + 1.

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial < is_simulacao-periodo_final.

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

      lv_periodo = lv_mes && `/` && lv_ano.

      DATA(ls_val_new)    = ls_valor_template.

      ls_val_new-reptext = ls_val_new-scrtext_s
        = ls_val_new-scrtext_m = ls_val_new-scrtext_l = lv_periodo.
      ls_val_new-parameter0 = 'VALOR'.
      ls_val_new-parameter1 = lv_ano && lv_mes.

*      ls_val_new-fieldname   = ls_val_new-fieldname && `_` && lv_id.
      ls_val_new-fieldname   =  lv_mes && `_` && lv_ano.

      CLEAR: ls_val_new-ref_table.
      ls_val_new-col_pos = lv_pos.

      APPEND: ls_val_new TO lt_fcat.

    ENDWHILE.

    mt_catalog = lt_fcat.
    return = lt_fcat.


  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_comp>  TYPE abap_componentdescr.

    DATA: lr_table      TYPE REF TO data,
          lr_line       TYPE REF TO data,
          lr_sdescr     TYPE REF TO cl_abap_structdescr,
          lr_tdescr     TYPE REF TO cl_abap_tabledescr,
          lt_components TYPE abap_component_tab,
          lt_style      TYPE lvc_t_styl,
          lt_color      TYPE lvc_t_scol.

    CLEAR mt_data.

    cl_alv_table_create=>create_dynamic_table(
      EXPORTING
        it_fieldcatalog = it_fcat
      IMPORTING
        ep_table        = mt_data "lr_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2 ).

    lr_tdescr  ?= cl_abap_tabledescr=>describe_by_data_ref( mt_data ).
    lr_sdescr ?= lr_tdescr->get_table_line_type( ).

    CREATE DATA ms_data TYPE HANDLE lr_sdescr.


  ENDMETHOD.


  METHOD create_input.

    DATA: ls_data       TYPE /qaps/s_var_input,
          ls_entry      TYPE /qaps/var_input,
          lt_entry      TYPE TABLE OF /qaps/var_input,
          ls_message    TYPE bapiret2,
          lr_data       TYPE REF TO data,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.

    CALL FUNCTION '/QAPS/FM_VAR_ELEM_INPUT'
      EXPORTING
        iv_action          = 'C'
        is_simulacao       = is_simulacao
        is_custo_elementar = is_data
      IMPORTING
        es_data            = ls_data
        es_message         = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_var_input = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial < ms_periodo-final.

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

      ls_entry-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_entry TO lt_entry.

    ENDWHILE.

*    MODIFY /qaps/var_input FROM TABLE lt_entry.

    return = abap_true.

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
        percentual             = iv_percentual ).

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


  METHOD criar_premissa.

*    DATA lv_modalidade TYPE /qaps/ed_modalidade.
*
*    criar_premissa_header( is_simulacao = is_simulacao ).
*
*    criar_premissa_item( is_simulacao = is_simulacao ).
*
*    criar_premissa_distribuicao( is_simulacao = is_simulacao ).
*
*    criar_premissa_trajeto( is_simulacao = is_simulacao ).

  ENDMETHOD.


  METHOD criar_premissa_distribuicao.

    DATA: lv_key_input    TYPE /qaps/key_input,
          lt_input_s_dest TYPE /qaps/t_inp_mat,
          lt_input_c_dest TYPE /qaps/t_inp_mat,
          lv_modalidade   TYPE /qaps/ed_modalidade,
          lv_where        TYPE string.

    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_prm_hdr
        WHERE id_simulacao = @is_simulacao-id_simulacao
        INTO TABLE @DATA(lt_header).

    LOOP AT lt_header INTO DATA(ls_header).

      DO 3 TIMES.

        CASE sy-index.
          WHEN 1. lv_modalidade = 'I'.
          WHEN 2. lv_modalidade = 'N'.
          WHEN 3. lv_modalidade = 'P' .
        ENDCASE.

        IF lv_modalidade = 'N' OR lv_modalidade = 'I'.

          criar_premissa_distr_by_dest( is_simulacao = is_simulacao
                                        is_destino = ls_header
                                        iv_modalidade = lv_modalidade ).
        ELSEIF lv_modalidade = 'P'.

          criar_premissa_distr_std_prd( is_simulacao = is_simulacao
                                        is_destino = ls_header
                                        iv_modalidade = 'P' ).
        ENDIF.

      ENDDO.



    ENDLOOP.

  ENDMETHOD.


  METHOD criar_premissa_distr_by_dest.

    DATA: lt_distrib    TYPE /qaps/t_prem_distr.


    SELECT *
      FROM /qaps/v_prm_dbas
      WHERE id_premissa  = @is_destino-id_premissa
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_premissa  = @is_destino-id_premissa
      INTO TABLE @lt_distrib.

    LOOP AT lt_item INTO DATA(ls_item).

      criar_premissa_distr_c_dest( is_simulacao    = is_simulacao
                                   is_item         = ls_item
                                   it_distribuicao = lt_distrib
                                   iv_modalidade   = iv_modalidade  ).

      criar_premissa_distr_s_dest( is_simulacao    = is_simulacao
                                   is_item         = ls_item
                                   it_distribuicao = lt_distrib
                                   iv_modalidade   = iv_modalidade  ).

    ENDLOOP.


  ENDMETHOD.


  METHOD criar_premissa_distr_c_dest.

    DATA: lt_distrib TYPE /qaps/t_prem_distr,
          lv_where   TYPE string,
          lv_full    TYPE abap_bool.

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_premissa = @is_item-id_premissa
      AND   id_item     = @is_item-id_item
      AND   modalidade = @iv_modalidade
      INTO TABLE @lt_distrib.

    CASE iv_modalidade.
      WHEN 'I'. lv_where = `( IMPORTACAO = 'X' )`.
      WHEN 'N'. lv_where = `( NACIONAL = 'X' )`.
    ENDCASE.

    SELECT DISTINCT id_simulacao, importacao,nacional,tipo_origem,id_origem,codigo_origem
      FROM /qaps/v_var_full
      WHERE id_simulacao = @is_item-id_simulacao
      AND (lv_where)
      AND has_destino = 'X'
      AND  (
            ( tipo_regra = 'GP' AND id_grupo_produto = @is_item-id_grupo_produto ) OR
            ( tipo_regra = 'AG' AND agregador = @is_item-agregador ) OR
            ( tipo_regra = 'MP' AND mat_planejado = @is_item-mat_planejado ) OR
            ( tipo_regra = 'MA' AND matnr = @is_item-matnr ) )
      INTO TABLE @DATA(lt_var_input).

    CHECK lines( lt_var_input ) > 0.

    IF lines( lt_var_input ) = 1.
      lv_full = abap_true.
    ENDIF.

    LOOP AT lt_var_input INTO DATA(ls_var_input).

      DATA(ls_data) = VALUE /qaps/prem_distr(
          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
          id_premissa     = is_item-id_premissa
          id_item         = is_item-id_item
          modalidade      = iv_modalidade
          tipo_origem     = ls_var_input-tipo_origem
          id_origem       = ls_var_input-id_origem ).
*          percentual      = COND #( WHEN lv_full = abap_true THEN 100 ELSE 0 ) ).

      create_distribuicao( is_simulacao = is_simulacao
                           is_data      = ls_data ).

    ENDLOOP.

  ENDMETHOD.


  METHOD CRIAR_PREMISSA_DISTR_STD_PRD.

    SELECT *
       FROM /qaps/v_prm_hdr
       INTO TABLE @DATA(lt_header).

    SELECT *
      FROM /qaps/simulacao
      WHERE id_simulacao = @is_simulacao-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

    SELECT *
      FROM /qaps/prem_item
      WHERE id_premissa  = @is_destino-id_premissa
      AND  premissa_rule = '3'
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_item
      WHERE id_item = @lt_item-id_item
      AND   id_premissa = @lt_item-id_premissa
      AND   modalidade = 'P'
      INTO TABLE @DATA(lt_distrib).

    select *
      from /qaps/v_ponto
      into TABLE @data(lt_ponto).

     LOOP AT lt_item INTO DATA(ls_item).

      CHECK NOT line_exists( lt_distrib[ id_premissa = ls_item-id_premissa
                                         id_item       = ls_item-id_item ] ).

      DATA(ls_origem) = lt_header[ id_premissa = ls_item-id_premissa ].
*      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_origem-id_simulacao ].

      DATA(ls_data) = VALUE /qaps/prem_distr(
          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
          id_premissa     = ls_item-id_premissa
          id_item         = ls_item-id_item
          modalidade      = 'P'
          tipo_origem     = COND #( WHEN ls_origem-werks IS INITIAL THEN 'G' ELSE 'W' )
          id_origem       = COND #( WHEN ls_origem-werks IS INITIAL
                                    THEN lt_ponto[ id_externo = ls_origem-id_grp_planta ]-id_ponto
                                    ELSE lt_ponto[ id_externo = ls_origem-id_centro ]-id_ponto )

      ).

      create_distribuicao( is_simulacao = is_simulacao
                           is_data      = ls_data ).


    ENDLOOP.


  ENDMETHOD.


  METHOD criar_premissa_distr_s_dest.

    DATA: lt_distrib       TYPE /qaps/t_prem_distr,
          lt_var_input     TYPE /qaps/t_var_full,
          lt_var_input_aux TYPE /qaps/t_var_full,
          lr_origem        TYPE RANGE OF guid16,
          lv_where         TYPE string,
          lv_full          TYPE abap_bool.

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_premissa = @is_item-id_premissa
      AND   id_item     = @is_item-id_item
      AND   modalidade = @iv_modalidade
      INTO TABLE @lt_distrib.

    SELECT DISTINCT id_matriz_abast,id_grp_planta,cod_grp_planta,dsc_grp_planta,id_centro,
           werks,id_item,tipo_regra,matnr,id_grupo_produto,agregador,mat_planejado,
           id_distribuicao,modalidade,tipo_origem,id_origem,codigo,cod_porto
      FROM /qaps/v_mtz_full
      WHERE id_grp_planta = @is_item-id_grp_planta
      AND   id_centro     = @is_item-id_centro
      INTO TABLE @DATA(lt_matriz).

    CASE iv_modalidade.
      WHEN 'I'. lv_where = `( IMPORTACAO = 'X' )`.
      WHEN 'N'. lv_where = `( NACIONAL = 'X' )`.
    ENDCASE.

    IF iv_modalidade = 'I'.

      SELECT DISTINCT id_simulacao, importacao,nacional,tipo_origem,id_origem,codigo_origem
        FROM /qaps/v_var_full
        WHERE id_simulacao = @is_item-id_simulacao
        AND (lv_where)
        AND has_destino = ''
        AND  (
              ( tipo_regra = 'GP' AND id_grupo_produto = @is_item-id_grupo_produto ) OR
              ( tipo_regra = 'AG' AND agregador = @is_item-agregador ) OR
              ( tipo_regra = 'MP' AND mat_planejado = @is_item-mat_planejado ) OR
              ( tipo_regra = 'MA' AND matnr = @is_item-matnr ) )
        INTO TABLE @lt_var_input_aux.

      LOOP AT lt_var_input_aux INTO DATA(ls_var_input_aux).
        CASE ls_var_input_aux-tipo_origem.
          WHEN 'I'.

            IF line_exists( lt_matriz[ codigo = ls_var_input_aux-codigo_origem ] ).
              APPEND ls_var_input_aux TO lt_var_input.
            ENDIF.

          WHEN 'P'.

            IF line_exists( lt_matriz[ codigo = ls_var_input_aux-codigo_origem ] ).
              APPEND ls_var_input_aux TO lt_var_input.
            ENDIF.
          WHEN OTHERS.
            APPEND ls_var_input_aux TO lt_var_input.
        ENDCASE.
      ENDLOOP.

    ELSE.
      SELECT DISTINCT id_simulacao, importacao,nacional,tipo_origem,id_origem,codigo_origem
      FROM /qaps/v_var_full
      WHERE id_simulacao = @is_item-id_simulacao
      AND (lv_where)
      AND has_destino = ''
      AND  (
            ( tipo_regra = 'GP' AND id_grupo_produto = @is_item-id_grupo_produto ) OR
            ( tipo_regra = 'AG' AND agregador = @is_item-agregador ) OR
            ( tipo_regra = 'MP' AND mat_planejado = @is_item-mat_planejado ) OR
            ( tipo_regra = 'MA' AND matnr = @is_item-matnr ) )
      INTO TABLE @lt_var_input.
    ENDIF.

    CHECK lines( lt_var_input ) > 0.

    IF lines( lt_var_input ) = 1.
      lv_full = abap_true.
    ENDIF.

*    IF iv_modalidade = 'I' AND line_exists( lt_var_input[ tipo_origem = 'I' ] ).
**      BREAK c060863.
*      SELECT SINGLE *
*        FROM /qaps/prem_hdr
*        WHERE id_premissa = @is_item-id_premissa
*        INTO @DATA(ls_prem_header).
*
*      IF is_item-id_centro = mc_guid_null.
*        SELECT DISTINCT id_origem,codigo, cod_porto
*          FROM /qaps/v_mtz_full
*          WHERE id_grp_planta = @is_item-id_grp_planta
*          AND tipo_origem = 'I'
*          INTO TABLE @DATA(lt_matriz).
*      ELSE.
*        SELECT DISTINCT id_origem,codigo, cod_porto
*        FROM /qaps/v_mtz_full
*        WHERE id_grp_planta = @is_item-id_grp_planta
*          AND id_centro     = @is_item-id_centro
*          AND tipo_origem = 'I'
*        INTO TABLE @lt_matriz.
*      ENDIF.
*
*      lr_origem = VALUE #( FOR wa IN lt_matriz ( sign = 'I' option = 'EQ' low = wa-id_origem ) ).
*
*      IF lines( lr_origem ) > 0.
*        DELETE lt_var_input WHERE tipo_origem = 'I' AND NOT id_origem IN lr_origem.
*      ENDIF.
*
*      lt_var_input = convert_cais_to_porto( lt_var_input ).
*    ENDIF.

    LOOP AT lt_var_input INTO DATA(ls_var_input).

      DATA(ls_data) = VALUE /qaps/prem_distr(
          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
          id_premissa     = is_item-id_premissa
          id_item         = is_item-id_item
          modalidade      = iv_modalidade
          tipo_origem     = ls_var_input-tipo_origem
          id_origem       = ls_var_input-id_origem
          percentual      = COND #( WHEN lv_full = abap_true THEN 100 ELSE 0 ) ).

      create_distribuicao( is_simulacao = is_simulacao
                           is_data      = ls_data ).

    ENDLOOP.

  ENDMETHOD.


  METHOD criar_premissa_header.

*    DATA: lr_data  TYPE REF TO data,
*          lr_ponto TYPE RANGE OF /qaps/id_ponto.
*
*    DATA lt_premissa_header TYPE TABLE OF /qaps/prem_hdr.
*
*    SELECT *
*      FROM /qaps/v_prm_dest
*      INTO TABLE @DATA(lt_destino).
*
*    SELECT *
*      FROM /qaps/prem_hdr
*      INTO TABLE @DATA(lt_header).
*
*    SELECT *
*      FROM /qaps/matriz_hdr
*      INTO TABLE @DATA(lt_matriz_hdr).
*
*
*    LOOP AT lt_destino INTO DATA(ls_destino).
*
*      CASE ls_destino-tipo.
*        WHEN 'C'.
*          "Grp Planta
*          IF NOT line_exists( lt_header[ id_simulacao     = is_simulacao-id_simulacao
*                                         id_grp_planta = ''
*                                         id_centro     = ls_destino-id_centro ] )
*            AND NOT line_exists( lt_premissa_header[ id_simulacao = is_simulacao-id_simulacao
*                                                     id_grp_planta = ''
*                                                     id_centro    = ls_destino-id_centro ] ).
*
*            data(ls_matriz_hdr) = value #( lt_matriz_hdr[ id_grp_planta = ls_destino-id_grp_planta
*                                                          id_centro     = ls_destino-id_centro ] OPTIONAL ).
*
*            APPEND VALUE /qaps/prem_hdr(
*                id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                id_simulacao     = is_simulacao-id_simulacao
*                id_centro        = ls_destino-id_centro
*                id_matriz_abast  = ls_matriz_hdr-id_matriz_abast ) TO lt_premissa_header.
*          ENDIF.
*        WHEN 'G'.
*
*          "Grp Planta
*          IF NOT line_exists( lt_header[ id_simulacao     = is_simulacao-id_simulacao
*                                            id_grp_planta = ls_destino-id_grp_planta
*                                            id_centro     = mc_guid_null ] )
*            AND NOT line_exists( lt_premissa_header[ id_simulacao = is_simulacao-id_simulacao
*                                                   id_grp_planta = ls_destino-id_grp_planta
*                                                   id_centro    = mc_guid_null ] ).
*
*            ls_matriz_hdr = value #( lt_matriz_hdr[ id_grp_planta = ls_destino-id_grp_planta
*                                                    id_centro     = mc_guid_null ] OPTIONAL ).
*
*            APPEND VALUE /qaps/prem_hdr(
*                id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                id_simulacao     = is_simulacao-id_simulacao
*                id_grp_planta    = ls_destino-id_grp_planta
*                id_matriz_abast  = ls_matriz_hdr-id_matriz_abast ) TO lt_premissa_header.
*          ENDIF.
*
*          "Grp Planta e/ou Centro
*          IF NOT line_exists( lt_header[ id_simulacao  = is_simulacao-id_simulacao
*                                            id_grp_planta = ls_destino-id_grp_planta
*                                            id_centro     = ls_destino-id_centro ] )
*            AND NOT line_exists( lt_premissa_header[ id_simulacao  = is_simulacao-id_simulacao
*                                                   id_grp_planta = ls_destino-id_grp_planta
*                                                   id_centro     = ls_destino-id_centro ] ).
*
*            ls_matriz_hdr = value #( lt_matriz_hdr[ id_grp_planta = ls_destino-id_grp_planta
*                                                    id_centro     = ls_destino-id_centro ] OPTIONAL ).
*
*            APPEND VALUE /qaps/prem_hdr(
*                id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                id_simulacao     = is_simulacao-id_simulacao
*                id_grp_planta    = ls_destino-id_grp_planta
*                id_centro        = ls_destino-id_centro
*                id_matriz_abast  = ls_matriz_hdr-id_matriz_abast ) TO lt_premissa_header.
*
*          ENDIF.
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    IF lines( lt_premissa_header ) > 0.
*      LOOP AT lt_premissa_header REFERENCE INTO lr_data.
*        preencher_dados_controle( CHANGING cr_data = lr_data ).
*      ENDLOOP.
*
*      MODIFY /qaps/prem_hdr FROM TABLE lt_premissa_header.
*      COMMIT WORK AND WAIT.
*
*    ENDIF.

  ENDMETHOD.


  METHOD criar_premissa_item.

    criar_premissa_item_importacao( is_simulacao ).

    criar_premissa_item_nacional( is_simulacao ).

*    criar_premissa_item_importado( ).

  ENDMETHOD.


  METHOD criar_premissa_item_c_dest.

    DATA lt_item TYPE /qaps/t_prm_dbas.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/v_prm_dbas
      WHERE id_premissa  = @is_header-id_premissa
      INTO TABLE @lt_item.

*    BREAK c060863.
    LOOP AT it_data INTO DATA(ls_data).

      CASE ls_data-tipo_destino.
        WHEN 'G'.
          CHECK ls_data-codigo_grp_planta = is_header-codigo.
        WHEN 'W'.
          CHECK ls_data-id_centro = is_header-id_centro.
      ENDCASE.

      CHECK material_exists( is_header  = is_header
                             it_item    = lt_item
                             it_buffer  = lt_data
                             is_data    = ls_data ) = abap_false.

      DATA(ls_entry) =  VALUE /qaps/prem_item( id_item            = cl_system_uuid=>create_uuid_x16_static( )
                                                 id_premissa      = is_header-id_premissa
                                                 tipo_regra       = ls_data-tipo_regra
                                                 matnr            = ls_data-matnr
                                                 id_grupo_produto = ls_data-id_grupo_produto
                                                 agregador        = ls_data-agregador
                                                 mat_planejado    = ls_data-mat_planejado
                                                 premissa_rule    = '1' ).

      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_entry TO lt_data.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD criar_premissa_item_importacao.


    criar_premissa_item_importado( ).

  ENDMETHOD.


  METHOD criar_premissa_item_importado.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/prm_matriz
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      INTO TABLE @DATA(lt_item_prm).

    SELECT *
      FROM /qaps/v_m_hd_itm
      FOR ALL ENTRIES IN @lt_header
      WHERE id_matriz_abast  = @lt_header-id_matriz_abast
      INTO TABLE @DATA(lt_item_mat).

*    BREAK c060863.
    LOOP AT lt_header INTO DATA(ls_header).

      DATA(lt_src) = lt_item_mat.
      DELETE lt_src WHERE id_matriz_abast <> ls_header-id_matriz_abast.

      LOOP AT lt_src INTO DATA(ls_src).

        IF NOT line_exists( lt_item_prm[ tipo_regra = ls_src-tipo_regra
                                         matnr = ls_src-matnr
                                         id_grupo_produto = ls_src-id_grupo_produto
                                         agregador = ls_src-agregador
                                         mat_planejado = ls_src-mat_planejado ] )
          AND NOT line_exists( lt_data[ tipo_regra = ls_src-tipo_regra
                                         matnr = ls_src-matnr
                                         id_grupo_produto = ls_src-id_grupo_produto
                                         agregador = ls_src-agregador
                                         mat_planejado = ls_src-mat_planejado ] ).

          DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                  id_premissa      = ls_header-id_premissa
                                                  tipo_regra       = ls_src-tipo_regra
                                                  matnr            = ls_src-matnr
                                                  id_grupo_produto = ls_src-id_grupo_produto
                                                  agregador        = ls_src-agregador
                                                  mat_planejado    = ls_src-mat_planejado
                                                  id_item_matriz   = ls_src-id_item
                                                  premissa_rule    = '4').

          lr_data = REF #( ls_data ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).
          APPEND ls_data TO lt_data.

        ELSE.

          ls_data = VALUE #( lt_item_prm[ tipo_regra = ls_src-tipo_regra
                              matnr = ls_src-matnr
                              id_grupo_produto = ls_src-id_grupo_produto
                              agregador = ls_src-agregador
                              mat_planejado = ls_src-mat_planejado ] OPTIONAL ).

          IF ls_data IS INITIAL.
            ls_data = lt_data[ tipo_regra = ls_src-tipo_regra
                              matnr = ls_src-matnr
                              id_grupo_produto = ls_src-id_grupo_produto
                              agregador = ls_src-agregador
                              mat_planejado = ls_src-mat_planejado ].
          ENDIF.


          ls_data-id_item_matriz   = ls_src-id_item.
          APPEND ls_data TO lt_data.

        ENDIF.


      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD CRIAR_PREMISSA_ITEM_NACIONAL.

    DATA: lv_key_input    TYPE /qaps/key_input,
          lt_input_s_dest TYPE /qaps/t_inp_mat,
          lt_input_c_dest TYPE /qaps/t_inp_mat,
          lv_where        TYPE string.

    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_prm_hdr
        WHERE id_simulacao = @is_simulacao-id_simulacao
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_inp_mat
      WHERE ( nacional = 'X' or importacao = 'X' )
      AND   id_destino <> @mc_guid_null
      AND tipo_destino IN ( 'W','G' )
      AND tipo_variavel = 'G'
      INTO TABLE @lt_input_c_dest.

    SELECT *
      FROM /qaps/v_inp_mat
      WHERE ( nacional = 'X' or importacao = 'X' )
      AND   id_destino = @mc_guid_null
      AND tipo_variavel = 'G'
      INTO TABLE @lt_input_s_dest.

    LOOP AT lt_header INTO DATA(ls_header).

      criar_premissa_item_c_dest( is_header = ls_header
                                  is_simulacao  = is_simulacao
                                  it_data       = lt_input_c_dest ).

      criar_premissa_item_s_dest( is_header = ls_header
                                  is_simulacao  = is_simulacao
                                  it_data       = lt_input_s_dest ).



      criar_premissa_item_std_prd( is_header = ls_header
                                   is_simulacao  = is_simulacao
                                   it_data       = lt_input_s_dest ).

    ENDLOOP.

    criar_premissa_item_importado( ).

  ENDMETHOD.


  METHOD criar_premissa_item_std_prd.

    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data,
          lv_matnr type matnr.

*    SELECT *
*        FROM /qaps/v_prm_hdr
*        INTO TABLE @DATA(lt_header).
*
*    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
*      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @is_header-id_premissa
      AND  premissa_rule = '3'
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_sim_stdp
*      FOR ALL ENTRIES IN @lt_header
      WHERE id_simulacao = @is_header-id_simulacao
      INTO TABLE @DATA(lt_std_producao).

*    LOOP AT lt_header INTO DATA(is_header).

    DATA(lt_filtered_input) = lt_std_producao.

    IF is_header-id_centro = mc_guid_null.
      DELETE lt_filtered_input WHERE id_grp_planta <> is_header-id_grp_planta.
*                                      OR id_centro <> is_header-id_centro.
    ELSE.
      DELETE lt_filtered_input WHERE id_grp_planta <> is_header-id_grp_planta
                                  OR id_centro <> is_header-id_centro.
    ENDIF.

    LOOP AT lt_filtered_input INTO DATA(ls_filtered_input).

      lv_matnr = |{ ls_filtered_input-matnr ALPHA = IN WIDTH = 18 }|.

      CHECK NOT line_exists( lt_item[ id_premissa  = is_header-id_premissa
                                      tipo_regra       = 'MA'
                                      matnr            = lv_matnr ] )
        AND NOT line_exists( lt_data[ id_premissa  = is_header-id_premissa
                                      tipo_regra       = 'MA'
                                      matnr            = lv_matnr  ] ).

      DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                              id_premissa  = is_header-id_premissa
                                              tipo_regra       = 'MA'
                                              matnr            = |{ ls_filtered_input-matnr ALPHA = IN WIDTH = 18 }|
                                              premissa_rule    = '3' ).

      lr_data = REF #( ls_data ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_data TO lt_data.

    ENDLOOP.

*    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.


  METHOD criar_premissa_item_s_dest.

    DATA lt_item TYPE /qaps/t_prm_dbas.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/v_prm_dbas
      WHERE id_premissa  = @is_header-id_premissa
      INTO TABLE @lt_item.


    LOOP AT it_data INTO DATA(ls_data).

      CHECK material_exists( is_header  = is_header
                             it_item    = lt_item
                             it_buffer  = lt_data
                             is_data    = ls_data ) = abap_false.

      DATA(ls_entry) =  VALUE /qaps/prem_item( id_item            = cl_system_uuid=>create_uuid_x16_static( )
                                               id_premissa      = is_header-id_premissa
                                               tipo_regra       = ls_data-tipo_regra
                                               matnr            = ls_data-matnr
                                               id_grupo_produto = ls_data-id_grupo_produto
                                               agregador        = ls_data-agregador
                                               mat_planejado    = ls_data-mat_planejado
                                               premissa_rule    = '9' ).

      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_entry TO lt_data.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD CRIAR_PREMISSA_TRAJETO.

    "Nacional/Transferência

    SELECT *
      FROM /qaps/v_prm_all
      WHERE modalidade = 'N' or modalidade = 'T' or modalidade = 'I'
      INTO TABLE @data(lt_distrib).

    LOOP AT lt_distrib INTO data(ls_distrib).
      execute_trajeto( ls_distrib ).
    ENDLOOP.

*    DATA: lv_key_input    TYPE /qaps/key_input,
*          lt_input_s_dest TYPE /qaps/t_inp_mat,
*          lt_input_c_dest TYPE /qaps/t_inp_mat,
*          lv_modalidade   TYPE /qaps/ed_modalidade,
*          lv_where        TYPE string.
*
*    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
*          lr_data TYPE REF TO data.
*
*    SELECT *
*        FROM /qaps/v_prm_hdr
*        WHERE id_simulacao = @is_simulacao-id_simulacao
*        INTO TABLE @DATA(lt_header).
*
*    LOOP AT lt_header INTO DATA(ls_header).
*
*      DO 3 TIMES.
*
*        CASE sy-index.
*          WHEN 1. lv_modalidade = 'I'.
*          WHEN 2. lv_modalidade = 'N'.
*          WHEN 3. lv_modalidade = 'P' .
*        ENDCASE.
*
*        IF lv_modalidade = 'N' OR lv_modalidade = 'I'.
*
*          criar_premissa_distr_by_dest( is_simulacao = is_simulacao
*                                        is_destino = ls_header
*                                        iv_modalidade = lv_modalidade ).
*        ELSEIF lv_modalidade = 'P'.
*
*          criar_premissa_distr_std_prd( is_simulacao = is_simulacao
*                                        is_destino = ls_header
*                                        iv_modalidade = 'P' ).
*        ENDIF.
*
*      ENDDO.
*
*
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD delete_input.

    DATA: lv_message TYPE string,
          lr_modalidade  type range of /qaps/prem_distr-modalidade,
          lr_id_distribuicao type range of /qaps/prem_distr-id_distribuicao
          .

    lr_modalidade = VALUE #( ( sign = 'I' option = 'EQ' low = 'I' )
                             ( sign = 'I' option = 'EQ' low = 'N' )
                             ( sign = 'I' option = 'EQ' low = 'S' ) ).

    SELECT DISTINCT modalidade
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_data
      WHERE id_distribuicao = @it_data-id_distribuicao
      AND modalidade IN @lr_modalidade
      INTO TABLE @DATA(lt_modalidade).

    IF lines( lt_modalidade ) > 0.
      MESSAGE 'Apenas transferências podem ser excluídas' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
      RETURN.
    ENDIF.

    lv_message = 'Deseja excluir as Transferências selecionadas?'.

    IF question( iv_message = lv_message ).

      lr_id_distribuicao = VALUE #( FOR wa IN it_data
                                  ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).

      SELECT id_distribuicao
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_data
      WHERE id_parent = @it_data-id_distribuicao
      AND modalidade = 'T'
      into TABLE @data(lt_children).

      loop at lt_children INTO data(ls_children).
        append value #( sign = 'I' option = 'EQ' low = ls_children-id_distribuicao ) to lr_id_distribuicao.
      ENDLOOP.

      SORT lr_id_distribuicao BY low.
      DELETE ADJACENT DUPLICATES FROM lr_id_distribuicao COMPARING low.

      break c060863.
      delete_transferencias( ir_id_distribuicao = lr_id_distribuicao
                             is_simulacao = is_simulacao ).
      return = abap_true.

    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD delete_transferencias.

    DATA: lr_id_premissa     TYPE RANGE OF /qaps/prem_hdr-id_premissa,
          lr_id_item         TYPE RANGE OF /qaps/prem_item-id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/prem_distr-id_distribuicao.

    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_distribuicao IN @ir_id_distribuicao
      AND   modalidade = 'T'
      and   id_simulacao  = @is_simulacao-id_simulacao
      INTO TABLE @DATA(lt_transfer).

    "Destinos
    SELECT id_simulacao, id_premissa, id_grp_planta, id_centro
      FROM /qaps/prem_hdr
      FOR ALL ENTRIES IN @lt_transfer
      WHERE id_grp_planta = @lt_transfer-id_grp_planta
      and   id_simulacao  = @is_simulacao-id_simulacao
      INTO TABLE @DATA(lt_destino).

    lr_id_premissa = VALUE #( FOR wa_destino IN lt_destino
                              ( sign = 'I' option  = 'EQ' low = wa_destino-id_premissa ) ).

    "Materiais
    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_transfer
      WHERE ( ( tipo_regra = 'AG' AND agregador = @lt_transfer-agregador )
              OR ( tipo_regra = 'MA' AND matnr = @lt_transfer-matnr ) )
      AND id_premissa IN @lr_id_premissa
      INTO TABLE @DATA(lt_material).

    SELECT *
    FROM /qaps/prem_item
    FOR ALL ENTRIES IN @lt_material
    WHERE id_parent =  @lt_material-id_item
    AND id_premissa IN @lr_id_premissa
    APPENDING TABLE @lt_material.

    lr_id_item = VALUE #( FOR wa_material IN lt_material
                              ( sign = 'I' option  = 'EQ' low = wa_material-id_item ) ).

    "selecionar distribuição
    SELECT DISTINCT id_distribuicao
      FROM /qaps/prem_distr
      WHERE id_item IN @lr_id_item
      AND modalidade = 'T'
      INTO TABLE @DATA(lt_distribuicao).

    lr_id_distribuicao = VALUE #( FOR wa_distrib IN lt_distribuicao
                            ( sign = 'I' option  = 'EQ' low = wa_distrib-id_distribuicao ) ).

    "delete trajetos
    DELETE FROM /qaps/prem_traj WHERE id_distribuicao IN lr_id_distribuicao.

    "Delete distribuição
    DELETE FROM /qaps/prem_distr WHERE id_distribuicao IN lr_id_distribuicao.
    COMMIT WORK AND WAIT.

*    "Items
    SELECT item~id_item, COUNT(*) AS qty
      FROM /qaps/prem_item AS item
      INNER JOIN /qaps/prem_distr AS distrib
      ON item~id_item = distrib~id_item
      WHERE item~id_item IN @lr_id_item
      GROUP BY item~id_item
      INTO TABLE @DATA(lt_item_delete).

    IF lines( lt_item_delete ) = 0.
      DELETE FROM /qaps/prem_item WHERE id_item IN @lr_id_item.
      COMMIT WORK AND WAIT.
    ELSE.
      LOOP AT lt_item_delete INTO DATA(ls_item_delete).
        CHECK ls_item_delete-qty = 0.
        DELETE FROM /qaps/prem_item WHERE id_item = ls_item_delete-id_item.
      ENDLOOP.
      COMMIT WORK AND WAIT.
    ENDIF.
*
    SELECT hdr~id_premissa, COUNT(*) AS qty
      FROM /qaps/prem_hdr as hdr
      inner join /qaps/prem_item as item
      on hdr~id_premissa = item~id_premissa
      WHERE hdr~id_premissa IN @lr_id_premissa
      and   id_simulacao  = @ms_simulacao-id_simulacao
      GROUP BY hdr~id_premissa
      INTO TABLE @DATA(lt_destino_delete).

    IF lines( lt_destino_delete ) = 0.
      DELETE FROM /qaps/prem_hdr WHERE id_premissa IN @lr_id_premissa
                                 and   id_simulacao  = @ms_simulacao-id_simulacao.
      COMMIT WORK AND WAIT.
    ELSE.
      LOOP AT lt_destino_delete INTO DATA(ls_destino_delete).
        CHECK ls_destino_delete-qty = 0.
        DELETE FROM /qaps/prem_hdr WHERE id_premissa  = @ls_destino_delete-id_premissa
                                   and   id_simulacao = @ms_simulacao-id_simulacao.
      ENDLOOP.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD execute_distribuicao.

    data lv_where_dest type string.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_origem-id_origem
*      and tipo_ponto = 'F'
      INTO @DATA(ls_origem).

*   check sy-subrc is INITIAL.

*   lv_where_dest = `( cod_porto = '` && ls_origem-codigo && `' )` .
*
**    case ls_origem-tipo_ponto.
**      when 'C'.
**        lv_where_dest = `( cod_porto = '` && ls_origem-codigo && `' )`   .
**    ENDCASE.
**    lv_where_dest = `( 1 = 1 ) `.
*    IF ls_origem-tipo_ponto = 'P'.
*      lv_where_dest = `( cod_porto = '` && ls_origem-codigo && `' )`   .
*    ELSEIF ls_origem-tipo_ponto = 'I'.
*      lv_where_dest = `( cod_cais = '` && ls_origem-codigo && `' AND cod_porto = '` && ls_origem-cod_porto && `' )`   .
*    ENDIF.
*
*    IF is_destino-id_grp_planta <> mc_guid_null AND is_destino-id_centro <> mc_guid_null.
*      lv_where_dest = lv_where_dest && ` AND ( id_grp_planta = '` && is_destino-id_grp_planta && `'` &&
*                      ` AND id_centro = '` && is_destino-id_centro && `' )`.
*    ELSEIF is_destino-id_grp_planta <> mc_guid_null AND is_destino-id_centro = mc_guid_null.
*      lv_where_dest = lv_where_dest && ` AND ( id_grp_planta = '` && is_destino-id_grp_planta && `' )`.
*    ELSEIF is_destino-id_grp_planta = mc_guid_null AND is_destino-id_centro <> mc_guid_null.
*      lv_where_dest = lv_where_dest && ` AND ( id_centro = '` && is_destino-id_centro && `' )`.
*    ENDIF.
*
*    SELECT *
*      FROM /qaps/v_prm_od
*      WHERE ativo = 'X'
*      AND  (lv_where_dest)
*      INTO TABLE @DATA(lt_origem_destino).
*
*    CHECK lines( lt_origem_destino ) > 0.

    DATA(ls_data) = VALUE /qaps/prem_distr(
        id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
        id_premissa     = is_destino-id_premissa
        id_item         = is_destino-id_item
        modalidade      = cond #( when is_origem-nacional = 'X' then 'N'
                                  when is_origem-importacao = 'X' then 'I' )
        tipo_origem     = is_origem-tipo_origem
        id_origem       = is_origem-id_origem
*        percentual      = COND #( WHEN iv_full = abap_true THEN 100 ELSE 0 )
    ).

    create_distribuicao( is_simulacao = is_simulacao
                         is_data      = ls_data ).

  ENDMETHOD.


  METHOD execute_trajeto.

    DATA(lt_trajeto) = get_trajeto( is_data ).

    CHECK lines( lt_trajeto ) > 0.

    LOOP AT lt_trajeto INTO DATA(ls_trajeto).

*      CHECK NOT line_exists( lt_check[ id_distribuicao        = ls_dst_matriz-id_distribuicao
*                                       id_distribuicao_matriz = ls_dst_matriz-id_distribuicao ] ).

      create_trajeto( is_trajeto        = ls_trajeto
                      is_origem_destino = is_data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD execute_trajeto_transfer.

    DATA(lt_trajeto) = get_trajeto_transfer( is_destino = is_destino
                                             is_data   = is_data ).

    CHECK lines( lt_trajeto ) > 0.

    data(lv_lines) = lines( lt_trajeto ).

    LOOP AT lt_trajeto INTO DATA(ls_trajeto).

*      CHECK NOT line_exists( lt_check[ id_distribuicao        = ls_dst_matriz-id_distribuicao
*                                       id_distribuicao_matriz = ls_dst_matriz-id_distribuicao ] ).

      create_trajeto( is_trajeto        = ls_trajeto
                      is_origem_destino = is_data
                      iv_percentual = cond #( when lv_lines = 1 then 100
                                              else 0 ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_description.

*    fill_domain_text( ).
*
*    DATA(lt_regiao) = mo_logistica->get_regioes( ).
*    DATA(lt_grp_planta) = mo_logistica->get_grp_planta( ).
*    DATA(lt_centro) = mo_logistica->get_centros( ).
*    DATA(lt_ponto) = mo_logistica->get_pontos( ).
*
*
**    break c060863.
*    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>).
*
*      <fs_data>-dsc_modalidade  = VALUE #( mt_modalidade[ domvalue_l = <fs_data>-modalidade ]-ddtext OPTIONAL ).
*      <fs_data>-dsc_tipo_regra  = VALUE #( mt_tipo_regra[ domvalue_l = <fs_data>-tipo_regra ]-ddtext OPTIONAL ).
*      <fs_data>-dsc_tipo_origem = VALUE #( mt_origem_destino[ domvalue_l = <fs_data>-tipo_origem ]-ddtext OPTIONAL ).
*
*      CASE <fs_data>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs_data>-id_origem ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs_data>-dsc_origem = ls_ponto-lifnr.
*              <fs_data>-dsc_tipo_origem = 'Fornecedor'.
*              TRY.
*                  <fs_data>-dsc_origem_name = /qaps/cl_helper_text=>get_lifnr_text( ls_ponto-lifnr  ).
*                CATCH /qaps/cx_general.  "
*              ENDTRY.
*            WHEN 'C'.
*              <fs_data>-dsc_origem = ls_ponto-kunnr.
*              <fs_data>-dsc_tipo_origem = 'Cliente'.
*              TRY.
*                  <fs_data>-dsc_origem_name = /qaps/cl_helper_text=>get_kunnr_text( ls_ponto-kunnr  ).
*                CATCH /qaps/cx_general.  "
*              ENDTRY.
*            WHEN 'W'.
*              <fs_data>-dsc_origem = ls_ponto-werks.
*              <fs_data>-dsc_tipo_origem = 'Centro'.
*              TRY.
*                  <fs_data>-dsc_origem_name = /qaps/cl_helper_text=>get_werks_text( ls_ponto-werks  ).
*                CATCH /qaps/cx_general.  "
*              ENDTRY.
**            WHEN 'I'.
**              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
**              <fs_data>-dsc_origem = ls_cais-cod_cais.
**              <fs_data>-dsc_tipo_origem = 'Cais'.
*            WHEN OTHERS.
*              CLEAR: <fs_data>-id_origem,
*                     <fs_data>-tipo_origem.
*          ENDCASE.
**          <fs_data>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs_data>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs_data>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs_data>-id_origem ]-codigo OPTIONAL ).
**        WHEN 'S'.
**          <fs_data>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs_data>-id_origem ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          DATA(ls_grp_planta) = VALUE #( lt_grp_planta[ id_grp_planta = <fs_data>-id_origem ] OPTIONAL ).
*          <fs_data>-dsc_origem      = ls_grp_planta-codigo.
*          <fs_data>-dsc_origem_name = ls_grp_planta-descricao.
*      ENDCASE.
*
*      CASE <fs_data>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = VALUE #( lt_ponto[ id_ponto = <fs_data>-id_destino ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs_data>-dsc_destino = ls_ponto-lifnr.
*              <fs_data>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs_data>-dsc_destino = ls_ponto-kunnr.
*              <fs_data>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs_data>-dsc_destino = ls_ponto-werks.
*              <fs_data>-dsc_tipo_destino = 'Centro'.
**            WHEN 'I'.
**              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
**              <fs_data>-dsc_destino = ls_cais-cod_cais.
**              <fs_data>-dsc_tipo_destino = 'Cais'.
*            WHEN OTHERS.
*              CLEAR: <fs_data>-id_destino,
*                     <fs_data>-tipo_destino.
*          ENDCASE.
**          <fs_data>-dsc_destino = VALUE #( lt_ponto[ id_ponto = <fs_data>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs_data>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs_data>-id_destino ]-codigo OPTIONAL ).
**        WHEN 'S'.
**          <fs_data>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs_data>-id_destino ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          ls_grp_planta = VALUE #( lt_grp_planta[ id_grp_planta = <fs_data>-id_destino ] OPTIONAL ).
*          <fs_data>-dsc_destino = ls_grp_planta-codigo.
*      ENDCASE.
*
*
*      IF NOT <fs_data>-id_grp_planta IS INITIAL.
*        ls_grp_planta = lt_grp_planta[ id_grp_planta = <fs_data>-id_grp_planta ].
*        <fs_data>-cod_grp_planta = ls_grp_planta-codigo.
*        <fs_data>-dsc_grp_planta = ls_grp_planta-descricao.
*      ENDIF.
*
*      IF NOT <fs_data>-werks IS INITIAL.
*        TRY.
*            <fs_data>-dsc_werks = /qaps/cl_helper_text=>get_werks_text( <fs_data>-werks ).
*            DATA(ls_centro) = lt_centro[ werks = <fs_data>-werks ].
*
*            IF NOT ls_centro-id_grp_planta IS INITIAL.
*              ls_grp_planta = lt_grp_planta[ id_grp_planta = ls_centro-id_grp_planta ].
*              <fs_data>-cod_grp_planta = ls_grp_planta-codigo.
*              <fs_data>-dsc_grp_planta = ls_grp_planta-descricao.
*            ENDIF.
*          CATCH /qaps/cx_general.
*        ENDTRY.
*
*      ENDIF.
*
*    ENDLOOP.


  ENDMETHOD.


  METHOD fill_domain_text.

    CHECK mv_text_domain_loaded = abap_false.

    mt_modalidade = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODALIDADE' ).
    mt_tipo_regra = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    mt_origem_destino = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).

    mv_text_domain_loaded = abap_true.

  ENDMETHOD.


  METHOD fill_std_producao.



*    DATA lr_matnr TYPE ranges_matnr.
*    DATA lv_has_std_producao TYPE abap_bool.
*
*    DATA(lt_data) = ct_data.
*    DATA(lt_data_new) = ct_data.
*    SORT lt_data BY id_premissa periodo.
*    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING id_premissa.
*
*    DATA(lt_std_producao) = get_std_producao( is_simulacao-id_std_producao ).
*
*    lr_matnr = VALUE #( FOR wa IN lt_std_producao
*                       ( sign = 'I'
*                        option = 'EQ'
*                        low = |{ wa-matnr ALPHA = IN WIDTH = 18 }| ) ).
*
*    DATA(lt_material) = mo_material->get_materiais_all( lr_matnr ).
*
*    LOOP AT is_simulacao-t_destino INTO DATA(ls_destino).
*
*      CASE is_header-tipo_regra.
*        WHEN 'AG'.
*          LOOP AT lt_material INTO DATA(ls_material).
*            CHECK ls_material-agregador = is_header-key_input.
*
*            IF NOT ls_destino-id_grp_planta IS INITIAL.
*              IF line_exists( lt_std_producao[ matnr = ls_material-matnr
*                                                  id_grp_planta = ls_destino-id_grp_planta ] ).
*
*                add_std_producao( is_simulacao       = is_simulacao
*                                  is_premissa_header = is_header
*                                  is_tree_destino    = ls_destino  ).
**                BREAK-POINT.
**                lv_has_std_producao = abap_true.
*                EXIT.
*
*              ENDIF.
*
*            ELSEIF  ls_destino-werks IS INITIAL.
*              IF  line_exists( lt_std_producao[ matnr = ls_material-matnr
*                                                werks = ls_destino-werks ] ).
*
*                 add_std_producao( is_simulacao       = is_simulacao
*                                  is_premissa_header = is_header
*                                  is_tree_destino    = ls_destino  ).
**                BREAK-POINT.
**                lv_has_std_producao = abap_true.
*                EXIT.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
**          DATA(ls_material) = lt_material[ agregador = <fs_data>-key_input ].
*        WHEN 'MA'.
*          ls_material = VALUE #( lt_material[ matnr = |{ is_header-key_input ALPHA = IN WIDTH = 18 }| ] OPTIONAL ).
*
*          IF NOT ls_destino-id_grp_planta IS INITIAL.
*            IF line_exists( lt_std_producao[ matnr = ls_material-matnr
*                                             id_grp_planta = ls_destino-id_grp_planta ] ).
*
*               add_std_producao( is_simulacao       = is_simulacao
*                                 is_premissa_header = is_header
*                                 is_tree_destino    = ls_destino  ).
*
**                BREAK-POINT.
**              lv_has_std_producao = abap_true.
*              EXIT.
*
*            ENDIF.
*
*          ELSEIF  ls_destino-werks IS INITIAL.
*            IF  line_exists( lt_std_producao[ matnr = ls_material-matnr
*                                              werks = ls_destino-werks ] ).
*
*                add_std_producao( is_simulacao       = is_simulacao
*                                  is_premissa_header = is_header
*                                  is_tree_destino    = ls_destino  ).
**                BREAK-POINT.
**              lv_has_std_producao = abap_true.
*              EXIT.
*            ENDIF.
*          ENDIF.
*
*      ENDCASE.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD get_custos_elementares.

    IF lines( mt_custo_elm ) = 0.
      mt_custo_elm = NEW /qaps/cl_mdl_custo_elementar( )->get_variavel( ).
    ENDIF.

    return = mt_custo_elm.

  ENDMETHOD.


  METHOD get_destino_material.

    DATA lr_id_premissa TYPE RANGE OF /qaps/ed_id_premissa.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_destino-id_ponto
      INTO @DATA(ls_ponto).

    "Destino
    CASE is_destino-tipo_ponto.
      WHEN 'G'. "Grupo de Planta + Centros
        SELECT *
          FROM /qaps/prem_hdr
          WHERE id_grp_planta = @ls_ponto-id_externo
          and   id_simulacao  = @is_simulacao-id_simulacao
          INTO TABLE @DATA(lt_destino).

        if sy-subrc ne 0.
          lt_destino = add_transfer_destino( is_simulacao = is_simulacao
                                             is_destino = is_destino ).
        endif.

      WHEN 'W'.
        SELECT *
         FROM /qaps/prem_hdr
         WHERE id_centro = @ls_ponto-id_externo
         and   id_simulacao  = @is_simulacao-id_simulacao
         INTO TABLE @lt_destino.

       if sy-subrc ne 0.
          lt_destino = add_transfer_destino( is_simulacao = is_simulacao
                                             is_destino = is_destino ).
        endif.

    ENDCASE.

    lr_id_premissa = VALUE #( FOR wa IN lt_destino
                              ( sign = 'I' option = 'EQ' low = wa-id_premissa ) ).

    SELECT *
      FROM /qaps/prem_item
      WHERE id_premissa IN @lr_id_premissa
      AND   tipo_regra = @is_material-tipo_regra
      AND   ( agregador = @is_material-agregador OR matnr = @is_material-matnr )
      INTO TABLE @DATA(lt_material).

    if lines( lt_material ) > 0.

      SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_material
      WHERE id_parent = @lt_material-id_item
      APPENDING TABLE @lt_material.

    endif.

    et_destino  = lt_destino.
    et_material = lt_material.

  ENDMETHOD.


  METHOD get_distribuicao.

    DATA(lt_return) = get_distribuicao_nacional( is_data ).
    APPEND LINES OF lt_return TO return.

  ENDMETHOD.


  METHOD get_distribuicao_nacional.

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_premissa = @is_data-id_premissa
      AND   id_item         = @is_data-id_item
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/v_ponto
*      WHERE tipo_ponto = 'F'
      INTO TABLE @DATA(lt_ponto).

    DATA(lt_modalidade) = /qaps/cl_helper_text=>get_domain_values( '/QAPS/D_MODALIDADE' ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs>-id_origem ]  OPTIONAL ).
      <fs>-dsc_tipo_origem = ls_ponto-dsc_tipo_ponto.
      <fs>-dsc_origem     = ls_ponto-descricao.

      <fs>-dsc_modalidade = lt_modalidade[ domvalue_l = <fs>-modalidade ]-ddtext.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_header.

    DATA lt_parent TYPE /qaps/t_premissa_header.

    IF mv_first_time = abap_true.
      initialize( ).
      mv_first_time = abap_false.
    ENDIF.

    IF NOT iv_id_simulacao IS INITIAL.

      SELECT *
        FROM /qaps/v_prm_hdr
        WHERE id_simulacao = @iv_id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ELSE.

      SELECT *
      FROM /qaps/v_prm_hdr
      INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    CHECK lines( return ) > 0.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @ms_simulacao.

    SELECT *
      FROM /qaps/v_pr_st_rs
      FOR ALL ENTRIES IN @return
      WHERE id_premissa = @return-id_premissa
      and   id_simulacao = @return-id_simulacao
      INTO TABLE @DATA(lt_status).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(lt_aux) = lt_status.
      DELETE lt_aux WHERE id_premissa <> <fs>-id_premissa
                      and id_simulacao <> <fs>-id_simulacao.

      IF line_exists( lt_aux[ red = 'X' ] ).
        <fs>-red = 'X'.
      ELSEIF line_exists( lt_aux[ yellow = 'X' ] ).
        <fs>-yellow = 'X'.
      ELSEIF line_exists( lt_aux[ green = 'X' ] ).
        <fs>-green = 'X'.
      ENDIF.

    ENDLOOP.

    "Grp Planta
    LOOP AT return ASSIGNING <fs>.

      IF <fs>-id_grp_planta <> mc_guid_null AND
         <fs>-id_centro  = mc_guid_null.

        SELECT *
          FROM /qaps/v_prm_hdr
          WHERE id_grp_planta = @<fs>-id_grp_planta
          and   id_simulacao = @<fs>-id_simulacao
*        AND   id_centro = '00000000000000000000000000000000'"@mc_guid_null
          INTO CORRESPONDING FIELDS OF TABLE @lt_parent.

        SELECT *
          FROM /qaps/v_pr_st_rs
          FOR ALL ENTRIES IN @lt_parent
          WHERE id_premissa = @lt_parent-id_premissa
          and   id_simulacao = @lt_parent-id_simulacao
          INTO TABLE @lt_status.

        DELETE lt_parent WHERE id_centro <> mc_guid_null.
        DATA(ls_parent) = lt_parent[ 1 ].

        IF line_exists( lt_status[ red = 'X' ] ).
          <fs>-red = 'X'.
        ELSEIF line_exists( lt_status[ yellow = 'X' ] ).
          <fs>-yellow = 'X'.
        ELSEIF line_exists( lt_status[ green = 'X' ] ).
          <fs>-green = 'X'.
        ENDIF.

      ENDIF.

    ENDLOOP.


    SORT return BY id_simulacao ASCENDING
                   tipo DESCENDING
                   id_grp_planta ASCENDING
                   id_centro ASCENDING.

  ENDMETHOD.


  METHOD get_input_by_simulacao.

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @is_simulacao-id_simulacao
      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
*    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
    DATA(lo_custo_elem) = NEW /qaps/cl_mdl_custo_elementar( ).

    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_ponto) = mo_logistica->get_pontos( ).
    DATA(lt_porto) = mo_logistica->get_portos( ).
    DATA(lt_regiao) = mo_logistica->get_regioes( ).
    DATA(lt_cais) = mo_logistica->get_cais( ).
    DATA(lt_grp_planta) = mo_logistica->get_grp_planta( ).
    DATA(lt_custo_elem) = lo_custo_elem->get_variaveis_by_tipo_lista(
                                          iv_id_tp_lista = is_simulacao-id_tp_lista ).

*    BREAK-POINT.
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      <fs>-dsc_custo_elm =
          VALUE #( lt_custo_elem[ id_custo_elementar = <fs>-id_custo_elementar ]-descricao OPTIONAL ).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

*      <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).
*
*      CASE <fs>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = lt_ponto[ id_ponto = <fs>-id_origem ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_origem = ls_ponto-lifnr.
*              <fs>-dsc_tipo_origem = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_origem = ls_ponto-kunnr.
*              <fs>-dsc_tipo_origem = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_origem = ls_ponto-werks.
*              <fs>-dsc_tipo_origem = 'Centro'.
*            WHEN 'I'.
*              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_origem = ls_cais-cod_cais.
*              <fs>-dsc_tipo_origem = 'Cais'.
*          ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs>-id_origem ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_origem = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_origem ]-codigo OPTIONAL ).
*      ENDCASE.
*
*      <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).
*
*      CASE <fs>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = lt_ponto[ id_ponto = <fs>-id_destino ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_destino = ls_ponto-lifnr.
*              <fs>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_destino = ls_ponto-kunnr.
*              <fs>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_destino = ls_ponto-werks.
*              <fs>-dsc_tipo_destino = 'Centro'.
*            WHEN 'I'.
*              ls_cais = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_destino = ls_cais-cod_cais.
*              <fs>-dsc_tipo_destino = 'Cais'.
*          ENDCASE.
*        WHEN 'R'.
*          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs>-id_destino ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_destino = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_destino ]-codigo OPTIONAL ).
*      ENDCASE.

      "Categoria
      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

      "Modal
      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

      "Processo
      <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.


  ENDMETHOD.


  METHOD get_items.

    DATA: lv_show_button TYPE abap_bool,
          lr_items       TYPE RANGE OF /qaps/prem_item-id_item.

    "Resumo Item
    SELECT item~id_premissa,
           item~id_item,
           distrib~periodo,
           SUM( distrib~percentual ) AS soma
      FROM /qaps/prem_item AS item
      left OUTER JOIN /qaps/prem_distr AS distrib ON item~id_item = distrib~id_item
      WHERE item~id_premissa = @is_data-id_premissa
      GROUP BY item~id_premissa,
               item~id_item,
               distrib~periodo
      INTO TABLE @DATA(lt_summary).

    CHECK lines( lt_summary ) > 0.

    lr_items = VALUE #( FOR wa IN lt_summary
                        ( sign = 'I' option = 'EQ' low = wa-id_item ) ).

    SELECT *
        FROM /qaps/prem_item
        WHERE id_premissa = @is_data-id_premissa
        AND id_item IN @lr_items
        INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    IF is_data-id_grp_planta <> mc_guid_null
        AND is_data-id_centro =  mc_guid_null.

      SELECT tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador, COUNT( * ) AS qty
        FROM /qaps/v_prm_hier
        WHERE id_grp_planta = @is_data-id_grp_planta
        AND   id_centro <> @mc_guid_null
        GROUP BY tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador
        INTO TABLE @DATA(lt_children).

      DATA(lt_style) = component_to_style( '/QAPS/S_PREMISSA_ITEM' ).

      ASSIGN lt_style[ fieldname = 'BTN_LINK' ] TO FIELD-SYMBOL(<fs_btn>).

      IF <fs_btn> IS ASSIGNED.
        <fs_btn>-style = cl_gui_alv_grid=>mc_style_button.
        <fs_btn>-style2 = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.
    ENDIF.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      lv_show_button = abap_false.

      IF is_data-id_grp_planta <> mc_guid_null.

        CASE <fs>-tipo_regra.
          WHEN 'GE'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'GE' ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'GP'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'GP'
                                                              id_grupo_produto = <fs>-id_grupo_produto ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'MP'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'MP'
                                                              mat_planejado = <fs>-mat_planejado ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'AG'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'AG'
                                                              agregador = <fs>-agregador ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'MT'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'MT'
                                                              matnr = <fs>-matnr ] )
                                     THEN abap_true
                                     ELSE abap_false ).
        ENDCASE.

      ENDIF.

      DATA(lt_summary_aux) = lt_summary.
      DELETE lt_summary_aux WHERE id_item <> <fs>-id_item.

      DATA(lt_summary_red) = lt_summary_aux.
      DATA(lt_summary_green) = lt_summary_aux.
      DATA(lt_summary_yellow) = lt_summary_aux.

      DELETE lt_summary_red   WHERE soma = 0 OR soma = 100.
      DELETE lt_summary_green WHERE soma <> 100.
      DELETE lt_summary_yellow WHERE soma <> 0.


      IF lines( lt_summary_red ) > 0.
        <fs>-icon = icon_red_light.
      ELSEIF lines( lt_summary_yellow ) > 0.
        <fs>-icon = icon_yellow_light.
      ELSEIF lines( lt_summary_green ) > 0.
        <fs>-icon = icon_green_light.
      ENDIF.

      IF lines( lt_style ) > 0 AND lv_show_button = abap_true.
        <fs>-style = lt_style.
        <fs>-btn_link = icon_insert_relation.
      ENDIF.

      CASE <fs>-tipo_regra.
        WHEN 'GE'.
          <fs>-ord = 1.
        WHEN 'GP'.
          <fs>-ord = 2.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-ord = 3.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-ord = 4.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          <fs>-ord = 5.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.

    SORT return BY ord ASCENDING key_input ASCENDING.

  ENDMETHOD.


  METHOD get_material_transfer_item.

    DATA(lt_item) = get_items( is_header ).

    CASE is_item-tipo_regra.
      WHEN 'GE'.
        return = lt_item[ tipo_regra = 'GE' ].
      WHEN 'MA'.
        return = lt_item[ tipo_regra = 'MA'
                          matnr = is_item-matnr ].
      WHEN 'AG'.
        return = lt_item[ tipo_regra = 'AG'
                          agregador = is_item-agregador ].

      WHEN 'MP'.
        return = lt_item[ tipo_regra = 'MP'
                          mat_planejado = is_item-mat_planejado ].
      WHEN 'GP'.
        return =  lt_item[ tipo_regra = 'GP'
                                 id_grupo_produto = is_item-id_grupo_produto ].
    ENDCASE.


  ENDMETHOD.


  METHOD get_referencias.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @it_data
      WHERE id_simulacao = @it_data-id_original
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_simulacao.

*    SELECT *
*      FROM /qaps/v_simul
*      INTO CORRESPONDING FIELDS OF TABLE return.
*
*    IF lines( return ) > 0.
*
*      DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
*
*      DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
*      DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
*      DATA(lt_status) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_STATUS_SIMUL' ).
*      DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
*      DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
*      DATA(lt_ponto) = mo_logistica->get_pontos( ).
*      DATA(lt_porto) = mo_logistica->get_portos( ).
*      DATA(lt_regiao) = mo_logistica->get_regioes( ).
*      DATA(lt_cais) = mo_logistica->get_cais( ).
*      DATA(lt_grp_planta) = mo_logistica->get_grp_planta( ).
*      DATA(lt_centros) = mo_logistica->get_centros( ).
*      DATA(lo_std_producao) = NEW /qaps/cl_mdl_std_producao( ).
*      DATA(lt_std_producao) = lo_std_producao->get_all( ).
*
**      break c060863.
*
*      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
*
*        <fs>-dsc_status = VALUE #( lt_status[ domvalue_l = <fs>-status ]-ddtext OPTIONAL ).
*        IF NOT <fs>-id_std_producao IS INITIAL.
*          DATA(ls_std_producao) = VALUE #( lt_std_producao[ id_std_producao = <fs>-id_std_producao ] OPTIONAL ).
*          <fs>-cod_std_producao = ls_std_producao-codigo.
*          <fs>-dsc_std_producao = ls_std_producao-descricao.
*        ENDIF.
*
**        "Destinos Pontos
**        SELECT DISTINCT '1' AS caso, id_simulacao, tipo_destino, id_destino, tipo_origem, id_origem,
**                /qaps/ponto~tipo_ponto,id_cidade,werks,kunnr,lifnr
**          FROM /qaps/var_input
**          INNER JOIN /qaps/custo_elm
**          ON /qaps/var_input~id_custo_elementar = /qaps/custo_elm~id_custo_elementar
**          INNER JOIN /qaps/ponto
**          ON /qaps/var_input~id_destino = /qaps/ponto~id_ponto
**          WHERE id_simulacao = @<fs>-id_simulacao
**          AND tipo_destino = 'P'  "Ponto
**          AND ( /qaps/custo_elm~importacao = 'X' OR /qaps/custo_elm~nacional = 'X' )
**          INTO TABLE @DATA(lt_var).
**
**        "Destinos em Branco
**        SELECT DISTINCT '2' AS caso,id_simulacao, tipo_destino, id_destino, tipo_origem, id_origem
**          FROM /qaps/var_input
**          INNER JOIN /qaps/custo_elm
**          ON /qaps/var_input~id_custo_elementar = /qaps/custo_elm~id_custo_elementar
**          WHERE id_simulacao = @<fs>-id_simulacao
**          AND ( /qaps/custo_elm~importacao = 'X' OR /qaps/custo_elm~nacional = 'X' )
**          AND tipo_destino = ''
**          APPENDING CORRESPONDING FIELDS OF TABLE @lt_var.
*
*        "Destinos - Região, Grupo de Plantas
*        SELECT DISTINCT '3' AS caso,id_simulacao, tipo_destino, id_destino, tipo_origem, id_origem
*          FROM /qaps/var_input
*          INNER JOIN /qaps/custo_elm
*          ON /qaps/var_input~id_custo_elementar = /qaps/custo_elm~id_custo_elementar
*          WHERE id_simulacao = @<fs>-id_simulacao
*          AND ( /qaps/custo_elm~importacao = 'X' OR /qaps/custo_elm~nacional = 'X' )
*          AND tipo_destino IN ('R','G') "Região, Grupo de Plantas
*          APPENDING CORRESPONDING FIELDS OF TABLE  @lt_var.
*
*        DELETE lt_var WHERE ( tipo_origem = 'S' "Porto
*                         OR ( tipo_origem = 'P' AND tipo_ponto = 'I' ) ). "Cais
*
*        <fs>-t_destino = CORRESPONDING #( lt_var ).
*
*        LOOP AT <fs>-t_destino ASSIGNING FIELD-SYMBOL(<fs_destino>).
*          <fs_destino>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs_destino>-tipo_destino ]-ddtext OPTIONAL ).
*        ENDLOOP.
*
*        DELETE <fs>-t_destino WHERE tipo_destino IS INITIAL.
*
*        LOOP AT <fs>-t_destino ASSIGNING FIELD-SYMBOL(<fs_var>).
*
*          CASE <fs_var>-tipo_destino.
*            WHEN 'P'.
*              DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs_var>-id_destino ] OPTIONAL ).
*              <fs_var>-tipo_ponto = ls_ponto-tipo_ponto.
*              CASE ls_ponto-tipo_ponto.
*                WHEN 'F'.
*                  <fs_var>-dsc_ponto = ls_ponto-lifnr.
*                WHEN 'C'.
*                  <fs_var>-dsc_ponto = ls_ponto-kunnr.
**                  <fs>-dsc_tipo_destino = 'Cliente'.
*                WHEN 'W'.
*                  <fs_var>-dsc_ponto = ls_ponto-werks.
*                  <fs_var>-werks = ls_ponto-werks.
**                  <fs>-dsc_tipo_destino = 'Centro'.
*                WHEN 'I'.
*                  DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*                  <fs_var>-dsc_ponto = ls_cais-cod_cais.
*
*                WHEN OTHERS.
*
*              ENDCASE.
*
*            WHEN 'S'.
*              <fs_var>-dsc_ponto = VALUE #( lt_porto[ id_porto = <fs_var>-id_destino ]-cod_porto OPTIONAL ).
*            WHEN 'G'.
*              DATA(ls_grp_planta) = VALUE #( lt_grp_planta[ id_grp_planta = <fs_var>-id_destino ] OPTIONAL ).
*              <fs_var>-dsc_ponto = ls_grp_planta-codigo.
*              <fs_var>-id_grp_planta = ls_grp_planta-id_grp_planta.
*              <fs_var>-cod_grp_planta = ls_grp_planta-codigo.
*          ENDCASE.
*        ENDLOOP.
*
*        LOOP AT lt_centros INTO DATA(ls_centro).
*
*          IF NOT ls_centro-id_grp_planta IS INITIAL.
*
*            ls_grp_planta = VALUE #( lt_grp_planta[ id_grp_planta = ls_centro-id_grp_planta ] OPTIONAL ).
*
*            IF NOT line_exists( <fs>-t_destino[ dsc_ponto = ls_grp_planta-codigo
*                                                tipo_destino = 'G' ] ).
*
*              APPEND INITIAL LINE TO <fs>-t_destino ASSIGNING FIELD-SYMBOL(<fs_entry>).
*              <fs_entry>-dsc_ponto = ls_grp_planta-codigo.
*              <fs_entry>-id_grp_planta = ls_grp_planta-id_grp_planta.
*              <fs_entry>-cod_grp_planta = ls_grp_planta-codigo.
*              <fs_entry>-tipo_destino = 'G'.
*              <fs_entry>-grupo = 'G'.
*              <fs_entry>-dsc_tipo_destino = 'Grupo de Plantas'.
*            ENDIF.
*
*          ELSEIF NOT line_exists( <fs>-t_destino[ dsc_ponto = ls_centro-werks ] ).
*
*            APPEND INITIAL LINE TO <fs>-t_destino ASSIGNING <fs_entry>.
*            <fs_entry>-dsc_ponto = ls_centro-werks.
*            <fs_entry>-werks = ls_centro-werks.
*            <fs_entry>-tipo_destino = 'W'.
*            <fs_entry>-grupo = 'W'.
*            <fs_entry>-dsc_tipo_destino = 'Plantas/Centros'.
*
*          ELSE.
*            ASSIGN <fs>-t_destino[ dsc_ponto = ls_centro-werks ] TO <fs_entry>.
*            <fs_entry>-tipo_destino = 'W'.
*            <fs_entry>-grupo = 'W'.
*
*          ENDIF.
*        ENDLOOP.
*
*        LOOP AT <fs>-t_destino ASSIGNING <fs_entry>.
*
*          IF <fs_entry>-tipo_destino = 'W'.
*            <fs_entry>-caso = '1'.
*          ELSEIF <fs_entry>-tipo_destino = 'G'.
*            <fs_entry>-caso = '2'.
*            <fs_entry>-grupo = 'G'.
*          ELSEIF <fs_entry>-tipo_destino = 'P'
*             AND <fs_entry>-tipo_ponto = 'W'.
*            <fs_entry>-caso = '3'.
*            <fs_entry>-grupo = 'W'.
*            <fs_entry>-dsc_tipo_destino = 'Plantas/Centros'.
*          ELSEIF <fs_entry>-grupo IS INITIAL.
**            <fs_entry>-caso = '3'.
*          ENDIF.
*
*        ENDLOOP.
*
*        SORT <fs>-t_destino BY tipo_destino dsc_ponto.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    SORT return BY  descricao DESCENDING.

  ENDMETHOD.


  METHOD get_single_header.

    SELECT SINGLE *
      FROM /qaps/prem_hdr
      WHERE id_premissa = @iv_id_premissa
      INTO @return.

  ENDMETHOD.


  METHOD get_status.

*    TYPES: BEGIN OF ts_status,
*             periodo    TYPE spmon,
*             percentual TYPE /qaps/percentual,
*           END OF ts_status.
*
*    DATA lt_status TYPE TABLE OF ts_status.
*
*    IF iv_tipo_destino = 'G'.
*
*      DATA(lt_data) = get_input_by_grp_plant( iv_id_simulacao = iv_id_simulacao
*                                              is_matriz_header = is_matriz_header
*                                              iv_id_grp_planta = iv_id_destino ).
*
*    ELSEIF iv_id_destino IS INITIAL.
*
*      lt_data = get_input_by_werks( iv_id_simulacao = iv_id_simulacao
*                                              is_matriz_header = is_matriz_header
*                                              iv_werks = CONV #( iv_werks ) ).
*    ELSE.
**      BREAK c060863.
*      lt_data = get_input_by_ponto( iv_id_simulacao = iv_id_simulacao
*                                              is_matriz_header = is_matriz_header
*                                              iv_destino = iv_id_destino ).
*
*    ENDIF.
*
*    LOOP AT lt_data INTO DATA(ls_data).
*
*      IF line_exists( lt_status[ periodo = ls_data-periodo ] ).
*        ASSIGN lt_status[ periodo = ls_data-periodo ] TO FIELD-SYMBOL(<fs>).
*        <fs>-percentual = <fs>-percentual + ls_data-percentual.
*      ELSE.
*        APPEND INITIAL LINE TO lt_status ASSIGNING <fs>.
*        <fs>-periodo    = ls_data-periodo.
*        <fs>-percentual = ls_data-percentual.
*      ENDIF.
*
*    ENDLOOP.
*
*    SORT lt_status BY percentual ASCENDING.
*
*    DATA(lv_status_min) = VALUE #( lt_status[ 1 ]-percentual OPTIONAL ).
*    DATA(lv_lines) = lines( lt_status ).
*    DATA(lv_status_max) = VALUE #( lt_status[ lv_lines ]-percentual OPTIONAL ).
*
*    IF lv_status_min = 100 AND lv_status_max = 100.
*      return = icon_green_light.
*    ELSEIF lv_status_min = 0 AND lv_status_max = 0.
*      return = icon_yellow_light.
*    ELSE.
*      return = icon_red_light.
*    ENDIF.
*
*
*
**    return = lt_data.

  ENDMETHOD.


  METHOD get_std_producao.

    IF NOT line_exists( mt_std_producao[ id_std_producao = iv_id_std_producao ] ).
      SELECT *
        FROM /qaps/std_prd_pa
        WHERE id_std_producao = @iv_id_std_producao
        INTO TABLE @mt_std_producao.

      LOOP AT mt_std_producao ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
      ENDLOOP.

    ENDIF.

    return = mt_std_producao.

  ENDMETHOD.


  METHOD get_trajeto.


    IF is_data-id_centro <> mc_guid_null.

      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @is_data-id_centro
        INTO @DATA(ls_ponto).

    ELSE.
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @is_data-id_grp_planta
        INTO @ls_ponto.
    ENDIF.

    SELECT *
        FROM /qaps/trajeto
        WHERE id_origem = @is_data-id_origem
        AND   id_destino = @ls_ponto-id_ponto
        INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK sy-subrc NE 0.

    SELECT SINGLE *
      FROM /qaps/centro
      WHERE id_centro = @is_data-id_centro
      INTO @DATA(ls_centro).

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_externo = @ls_centro-id_grp_planta
      INTO @ls_ponto.

    SELECT *
        FROM /qaps/trajeto
        WHERE id_origem = @is_data-id_origem
        AND   id_destino = @ls_ponto-id_ponto
        INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_trajeto_by_distribuicao.

    SELECT *
      FROM /qaps/prem_traj
      WHERE id_distribuicao = @is_data-id_distribuicao
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/trajeto
      INTO TABLE @DATA(lt_trajeto).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_trajeto) = VALUE #( lt_trajeto[ id_trajeto = <fs>-id_trajeto ] OPTIONAL ).
      <fs>-codigo = ls_trajeto-codigo.
      <fs>-descricao = ls_trajeto-descricao.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_trajeto_transfer.

    "Origem
    IF is_data-id_centro <> mc_guid_null.

      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @is_data-id_centro
        INTO @DATA(ls_origem).

    ELSE.
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @is_data-id_grp_planta
        INTO @ls_origem.
    ENDIF.

    SELECT *
        FROM /qaps/trajeto
        WHERE id_origem = @is_destino-id_ponto
        AND   id_destino = @ls_origem-id_ponto
        INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK sy-subrc NE 0.

    SELECT SINGLE *
      FROM /qaps/centro
      WHERE id_centro = @ls_origem-id_externo
      INTO @DATA(ls_centro).

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_externo = @ls_centro-id_grp_planta
      INTO @ls_origem.

    SELECT *
        FROM /qaps/trajeto
        WHERE id_origem = @is_destino-id_ponto
        AND   id_destino = @ls_origem-id_ponto
        INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_transfer_hierarquia.

    DATA: lr_data TYPE REF TO data.
    DATA lt_entry TYPE TABLE OF /qaps/prem_item.
    DATA lv_matnr TYPE matnr.

    IF is_material-tipo_regra = 'MA'.
      APPEND is_material TO return.
    ELSE.

      APPEND is_material TO return.

      SELECT *
        FROM /qaps/material
        WHERE agregador = @is_material-agregador
        INTO TABLE @DATA(lt_material).

      LOOP AT lt_material INTO DATA(ls_material).

        lv_matnr = |{ ls_material-matnr ALPHA = OUT }|.

        SELECT *
          FROM /qaps/prem_item
*          FOR ALL ENTRIES IN @lt_material
          WHERE ( matnr = @ls_material-matnr OR matnr = @lv_matnr )
          AND   tipo_regra = 'MA'
          AND   id_premissa = @is_material-id_premissa
          APPENDING TABLE @DATA(lt_prem_item).

      ENDLOOP.

      LOOP AT lt_material INTO ls_material.

        lv_matnr = |{ ls_material-matnr ALPHA = OUT }|.

        IF NOT line_exists( lt_prem_item[ tipo_regra = 'MA'
                                          matnr = ls_material-matnr ] )
          and not line_exists( lt_prem_item[ tipo_regra = 'MA'
                                          matnr = lv_matnr ] ).


          DATA(ls_entry) =
              VALUE /qaps/prem_item( id_item = cl_system_uuid=>create_uuid_x16_static( )
                                     id_premissa      = is_material-id_premissa
                                     tipo_regra       = 'MA'
                                     matnr            = ls_material-matnr
                                     oculto           = 'X'
                                     id_parent        = is_material-id_item  ).

          lr_data = REF #( ls_entry ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).
          APPEND ls_entry TO lt_entry.

          APPEND ls_entry TO return.

        ELSE.
          try.
            DATA(ls_data) = lt_prem_item[ tipo_regra = 'MA'
                                          matnr = ls_material-matnr ].
          catch cx_sy_itab_line_not_found.
            ls_data = lt_prem_item[ tipo_regra = 'MA'
                                          matnr = lv_matnr ].
          endtry.
          APPEND ls_data TO return.
        ENDIF.

      ENDLOOP.

      IF lines( lt_entry ) > 0.
        MODIFY /qaps/prem_item FROM ls_entry.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD initialize.

*    initialize_header( ).
*    initialize_item( ).
*    initialize_distribuicao( ).
*    initialize_trajeto( ).

  ENDMETHOD.


  METHOD initialize_distribuicao.

    init_distrib_com_dest( ).
    init_distrib_sem_dest( ).

    init_distrib_std_producao( ).
    init_distrib_importacao( ).

    "Quando for 100%
    set_distribuicao_inicial( ).

  ENDMETHOD.


  METHOD initialize_header.

*    DATA: lr_data  TYPE REF TO data,
*          lr_ponto TYPE RANGE OF /qaps/id_ponto.
*
*    DATA lt_premissa_header TYPE TABLE OF /qaps/prem_hdr.
*
*    SELECT *
*      FROM /qaps/v_prm_dest
*      INTO TABLE @DATA(lt_destino).
*
*    SELECT id_simulacao
*      FROM /qaps/simulacao
*      INTO TABLE @DATA(lt_simulacao).
*
*    IF lines( lt_simulacao ) > 0.
*
*      SELECT *
*        FROM /qaps/prem_hdr
*        INTO TABLE @DATA(lt_header).
*
*      LOOP AT lt_simulacao INTO DATA(ls_simulacao).
*
*        LOOP AT lt_destino INTO DATA(ls_destino).
*
*          CASE ls_destino-tipo.
*            WHEN 'C'.
*              "Grp Planta
*              IF NOT line_exists( lt_header[ id_simulacao     = ls_simulacao-id_simulacao
*                                                id_grp_planta = ''
*                                                id_centro     = ls_destino-id_centro ] )
*                AND NOT line_exists( lt_premissa_header[ id_simulacao = ls_simulacao-id_simulacao
*                                                       id_grp_planta = ''
*                                                       id_centro    = ls_destino-id_centro ] ).
*
*
*                APPEND VALUE /qaps/prem_hdr(
*                    id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                    id_simulacao     = ls_simulacao-id_simulacao
*                    id_centro        = ls_destino-id_centro ) TO lt_premissa_header.
*              ENDIF.
*            WHEN 'G'.
*
*              "Grp Planta
*              IF NOT line_exists( lt_header[ id_simulacao     = ls_simulacao-id_simulacao
*                                                id_grp_planta = ls_destino-id_grp_planta
*                                                id_centro     = '' ] )
*                AND NOT line_exists( lt_premissa_header[ id_simulacao = ls_simulacao-id_simulacao
*                                                       id_grp_planta = ls_destino-id_grp_planta
*                                                       id_centro    = '' ] ).
*
*
*                APPEND VALUE /qaps/prem_hdr(
*                    id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                    id_simulacao     = ls_simulacao-id_simulacao
*                    id_grp_planta    = ls_destino-id_grp_planta ) TO lt_premissa_header.
*              ENDIF.
*
*              "Grp Planta e/ou Centro
*              IF NOT line_exists( lt_header[ id_simulacao  = ls_simulacao-id_simulacao
*                                                id_grp_planta = ls_destino-id_grp_planta
*                                                id_centro     = ls_destino-id_centro ] )
*                AND NOT line_exists( lt_premissa_header[ id_simulacao  = ls_simulacao-id_simulacao
*                                                       id_grp_planta = ls_destino-id_grp_planta
*                                                       id_centro     = ls_destino-id_centro ] ).
*
*
*                APPEND VALUE /qaps/prem_hdr(
*                    id_premissa  = cl_system_uuid=>create_uuid_x16_static( )
*                    id_simulacao     = ls_simulacao-id_simulacao
*                    id_grp_planta    = ls_destino-id_grp_planta
*                    id_centro        = ls_destino-id_centro ) TO lt_premissa_header.
*
*              ENDIF.
*
*          ENDCASE.
*
*        ENDLOOP.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    IF lines( lt_premissa_header ) > 0.
*      LOOP AT lt_premissa_header REFERENCE INTO lr_data.
*        preencher_dados_controle( CHANGING cr_data = lr_data ).
*      ENDLOOP.
*
*      MODIFY /qaps/prem_hdr FROM TABLE lt_premissa_header.
*      COMMIT WORK AND WAIT.
*
*    ENDIF.

  ENDMETHOD.


  METHOD initialize_item.

    initialize_item_com_destino( ). "1
    initialize_item_sem_destino( ). "2

    initialize_item_std_producao( ). "3
    initialize_item_importacao( ). "4

  ENDMETHOD.


  METHOD initialize_item_com_destino.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_prm_hdr
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_inp_prm
      WHERE id_destino <> @mc_guid_null
      INTO TABLE @DATA(lt_var_input).

    LOOP AT lt_header INTO DATA(ls_header).

      DATA(lt_filtered_input) = lt_var_input.

      CASE ls_header-tipo.
        WHEN 'G'.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta
                                      OR id_centro <> ls_header-id_centro.
        WHEN 'C'.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta
                                      OR id_centro <> ls_header-id_centro.
      ENDCASE.

      LOOP AT lt_filtered_input INTO DATA(ls_filtered_input).

        CHECK NOT line_exists( lt_item[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] )
          AND NOT line_exists( lt_data[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] ).

        DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                 id_premissa  = ls_header-id_premissa
                                                 tipo_regra       = ls_filtered_input-tipo_regra
                                                 matnr            = ls_filtered_input-matnr
                                                 id_grupo_produto = ls_filtered_input-id_grupo_produto
                                                 agregador        = ls_filtered_input-agregador
                                                 mat_planejado    = ls_filtered_input-mat_planejado
                                                 premissa_rule    = '1' ).

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.


  METHOD initialize_item_importacao.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/prm_matriz
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      INTO TABLE @DATA(lt_item_prm).

    SELECT *
      FROM /qaps/matriz_itm
      FOR ALL ENTRIES IN @lt_header
      WHERE id_matriz_abast  = @lt_header-id_matriz_abast
      INTO TABLE @DATA(lt_item_mat).

    LOOP AT lt_header INTO DATA(ls_header).

      DATA(lt_src) = lt_item_mat.
      DELETE lt_src WHERE id_matriz_abast <> ls_header-id_matriz_abast.

      LOOP AT lt_src INTO DATA(ls_src).

        CHECK NOT line_exists( lt_item_prm[ tipo_regra = ls_src-tipo_regra
                                            matnr = ls_src-matnr
                                            id_grupo_produto = ls_src-id_grupo_produto
                                            agregador = ls_src-agregador
                                            mat_planejado = ls_src-mat_planejado ] ).

        DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                id_premissa      = ls_header-id_premissa
                                                tipo_regra       = ls_src-tipo_regra
                                                matnr            = ls_src-matnr
                                                id_grupo_produto = ls_src-id_grupo_produto
                                                agregador        = ls_src-agregador
                                                mat_planejado    = ls_src-mat_planejado
                                                id_item_matriz   = ls_src-id_item
                                                premissa_rule    = '4').

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_item_sem_destino.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_prm_hdr
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_inp_prs
      WHERE id_destino = @mc_guid_null
      INTO TABLE @DATA(lt_var_input).

    DELETE lt_var_input WHERE tipo_ponto = 'I' OR tipo_ponto = 'P'.

    LOOP AT lt_header INTO DATA(ls_header).

      LOOP AT lt_var_input INTO DATA(ls_filtered_input).

        CHECK NOT line_exists( lt_item[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] )
          AND NOT line_exists( lt_data[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] ).

        DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                 id_premissa  = ls_header-id_premissa
                                                 tipo_regra       = ls_filtered_input-tipo_regra
                                                 matnr            = ls_filtered_input-matnr
                                                 id_grupo_produto = ls_filtered_input-id_grupo_produto
                                                 agregador        = ls_filtered_input-agregador
                                                 mat_planejado    = ls_filtered_input-mat_planejado
                                                 premissa_rule    = '2' ).

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.


  METHOD initialize_item_std_producao.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/prem_item,
          lr_data TYPE REF TO data,
          lv_matnr type matnr.

    SELECT *
        FROM /qaps/v_prm_hdr
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      AND  premissa_rule = '3'
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_sim_stdp
      FOR ALL ENTRIES IN @lt_header
      WHERE id_simulacao = @lt_header-id_simulacao
      INTO TABLE @DATA(lt_std_producao).

    LOOP AT lt_header INTO DATA(ls_header).

      DATA(lt_filtered_input) = lt_std_producao.

      if ls_header-id_centro = mc_guid_null.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta.
*                                      OR id_centro <> ls_header-id_centro.
      else.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta
                                      OR id_centro <> ls_header-id_centro.
      endif.

      LOOP AT lt_filtered_input INTO DATA(ls_filtered_input).

        lv_matnr = |{ ls_filtered_input-matnr ALPHA = IN WIDTH = 18 }|.

        CHECK NOT line_exists( lt_item[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = 'MA'
                                        matnr            = lv_matnr ] )
          AND NOT line_exists( lt_data[ id_premissa  = ls_header-id_premissa
                                        tipo_regra       = 'MA'
                                        matnr            = lv_matnr  ] ).

        DATA(ls_data) =  VALUE /qaps/prem_item( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                id_premissa  = ls_header-id_premissa
                                                tipo_regra       = 'MA'
                                                matnr            = |{ ls_filtered_input-matnr ALPHA = IN WIDTH = 18 }|
                                                premissa_rule    = '3' ).

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/prem_item FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.


  METHOD initialize_premissa.

*    SELECT *
*      FROM /qaps/simulacao
*      INTO TABLE @DATA(lt_simulacao).
*
*    progress( iv_texto = 'Atualizando premissa...' ).
*
*    LOOP AT lt_simulacao INTO DATA(ls_simulacao).
*      criar_premissa( ls_simulacao ).
*    ENDLOOP.
*
*    MESSAGE 'Atualização concluída' TYPE 'S'.

  ENDMETHOD.


  METHOD initialize_trajeto.

    "Importação
    SELECT *
      FROM /qaps/v_prm_all
      WHERE id_item_matriz <> @mc_guid_null
      INTO TABLE @DATA(lt_distrib).

    LOOP AT lt_distrib INTO DATA(ls_distrib).
      execute_trajeto( ls_distrib ).
    ENDLOOP.

    "Nacional/Transferência

    SELECT *
      FROM /qaps/v_prm_all
      WHERE modalidade = 'N' or modalidade = 'T'
      INTO TABLE @lt_distrib.

    LOOP AT lt_distrib INTO ls_distrib.
      execute_trajeto( ls_distrib ).
    ENDLOOP.

  ENDMETHOD.


  METHOD init_distrib_com_dest.

    DATA lt_origem TYPE /qaps/t_prm_ibas.

    "Item
    SELECT *
      FROM /qaps/v_prm_dbas
      INTO TABLE @DATA(lt_destino).

    CHECK lines( lt_destino ) > 0.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_simulacao = @lt_destino-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

*    SELECT *
*      FROM /qaps/v_prm_hier ""/qaps/prem_distr
*      FOR ALL ENTRIES IN @lt_destino
*      WHERE id_item = @lt_destino-id_item
*      AND   id_premissa = @lt_destino-id_premissa
*      INTO TABLE @DATA(lt_distrib).

    SORT lt_destino BY id_premissa id_item.

*    BREAK c060863.
    LOOP AT lt_destino INTO DATA(ls_destino).

      check check_distrib_exists( ls_destino ) = abap_true.

*      DATA(lt_distrib_check) = lt_distrib.
*      DELETE lt_distrib_check WHERE id_item <> ls_destino-id_item.
*
*      CHECK NOT line_exists( lt_distrib_check[ id_premissa = ls_destino-id_premissa
*                                               id_item     = ls_destino-id_item
*                                               tipo_regra  = ls_destino-tipo_regra ] ).

      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_destino-id_simulacao ].

      IF ls_destino-tipo_regra <> 'GE'.

        SELECT *
          FROM /qaps/v_prm_ibas
          WHERE ( ( tipo_regra = 'GP' AND id_grupo_produto = @ls_destino-id_grupo_produto ) OR
                ( tipo_regra = 'AG'   AND agregador = @ls_destino-agregador ) OR
                ( tipo_regra = 'MP'   AND mat_planejado = @ls_destino-mat_planejado ) OR
                ( tipo_regra = 'MA'   AND matnr = @ls_destino-matnr ) )
          AND id_simulacao = @ls_destino-id_simulacao
          AND has_destino = 'X'
          AND cod_destino = @ls_destino-codigo
          INTO TABLE @lt_origem.
      ELSE.
        SELECT *
        FROM /qaps/v_prm_ibas
        WHERE (  tipo_regra = 'GE' )
        AND id_simulacao = @ls_destino-id_simulacao
        AND has_destino = 'X'
        AND cod_destino = @ls_destino-codigo
*        AND ( tipo_origem <> 'I' and tipo_origem <> 'P' )
*        AND cod_destino = @ls_destino-codigo
        INTO TABLE @lt_origem.
      ENDIF.

      DELETE lt_origem WHERE tipo_origem = 'I' OR tipo_origem = 'P' OR tipo_origem = 'C'.

      CHECK lines( lt_origem ) > 0.

*      BREAK c060863.

      IF lines( lt_origem ) = 1.
        DATA(lv_full) = abap_true.
      ELSE.
        lv_full = abap_false.
      ENDIF.

      LOOP AT lt_origem INTO DATA(ls_origem).

        execute_distribuicao( is_origem    = ls_origem
                              is_destino   = ls_destino
                              is_simulacao = ls_simulacao ).

      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD init_distrib_importacao.

    DATA lr_data TYPE REF TO data.
    DATA: lt_data     TYPE TABLE OF /qaps/prem_distr,
          lr_id_porto TYPE RANGE OF /qaps/ed_id_porto.
    DATA lv_key TYPE /qaps/ed_id_distribuicao.

    "Item
    SELECT *
      FROM /qaps/v_prm_dbas
      WHERE id_item_matriz <> @mc_guid_null
      INTO TABLE @DATA(lt_destino).

    CHECK lines( lt_destino ) > 0.

    loop at lt_destino into data(ls_destino).

      create_distribuicao_by_matriz( ls_destino ).

    ENDLOOP.

  ENDMETHOD.


  METHOD init_distrib_sem_dest.

    DATA lt_origem TYPE /qaps/t_prm_ibas.

    "Item
    SELECT *
      FROM /qaps/v_prm_dbas
      INTO TABLE @DATA(lt_destino).

    CHECK lines( lt_destino ) > 0.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_simulacao = @lt_destino-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

    SELECT *
      FROM /qaps/v_prm_hier "/qaps/prem_distr
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_item = @lt_destino-id_item
      AND   id_premissa = @lt_destino-id_premissa
      INTO TABLE @DATA(lt_distrib).

    SORT lt_destino BY id_premissa id_item.

*    BREAK c060863.

    LOOP AT lt_destino INTO DATA(ls_destino).

      CHECK NOT line_exists( lt_distrib[ id_premissa = ls_destino-id_premissa
                                         id_item         = ls_destino-id_item
                                         tipo_regra  = ls_destino-tipo_regra ] ).

      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_destino-id_simulacao ].

      IF ls_destino-tipo_regra <> 'GE'.

        SELECT *
          FROM /qaps/v_prm_ibas
          WHERE ( ( tipo_regra = 'GP' AND id_grupo_produto = @ls_destino-id_grupo_produto ) OR
                ( tipo_regra = 'AG'   AND agregador = @ls_destino-agregador ) OR
                ( tipo_regra = 'MP'   AND mat_planejado = @ls_destino-mat_planejado ) OR
                ( tipo_regra = 'MA'   AND matnr = @ls_destino-matnr ) )
          AND id_simulacao = @ls_destino-id_simulacao
          AND has_destino = ''
          INTO TABLE @lt_origem.
      ELSE.
        SELECT *
        FROM /qaps/v_prm_ibas
        WHERE (  tipo_regra = 'GE' )
        AND id_simulacao = @ls_destino-id_simulacao
        AND has_destino = ''
        AND ( tipo_origem <> 'I' and tipo_origem <> 'P' )
        INTO TABLE @lt_origem.
      ENDIF.

      delete lt_origem where tipo_origem = 'I' or tipo_origem = 'P'.

      CHECK lines( lt_origem ) > 0.

      LOOP AT lt_origem INTO DATA(ls_origem).

        execute_distribuicao( is_origem    = ls_origem
                              is_destino   = ls_destino
                              is_simulacao = ls_simulacao ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD init_distrib_std_producao.

    "Item
    SELECT *
       FROM /qaps/v_prm_hdr
       INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @lt_header
      WHERE id_simulacao = @lt_header-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header
      WHERE id_premissa  = @lt_header-id_premissa
      AND  premissa_rule = '3'
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_item
      WHERE id_item = @lt_item-id_item
      AND   id_premissa = @lt_item-id_premissa
      AND   modalidade = 'P'
      INTO TABLE @DATA(lt_distrib).

    select *
      from /qaps/v_ponto
      into TABLE @data(lt_ponto).

*    BREAK c060863.

    LOOP AT lt_item INTO DATA(ls_item).

      CHECK NOT line_exists( lt_distrib[ id_premissa = ls_item-id_premissa
                                       id_item       = ls_item-id_item ] ).
      DATA(ls_origem) = lt_header[ id_premissa = ls_item-id_premissa ].
      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_origem-id_simulacao ].

      DATA(ls_data) = VALUE /qaps/prem_distr(
          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
          id_premissa     = ls_item-id_premissa
          id_item         = ls_item-id_item
          modalidade      = 'P'
          tipo_origem     = COND #( WHEN ls_origem-werks IS INITIAL THEN 'G' ELSE 'W' )
          id_origem       = COND #( WHEN ls_origem-werks IS INITIAL
                                    THEN lt_ponto[ id_externo = ls_origem-id_grp_planta ]-id_ponto
                                    ELSE lt_ponto[ id_externo = ls_origem-id_centro ]-id_ponto )

      ).

      create_distribuicao( is_simulacao = ls_simulacao
                           is_data      = ls_data ).


    ENDLOOP.

  ENDMETHOD.


  METHOD material_exists.

    IF lines( it_item ) = 0.
      return = abap_false.
    ELSE.
      IF is_header-id_centro = mc_guid_null.
        CASE is_data-tipo_regra.
          WHEN 'GE'.
            IF line_exists( it_item[ tipo_regra = 'GE'
                                     id_grp_planta = is_header-id_grp_planta  ] )
            OR line_exists( it_buffer[ tipo_regra = 'GE' ] ).
              return = abap_true.
            ENDIF.
          WHEN 'AG'.
            IF line_exists( it_item[ tipo_regra = 'AG' agregador = is_data-agregador
                                     id_grp_planta = is_header-id_grp_planta ] )
            OR line_exists( it_buffer[ tipo_regra = 'AG' agregador = is_data-agregador  ] ).
              return = abap_true.
            ENDIF.
          WHEN 'MP'.
            IF line_exists( it_item[ tipo_regra = 'MP' mat_planejado = is_data-mat_planejado
                                     id_grp_planta = is_header-id_grp_planta ] )
            OR line_exists( it_buffer[ tipo_regra = 'MP' mat_planejado = is_data-mat_planejado  ] ).
              return = abap_true.
            ENDIF.
          WHEN 'GP'.
            IF line_exists( it_item[ tipo_regra = 'GP' id_grupo_produto = is_data-id_grupo_produto
                                     id_grp_planta = is_header-id_grp_planta ] )
            OR line_exists( it_buffer[ tipo_regra = 'GP' id_grupo_produto = is_data-id_grupo_produto ] ).
              return = abap_true.
            ENDIF.
*        when 'AG'.
        ENDCASE.
      ELSE.
        CASE is_data-tipo_regra.
          WHEN 'GE'.
            IF line_exists( it_item[ tipo_regra = 'GE' id_centro = is_data-id_centro ] )
            OR line_exists( it_buffer[ tipo_regra = 'GE' ] ).
              return = abap_true.
            ENDIF.
          WHEN 'AG'.
            IF line_exists( it_item[ tipo_regra = 'AG' agregador = is_data-agregador
                                     id_centro = is_data-id_centro ] )
            OR line_exists( it_buffer[ tipo_regra = 'AG' agregador = is_data-agregador  ] ).
              return = abap_true.
            ENDIF.
          WHEN 'MP'.
            IF line_exists( it_item[ tipo_regra = 'MP' mat_planejado = is_data-mat_planejado
                                     id_centro = is_data-id_centro ] )
            OR line_exists( it_buffer[ tipo_regra = 'MP' mat_planejado = is_data-mat_planejado ] ).
              return = abap_true.
            ENDIF.
          WHEN 'GP'.
            IF line_exists( it_item[ tipo_regra = 'GP' id_grupo_produto = is_data-id_grupo_produto
                                     id_centro = is_data-id_centro ] )
            OR line_exists( it_buffer[ tipo_regra = 'GP' id_grupo_produto = is_data-id_grupo_produto  ] ).
              return = abap_true.
            ENDIF.
*        when 'AG'.
        ENDCASE.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD material_transfer_exists.

    DATA(lt_item) = get_items( is_header ).

    CASE is_item-tipo_regra.
      WHEN 'GE'.
        IF line_exists( lt_item[ tipo_regra = 'GE' ] ).
          return = abap_true.
        ENDIF.
      WHEN 'AG'.
        IF line_exists( lt_item[ tipo_regra = 'AG'
                                 agregador = is_item-agregador ] ).
          return = abap_true.
        ENDIF.
      WHEN 'MP'.
        IF line_exists( lt_item[ tipo_regra = 'MP'
                                 mat_planejado = is_item-mat_planejado ] ).
          return = abap_true.
        ENDIF.
      WHEN 'MA'.
        IF line_exists( lt_item[ tipo_regra = 'MA'
                                 matnr = is_item-matnr ] ).
          return = abap_true.
        ENDIF.
      WHEN 'GP'.
        IF line_exists( lt_item[ tipo_regra = 'GP'
                                 id_grupo_produto = is_item-id_grupo_produto ] ).
          return = abap_true.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD merge_data.

    TYPES: BEGIN OF ts_matriz_abs,
             id_matriz_abast TYPE /qaps/matriz_abs-id_matriz_abast,
           END OF ts_matriz_abs.

    DATA lo_sdescr_main     TYPE REF TO cl_abap_structdescr.
    DATA lo_sdescr_append     TYPE REF TO cl_abap_structdescr.
    DATA lr_line       TYPE REF TO data.
    DATA lv_name TYPE string.
    DATA lv_name_prefixo TYPE string.
    DATA lv_name_sufixo TYPE string.
    DATA lt_matriz_abs TYPE TABLE OF ts_matriz_abs.

    FIELD-SYMBOLS: <ft>      TYPE ANY TABLE,
                   <fs_line> TYPE any.

    ASSIGN mt_data->* TO <ft>.
    ASSIGN ms_data->* TO <fs_line>.


    lo_sdescr_main ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/S_MATRIZ_ABASTECIMENTO' ).
    DATA(lt_components) = lo_sdescr_main->get_components( ).

    DELETE lt_components WHERE name = 'PERIODO'
                            OR name = 'MANDT'
                            OR name = 'VALOR'
                            OR name = 'PERCENTUAL'
                            OR name = ''.

    lo_sdescr_append ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/S_MATRIZ_ABASTECIMENTO' ).
    DATA(lt_comp_append) = lo_sdescr_append->get_components( ).

    APPEND LINES OF lt_comp_append  TO lt_components.

    lt_matriz_abs = VALUE #( FOR wa IN it_data
                            ( id_matriz_abast = wa-id_matriz_abast ) ).

    SORT lt_matriz_abs BY id_matriz_abast ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_matriz_abs COMPARING id_matriz_abast.

    DELETE lt_components WHERE as_include = 'X'.
    DATA(lv_lines) = lines( lt_components ).

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_variaveis) = lo_custo_elementar->get_variavel( ).

    LOOP AT lt_matriz_abs INTO DATA(ls_matriz_abs).

      DATA(ls_data) = it_data[ id_matriz_abast = ls_matriz_abs-id_matriz_abast ].

      DO lv_lines TIMES.
        lv_name = lt_components[ sy-index ]-name.
        ASSIGN COMPONENT lv_name OF STRUCTURE ls_data TO FIELD-SYMBOL(<fv_src>).
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_trg>).

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDDO.

      "Fill Value
      LOOP AT it_data INTO ls_data
        WHERE id_matriz_abast = ls_matriz_abs-id_matriz_abast.

        CHECK NOT ls_matriz_abs-id_matriz_abast IS INITIAL.

        DATA(ls_fcat) = it_fcat[ parameter1 = ls_data-periodo ].

        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.

        lv_name = ls_fcat-fieldname.

        ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE ls_data TO <fv_src>.
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

      "Fill Period
*      LOOP AT it_data INTO ls_data
*        WHERE id_var_input = ls_matriz_abs-id_matriz_abast.
*
*        CHECK NOT ls_matriz_abs-id_matriz_abast IS INITIAL.
*
*        ls_fcat = it_fcat[ parameter1 = ls_data-periodo ].
*
*        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.
*
*        lv_name = `PERIODO_` && lv_name_sufixo.
*        ASSIGN COMPONENT 'PERIODO' OF STRUCTURE ls_data TO <fv_src>.
*        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.
*
*        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.
*
*        <fv_trg> = <fv_src>.
*        UNASSIGN: <fv_src>, <fv_trg>.
*
*      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.

    ENDLOOP.

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


  METHOD set_distribuicao_inicial.

    SELECT DISTINCT id_item
      FROM /qaps/prem_item
      INTO TABLE @DATA(lt_itm).

    SELECT DISTINCT id_item, id_distribuicao
      FROM /qaps/prem_distr
      INTO TABLE @DATA(lt_distrib).

*    BREAK-POINT.

    LOOP AT lt_itm INTO DATA(ls_itm).

      DATA(lt_distrib_aux) = lt_distrib.
      DELETE lt_distrib_aux WHERE id_item <> ls_itm-id_item.

      CHECK lines( lt_distrib_aux ) = 1.
      UPDATE /qaps/prem_distr
      SET percentual = 100
      WHERE id_item = ls_itm-id_item.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_periodo.
    ms_periodo = is_periodo.
  ENDMETHOD.


  METHOD update_input.

    DATA lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_distribuicao = @it_changed_data-id_distribuicao
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @DATA(lt_update).

    LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_changed_data) = VALUE #( it_changed_data[ id_distribuicao = <fs>-id_distribuicao
                                               periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_changed_data IS INITIAL.

      <fs>-percentual = ls_changed_data-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/prem_distr FROM TABLE lt_update.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_parent = @it_changed_data-id_distribuicao
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @lt_update.

    LOOP AT lt_update ASSIGNING <fs>.

      ls_changed_data = VALUE #( it_changed_data[ id_distribuicao = <fs>-id_parent
                                                  periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_changed_data IS INITIAL.

      <fs>-percentual = ls_changed_data-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/prem_distr FROM TABLE lt_update.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_update
      WHERE id_parent = @lt_update-id_distribuicao
      AND   periodo      = @lt_update-periodo
      INTO TABLE @data(lt_update_children).

   LOOP AT lt_update_children ASSIGNING <fs>.

      data(ls_update) = VALUE #( lt_update[ id_distribuicao = <fs>-id_parent
                                            periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_update IS INITIAL.

      <fs>-percentual = ls_update-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    if lines( lt_update_children ) > 0.
      MODIFY /qaps/prem_distr FROM TABLE lt_update_children.
      commit WORK.
    endif.

  ENDMETHOD.


  METHOD update_trajeto.

    DATA lr_data TYPE REF TO data.
*    BREAK-POINT.
    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_prem_trajeto = @it_changed_data-id_prem_trajeto
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @DATA(lt_update).

    LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_changed_data) = VALUE #( it_changed_data[ id_prem_trajeto = <fs>-id_prem_trajeto
                                                        periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_changed_data IS INITIAL.

      <fs>-percentual = ls_changed_data-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/prem_traj FROM TABLE lt_update.

    "Herdeiros
    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_parent = @it_changed_data-id_prem_trajeto
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @lt_update.

    LOOP AT lt_update ASSIGNING <fs>.

      ls_changed_data = VALUE #( it_changed_data[ id_prem_trajeto = <fs>-id_parent
                                                  periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_changed_data IS INITIAL.

      <fs>-percentual = ls_changed_data-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/prem_traj FROM TABLE lt_update.
    commit work and wait.

    "Herdeiros do Herdeiros
    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @lt_update
      WHERE id_parent = @lt_update-id_prem_trajeto
      AND   periodo      = @lt_update-periodo
      INTO TABLE @data(lt_hedeiro).

    LOOP AT lt_hedeiro ASSIGNING <fs>.

      data(ls_update) = VALUE #( lt_update[ id_prem_trajeto = <fs>-id_parent
                                            periodo         = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_update IS INITIAL.

      <fs>-percentual = ls_update-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/prem_traj FROM TABLE lt_hedeiro.

  ENDMETHOD.
ENDCLASS.
