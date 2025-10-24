class /QAPS/CL_MDL_LOGISTICA definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods ACTIVE_CENTRO_PORTO
    importing
      !IT_DATA type /QAPS/T_CENTRO_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_CAIS
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_CENTRO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_CIDADE
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_FORNECEDOR
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_GRP_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_GRP_PLANTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_PONTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_REGIAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_TRAJETO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_TRECHO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_TRECHO_BY_TRAJETO
    importing
      !IS_TRAJETO type /QAPS/S_TRAJETO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DEACTIVE_CENTRO_PORTO
    importing
      !IT_DATA type /QAPS/T_CENTRO_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_CAIS
    importing
      !IT_DATA type /QAPS/T_CAIS
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_CENTRO
    importing
      !IT_DATA type /QAPS/T_CENTRO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_CIDADE
    importing
      !IT_DATA type /QAPS/T_CIDADE
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_CLIENTE
    importing
      !IT_DATA type /QAPS/T_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_FORNECEDOR
    importing
      !IT_DATA type /QAPS/T_FORNECEDOR
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_GRP_CLIENTE
    importing
      !IT_DATA type /QAPS/T_GRP_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_GRP_PLANTA
    importing
      !IT_DATA type /QAPS/T_GRP_PLANTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_PONTO
    importing
      !IT_DATA type /QAPS/T_PONTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_PORTO
    importing
      !IT_DATA type /QAPS/T_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_REGIAO
    importing
      !IT_DATA type /QAPS/T_REGIAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_TRECHO_BY_TRAJETO
    importing
      !IT_DATA type /QAPS/T_TRAJETO_TRECHO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_TRAJETO
    importing
      !IT_DATA type /QAPS/T_TRAJETO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_TRECHO
    importing
      !IT_DATA type /QAPS/T_TRECHO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_CAIS
    importing
      !IS_DATA type /QAPS/S_CAIS
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_CENTRO
    importing
      !IS_DATA type /QAPS/S_CENTRO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_CENTRO_PORTO
    importing
      !IS_DATA type /QAPS/S_CENTRO_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_CIDADE
    importing
      !IS_DATA type /QAPS/S_CIDADE
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_CLIENTE
    importing
      !IS_DATA type /QAPS/S_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_FORNECEDOR
    importing
      !IS_DATA type /QAPS/S_FORNECEDOR
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_GRP_CLIENTE
    importing
      !IS_DATA type /QAPS/S_GRP_CLIENTE
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_GRP_PLANTA
    importing
      !IS_DATA type /QAPS/S_GRP_PLANTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_PONTO
    importing
      !IS_DATA type /QAPS/S_PONTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_PORTO
    importing
      !IS_DATA type /QAPS/S_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_REGIAO
    importing
      !IS_DATA type /QAPS/S_REGIAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_TRAJETO
    importing
      !IS_DATA type /QAPS/S_TRAJETO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_TRECHO
    importing
      !IS_DATA type /QAPS/S_TRECHO
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_CAIS
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
      !IV_ID_CAIS type /QAPS/ED_ID_CAIS optional
    returning
      value(RETURN) type /QAPS/T_CAIS .
  methods GET_CENTROS
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_CENTRO .
  methods GET_CENTRO_PORTO
    importing
      !IV_WERKS type WERKS_D optional
      !IV_ID_PORTO type /QAPS/ED_ID_PORTO optional
      !IV_UPDATE type FLAG default ''
    returning
      value(RETURN) type /QAPS/T_CENTRO_PORTO .
  methods GET_CIDADES
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
      !IV_ID_CIDADE type /QAPS/ED_ID_CIDADE optional
    returning
      value(RETURN) type /QAPS/T_CIDADE .
  methods GET_CLIENTES
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_CLIENTE .
  methods GET_FORNECEDOR
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_FORNECEDOR .
  methods GET_GRP_CLIENTE
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_GRP_CLIENTE .
  methods GET_GRP_PLANTA
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RETURN) type /QAPS/T_GRP_PLANTA .
  methods GET_PONTOS
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
      !IV_ID_PONTO type /QAPS/ID_PONTO optional
    returning
      value(RETURN) type /QAPS/T_PONTO .
  methods GET_PORTOS
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
      !IV_ID_PORTO type /QAPS/ED_ID_PORTO optional
    returning
      value(RETURN) type /QAPS/T_PORTO .
  methods GET_REGIAO_BY_CODE
    importing
      !IV_CODIGO type /QAPS/ED_COD_REGIAO optional
    returning
      value(RETURN) type /QAPS/S_REGIAO .
  methods GET_REGIAO_BY_ID
    importing
      !IV_ID_REGIAO type /QAPS/ED_ID_REGIAO optional
    returning
      value(RETURN) type /QAPS/S_REGIAO .
  methods GET_REGIOES
    importing
      !IV_FORCE_UPDATE type ABAP_BOOL default ABAP_FALSE
      !IV_ID_REGIAO type /QAPS/ED_ID_REGIAO optional
    returning
      value(RETURN) type /QAPS/T_REGIAO .
  methods GET_TRAJETOS
    returning
      value(RETURN) type /QAPS/T_TRAJETO .
  methods GET_TRECHOS
    returning
      value(RETURN) type /QAPS/T_TRECHO .
  methods GET_TRECHOS_BY_TRAJETO
    importing
      !IS_DATA type /QAPS/S_TRAJETO
    returning
      value(RETURN) type /QAPS/T_TRAJETO_TRECHO .
  methods GET_VIEW_PONTO
    returning
      value(RETURN) type /QAPS/T_VIEW_PONTO .
  methods INITIALIZE_CENTRO_PORTO
    returning
      value(RETURN) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  data MT_CAIS type /QAPS/T_CAIS .
  data MT_CENTRO type /QAPS/T_CENTRO .
  data MT_CENTRO_PORTO type /QAPS/T_CENTRO_PORTO .
  data MT_CIDADE type /QAPS/T_CIDADE .
  data MT_CLIENTE type /QAPS/T_CLIENTE .
  data MT_FORNECEDOR type /QAPS/T_FORNECEDOR .
  data MT_GRP_CLIENTE type /QAPS/T_GRP_CLIENTE .
  data MT_GRP_PLANTA type /QAPS/T_GRP_PLANTA .
  data MT_PONTO type /QAPS/T_PONTO .
  data MT_PORTO type /QAPS/T_PORTO .
  data MT_REGIAO type /QAPS/T_REGIAO .

  methods GET_NEXT_COD_TRECHO
    returning
      value(RETURN) type /QAPS/ED_COD_TRECHO .
  methods CREATE_PONTO_BY_TYPE
    importing
      !IV_TIPO_PONTO type /QAPS/ED_TIPO_PONTO
      !IV_ID_EXTERNO type /QAPS/ID_EXTERNO
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_NEXT_NUMBER
    returning
      value(RETURN) type /QAPS/ID_GRUPO_PRODUTO .
  methods PREENCHER_MASTER_DATA
    changing
      !CS_DATA type /QAPS/MATERIAL .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods SYNC_PONTO .
ENDCLASS.



CLASS /QAPS/CL_MDL_LOGISTICA IMPLEMENTATION.


  METHOD active_centro_porto.

    DATA: lv_message TYPE string,
          lr_data    TYPE REF TO data.

    lv_message = TEXT-m02.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.

      SELECT *
        FROM /qaps/centro_por
        FOR ALL ENTRIES IN @it_data
        WHERE werks = @it_data-werks
        AND   id_porto = @it_data-id_porto
        INTO TABLE @DATA(lt_data).

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
        <fs_data>-ativo = 'X'.
        lr_data = REF #( <fs_data> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
      ENDLOOP.

      MODIFY /qaps/centro_por FROM TABLE lt_data.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD create_cais.

    DATA: ls_data    TYPE /qaps/s_cais,
          ls_entry   TYPE /qaps/cais,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CAIS_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CAIS    =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_cais = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/cais FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'I'
                          iv_id_externo = ls_entry-id_cais  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_centro.

    DATA: ls_data    TYPE /qaps/s_centro,
          ls_entry   TYPE /qaps/centro,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CENTRO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_centro = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/centro FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'W'
                          iv_id_externo = ls_entry-id_centro  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_cidade.

    DATA: ls_data    TYPE /qaps/s_cidade,
          ls_entry   TYPE /qaps/cidade,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CIDADE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_cidade = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/cidade FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'C'
                          iv_id_externo = ls_entry-id_cidade  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_cliente.

    DATA: ls_data    TYPE /qaps/s_cliente,
          ls_entry   TYPE /qaps/cliente,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CLIENTE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_cliente = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/cliente FROM ls_entry.

    CHECK sy-subrc IS INITIAL.
    create_ponto_by_type( iv_tipo_ponto = 'K'
                          iv_id_externo = ls_entry-id_cliente ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_fornecedor.

    DATA: ls_data    TYPE /qaps/s_fornecedor,
          ls_entry   TYPE /qaps/fornecedor,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_FORNECEDOR_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_fornecedor = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/fornecedor FROM ls_entry.

    CHECK sy-subrc IS INITIAL.
    create_ponto_by_type( iv_tipo_ponto = 'F'
                          iv_id_externo = ls_entry-id_fornecedor ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_grp_cliente.

    DATA: ls_data    TYPE /qaps/s_grp_cliente,
          ls_entry   TYPE /qaps/grp_cli,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_GRP_CLIENTE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_grp_cliente = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/grp_cli FROM ls_entry.
    COMMIT WORK and wait.
*    create_ponto_by_type( iv_tipo_ponto = 'G'
*                          iv_id_externo = ls_entry-id_grp_planta  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_grp_planta.

    DATA: ls_data    TYPE /qaps/s_grp_planta,
          ls_entry   TYPE /qaps/grp_planta,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_GRP_PLANTA_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_grp_planta = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/grp_planta FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'G'
                          iv_id_externo = ls_entry-id_grp_planta  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_ponto.

    DATA: ls_data    TYPE /qaps/s_ponto,
          ls_entry   TYPE /qaps/ponto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_PONTO_INPUT'
      EXPORTING
        iv_action  = 'C'
*       id_ponto   =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_ponto = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/ponto FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD create_ponto_by_type.

    DATA: ls_entry   TYPE /qaps/ponto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    ls_entry-id_ponto = cl_system_uuid=>create_uuid_x16_static( ).
    ls_entry-tipo_ponto = iv_tipo_ponto.
    ls_entry-id_externo = iv_id_externo.

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/ponto FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_porto.

    DATA: ls_data    TYPE /qaps/s_porto,
          ls_entry   TYPE /qaps/porto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_PORTO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_porto = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/porto FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'P'
                          iv_id_externo = ls_entry-id_porto  ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_regiao.

    DATA: ls_data    TYPE /qaps/s_regiao,
          ls_entry   TYPE /qaps/regiao_prc,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_REGIAO_INPUT'
      EXPORTING
        iv_action  = 'C'
*       id_regiao  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_regiao = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/regiao_prc FROM ls_entry.

    create_ponto_by_type( iv_tipo_ponto = 'R'
                          iv_id_externo = ls_entry-id_regiao  ).

    return = abap_true.


  ENDMETHOD.


  METHOD create_trajeto.

    DATA: ls_data    TYPE /qaps/s_trajeto,
          ls_entry   TYPE /qaps/trajeto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_TRAJETO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_trajeto = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/trajeto FROM ls_entry.
    commit work and WAIT.

    data(lo_rule) = new /qaps/cl_rule_sincronizacao( ).
    lo_rule->sincronizar_trajeto_create( ls_entry-id_trajeto ).

    return = abap_true.

  ENDMETHOD.


  METHOD create_trecho.

    DATA: ls_data    TYPE /qaps/s_trecho,
          ls_entry   TYPE /qaps/trecho,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_TRECHO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       ID_CIDADE  =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_trecho = cl_system_uuid=>create_uuid_x16_static( ).
    ls_entry-cod_trecho = get_next_cod_trecho( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/trecho FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD create_trecho_by_trajeto.

    DATA: lt_data    TYPE /qaps/t_trajeto_trecho,
          ls_entry   TYPE /qaps/traj_trech,
          lt_entry   TYPE TABLE OF /qaps/traj_trech,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_TRAJ_TRECHO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
        is_trajeto = is_trajeto
      IMPORTING
        et_data    = lt_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    SELECT *
      FROM /qaps/traj_trech
      WHERE id_trajeto = @is_trajeto-id_trajeto
      INTO TABLE @DATA(lt_conf).

    DATA(lv_lines) = lines( lt_conf ).

    LOOP AT lt_data INTO DATA(ls_data).

      CHECK NOT line_exists( lt_conf[ id_trecho = ls_data-id_trecho ] ).

      lv_lines = lv_lines + 1.

      ls_entry = VALUE /qaps/traj_trech(
                            id_traj_trecho = cl_system_uuid=>create_uuid_x16_static( )
                            id_trajeto     = ls_data-id_trajeto
                            id_trecho      = ls_data-id_trecho
                            ordem          = lv_lines ).

      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      APPEND ls_entry TO lt_entry.

    ENDLOOP.

    IF lines( lt_entry ) > 0.
      MODIFY /qaps/traj_trech FROM TABLE lt_entry.
    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD deactive_centro_porto.

    DATA: lv_message TYPE string,
          lr_data    TYPE REF TO data.

    lv_message = TEXT-m03.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.

      SELECT *
        FROM /qaps/centro_por
        FOR ALL ENTRIES IN @it_data
        WHERE werks = @it_data-werks
        AND   id_porto = @it_data-id_porto
        INTO TABLE @DATA(lt_data).

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
        <fs_data>-ativo = ''.
        lr_data = REF #( <fs_data> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
      ENDLOOP.

      MODIFY /qaps/centro_por FROM TABLE lt_data.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_cais.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/cais WHERE id_cais = ls_data-id_cais.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_centro.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/centro WHERE werks = ls_data-werks.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_cidade.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/cidade WHERE id_cidade = ls_data-id_cidade.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_cliente.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/cliente WHERE id_cliente = ls_data-id_cliente.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_fornecedor.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/fornecedor WHERE id_fornecedor = ls_data-id_fornecedor.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_grp_cliente.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/grp_cli WHERE id_grp_cliente = ls_data-id_grp_cliente.
      ENDLOOP.

      COMMIT WORK AND WAIT.

*      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_grp_planta.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/grp_planta WHERE id_grp_planta = ls_data-id_grp_planta.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_ponto.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/ponto WHERE id_ponto = ls_data-id_ponto.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_porto.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/porto WHERE id_porto = ls_data-id_porto.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_regiao.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/regiao_prc WHERE id_regiao = ls_data-id_regiao.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_trajeto.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.

      DATA(lo_rule) = NEW /qaps/cl_rule_sincronizacao( ).
      LOOP AT it_data INTO DATA(ls_data).
        DATA(lv_return) =
          lo_rule->sincronizar_trajeto_delete( iv_id_trajeto = ls_data-id_trajeto
                                               iv_check = COND #( WHEN sy-tabix = 1
                                                                  THEN abap_true
                                                                  ELSE abap_false ) ).

        IF lv_return = abap_false.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
          RETURN.
        ENDIF.

        DELETE FROM /qaps/traj_trech WHERE id_trajeto = ls_data-id_trajeto.
        DELETE FROM /qaps/trajeto    WHERE id_trajeto = ls_data-id_trajeto.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_trecho.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/trecho WHERE id_trecho = ls_data-id_trecho.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      sync_ponto( ).

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_trecho_by_trajeto.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/traj_trech WHERE id_traj_trecho = ls_data-id_traj_trecho.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      ls_data = it_data[ 1 ].

      SELECT *
        FROM /qaps/traj_trech
        WHERE id_trajeto = @ls_data-id_trajeto
        INTO TABLE @DATA(lt_traj_trech).

      SORT lt_traj_trech BY ordem.

      LOOP AT lt_traj_trech ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-ordem = sy-tabix.
      ENDLOOP.

      MODIFY /qaps/traj_trech FROM TABLE lt_traj_trech.

      COMMIT WORK.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD edit_cais.

    DATA: ls_data    TYPE /qaps/s_cais,
          ls_entry   TYPE /qaps/cais,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CAIS_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/cais FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_centro.

    DATA: ls_data    TYPE /qaps/s_centro,
          ls_entry   TYPE /qaps/centro,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CENTRO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/centro FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_centro_porto.

    DATA: ls_data    TYPE /qaps/s_centro_porto,
          ls_entry   TYPE /qaps/centro_por,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

*    CALL FUNCTION '/QAPS/FM_CAIS_INPUT'
*      DESTINATION 'NONE'
*      EXPORTING
*        iv_action  = 'E'
*        is_data = is_data
*      IMPORTING
*        es_data    = ls_data
*        es_message = ls_message.
*
*
*    IF ls_message-type = 'E'.
*      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
*      RETURN.
*    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/centro_por FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_cidade.

    DATA: ls_data    TYPE /qaps/s_cidade,
          ls_entry   TYPE /qaps/cidade,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CIDADE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).


    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/cidade FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_cliente.

    DATA: ls_data    TYPE /qaps/s_cliente,
          ls_entry   TYPE /qaps/cliente,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_CLIENTE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/cliente FROM ls_entry.

    CHECK sy-subrc IS INITIAL.
    create_ponto_by_type( iv_tipo_ponto = 'K'
                          iv_id_externo = ls_entry-id_cliente ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_fornecedor.

    DATA: ls_data    TYPE /qaps/s_fornecedor,
          ls_entry   TYPE /qaps/fornecedor,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_FORNECEDOR_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/fornecedor FROM ls_entry.

    CHECK sy-subrc IS INITIAL.
    create_ponto_by_type( iv_tipo_ponto = 'F'
                          iv_id_externo = ls_entry-id_fornecedor ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_grp_cliente.

    DATA: ls_data    TYPE /qaps/s_grp_cliente,
          ls_entry   TYPE /qaps/grp_cli,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_GRP_CLIENTE_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/grp_cli FROM ls_entry.

*    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_grp_planta.

    DATA: ls_data    TYPE /qaps/s_grp_planta,
          ls_entry   TYPE /qaps/grp_planta,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_GRP_PLANTA_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/grp_planta FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_ponto.

    DATA: ls_data    TYPE /qaps/s_ponto,
          ls_entry   TYPE /qaps/ponto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_PONTO_INPUT'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/ponto FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_porto.

    DATA: ls_data    TYPE /qaps/s_porto,
          ls_entry   TYPE /qaps/porto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_PORTO_INPUT'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/porto FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD edit_regiao.

    DATA: ls_data    TYPE /qaps/s_regiao,
          ls_entry   TYPE /qaps/regiao_prc,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_REGIAO_INPUT'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/regiao_prc FROM ls_entry.

    sync_ponto( ).

    return = abap_true.


  ENDMETHOD.


  METHOD edit_trajeto.

    DATA: ls_data    TYPE /qaps/s_trajeto,
          ls_entry   TYPE /qaps/trajeto,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_TRAJETO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_data    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
*    ls_entry-id_trecho = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/trajeto FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD edit_trecho.

    DATA: ls_data    TYPE /qaps/s_trecho,
          ls_entry   TYPE /qaps/trecho,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

*    CALL FUNCTION '/QAPS/FM_trecho_INPUT'
*      DESTINATION 'NONE'
*      EXPORTING
*        iv_action  = 'C'
**       ID_CIDADE  =
*      IMPORTING
*        es_data    = ls_data
*        es_message = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_trecho = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/trecho FROM ls_entry.

    sync_ponto( ).

    return = abap_true.

  ENDMETHOD.


  METHOD get_cais.

    IF mt_cais IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/cais
        INTO CORRESPONDING FIELDS OF TABLE @mt_cais.

      DATA(lt_portos) = get_portos( ).
      DATA(lt_cidade) = get_cidades( ).

      LOOP AT mt_cais ASSIGNING FIELD-SYMBOL(<fs>).

        DATA(ls_porto) = VALUE #( lt_portos[ id_porto = <fs>-id_porto ] OPTIONAL ).
        <fs>-cod_porto = ls_porto-cod_porto.
        <fs>-dsc_porto = ls_porto-porto.

        IF NOT <fs>-id_cidade IS INITIAL.
          DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
          <fs>-dsc_uf = ls_cidade-uf.
          <fs>-dsc_cidade = ls_cidade-cidade.
        ENDIF.

      ENDLOOP.

    ENDIF.

    SORT mt_cais BY cod_cais ASCENDING cod_porto ASCENDING.

    return = mt_cais.

  ENDMETHOD.


  METHOD get_centros.

    IF lines( mt_centro ) = 0 OR iv_force_update = abap_true.

      SELECT *
        FROM /qaps/centro
        INTO CORRESPONDING FIELDS OF TABLE @mt_centro.

      DATA(lt_grp_planta) = get_grp_planta( ).
      DATA(lt_cidade) = get_cidades( ).

      LOOP AT mt_centro ASSIGNING FIELD-SYMBOL(<fs>).

        TRY.
            <fs>-dsc_werks = /qaps/cl_helper_text=>get_werks_text( <fs>-werks ).
          CATCH /qaps/cx_general.  "
        ENDTRY.

        IF NOT <fs>-id_grp_planta IS INITIAL.
          DATA(ls_grp_planta) = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_grp_planta ] OPTIONAL ).
          <fs>-cod_grp_planta = ls_grp_planta-codigo.
          <fs>-dsc_grp_planta = ls_grp_planta-descricao.
        ENDIF.

        IF NOT <fs>-id_cidade IS INITIAL.
          DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
          <fs>-dsc_uf = ls_cidade-uf.
          <fs>-dsc_cidade = ls_cidade-cidade.
        ENDIF.

      ENDLOOP.

    ENDIF.

    SORT mt_centro BY werks ASCENDING.

    return = mt_centro.

  ENDMETHOD.


  METHOD get_centro_porto.

    IF mt_centro_porto IS INITIAL OR iv_update = abap_true.
      SELECT *
          FROM /qaps/centro_por
          INTO CORRESPONDING FIELDS OF TABLE @mt_centro_porto.
    ENDIF.

    return = mt_centro_porto.

    IF NOT iv_werks IS INITIAL.
      DELETE return WHERE werks <> iv_werks.
    ENDIF.

    DATA(lt_portos) = get_portos( ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      TRY.
          DATA(ls_porto) = lt_portos[ id_porto = <fs>-id_porto ].
          <fs>-dsc_werks = /qaps/cl_helper_text=>get_werks_text( <fs>-werks ).
          <fs>-cod_porto = ls_porto-cod_porto.
          <fs>-dsc_porto = ls_porto-porto.
        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

    ENDLOOP.

    DELETE return WHERE cod_porto IS INITIAL.

    SORT return  BY werks cod_porto ASCENDING.

  ENDMETHOD.


  METHOD get_cidades.

    IF mt_cidade IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/cidade
        INTO CORRESPONDING FIELDS OF TABLE @mt_cidade.

      CHECK lines( mt_cidade ) > 0.

      DATA(lt_regiao) = get_regioes( ).

      LOOP AT mt_cidade ASSIGNING FIELD-SYMBOL(<fs>).

        CHECK NOT <fs>-id_regiao IS INITIAL.
        DATA(ls_regiao) = lt_regiao[ id_regiao = <fs>-id_regiao ].
        <fs>-cod_regiao = ls_regiao-codigo.
        <fs>-dsc_regiao = ls_regiao-descricao.

      ENDLOOP.

    ENDIF.

    SORT mt_cidade BY uf ASCENDING cidade ASCENDING.

    return = mt_cidade.

  ENDMETHOD.


  METHOD get_clientes.

    IF lines( mt_cliente ) = 0 OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/cliente
        INTO CORRESPONDING FIELDS OF TABLE @mt_cliente.

      DATA(lt_cidade) = get_cidades( ).
      DATA(lt_grp_cliente) = get_grp_cliente( ).

      LOOP AT mt_cliente ASSIGNING FIELD-SYMBOL(<fs>).
        TRY.
            <fs>-name1 = /qaps/cl_helper_text=>get_kunnr_text( iv_kunnr = <fs>-kunnr ).
          CATCH /qaps/cx_general.  "
        ENDTRY.

        IF NOT <fs>-id_cidade IS INITIAL.
          DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
          <fs>-dsc_uf = ls_cidade-uf.
          <fs>-dsc_cidade = ls_cidade-cidade.
        ENDIF.

        IF <fs>-id_grp_cliente <> mc_guid_null.
          DATA(ls_grp_cliente) = lt_grp_cliente[ id_grp_cliente = <fs>-id_grp_cliente ].
          <fs>-cod_grp_cliente = ls_grp_cliente-codigo.
          <fs>-dsc_grp_cliente = ls_grp_cliente-descricao.
        ENDIF.

      ENDLOOP.

    ENDIF.

    return = mt_cliente.

  ENDMETHOD.


  METHOD get_fornecedor.

    IF lines( mt_fornecedor ) = 0 OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/fornecedor
        INTO CORRESPONDING FIELDS OF TABLE @mt_fornecedor.

      DATA(lt_cidade) = get_cidades( ).

      LOOP AT mt_fornecedor ASSIGNING FIELD-SYMBOL(<fs>).
        TRY.
            <fs>-name1 = /qaps/cl_helper_text=>get_lifnr_text( iv_lifnr = <fs>-lifnr ).
          CATCH /qaps/cx_general.  "
        ENDTRY.

        IF NOT <fs>-id_cidade IS INITIAL.
          DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
          <fs>-dsc_uf = ls_cidade-uf.
          <fs>-dsc_cidade = ls_cidade-cidade.
        ENDIF.

      ENDLOOP.

    ENDIF.

    return = mt_fornecedor.

  ENDMETHOD.


  METHOD get_grp_cliente.

    IF mt_grp_cliente IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/grp_cli
        INTO CORRESPONDING FIELDS OF TABLE @mt_grp_cliente.

    ENDIF.

    SORT mt_grp_cliente BY codigo ASCENDING.

    return = mt_grp_cliente.

  ENDMETHOD.


  METHOD get_grp_planta.

    IF mt_grp_planta IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/grp_planta
        INTO CORRESPONDING FIELDS OF TABLE @mt_grp_planta.
    ENDIF.

    SORT mt_grp_planta BY codigo ASCENDING.

    return = mt_grp_planta.

  ENDMETHOD.


  METHOD get_next_cod_trecho.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = '/QAPS/TRCH'
*       QUANTITY    = '1'
*       SUBOBJECT   = ' '
*       TOYEAR      = '0000'
*       IGNORE_BUFFER                 = ' '
      IMPORTING
        number      = return
*       QUANTITY    =
*       RETURNCODE  =
*     EXCEPTIONS
*       INTERVAL_NOT_FOUND            = 1
*       NUMBER_RANGE_NOT_INTERN       = 2
*       OBJECT_NOT_FOUND              = 3
*       QUANTITY_IS_0                 = 4
*       QUANTITY_IS_NOT_1             = 5
*       INTERVAL_OVERFLOW             = 6
*       BUFFER_OVERFLOW               = 7
*       OTHERS      = 8
      .

  ENDMETHOD.


  METHOD get_next_number.

    return = cl_system_uuid=>create_uuid_x16_static( ).
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr = '01'
*        object      = '/QAPS/GRPP'
**       QUANTITY    = '1'
**       SUBOBJECT   = ' '
**       TOYEAR      = '0000'
**       IGNORE_BUFFER                 = ' '
*      IMPORTING
*        number      = return
**       QUANTITY    =
**       RETURNCODE  =
**     EXCEPTIONS
**       INTERVAL_NOT_FOUND            = 1
**       NUMBER_RANGE_NOT_INTERN       = 2
**       OBJECT_NOT_FOUND              = 3
**       QUANTITY_IS_0                 = 4
**       QUANTITY_IS_NOT_1             = 5
**       INTERVAL_OVERFLOW             = 6
**       BUFFER_OVERFLOW               = 7
**       OTHERS      = 8
*      .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


  ENDMETHOD.


  METHOD get_pontos.

*    IF mt_ponto IS INITIAL.

    SELECT *
      FROM /qaps/v_ponto
      INTO CORRESPONDING FIELDS OF TABLE @mt_ponto.

*      DATA(lt_cidade) = get_cidades( ).
*      DATA(lt_cais) = get_cais( ).
*      DATA(lt_tipo_ponto) = /qaps/cl_helper_text=>get_domain_values( '/QAPS/D_TIPO_PONTO' ).
*
*      LOOP AT mt_ponto ASSIGNING FIELD-SYMBOL(<fs>).
*
*        DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
*        <fs>-dsc_cidade = ls_cidade-cidade.
*        <fs>-dsc_uf = ls_cidade-uf.
*
*        <fs>-dsc_tipo_ponto = VALUE #( lt_tipo_ponto[ domvalue_l = <fs>-tipo_ponto ]-ddtext OPTIONAL ).
*
*        CASE <fs>-tipo_ponto.
*          WHEN 'W'.
*            <fs>-cod_ponto = <fs>-werks.
*            <fs>-dsc_ponto = /qaps/cl_helper_text=>get_werks_text( <fs>-werks ).
*          WHEN 'C'.
*            <fs>-cod_ponto = <fs>-kunnr.
*            <fs>-dsc_ponto = /qaps/cl_helper_text=>get_kunnr_text( <fs>-kunnr ).
*          WHEN 'F'.
*            <fs>-cod_ponto = <fs>-lifnr.
*            <fs>-dsc_ponto = /qaps/cl_helper_text=>get_lifnr_text( <fs>-lifnr ).
**        WHEN 'P'.
**          <fs>-cod_ponto = <fs>-cod_porto.
**          <fs>-dsc_ponto = <fs>-porto.
*          WHEN 'I'.
*            DATA(ls_cais) = VALUE #( lt_cais[ id_cais = <fs>-id_cais ] OPTIONAL ).
*            <fs>-cod_cais = ls_cais-cod_cais.
*            <fs>-dsc_cais = ls_cais-cais.
*
*            <fs>-cod_ponto = ls_cais-cod_cais.
*            <fs>-dsc_ponto = ls_cais-cais.
*        ENDCASE.
*
*      ENDLOOP.
*
*    ENDIF.

    return = mt_ponto.

  ENDMETHOD.


  METHOD get_portos.

*    BREAK c060863.
    IF mt_porto IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/porto
        INTO CORRESPONDING FIELDS OF TABLE @mt_porto.

      DATA(lt_cidade) = get_cidades( ).
      DATA(lt_tipo_ponto) = /qaps/cl_helper_text=>get_domain_values( '/QAPS/D_TIPO_PONTO' ).

      LOOP AT mt_porto ASSIGNING FIELD-SYMBOL(<fs>).

        DATA(ls_cidade) = VALUE #( lt_cidade[ id_cidade = <fs>-id_cidade ] OPTIONAL ).
        <fs>-dsc_cidade = ls_cidade-cidade.
        <fs>-dsc_uf = ls_cidade-uf.

      ENDLOOP.

    ENDIF.

    SORT mt_porto BY cod_porto ASCENDING.

    return = mt_porto.

  ENDMETHOD.


  METHOD get_regiao_by_code.

    SELECT SINGLE *
      FROM /qaps/regiao_prc
      WHERE codigo = @iv_codigo
      INTO CORRESPONDING FIELDS OF @return.

  ENDMETHOD.


  METHOD get_regiao_by_id.

    SELECT SINGLE *
      FROM /qaps/regiao_prc
      WHERE id_regiao = @iv_id_regiao
      INTO CORRESPONDING FIELDS OF @return.

  ENDMETHOD.


  METHOD get_regioes.

    IF mt_regiao IS INITIAL OR iv_force_update = abap_true.
      SELECT *
        FROM /qaps/regiao_prc
        INTO CORRESPONDING FIELDS OF TABLE @mt_regiao.
    ENDIF.

    SORT mt_regiao BY codigo ASCENDING.

    return = mt_regiao.

  ENDMETHOD.


  METHOD GET_TRAJETOS.

    SELECT *
      FROM /qaps/trajeto
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @return
      WHERE id_ponto = @return-id_origem
      INTO TABLE @DATA(lt_ponto).

    SELECT *
     FROM /qaps/v_ponto
     FOR ALL ENTRIES IN @return
     WHERE id_ponto = @return-id_destino
     APPENDING TABLE @lt_ponto.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(ls_origem) = VALUE #( lt_ponto[ id_ponto = <fs>-id_origem ] OPTIONAL ).
      <fs>-dsc_tipo_origem = ls_origem-dsc_tipo_ponto.
      <fs>-dsc_origem  = |{ ls_origem-codigo ALPHA = out }|.
      <fs>-origem  = ls_origem-descricao.

      DATA(ls_destino) = VALUE #( lt_ponto[ id_ponto = <fs>-id_destino ] OPTIONAL ).
      <fs>-dsc_tipo_destino = ls_destino-dsc_tipo_ponto.
      <fs>-dsc_destino = |{ ls_destino-codigo  ALPHA = out }|.
      <fs>-destino    = ls_destino-descricao.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_trechos.

    SELECT *
      FROM /qaps/trecho
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @return
      WHERE id_ponto = @return-id_origem
      INTO TABLE @DATA(lt_ponto).

    SELECT *
     FROM /qaps/v_ponto
     FOR ALL ENTRIES IN @return
     WHERE id_ponto = @return-id_destino
     APPENDING TABLE @lt_ponto.

    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( '/QAPS/D_MODAL' ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(ls_origem) = VALUE #( lt_ponto[ id_ponto = <fs>-id_origem ] OPTIONAL ).
      <fs>-dsc_tipo_origem = ls_origem-dsc_tipo_ponto.
      <fs>-dsc_origem  = |{ ls_origem-codigo ALPHA = out }|.
      <fs>-origem  = ls_origem-descricao.

      DATA(ls_destino) = VALUE #( lt_ponto[ id_ponto = <fs>-id_destino ] OPTIONAL ).
      <fs>-dsc_tipo_destino = ls_destino-dsc_tipo_ponto.
      <fs>-dsc_destino = |{ ls_destino-codigo  ALPHA = out }|.
      <fs>-destino    = ls_destino-descricao.

      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_trechos_by_trajeto.

    SELECT *
      FROM /qaps/traj_trech
      WHERE id_trajeto = @is_data-id_trajeto
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    DATA(lt_trecho) = get_trechos( ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(lv_ordem) = <fs>-ordem.
      TRY.
          DATA(ls_trecho) = lt_trecho[ id_trecho = <fs>-id_trecho ].

          <fs>-id_trecho          =  ls_trecho-id_trecho.
          <fs>-cod_trecho         =  ls_trecho-cod_trecho.
          <fs>-created_by         =  ls_trecho-created_by.
          <fs>-created_in         =  ls_trecho-created_in.
          <fs>-created_on         =  ls_trecho-created_on.
          <fs>-modified_by        =  ls_trecho-modified_by.
          <fs>-modified_in        =  ls_trecho-modified_in.
          <fs>-modified_on        =  ls_trecho-modified_on.
          <fs>-tipo_origem        =  ls_trecho-tipo_origem.
          <fs>-id_origem          =  ls_trecho-id_origem.
          <fs>-tipo_destino       =  ls_trecho-tipo_destino.
          <fs>-id_destino         =  ls_trecho-id_destino.
          <fs>-id_modal           =  ls_trecho-id_modal.
          <fs>-tempo_deslocamento =  ls_trecho-tempo_deslocamento.
          <fs>-distancia          =  ls_trecho-distancia.
          <fs>-dsc_modal          =  ls_trecho-dsc_modal.
          <fs>-dsc_tipo_origem    =  ls_trecho-dsc_tipo_origem.
          <fs>-dsc_origem         =  ls_trecho-dsc_origem.
          <fs>-origem             =  ls_trecho-origem.
          <fs>-dsc_tipo_destino   =  ls_trecho-dsc_tipo_destino.
          <fs>-dsc_destino        =  ls_trecho-dsc_destino.
          <fs>-destino            =  ls_trecho-destino.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDLOOP.

    SORT return BY ordem.

  ENDMETHOD.


  METHOD get_view_ponto.

    SELECT *
      FROM /qaps/v_ponto
      INTO TABLE @return.

  ENDMETHOD.


  METHOD initialize_centro_porto.

    DATA: ls_data  TYPE /qaps/s_centro_porto,
          lt_entry TYPE TABLE OF /qaps/centro_por,
          lr_data  TYPE REF TO data.

    SELECT *
      FROM /qaps/centro
      INTO TABLE @DATA(lt_centro).

    SELECT *
      FROM /qaps/porto
      INTO TABLE @DATA(lt_porto).

    SELECT *
      FROM /qaps/centro_por
      INTO TABLE @DATA(lt_centro_por).

    LOOP AT lt_centro INTO DATA(ls_centro).

      LOOP AT lt_porto INTO DATA(ls_porto).

        CHECK NOT line_exists( lt_centro_por[ werks = ls_centro-werks
                                              id_porto = ls_porto-id_porto ] ).

        APPEND VALUE #( werks = ls_centro-werks
                        id_porto = ls_porto-id_porto ) TO lt_entry.

      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_entry ASSIGNING FIELD-SYMBOL(<fs_entry>).

      lr_data = REF #( <fs_entry> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/centro_por FROM TABLE lt_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD preencher_master_data.

    DATA: lr_atnam  TYPE RANGE OF atnam,
          lv_object TYPE ausp-objek.

    DATA:lt_class      TYPE wrf_class_tty,
         lt_objectdata TYPE rihclobjdat_tab.

    lv_object = cs_data-matnr.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
*       class              = 'CHEMIC_PHYS_CHARAC'
        classtext          = 'X'
        classtype          = 'Z02'
*       CLINT              = 0
*       FEATURES           = 'X'
*       LANGUAGE           = SY-LANGU
        object             = lv_object
*       OBJECTTABLE        = ' '
*       KEY_DATE           = SY-DATUM
*       INITIAL_CHARACT    = 'X'
*       NO_VALUE_DESCRIPT  =
*       CHANGE_SERVICE_CLF = 'X'
*       INHERITED_CHAR     = ' '
*       CHANGE_NUMBER      = ' '
      TABLES
        t_class            = lt_class
        t_objectdata       = lt_objectdata
*       I_SEL_CHARACTERISTIC       =
*       T_NO_AUTH_CHARACT  =
      EXCEPTIONS
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    BREAK-POINT.

    IF lines( lt_objectdata ) > 0.
      cs_data-agregador = VALUE #( lt_objectdata[ smbez = 'AGREGADOR' ]-ausp1 OPTIONAL ).
      cs_data-classif_perigosa = VALUE #( lt_objectdata[ smbez = 'CLASSIFICACAO PERIGOSA' ]-ausp1 OPTIONAL ).

    ENDIF.

    SELECT *
      FROM pgmi
      WHERE nrmit = @cs_data-matnr
      INTO TABLE @DATA(lt_pgmi).

    IF lines( lt_pgmi ) > 0.
      cs_data-mat_planejado = lt_pgmi[ 1 ]-prgrp.
    ENDIF.

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


  METHOD sync_ponto.

    DATA lr_delete TYPE RANGE OF guid16.
    DATA: lt_ponto TYPE TABLE OF /qaps/ponto,
          lr_data  TYPE REF TO data.

    "Deleção
    SELECT *
        FROM /qaps/v_sync_del
*        WHERE src_codigo = ''
        INTO TABLE @DATA(lt_data_del).

*    BREAK-POINT.
    DELETE lt_data_del WHERE src_codigo <> ''.

    lr_delete = VALUE #(  FOR wa IN lt_data_del
                          ( sign = 'I' option = 'EQ' low = wa-id_ponto ) ).

    CHECK lines(  lr_delete  ) > 0.

    DELETE FROM /qaps/ponto WHERE id_ponto IN lr_delete.
    COMMIT WORK.

    "Inserção
    SELECT *
       FROM /qaps/v_sync_ins
*        WHERE src_codigo = ''
       INTO TABLE @DATA(lt_data_ins).

    LOOP AT lt_data_ins INTO DATA(ls_data_ins).

      APPEND INITIAL LINE TO lt_ponto ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-id_ponto = cl_system_uuid=>create_uuid_x16_static( ).
      <fs>-id_externo = ls_data_ins-id.
      <fs>-tipo_ponto = ls_data_ins-tipo_ponto.

      lr_data = ref #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    CHECK lines( lt_ponto ) > 0.
    MODIFY /qaps/ponto FROM TABLE lt_ponto.

  ENDMETHOD.
ENDCLASS.
