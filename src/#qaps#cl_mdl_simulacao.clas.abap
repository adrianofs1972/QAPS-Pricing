class /QAPS/CL_MDL_SIMULACAO definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods SET_STATUS
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_STATUS type /QAPS/ED_STATUS_SIMUL
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_SIMULACAO_ALL
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods SET_PERIODO
    returning
      value(RETURN) type /QAPS/S_PERIODO .
  methods GET_SINGLE_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
    returning
      value(RETURN) type /QAPS/S_TP_LISTA .
  methods GET_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
    returning
      value(RETURN) type /QAPS/T_TP_LISTA .
  methods ASSIGN_STD_PRODUCAO
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IS_DATA type /QAPS/S_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_SIMULACAO
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IS_DATA type /QAPS/S_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods COPY_SIMULACAO
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IS_DATA type /QAPS/S_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_SIMULACAO_BY_ID
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type /QAPS/S_SIMULACAO .
  methods GET_SIMULACAO
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods CREATE_SIMULACAO
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods UNASSIGN_STD_PRODUCAO
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_SIMULACAO
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF ts_from_to,
           id_from TYPE sysuuid_x16,
           id_to   TYPE sysuuid_x16,
         END OF ts_from_to .
  types:
    tt_from_to TYPE TABLE OF ts_from_to .

  data MS_PERIODO type /QAPS/S_PERIODO .

  methods CHECK_STATUS_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_STATUS type /QAPS/ED_STATUS_SIMUL
    returning
      value(RETURN) type ABAP_BOOL .
  methods COPY_SIMULACAO_PREM_TRAJETO
    importing
      !IT_PREM_TRAJETO type /QAPS/T_PREM_TRAJ
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO .
  methods COPY_SIMULACAO_PREM_DISTRB
    importing
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO .
  methods COPY_SIMULACAO_MATRIZ_DISTRIB
    importing
      !IT_DISTRIBUICAO type /QAPS/T_MATRIZ_DST
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO .
  methods DELETE_SIMULACAO_MATRIZ
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO .
  methods DELETE_SIMULACAO_PREMISSA
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO .
  methods GET_NEXT_ID
    returning
      value(RETURN) type /QAPS/ED_ID_SIMULACAO .
  methods GET_REFERENCIAS
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods COPY_SIMULACAO_PREMISSA
    importing
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO
      !IT_HEADER_MTZ_REF type TT_FROM_TO
      !IT_ITEM_MTZ_REF type TT_FROM_TO
      !IT_DISTRIB_MTZ_REF type TT_FROM_TO
      !IT_INPUTS_REF type TT_FROM_TO .
  methods COPY_SIMULACAO_MATRIZ
    importing
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO
      !IT_INPUTS_REF type TT_FROM_TO
    exporting
      !ET_HEADER_MTZ_REF type TT_FROM_TO
      !ET_ITEM_MTZ_REF type TT_FROM_TO
      !ET_DISTRIB_MTZ_REF type TT_FROM_TO .
  methods COPY_SIMULACAO_INPUTS
    importing
      !IS_SIMULACAO_FROM type /QAPS/S_SIMULACAO
      !IS_SIMULACAO_TO type /QAPS/S_SIMULACAO
    exporting
      !ET_INPUTS_REF type TT_FROM_TO .
  methods COPY_SIMULACAO_OBJECTS
    importing
      !IV_ID_SIMULACAO_FROM type /QAPS/ED_ID_SIMULACAO
      !IV_ID_SIMULACAO_TO type /QAPS/ED_ID_SIMULACAO .
ENDCLASS.



CLASS /QAPS/CL_MDL_SIMULACAO IMPLEMENTATION.


  METHOD assign_std_producao.

    DATA lv_message TYPE string.
    DATA:ls_entry           TYPE /qaps/simulacao,
         lv_id_std_producao TYPE /qaps/std_prd_h-id_std_producao,
         ls_message         TYPE bapiret2,
         lr_data            TYPE REF TO data.

    IF check_status_simulacao( iv_id_simulacao = is_data-id_simulacao
                               iv_status = 'A' ) = abap_false.
      MESSAGE 'Operação não permitida em simulação no status atual' TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

    lv_message = TEXT-m04.

    IF question( lv_message ) = abap_true.

      CALL FUNCTION '/QAPS/FM_STD_PROD_SEL_INPUT'
        IMPORTING
          ev_id_std_producao = lv_id_std_producao
          es_message         = ls_message.

      IF ls_message-type = 'E'.
        MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
        RETURN.
      ENDIF.

      ls_entry = CORRESPONDING #( is_data ).
      ls_entry-id_std_producao = lv_id_std_producao.

      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      MODIFY /qaps/simulacao FROM ls_entry.
      COMMIT WORK AND WAIT.

      return = abap_true.

      DATA(lo_rule_sincronizacao) = NEW /qaps/cl_rule_sincronizacao( ).
      lo_rule_sincronizacao->sincronizar_std_prd_assign( iv_id_tp_lista = iv_id_tp_lista
                                                         is_data = ls_entry ).

    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD check_status_simulacao.

    DATA(ls_simulacao) = get_simulacao_by_id( iv_id_simulacao ).

    IF ls_simulacao-status = iv_status.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD copy_simulacao.

    DATA: ls_data    TYPE /qaps/s_simulacao,
          ls_entry   TYPE /qaps/simulacao,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_SIMULACAO_INPUT'
      EXPORTING
        iv_action      = 'P'
        iv_id_tp_lista = iv_id_tp_lista
        is_data        = is_data
      IMPORTING
        es_data        = ls_data
        es_message     = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_simulacao = get_next_id( ).
    ls_entry-status = 'A'.

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/simulacao FROM ls_entry.

    copy_simulacao_objects(
      EXPORTING
        iv_id_simulacao_from = is_data-id_simulacao
        iv_id_simulacao_to   = ls_entry-id_simulacao ).

    return = abap_true.

  ENDMETHOD.


  METHOD copy_simulacao_inputs.

    DATA lt_data TYPE TABLE OF /qaps/var_input.
    DATA ls_to TYPE /qaps/var_input.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    SELECT DISTINCT id_simulacao, id_var_input, MAX( periodo ) AS last_periodo
      FROM /qaps/var_input
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      GROUP BY id_simulacao, id_var_input
      INTO TABLE @DATA(lt_from_index).

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      INTO TABLE @DATA(lt_from).

*    break c060863.
    LOOP AT lt_from_index INTO DATA(ls_from_index).

      REFRESH lt_data.

      DATA(ls_from) = lt_from[ id_simulacao = ls_from_index-id_simulacao
                               id_var_input = ls_from_index-id_var_input
                               periodo      = ls_from_index-last_periodo ].

      DATA(lv_periodo_inicial) = is_simulacao_to-periodo_inicial.

      ls_periodo-year = is_simulacao_to-periodo_inicial(4).
      ls_periodo-month = is_simulacao_to-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = is_simulacao_to-periodo_inicial
          final   = is_simulacao_to-periodo_final    ).

      ls_to-id_simulacao = is_simulacao_to-id_simulacao.
      ls_to-id_var_input = cl_system_uuid=>create_uuid_x16_static( ).

      IF NOT line_exists( et_inputs_ref[ id_from = ls_from_index-id_var_input ] ).
        APPEND VALUE ts_from_to(
            id_from = ls_from_index-id_var_input
            id_to   = ls_to-id_var_input ) TO et_inputs_ref.
      ENDIF.

      ls_to-id_custo_elementar  = ls_from-id_custo_elementar.
      ls_to-tipo_regra          = ls_from-tipo_regra        .
      ls_to-matnr               = ls_from-matnr             .
      ls_to-id_grupo_produto    = ls_from-id_grupo_produto  .
      ls_to-agregador           = ls_from-agregador         .
      ls_to-mat_planejado       = ls_from-mat_planejado     .
      ls_to-tipo_origem         = ls_from-tipo_origem       .
      ls_to-id_origem           = ls_from-id_origem         .
      ls_to-tipo_destino        = ls_from-tipo_destino      .
      ls_to-id_destino          = ls_from-id_destino        .
      ls_to-id_modal            = ls_from-id_modal          .
      ls_to-id_categoria        = ls_from-id_categoria      .
      ls_to-id_processo         = ls_from-id_processo       .
      ls_to-id_trecho           = ls_from-id_trecho         .

      lr_data = REF #( ls_to ).
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

        IF line_exists( lt_from[ id_var_input = ls_from_index-id_var_input
                                     periodo = lv_periodo_inicial ] ).
          DATA(ls_to_value) = lt_from[ id_var_input = ls_from_index-id_var_input
                                       periodo = lv_periodo_inicial ].
        ELSE.
          ls_to_value = lt_from[ id_var_input = ls_from_index-id_var_input
                                 periodo = ls_from_index-last_periodo ].
        ENDIF.

        ls_to-valor = ls_to_value-valor.
        ls_to-percentual = ls_to_value-percentual.
        ls_to-periodo = lv_periodo = lv_ano && lv_mes.
        APPEND ls_to TO lt_data.

      ENDWHILE.


      IF lines( lt_data ) > 0.
        INSERT /qaps/var_input FROM TABLE lt_data.
        COMMIT WORK.
      ENDIF.

    ENDLOOP.

*    IF lines( lt_data ) > 0.
*      MODIFY /qaps/var_input FROM TABLE lt_data.
*      COMMIT WORK.
*    ENDIF.

  ENDMETHOD.


  METHOD copy_simulacao_matriz.

    DATA: lv_new_key TYPE sysuuid_x16,
          lv_distrib TYPE sysuuid_x16.

    DATA lt_distrib TYPE /qaps/t_matriz_dst.

    "Matriz Abastecimento
    DATA: lt_header_ref  TYPE tt_from_to,
          lt_item_ref    TYPE tt_from_to,
          lt_distrib_ref TYPE tt_from_to.

    "Header
    SELECT *
      FROM /qaps/matriz_hdr
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      INTO TABLE @DATA(lt_header).

    SORT lt_header BY id_parent.

    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).

      lv_new_key = cl_system_uuid=>create_uuid_x16_static( ).
      APPEND VALUE ts_from_to(
          id_from = <fs_header>-id_matriz_abast
          id_to   = lv_new_key     ) TO lt_header_ref.

      <fs_header>-id_simulacao = is_simulacao_to-id_simulacao.
      <fs_header>-id_matriz_abast = lv_new_key.

      IF NOT <fs_header>-id_parent IS INITIAL.
        DATA(ls_parent) = lt_header_ref[ id_from = <fs_header>-id_parent ].
        <fs_header>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

    MODIFY /qaps/matriz_hdr FROM TABLE lt_header.

    "Items
    SELECT *
      FROM /qaps/matriz_itm
      FOR ALL ENTRIES IN @lt_header_ref
      WHERE id_matriz_abast = @lt_header_ref-id_from
      INTO TABLE @DATA(lt_item).

    SORT lt_item BY id_parent.

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).

      lv_new_key = cl_system_uuid=>create_uuid_x16_static( ).
      APPEND VALUE ts_from_to(
          id_from = <fs_item>-id_item
          id_to   = lv_new_key     ) TO lt_item_ref.

      DATA(ls_header_ref) = lt_header_ref[ id_from = <fs_item>-id_matriz_abast ].

      <fs_item>-id_matriz_abast = ls_header_ref-id_to.
      <fs_item>-id_item = lv_new_key.

      IF NOT <fs_item>-id_parent IS INITIAL.
        ls_parent = lt_item_ref[ id_from = <fs_item>-id_parent ].
        <fs_item>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

    MODIFY /qaps/matriz_itm FROM TABLE lt_item.

    "Distrib
    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @lt_item_ref
      WHERE id_item = @lt_item_ref-id_from
      INTO TABLE @lt_distrib.

    SORT lt_distrib BY id_parent.

    BREAK c060863.
    LOOP AT lt_distrib ASSIGNING FIELD-SYMBOL(<fs_distrib>).

      IF NOT line_exists( lt_distrib_ref[ id_from = <fs_distrib>-id_distribuicao ] ).

        lv_distrib = cl_system_uuid=>create_uuid_x16_static( ).
        APPEND VALUE ts_from_to(
            id_from = <fs_distrib>-id_distribuicao
            id_to   = lv_distrib     ) TO lt_distrib_ref.
      ELSE.
        lv_distrib = lt_distrib_ref[ id_from = <fs_distrib>-id_distribuicao ]-id_to.
      ENDIF.

      ls_header_ref = lt_header_ref[ id_from = <fs_distrib>-id_matriz_abast ].
      DATA(ls_item_ref) = lt_item_ref[ id_from = <fs_distrib>-id_item ].

      <fs_distrib>-id_matriz_abast = ls_header_ref-id_to.
      <fs_distrib>-id_item = ls_item_ref-id_to.
      <fs_distrib>-id_distribuicao = lv_distrib.

      IF NOT <fs_distrib>-id_parent IS INITIAL.
        ls_parent = lt_distrib_ref[ id_from = <fs_distrib>-id_parent ].
        <fs_distrib>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

*    MODIFY /qaps/matriz_dst FROM TABLE lt_distrib.
    copy_simulacao_matriz_distrib( it_distribuicao   = lt_distrib
                                   is_simulacao_from = is_simulacao_from
                                   is_simulacao_to   = is_simulacao_to
    ).

    SELECT *
      FROM /qaps/custo_mtz
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      INTO TABLE @DATA(lt_custo_mtz).

    LOOP AT lt_custo_mtz ASSIGNING FIELD-SYMBOL(<fs_custo_mtz>).

      ls_header_ref = lt_header_ref[ id_from = <fs_custo_mtz>-id_matriz_abast ].
      ls_item_ref = lt_item_ref[ id_from = <fs_custo_mtz>-id_item ].
      DATA(ls_distr_ref) = lt_distrib_ref[ id_from = <fs_custo_mtz>-id_distribuicao ].
      DATA(ls_var_input_ref) = it_inputs_ref[ id_from = <fs_custo_mtz>-id_var_input ].

      <fs_custo_mtz>-id_custo_matriz = cl_system_uuid=>create_uuid_x16_static( ).
      <fs_custo_mtz>-id_simulacao = is_simulacao_to-id_simulacao.
      <fs_custo_mtz>-id_matriz_abast = ls_header_ref-id_to.
      <fs_custo_mtz>-id_item = ls_item_ref-id_to.
      <fs_custo_mtz>-id_matriz_abast = ls_distr_ref-id_to.
      <fs_custo_mtz>-id_var_input = ls_var_input_ref-id_to.

    ENDLOOP.

    MODIFY /qaps/custo_mtz FROM TABLE lt_custo_mtz.

    COMMIT WORK AND WAIT.

    et_header_mtz_ref = lt_header_ref.
    et_item_mtz_ref   = lt_item_ref.
    et_distrib_mtz_ref = lt_distrib_ref.


  ENDMETHOD.


  METHOD copy_simulacao_matriz_distrib.

    BREAK c060863.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    data: lt_data type TABLE OF /qaps/matriz_dst.
*          ls_data type /qaps/matriz_dst.

    DATA(lt_ref) = it_distribuicao.
    DELETE lt_ref WHERE periodo <> is_simulacao_from-periodo_final.

    loop at lt_ref into data(ls_data).

      REFRESH lt_data.

      DATA(lv_periodo_inicial) = is_simulacao_to-periodo_inicial.

      ls_periodo-year = is_simulacao_to-periodo_inicial(4).
      ls_periodo-month = is_simulacao_to-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = is_simulacao_to-periodo_inicial
          final   = is_simulacao_to-periodo_final    ).

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
        MODIFY /qaps/matriz_dst FROM TABLE lt_data.
        COMMIT WORK.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD copy_simulacao_objects.


    DATA(ls_from) = get_simulacao_by_id( iv_id_simulacao_from ).
    DATA(ls_to) = get_simulacao_by_id( iv_id_simulacao_to ).
    "Inputs
    copy_simulacao_inputs( EXPORTING is_simulacao_from = ls_from
                                     is_simulacao_to   = ls_to
                           IMPORTING et_inputs_ref = DATA(lt_inputs_ref) ).

    "Sincroniza Matriz e Premissa
    DATA(lo_sync) = NEW /qaps/cl_rule_sincronizacao( ).

    lo_sync->execute( iv_id_simulacao = iv_id_simulacao_to
                      iv_id_simul_ref = iv_id_simulacao_from ).

  ENDMETHOD.


  METHOD copy_simulacao_premissa.

    DATA: lv_new_key   TYPE sysuuid_x16,
          lv_distrib   TYPE sysuuid_x16,
          lv_prem_traj TYPE sysuuid_x16.

    "Matriz Abastecimento
    DATA: lt_header_ref  TYPE tt_from_to,
          lt_item_ref    TYPE tt_from_to,
          lt_distrib_ref TYPE tt_from_to,
          lt_trajeto_ref TYPE tt_from_to.

    DATA: lt_distrib   TYPE /qaps/t_prem_distr,
          lt_prem_traj TYPE /qaps/t_prem_traj.

    "Header
    SELECT *
      FROM /qaps/prem_hdr
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      INTO TABLE @DATA(lt_header).

    SORT lt_header BY id_parent.

    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).

      lv_new_key = cl_system_uuid=>create_uuid_x16_static( ).
      APPEND VALUE ts_from_to( id_from = <fs_header>-id_premissa
                               id_to   = lv_new_key     ) TO lt_header_ref.

      <fs_header>-id_simulacao = is_simulacao_to-id_simulacao.
      <fs_header>-id_premissa = lv_new_key.

      IF NOT <fs_header>-id_matriz_abast IS INITIAL.
        DATA(ls_matriz_hdr) = it_header_mtz_ref[ id_from = <fs_header>-id_matriz_abast ].
        <fs_header>-id_matriz_abast = ls_matriz_hdr-id_to.
      ENDIF.

      IF NOT <fs_header>-id_parent IS INITIAL.
        DATA(ls_parent) = lt_header_ref[ id_from = <fs_header>-id_parent ].
        <fs_header>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

    MODIFY /qaps/prem_hdr FROM TABLE lt_header.

    "Items
    SELECT *
      FROM /qaps/prem_item
      FOR ALL ENTRIES IN @lt_header_ref
      WHERE id_premissa = @lt_header_ref-id_from
      INTO TABLE @DATA(lt_item).

    SORT lt_item BY id_parent.

*    BREAK c060863.
    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).

      lv_new_key = cl_system_uuid=>create_uuid_x16_static( ).
      APPEND VALUE ts_from_to( id_from = <fs_item>-id_item
                               id_to   = lv_new_key     ) TO lt_item_ref.

      DATA(ls_header_ref) = lt_header_ref[ id_from = <fs_item>-id_premissa ].

      <fs_item>-id_premissa = ls_header_ref-id_to.
      <fs_item>-id_item = lv_new_key.

      IF NOT <fs_item>-id_item_matriz IS INITIAL.
        DATA(ls_matriz_item) = it_item_mtz_ref[ id_from = <fs_item>-id_item_matriz ].
        <fs_item>-id_item_matriz = ls_matriz_item-id_to.
      ENDIF.

      IF NOT <fs_item>-id_parent IS INITIAL.
        ls_parent = lt_item_ref[ id_from = <fs_item>-id_parent ].
        <fs_item>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

    MODIFY /qaps/prem_item FROM TABLE lt_item.

    "Distrib
    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_item_ref
      WHERE id_item = @lt_item_ref-id_from
      INTO TABLE @lt_distrib.

    SORT lt_distrib BY id_parent.

    LOOP AT lt_distrib ASSIGNING FIELD-SYMBOL(<fs_distrib>).

      IF NOT line_exists( lt_distrib_ref[ id_from = <fs_distrib>-id_distribuicao ] ).

        lv_distrib = cl_system_uuid=>create_uuid_x16_static( ).
        APPEND VALUE ts_from_to( id_from = <fs_distrib>-id_distribuicao
                                 id_to   = lv_distrib     ) TO lt_distrib_ref.
      ELSE.
        lv_distrib = lt_distrib_ref[ id_from = <fs_distrib>-id_distribuicao ]-id_to.
      ENDIF.

      ls_header_ref = lt_header_ref[ id_from = <fs_distrib>-id_premissa ].
      DATA(ls_item_ref) = lt_item_ref[ id_from = <fs_distrib>-id_item ].

      <fs_distrib>-id_premissa = ls_header_ref-id_to.
      <fs_distrib>-id_item = ls_item_ref-id_to.
      <fs_distrib>-id_distribuicao = lv_distrib.

      IF NOT <fs_distrib>-id_distr_matriz IS INITIAL.
        DATA(ls_matriz_distr) = it_distrib_mtz_ref[ id_from = <fs_distrib>-id_distr_matriz ].
        <fs_distrib>-id_distr_matriz = ls_matriz_distr-id_to.
      ENDIF.

      IF NOT <fs_distrib>-id_parent IS INITIAL.
        ls_parent = lt_distrib_ref[ id_from = <fs_distrib>-id_parent ].
        <fs_distrib>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

*    MODIFY /qaps/prem_distr FROM TABLE lt_distrib.
    copy_simulacao_prem_distrb(
      EXPORTING
        it_distribuicao   = lt_distrib     " Ctg Tabela /QAPS/MATRIZ_DST
        is_simulacao_from = is_simulacao_from     " QAPS Simulação
        is_simulacao_to   = is_simulacao_to       " QAPS Simulação
    ).

    "Trajeto
    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @lt_distrib_ref
      WHERE id_distribuicao  = @lt_distrib_ref-id_from
      INTO TABLE @lt_prem_traj.

    SORT lt_prem_traj BY id_parent.

    LOOP AT lt_prem_traj ASSIGNING FIELD-SYMBOL(<ls_prem_traj>).

      IF NOT line_exists( lt_trajeto_ref[ id_from = <ls_prem_traj>-id_prem_trajeto ] ).

        lv_prem_traj = cl_system_uuid=>create_uuid_x16_static( ).
        APPEND VALUE ts_from_to( id_from = <ls_prem_traj>-id_prem_trajeto
                                 id_to   = lv_prem_traj     ) TO lt_trajeto_ref.
      ELSE.
        lv_prem_traj = lt_trajeto_ref[ id_from = <ls_prem_traj>-id_prem_trajeto ]-id_to.
      ENDIF.

      DATA(ls_distr_ref) = lt_distrib_ref[ id_from = <ls_prem_traj>-id_distribuicao ].

      <ls_prem_traj>-id_prem_trajeto = lv_prem_traj.
      <ls_prem_traj>-id_distribuicao = ls_distr_ref-id_to.

      IF NOT <ls_prem_traj>-id_parent IS INITIAL.
        ls_parent = lt_trajeto_ref[ id_from = <ls_prem_traj>-id_parent ].
        <ls_prem_traj>-id_parent = ls_parent-id_to.
      ENDIF.

    ENDLOOP.

*    MODIFY /qaps/prem_traj FROM TABLE lt_prem_traj.
    copy_simulacao_prem_trajeto(
      EXPORTING
        it_prem_trajeto   = lt_prem_traj    " Ctg Tabela /QAPS/MATRIZ_DST
        is_simulacao_from = is_simulacao_from    " QAPS Simulação
        is_simulacao_to   = is_simulacao_to      " QAPS Simulação
    ).

    "Custo x premissa
    SELECT *
      FROM /qaps/custo_prm
      WHERE id_simulacao = @is_simulacao_from-id_simulacao
      INTO TABLE @DATA(lt_custo_prm).

    LOOP AT lt_custo_prm ASSIGNING FIELD-SYMBOL(<fs_custo_prm>).

      ls_header_ref = lt_header_ref[ id_from = <fs_custo_prm>-id_premissa ].
      ls_item_ref = lt_item_ref[ id_from = <fs_custo_prm>-id_item ].
      ls_distr_ref = lt_distrib_ref[ id_from = <fs_custo_prm>-id_distribuicao ].
      DATA(ls_var_input_ref) = it_inputs_ref[ id_from = <fs_custo_prm>-id_var_input ].

      <fs_custo_prm>-id_custo_premisa = cl_system_uuid=>create_uuid_x16_static( ).
      <fs_custo_prm>-id_simulacao = is_simulacao_to-id_simulacao.
      <fs_custo_prm>-id_premissa = ls_header_ref-id_to.
      <fs_custo_prm>-id_item = ls_item_ref-id_to.
      <fs_custo_prm>-id_distribuicao = ls_distr_ref-id_to.
      <fs_custo_prm>-id_var_input = ls_var_input_ref-id_to.

    ENDLOOP.

    MODIFY /qaps/custo_prm FROM TABLE lt_custo_prm.

    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD copy_simulacao_prem_distrb.

    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    DATA: lt_data TYPE TABLE OF /qaps/prem_distr.
*          ls_data type /qaps/matriz_dst.

    DATA(lt_ref) = it_distribuicao.
    DELETE lt_ref WHERE periodo <> is_simulacao_from-periodo_final.

    LOOP AT lt_ref INTO DATA(ls_data).

      REFRESH lt_data.

      DATA(lv_periodo_inicial) = is_simulacao_to-periodo_inicial.

      ls_periodo-year = is_simulacao_to-periodo_inicial(4).
      ls_periodo-month = is_simulacao_to-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = is_simulacao_to-periodo_inicial
          final   = is_simulacao_to-periodo_final    ).

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
        MODIFY /qaps/prem_distr FROM TABLE lt_data.
        COMMIT WORK.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD copy_simulacao_prem_trajeto.

    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
*          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    DATA: lt_data TYPE TABLE OF /qaps/prem_traj.
*          ls_data type /qaps/matriz_dst.

    DATA(lt_ref) = it_prem_trajeto.
    DELETE lt_ref WHERE periodo <> is_simulacao_from-periodo_final.

    LOOP AT lt_ref INTO DATA(ls_data).

      REFRESH lt_data.

      DATA(lv_periodo_inicial) = is_simulacao_to-periodo_inicial.

      ls_periodo-year = is_simulacao_to-periodo_inicial(4).
      ls_periodo-month = is_simulacao_to-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = is_simulacao_to-periodo_inicial
          final   = is_simulacao_to-periodo_final    ).

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

    ENDLOOP.

  ENDMETHOD.


  METHOD create_simulacao.

    DATA: ls_data    TYPE /qaps/s_simulacao,
          ls_entry   TYPE /qaps/simulacao,
          ls_message TYPE bapiret2,
          lr_data type ref to data.

    CALL FUNCTION '/QAPS/FM_SIMULACAO_INPUT'
      EXPORTING
        iv_action      = 'C'
        iv_id_tp_lista = iv_id_tp_lista
*       IV_ID_REFERENCIA       =
*       IS_DATA        =
      IMPORTING
        es_data        = ls_data
        es_message     = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_simulacao = get_next_id( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/simulacao FROM ls_entry.

    return = abap_true.


  ENDMETHOD.


  METHOD delete_simulacao.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT it_data INTO DATA(ls_data).

        IF check_status_simulacao( iv_id_simulacao = ls_data-id_simulacao
                                   iv_status = 'A' ) = abap_false.
          MESSAGE 'Operação não permitida em simulação no status atual' TYPE 'S'
            DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

    IF question( lv_message ) = abap_true.

      LOOP AT it_data INTO ls_data.

        delete_simulacao_premissa( ls_data-id_simulacao ).
        delete_simulacao_matriz( ls_data-id_simulacao ).
        DELETE FROM /qaps/var_input WHERE id_simulacao = ls_data-id_simulacao.
        DELETE FROM /qaps/simulacao WHERE id_simulacao = ls_data-id_simulacao.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_simulacao_matriz.

    DATA: lr_id_item  TYPE RANGE OF /qaps/ed_id_item,
          lr_id_distr TYPE RANGE OF /qaps/ed_id_distribuicao.

    SELECT *
      FROM /qaps/v_mtz_full
      WHERE id_simulacao = @iv_id_simulacao
      INTO TABLE @DATA(lt_mtz_full).

    check sy-subrc is INITIAL.

    lr_id_item = VALUE #( FOR wa IN lt_mtz_full
                          ( sign = 'I' option = 'EQ' low = wa-id_item ) ).

    lr_id_distr = VALUE #( FOR wa IN lt_mtz_full
                          ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).


    DELETE FROM /qaps/matriz_dst WHERE id_distribuicao IN lr_id_distr.

    DELETE FROM /qaps/matriz_itm WHERE id_item IN lr_id_item.

    DELETE FROM /qaps/matriz_hdr WHERE id_simulacao = iv_id_simulacao.
    DELETE FROM /qaps/custo_mtz WHERE id_simulacao = iv_id_simulacao.

  ENDMETHOD.


  METHOD delete_simulacao_premissa.

    DATA: lr_id_item  TYPE RANGE OF /qaps/ed_id_item,
          lr_id_distr TYPE RANGE OF /qaps/ed_id_distribuicao.


      select *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @iv_id_simulacao
      INTO TABLE @DATA(lt_prm_full).


    check sy-subrc is INITIAL.

    lr_id_item = value #( for wa in lt_prm_full
                          ( sign = 'I' option = 'EQ' low = wa-id_item ) ).

    lr_id_distr = value #( for wa in lt_prm_full
                          ( sign = 'I' option = 'EQ' low = wa-id_distribuicao ) ).

    delete from /qaps/prem_traj where id_distribuicao in lr_id_distr.
    delete from /qaps/prem_distr where id_distribuicao in lr_id_distr.

    delete from /qaps/prem_item where id_item in lr_id_item.

    delete from /qaps/prem_hdr where id_simulacao = iv_id_simulacao.

    delete from /qaps/custo_prm where id_simulacao = iv_id_simulacao.

  ENDMETHOD.


  METHOD edit_simulacao.

    DATA: ls_data    TYPE /qaps/s_simulacao,
          ls_entry   TYPE /qaps/simulacao,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    IF check_status_simulacao( iv_id_simulacao = is_data-id_simulacao
                               iv_status = 'A' ) = abap_false.
      MESSAGE 'Operação não permitida em simulação no status atual' TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_SIMULACAO_INPUT'
      EXPORTING
        iv_action      = 'U'
        iv_id_tp_lista = iv_id_tp_lista
        is_data        = is_data
      IMPORTING
        es_data        = ls_data
        es_message     = ls_message.


    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/simulacao FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD get_next_id.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = '/QAPS/SIMU'
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = return
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD get_referencias.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @it_data
      WHERE id_simulacao = @it_data-id_original
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_simulacao.

    SELECT *
      FROM /qaps/simulacao
      WHERE id_tp_lista = @iv_id_tp_lista
      INTO CORRESPONDING FIELDS OF TABLE @return.

    IF lines( return ) > 0.

      DATA(lt_ref) = get_referencias( return ).
      data(lo_std_producao) = new /qaps/cl_mdl_std_producao( ).
      data(lt_std_producao) = lo_std_producao->get_all( ).

      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

        CASE <fs>-status.
          WHEN 'A'. <fs>-icon = icon_green_light.
          WHEN 'F'. <fs>-icon = icon_locked.
          WHEN 'E'. <fs>-icon = icon_complete.
        ENDCASE.

        <fs>-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_SIMUL'
                                                                iv_value  = CONV #( <fs>-status ) ).

        <fs>-dsc_referencia = value #( lt_ref[ id_simulacao = <fs>-id_original ]-descricao OPTIONAL ).

        if not <fs>-id_std_producao is INITIAL.
          data(ls_std_producao) = value #( lt_std_producao[ id_std_producao = <fs>-id_std_producao ] OPTIONAL ).
          <fs>-cod_std_producao = ls_std_producao-codigo.
          <fs>-dsc_std_producao = ls_std_producao-descricao.
        endif.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_simulacao_all.

    SELECT *
      FROM /qaps/simulacao
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_simulacao_by_id.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO CORRESPONDING FIELDS OF @return.

    CHECK sy-subrc = 0.

    DATA(lt_ref) = get_referencias( VALUE #( ( return ) ) ).
    DATA(lo_std_producao) = NEW /qaps/cl_mdl_std_producao( ).
    DATA(lt_std_producao) = lo_std_producao->get_all( ).

    CASE return-status.
      WHEN 'A'. return-icon = icon_green_light.
      WHEN 'F'. return-icon = icon_red_light.
    ENDCASE.

    return-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_SIMUL'
                                                            iv_value  = CONV #( return-status ) ).

    return-dsc_referencia = VALUE #( lt_ref[ id_simulacao = return-id_original ]-descricao OPTIONAL ).

    IF NOT return-id_std_producao IS INITIAL.
      DATA(ls_std_producao) = VALUE #( lt_std_producao[ id_std_producao = return-id_std_producao ] OPTIONAL ).
      return-cod_std_producao = ls_std_producao-codigo.
      return-dsc_std_producao = ls_std_producao-descricao.
    ENDIF.

  ENDMETHOD.


  METHOD get_single_tipo_lista.

    SELECT SINGLE *
      FROM /qaps/tp_lista
      WHERE id_tp_lista = @iv_id_tipo_lista
      INTO CORRESPONDING FIELDS OF @return.

  ENDMETHOD.


  METHOD GET_TIPO_LISTA.

    IF iv_id_tipo_lista IS INITIAL.

      SELECT *
        FROM /qaps/tp_lista
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/tp_lista
        WHERE id_tp_lista = @iv_id_tipo_lista
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD QUESTION.

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


  METHOD SET_PERIODO.

    DATA ls_message     TYPE bapiret2.

    CHECK ms_periodo IS INITIAL.

    CALL FUNCTION '/QAPS/FM_PERIODO_INPUT'
      IMPORTING
        es_data    = ms_periodo
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ELSE.
      return = ms_periodo.
    ENDIF.

  ENDMETHOD.


  METHOD set_status.

    DATA lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    CHECK sy-subrc IS INITIAL and ls_simulacao <> iv_status.
    lr_data = REF #( ls_simulacao ).
    ls_simulacao-status = iv_status.

    preencher_dados_controle( CHANGING cr_data = lr_data  ).
    MODIFY /qaps/simulacao FROM ls_simulacao.


  ENDMETHOD.


  METHOD unassign_std_producao.

    DATA lr_data TYPE REF TO data.
    DATA lv_message TYPE string.
    DATA lv_qty TYPE i.

    lv_message = TEXT-m05.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      SELECT *
        FROM /qaps/simulacao
        FOR ALL ENTRIES IN @it_data
        WHERE id_simulacao = @it_data-id_simulacao
        INTO TABLE @DATA(lt_data).

      LOOP AT lt_data INTO DATA(ls_data).

        IF check_status_simulacao( iv_id_simulacao = ls_data-id_simulacao
                                   iv_status = 'A' ) = abap_false.
          MESSAGE 'Operação não permitida em simulação no status atual' TYPE 'S'
            DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

*        DATA(lo_rule_sincronizacao) = NEW /qaps/cl_rule_sincronizacao( ).
*        DATA(lv_return) = lo_rule_sincronizacao->sincronizar_std_prd_unassign( <fs_data> ).
*
*        IF lv_return = abap_true.
          lv_qty = lv_qty + 1.
          CLEAR <fs_data>-id_std_producao.
          lr_data = REF #( <fs_data> ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).
*        ENDIF.
      ENDLOOP.

      IF lv_qty > 0.
        MODIFY /qaps/simulacao FROM TABLE lt_data.
        COMMIT WORK AND WAIT.
        return = abap_true.
      ELSEIF lv_qty = 0.
        MESSAGE 'Operação falhou' TYPE 'S' DISPLAY LIKE 'E'.
        return = abap_false.
      ENDIF.

    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
