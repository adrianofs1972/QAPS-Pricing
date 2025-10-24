class /QAPS/CL_RULE_SIMULACAO_UPDATE definition
  public
  final
  create public .

public section.

  methods EXECUTE
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional .
protected section.
private section.

  data MS_SIMULACAO type /QAPS/SIMULACAO .

  methods EXPANDIR_DESTINO_MATRIZ
    importing
      !IS_DATA type /QAPS/MATRIZ_HDR
    returning
      value(RETURN) type /QAPS/T_MATRIZ_HDR .
  methods EXPANDIR_DESTINO_PREMISSA
    importing
      !IS_DATA type /QAPS/PREM_HDR
    returning
      value(RETURN) type /QAPS/T_PREM_DESTINO .
  methods APPEND_NEW_TRAJETO
    importing
      value(IS_TEMPLATE) type /QAPS/V_PRM_FULL
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO .
  methods CREATE_TRAJETO .
  methods GET_HIERARQUIA_DESTINO
    changing
      !CT_HIERARQUIA_DESTINO type /QAPS/T_HIERARQUIA_DESTINO .
  methods GET_HIERARQUIA_ORIGEM
    changing
      !CT_HIERARQUIA_ORIGEM type /QAPS/T_HIERARQUIA_ORIGEM .
  methods GET_PONTO_BY_ID
    importing
      !IV_ID_PONTO type /QAPS/ID_PONTO optional
      !IV_ID_EXTERNO type /QAPS/ID_EXTERNO optional
    returning
      value(RETURN) type /QAPS/V_PONTO .
  methods GET_PREMISSA_FULL
    returning
      value(RETURN) type /QAPS/T_PRM_FULL .
  methods LINK_TRAJETO_TO_PREMISSA
    importing
      !IS_TRAJETO type /QAPS/TRAJETO .
  methods PREENCHER_DADOS_CONTROLE
    changing
      !CR_DATA type ref to DATA .
  methods UPDATE_PERCENTUAL .
  methods UPDATE_PERCENTUAL_TRAJETO .
  methods UPDATE_PERCENTUAL_PREMISSA .
  methods CREATE_IMPORTACAO .
  methods FILL_ORIGEM_PERIODO_PREMISSA
    importing
      !IT_DATA type /QAPS/T_PREM_DISTR
    returning
      value(RETURN) type /QAPS/T_PREM_DISTR .
  methods FILL_ORIGEM_PERIODO_MATRIZ
    importing
      !IT_DATA type /QAPS/T_MATRIZ_DST
    returning
      value(RETURN) type /QAPS/T_MATRIZ_DST .
  methods GET_DISTRIBUICAO_LOGISTICA
    importing
      !IS_DATA type /QAPS/V_COST_INP
    returning
      value(RETURN) type /QAPS/T_DISTRIB_LOGISTICA .
  methods CLEAN_DATA_MATRIZ .
  methods CLEAN_DATA_PREMISSA .
  methods CLEAN_DATA .
  methods CHOOSE_SIMULACAO
    returning
      value(RETURN) type /QAPS/ED_ID_SIMULACAO
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type /QAPS/SIMULACAO .
ENDCLASS.



CLASS /QAPS/CL_RULE_SIMULACAO_UPDATE IMPLEMENTATION.


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




  ENDMETHOD.


  METHOD CLEAN_DATA_PREMISSA.

    DATA: lr_id_item         TYPE RANGE OF /qaps/ed_id_item,
          lr_id_distribuicao TYPE RANGE OF /qaps/ed_id_distribuicao.
    "Premissa
    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_premissa).

    check lines( lt_premissa ) > 0.

    lr_id_item = VALUE #( FOR ls IN lt_premissa
                          ( sign = 'I' option = 'EQ' low = ls-id_item ) ).

    lr_id_distribuicao = VALUE #( FOR ls IN lt_premissa
                          ( sign = 'I' option = 'EQ' low = ls-id_distribuicao ) ).

    "Delete trajeto e Distribuicao
    if lines( lr_id_distribuicao ) > 0.
      delete from /qaps/prem_traj where id_distribuicao in lr_id_distribuicao.
      delete from /qaps/prem_distr where id_distribuicao in lr_id_distribuicao.
    endif.

    "Delete items/materiais
    if lines( lr_id_item ) > 0.
      delete from /qaps/prem_item where id_item in lr_id_item.
    endif.

    delete from /qaps/prem_hdr where id_simulacao = ms_simulacao-id_simulacao.


  ENDMETHOD.


  METHOD create_importacao.

    DATA: lt_destino_matriz  TYPE TABLE OF /qaps/matriz_hdr,
          lt_material_matriz TYPE TABLE OF /qaps/matriz_itm,
          lt_origem_matriz   TYPE /qaps/t_matriz_dst.

    DATA: lt_destino_premissa  TYPE TABLE OF /qaps/prem_hdr,
          lt_material_premissa TYPE TABLE OF /qaps/prem_item,
          lt_origem_premissa   TYPE          /qaps/t_prem_distr.

    DATA lr_data TYPE REF TO data.

    DEFINE set_data_control.
      lr_data = REF #( &1 ).
      /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).
    END-OF-DEFINITION.

    SELECT DISTINCT tipo_destino,id_destino,tipo_regra,
                    matnr,
                    id_grupo_produto,
                    agregador,
                    mat_planejado,
                    tipo_origem,
                    id_origem
      FROM /qaps/v_cost_inp
      WHERE importacao = 'X'
      AND ( tipo_origem = 'I' OR tipo_origem = 'P' )
      AND id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_var_custo).

    CHECK lines( lt_var_custo ) > 0.

    SELECT *
      FROM /qaps/v_ponto
      WHERE tipo_ponto IN ('G','W')
      INTO TABLE @DATA(lt_ponto).

    LOOP AT lt_var_custo INTO DATA(ls_var_custo).

      DATA(ls_ponto) = lt_ponto[ id_ponto = ls_var_custo-id_destino ].

      CASE ls_var_custo-tipo_destino.
        WHEN 'G'.
          IF NOT line_exists( lt_destino_matriz[ id_grp_planta = ls_ponto-id_externo
                                                 id_simulacao    = ms_simulacao-id_simulacao ] ).
            "Matriz Header
            DATA(ls_destino_matriz) = VALUE /qaps/matriz_hdr(
                id_matriz_abast = cl_system_uuid=>create_uuid_x16_static( )
                id_simulacao    = ms_simulacao-id_simulacao
                id_grp_planta   = ls_ponto-id_externo )."ls_var_custo-id_destino            ).
*            set_data_control ls_destino_matriz.
*            APPEND ls_destino_matriz TO lt_destino_matriz.
            lt_destino_matriz = expandir_destino_matriz( ls_destino_matriz ).

            "Premissa Header
            DATA(ls_destino_premissa) = VALUE /qaps/prem_hdr(
                id_premissa = cl_system_uuid=>create_uuid_x16_static( )
                id_matriz_abast = ls_destino_matriz-id_matriz_abast
                id_simulacao    = ms_simulacao-id_simulacao
                id_grp_planta   = ls_ponto-id_externo )."ls_var_custo-id_destino            ).
*            set_data_control ls_destino_premissa.
*            APPEND ls_destino_premissa TO lt_destino_premissa.
            lt_destino_premissa = expandir_destino_premissa( ls_destino_premissa ).

          ELSE.
            ls_destino_matriz = lt_destino_matriz[
                id_simulacao    = ms_simulacao-id_simulacao
                id_grp_planta   = ls_ponto-id_externo ]. "ls_var_custo-id_destino ].
          ENDIF.
        WHEN 'W'.
          IF NOT line_exists( lt_destino_matriz[ id_simulacao    = ms_simulacao-id_simulacao
                                                 id_centro = ls_ponto-id_externo ] )."ls_var_custo-id_destino ] ).
            "Material Matriz
            ls_destino_matriz = VALUE /qaps/matriz_hdr(
                id_matriz_abast = cl_system_uuid=>create_uuid_x16_static( )
                id_simulacao    = ms_simulacao-id_simulacao
                id_centro   = ls_ponto-id_externo ). "ls_var_custo-id_destino            ).
            set_data_control ls_destino_matriz.
            APPEND ls_destino_matriz TO lt_destino_matriz.

            ls_destino_premissa = VALUE /qaps/prem_hdr(
                id_premissa = cl_system_uuid=>create_uuid_x16_static( )
                id_matriz_abast = ls_destino_matriz-id_matriz_abast
                id_simulacao    = ms_simulacao-id_simulacao
                id_centro       = ls_ponto-id_externo ). "ls_var_custo-id_destino            ).
            set_data_control ls_destino_premissa.
            APPEND ls_destino_premissa TO lt_destino_premissa.

          ELSE.
            ls_destino_matriz = lt_destino_matriz[
                id_simulacao    = ms_simulacao-id_simulacao
                id_centro   = ls_ponto-id_externo ]."ls_var_custo-id_destino ].
          ENDIF.
      ENDCASE.

    ENDLOOP.


*      "Material
*      CASE ls_var_custo-tipo_regra.
*        WHEN 'GE'.
*          IF NOT line_exists( lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                           tipo_regra = 'GE' ] ).
*            "Matriz Material
*            DATA(ls_material_matriz) = VALUE /qaps/matriz_itm(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                tipo_regra       = 'GE' ).
*            set_data_control ls_material_matriz.
*            APPEND ls_material_matriz TO lt_material_matriz.
*
*            "Premissa Material
*            DATA(ls_material_premissa) = VALUE /qaps/prem_item(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_premissa      = ls_destino_premissa-id_premissa
*                id_item_matriz   = ls_material_matriz-id_item
*                tipo_regra       = 'GE' ).
*            set_data_control ls_material_premissa.
*            APPEND ls_material_premissa TO lt_material_premissa.
*
*          ELSE.
*            ls_material_matriz = lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                       tipo_regra       = 'GE' ].
*          ENDIF.
*        WHEN 'GP'.
*          IF NOT line_exists( lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                           tipo_regra = 'GP'
*                                           id_grupo_produto = ls_var_custo-id_grupo_produto ] ).
*            ls_material_matriz = VALUE /qaps/matriz_itm(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                tipo_regra       = 'GP'
*                id_grupo_produto = ls_var_custo-id_grupo_produto ).
*            set_data_control ls_material_matriz.
*            APPEND ls_material_matriz TO lt_material_matriz.
*
*            ls_material_premissa = VALUE /qaps/prem_item(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_premissa      = ls_destino_premissa-id_premissa
*                id_item_matriz   = ls_material_matriz-id_item
*                tipo_regra       = 'GP'
*                id_grupo_produto = ls_var_custo-id_grupo_produto ).
*            set_data_control ls_material_premissa.
*            APPEND ls_material_premissa TO lt_material_premissa.
*
*          ELSE.
*            ls_material_matriz = lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                       tipo_regra       = 'GP'
*                                       id_grupo_produto = ls_var_custo-id_grupo_produto ].
*          ENDIF.
*        WHEN 'MP'.
*          IF NOT line_exists( lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                           tipo_regra = 'MP'
*                                           mat_planejado = ls_var_custo-mat_planejado ] ).
*            ls_material_matriz = VALUE /qaps/matriz_itm(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                tipo_regra       = 'MP'
*                mat_planejado = ls_var_custo-mat_planejado ).
*            set_data_control ls_material_matriz.
*            APPEND ls_material_matriz TO lt_material_matriz.
*
*            ls_material_premissa = VALUE /qaps/prem_item(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_premissa      = ls_destino_premissa-id_premissa
*                id_item_matriz   = ls_material_matriz-id_item
*                tipo_regra       = 'MP'
*                mat_planejado = ls_var_custo-mat_planejado ).
*            set_data_control ls_material_premissa.
*            APPEND ls_material_premissa TO lt_material_premissa.
*
*          ELSE.
*            ls_material_matriz = lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                       tipo_regra       = 'MP'
*                                       mat_planejado = ls_var_custo-mat_planejado ].
*          ENDIF.
*        WHEN 'AG'.
*          IF NOT line_exists( lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                           tipo_regra = 'AG'
*                                           agregador = ls_var_custo-agregador ] ).
*            ls_material_matriz = VALUE /qaps/matriz_itm(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                tipo_regra       = 'AG'
*                agregador = ls_var_custo-agregador ).
*            set_data_control ls_material_matriz.
*            APPEND ls_material_matriz TO lt_material_matriz.
*
*            ls_material_premissa = VALUE /qaps/prem_item(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_premissa      = ls_destino_premissa-id_premissa
*                id_item_matriz   = ls_material_matriz-id_item
*                 tipo_regra       = 'AG'
*                agregador = ls_var_custo-agregador ).
*            set_data_control ls_material_premissa.
*            APPEND ls_material_premissa TO lt_material_premissa.
*
*          ELSE.
*            ls_material_matriz = lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                       tipo_regra       = 'AG'
*                                       agregador = ls_var_custo-agregador ].
*          ENDIF.
*        WHEN 'MA'.
*          IF NOT line_exists( lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                           tipo_regra = 'MA'
*                                           matnr = ls_var_custo-matnr ] ).
*            ls_material_matriz = VALUE /qaps/matriz_itm(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                tipo_regra       = 'MA'
*                matnr = ls_var_custo-matnr ).
*            APPEND ls_material_matriz TO lt_material_matriz.
*
*            ls_material_premissa = VALUE /qaps/prem_item(
*                id_item          = cl_system_uuid=>create_uuid_x16_static( )
*                id_premissa      = ls_destino_premissa-id_premissa
*                id_item_matriz   = ls_material_matriz-id_item
*                tipo_regra       = 'MA'
*                matnr = ls_var_custo-matnr ).
*            set_data_control ls_material_premissa.
*            APPEND ls_material_premissa TO lt_material_premissa.
*
*          ELSE.
*            ls_material_matriz = lt_material_matriz[ id_matriz_abast  = ls_destino_matriz-id_matriz_abast
*                                       tipo_regra       = 'MA'
*                                       matnr = ls_var_custo-matnr ].
*          ENDIF.
*      ENDCASE.
*
*      "Origem
*      DATA(ls_origem_matriz) = VALUE /qaps/matriz_dst(
*          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
**          periodo         =
*          id_matriz_abast = ls_destino_matriz-id_matriz_abast
*          id_item         = ls_material_matriz-id_item
*          modalidade      = 'I'
*          tipo_origem     = ls_var_custo-tipo_origem
*          id_origem       = ls_var_custo-id_origem ).
*
*      APPEND ls_origem_matriz TO lt_origem_matriz.
*
*      DATA(ls_origem_premissa) = VALUE /qaps/prem_distr(
*          id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
**          periodo         =
*          id_premissa     = ls_destino_premissa-id_premissa
*          id_item         = ls_material_premissa-id_item
*          modalidade      = 'I'
*          tipo_origem     = ls_var_custo-tipo_origem
*          id_origem       = ls_var_custo-id_origem ).
*
*      APPEND ls_origem_premissa TO lt_origem_premissa.
*
*    ENDLOOP.
*
**    BREAK c060863.
*    "Percentual
*    LOOP AT lt_material_matriz INTO ls_material_matriz.
*
*      DATA(lt_count) = lt_origem_matriz.
*      DELETE lt_count WHERE id_item <> ls_material_matriz-id_item.
*
*      CHECK lines( lt_count ) = 1.
*      ls_origem_matriz-percentual = 100.
*
*      DATA(ls_count) = lt_count[ 1 ].
*
*      MODIFY lt_origem_matriz
*      FROM  ls_origem_matriz
*      TRANSPORTING percentual
*      WHERE id_distribuicao = ls_count-id_distribuicao.
*
*      ls_origem_premissa-percentual = 100.
*      MODIFY lt_origem_premissa
*      FROM  ls_origem_premissa
*      TRANSPORTING percentual
*      WHERE id_distr_matriz = ls_count-id_distribuicao.
*
*    ENDLOOP.
*
*    lt_origem_matriz = fill_origem_periodo_matriz( lt_origem_matriz ).
*    lt_origem_premissa = fill_origem_periodo_premissa( lt_origem_premissa ).

    "Matriz
    INSERT /qaps/matriz_hdr FROM TABLE lt_destino_matriz.
*    INSERT /qaps/matriz_itm FROM TABLE lt_material_matriz.
*    INSERT /qaps/matriz_dst FROM TABLE lt_origem_matriz.

    "Premissa
    INSERT /qaps/prem_hdr   FROM TABLE lt_destino_premissa.
*    INSERT /qaps/prem_item  FROM TABLE lt_material_premissa.
*    INSERT /qaps/prem_distr FROM TABLE lt_origem_premissa.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD create_trajeto.

    SELECT *
      FROM /qaps/trajeto
      INTO TABLE @DATA(lt_trajeto).

    LOOP AT lt_trajeto INTO DATA(ls_trajeto).
      link_trajeto_to_premissa( ls_trajeto ).
    ENDLOOP.

  ENDMETHOD.


  METHOD execute.

    IF iv_id_simulacao IS INITIAL.
      TRY.
          DATA(lv_id_simulacao) = choose_simulacao(  ).
        CATCH /qaps/cx_pricing_error.    "
          RETURN.
      ENDTRY.
      ms_simulacao = get_simulacao( lv_id_simulacao ).
    ELSE.
      ms_simulacao = get_simulacao( iv_id_simulacao ).
    ENDIF.

    "apaga dados de Matriz Abast e Premissa desta simulação
    clean_data( ).

    "Criar Importacao
    create_importacao( ).

    "Trajetos
    create_trajeto( ).

    "Set 100%
    update_percentual( ).


  ENDMETHOD.


  METHOD expandir_destino_matriz.

*    DATA(lt_distr_logist) = get_distribuicao_logistica( is_data ).

  ENDMETHOD.


  method EXPANDIR_DESTINO_PREMISSA.
  endmethod.


  METHOD fill_origem_periodo_matriz.

    DATA: lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
*          ls_entry      TYPE /qaps/matriz_dst,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    LOOP AT it_data INTO DATA(ls_data).

      DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

      ls_periodo-year = ms_simulacao-periodo_inicial(4).
      ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = ms_simulacao-periodo_inicial
          final   = ms_simulacao-periodo_final    ).

      lr_data = REF #( ls_data ).
*      preencher_dados_controle( CHANGING cr_data = lr_data ).
      /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

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
        APPEND ls_data TO return.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD FILL_ORIGEM_PERIODO_PREMISSA.

   DATA: lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
*          ls_entry      TYPE /qaps/matriz_dst,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    LOOP AT it_data INTO DATA(ls_data).

      DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

      ls_periodo-year = ms_simulacao-periodo_inicial(4).
      ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = ms_simulacao-periodo_inicial
          final   = ms_simulacao-periodo_final    ).

      lr_data = REF #( ls_data ).
*      preencher_dados_controle( CHANGING cr_data = lr_data ).
      /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

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
        APPEND ls_data TO return.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  method GET_DISTRIBUICAO_LOGISTICA.
  endmethod.


  METHOD get_hierarquia_destino.

    DATA lt_append TYPE /qaps/t_hierarquia_destino.

    SELECT *
      FROM /qaps/v_centro
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
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_ponto = @iv_id_ponto
        INTO @return.
    ELSEIF NOT iv_id_externo IS INITIAL.
      SELECT SINGLE *
        FROM /qaps/v_ponto
        WHERE id_externo = @iv_id_externo
        INTO @return.
    ENDIF.

  ENDMETHOD.


  METHOD get_premissa_full.

    SELECT *
      FROM /qaps/v_prm_full
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_simulacao.
    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @return.
  ENDMETHOD.


  method LINK_TRAJETO_TO_PREMISSA.

    DATA: lt_origem  TYPE /qaps/t_hierarquia_origem,
          lt_destino TYPE /qaps/t_hierarquia_destino,
          lt_create  type /QAPS/T_PRM_FULL.

*    DATA(is_trajeto) = get_trajeto_by_id( iv_id_trajeto ).

    data(ls_ponto_origem) = get_ponto_by_id( iv_id_ponto = is_trajeto-id_origem  ).
    data(ls_ponto_destino) = get_ponto_by_id( iv_id_ponto = is_trajeto-id_destino  ).

    append value /qaps/s_hierarquia_origem(
        tipo_origem = is_trajeto-tipo_origem
        id_origem   = is_trajeto-id_origem
        codigo      = ls_ponto_origem-codigo ) to lt_origem.

    append value /qaps/s_hierarquia_destino(
        tipo_destino = is_trajeto-tipo_destino
        id_destino   = is_trajeto-id_destino
        codigo       = ls_ponto_destino-codigo ) to lt_destino.

    get_hierarquia_origem( CHANGING ct_hierarquia_origem = lt_origem ).
    get_hierarquia_destino( CHANGING ct_hierarquia_destino = lt_destino ).

    loop at lt_destino ASSIGNING FIELD-SYMBOL(<fs_destino>).
      check <fs_destino>-tipo_destino = 'G'.
      <fs_destino>-id_destino = ls_ponto_destino-id_externo.
    ENDLOOP.

    data(lt_prm_full) = get_premissa_full( ).
    delete lt_prm_full where status <> 'A'.

    loop at lt_prm_full into data(ls_prm_full).

      check line_exists( lt_origem[ tipo_origem = ls_prm_full-tipo_origem
                                    id_origem   = ls_prm_full-id_origem ] )
        and ( line_exists( lt_destino[ tipo_destino = 'G'
                                       id_destino   = ls_prm_full-id_grp_planta ] ) or
              line_exists( lt_destino[ tipo_destino = 'W'
                                       id_destino   = ls_prm_full-id_centro ] )
                                       ).

        append ls_prm_full to lt_create.

    endloop.

    loop at lt_create into data(ls_create).
      append_new_trajeto( is_template   = ls_create
                          iv_id_trajeto = is_trajeto-id_trajeto ).
    endloop.


  endmethod.


  METHOD preencher_dados_controle.
    /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = cr_data ).
  ENDMETHOD.


  method UPDATE_PERCENTUAL.

    "Distribuição
    update_percentual_premissa( ).

    "Trajeto
    update_percentual_trajeto( ).

  endmethod.


  METHOD update_percentual_premissa.

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
