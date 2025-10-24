class /QAPS/CL_GER_LISTA_CUSTO_BASE definition
  public
  abstract
  create public .

public section.

  interfaces /QAPS/IF_LISTA_CUSTO .

  aliases GERAR
    for /QAPS/IF_LISTA_CUSTO~GERAR .

  methods CONSTRUCTOR
    importing
      !IV_TYPE type CHAR1 .
  class-methods GET_INSTANCE
    importing
      !IV_TYPE type CHAR1
    returning
      value(RETURN) type ref to /QAPS/IF_LISTA_CUSTO .
protected section.

  data MT_CUSTO_ELM type /QAPS/T_CUSTO_ELEMENTAR .
  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MT_MATERIAL type /QAPS/T_MATERIAL .
  data MV_TYPE type CHAR1 .
  data MS_SIMULACAO type /QAPS/S_SIMULACAO .
  data MT_CALC_NODE type /QAPS/T_CALCULATION_NODE .
  data MT_CENTRO type /QAPS/T_CENTRO .

  methods PROCESS_BY_PERIODO
    importing
      !IV_PERIODO type SPMON .
private section.
ENDCLASS.



CLASS /QAPS/CL_GER_LISTA_CUSTO_BASE IMPLEMENTATION.


  METHOD /qaps/if_lista_custo~gerar.

    DATA:
      lv_id(2)      TYPE n,
      lv_mes(2)     TYPE n,
      lv_ano(4)     TYPE n,
      lv_periodo    TYPE spmon,
      lv_pos(2)     TYPE n,
      ls_periodo    TYPE /qaps/s_periodo.

    "Preenche dados iniciais
    ms_lista_custo = NEW /qaps/cl_mdl_lista_custo( )->get_single_lista_custo( iv_cod_lista_custo ).
    ms_simulacao = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ms_lista_custo-id_simulacao ).
    mt_centro = NEW /qaps/cl_mdl_logistica( )->get_centros( ).
    mt_material = NEW /qaps/cl_mdl_material( )->get_materiais_all( ir_matnr = VALUE #( ) ).
    mt_calc_node = NEW /qaps/cl_mdl_input_custo_calc( )->get_all_nodes( ms_simulacao-id_tp_lista ).
    mt_custo_elm = NEW /qaps/cl_mdl_custo_elementar( )->get_variaveis_by_tipo_lista( ms_simulacao-id_tp_lista ).

    BREAK-POINT.
    "Execução por Período

    DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

    ls_periodo-year = ms_simulacao-periodo_inicial(4).
    ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial <= ms_simulacao-periodo_final.

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

      lv_periodo = lv_ano && lv_mes.

      check lv_periodo_inicial <= ms_simulacao-periodo_final.

      "Process By Periodo
      process_by_periodo( iv_periodo = lv_periodo ).

    ENDWHILE.

  ENDMETHOD.


  METHOD constructor.
    mv_type = iv_type.
  ENDMETHOD.


  METHOD get_instance.

    CASE iv_type.
      WHEN 'I'.
        return ?= NEW /qaps/cl_ger_lista_custo_imp( iv_type ).
      WHEN 'N'.
        return ?= NEW /qaps/cl_ger_lista_custo_nac( iv_type ).
      WHEN 'T'.
        return ?= NEW /qaps/cl_ger_lista_custo_tran( iv_type ).
      WHEN 'S'.
        return ?= NEW /qaps/cl_ger_lista_custo_std( iv_type ).
    ENDCASE.

  ENDMETHOD.


  METHOD process_by_periodo.

    BREAK c060863.

  ENDMETHOD.
ENDCLASS.
