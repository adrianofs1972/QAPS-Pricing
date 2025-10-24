class /QAPS/CL_CUSTO_CALCULATE_BASE definition
  public
  create public .

public section.

  methods PRE_GENERATE_EXPRESSOES
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO .
  methods CONSTRUCTOR
    importing
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_MOEDA_FINAL type /QAPS/ED_MOEDA_FINAL
      !IT_TAXA_CAMBIO type /QAPS/T_TAXA_CAMBIO_PERIODO
      !IV_STD_PRD_TYPE type CHAR1 optional .
  class-methods GET_INSTANCE
    importing
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_MOEDA_FINAL type /QAPS/ED_MOEDA_FINAL
      !IT_TAXA_CAMBIO type /QAPS/T_TAXA_CAMBIO_PERIODO
      !IV_STD_PRD_TYPE type CHAR1 optional
    returning
      value(RETURN) type ref to /QAPS/CL_CUSTO_CALCULATE_BASE .
  methods GET_TRANSFERENCIAS_STD_PRD
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_ORIGEM_PROCESSADA type /QAPS/T_RETORNO_CALCULO optional
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
  methods GET_TRANSFERENCIAS
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_ORIGEM_PROCESSADA type /QAPS/T_RETORNO_CALCULO optional
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
  methods GET_STD_PRODUCAO_CONVERSAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_IMPORTADO type /QAPS/T_RETORNO_CALCULO
      !IT_NACIONAL type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_IMPORTACAO type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_NACIONAL type /QAPS/T_RETORNO_CALCULO
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
  methods GET_STD_PRODUCAO_PRODUCAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_IMPORTADO type /QAPS/T_RETORNO_CALCULO
      !IT_NACIONAL type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_IMPORTACAO type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_NACIONAL type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_STD_PRD_CONVERSAO type /QAPS/T_RETORNO_CALCULO
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
  methods GET_STD_PRODUCAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_IMPORTADO type /QAPS/T_RETORNO_CALCULO
      !IT_NACIONAL type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_IMPORTACAO type /QAPS/T_RETORNO_CALCULO
      !IT_TRANSF_NACIONAL type /QAPS/T_RETORNO_CALCULO
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
  methods EXECUTE
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .
protected section.

  data MO_CALC_PRODUCAO_CONV type ref to /QAPS/CL_CUSTO_CALC_SUM_PRD_C .
  data MO_CALC_PRODUCAO_PROD type ref to /QAPS/CL_CUSTO_CALC_SUM_PRD_P .
  data MV_MOEDA_FINAL type /QAPS/ED_MOEDA_FINAL .
  data MV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL .
  data MT_TAXA_CAMBIO type /QAPS/T_TAXA_CAMBIO_PERIODO .
  data MS_EXPRESSAO type /QAPS/S_EXPRESSAO .
  data MS_SIMULACAO type /QAPS/SIMULACAO .
  data MT_EXPRESSAO type /QAPS/T_EXPRESSAO .
  data MV_MODALIDADE type /QAPS/ED_MODALIDADE .

  methods FILL_MOEDA_SAIDA
    changing
      !CT_EXPRESSAO type /QAPS/T_EXPRESSAO .
  methods FILL_PARAMETER_NAME
    changing
      !CT_DATA type /QAPS/T_EXPRESSAO .
  methods CALCULAR_PARAMETRO
    importing
      value(IT_PARAMETROS) type /QAPS/T_EXPRESSAO
    changing
      !CS_EXPRESSAO type /QAPS/S_EXPRESSAO .
  methods FILL_VALORES_ELEM_COM_TRAJETO
    importing
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IS_CUSTO_ELEMENTAR type /QAPS/CUSTO_ELM
      !IS_ORIGEM type /QAPS/S_V_PRM_MAT
      !IS_TRAJETO type /QAPS/DIST_TRAJ
    changing
      value(CT_VALUES) type /QAPS/T_VALUES .
  methods FILL_VALORES_ELEMENTARES
    importing
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IS_CUSTO_ELEMENTAR type /QAPS/CUSTO_ELM
      !IS_ORIGEM type /QAPS/S_V_PRM_MAT
    changing
      value(CT_VALUES) type /QAPS/T_VALUES .
  methods SOLVE_EXPRESSAO .
  methods FILL_VARIAVEIS_ELEMENTARES
    importing
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IS_ORIGEM type /QAPS/S_V_PRM_MAT
      !IS_TRAJETO type /QAPS/DIST_TRAJ .
  methods GET_EXPRESSAO .
  methods GET_INITIAL_TABLE_VALUE
    returning
      value(RETURN) type /QAPS/T_VALUES .
  methods CHECK_ERRORS_PONDERACAO
    importing
      !IT_CHILDREN type /QAPS/T_RETORNO_CALCULO
    changing
      !CS_DATA type /QAPS/S_PONDERACAO .
  methods CHECK_ERRORS_RETORNO_CALCULO
    changing
      !CT_DATA type /QAPS/T_RETORNO_CALCULO .
private section.

  methods CRIAR_PROGRAMA
    importing
      value(IS_EXPRESSAO) type /QAPS/S_EXPRESSAO
      value(IT_PARAMETROS) type /QAPS/T_EXPRESSAO
      value(IS_SIMULACAO) type /QAPS/SIMULACAO .
ENDCLASS.



CLASS /QAPS/CL_CUSTO_CALCULATE_BASE IMPLEMENTATION.


  METHOD calcular_parametro.

    DATA: lt_parametros TYPE /qaps/t_calc_parameter,
          lv_valor      TYPE /qaps/valor_absoluto.

    LOOP AT cs_expressao-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).

      CHECK NOT cs_expressao-expressao IS INITIAL.

      REFRESH: lt_parametros.

      LOOP AT it_parametros INTO DATA(ls_param).

        DATA(ls_valor) = ls_param-t_valores[ periodo = <fs_valores>-periodo ].

        APPEND VALUE /qaps/s_calc_parameter(
            fieldname  = ls_param-parameter_name
            tipo_dado  = COND #( WHEN ls_param-tipo_dado = 1 OR ls_param-tipo_dado = 2 THEN ls_param-tipo_dado
                                 ELSE 1 )
            valor      = ls_valor-valor
            percentual = ls_valor-percentual
        ) TO lt_parametros.
      ENDLOOP.

*      if cs_expressao-fieldname = 'CALC_004'.
*        BREAK-POINT.
*      ENDIF.

      CALL FUNCTION '/QAPS/FM_CALCULAR_EXPRESSAO'
*        DESTINATION 'NONE'
        EXPORTING
          is_simulacao  = ms_simulacao
          is_expressao  = cs_expressao
          it_parametros = lt_parametros
        IMPORTING
          ev_valor      = lv_valor
*         EV_PRECENTUAL =
        .

      <fs_valores>-valor = lv_valor.

    ENDLOOP.


  ENDMETHOD.


  method CHECK_ERRORS_PONDERACAO.
  endmethod.


  method CHECK_ERRORS_RETORNO_CALCULO.
  endmethod.


  METHOD constructor.
    mv_modalidade  = iv_modalidade.
    mv_moeda_local = iv_moeda_local.
    mv_moeda_final = iv_moeda_final.
    mt_taxa_cambio = it_taxa_cambio.

    check iv_modalidade = 'P'.

    CASE IV_STD_PRD_TYPE.
      WHEN 'P'.
*        break c060863.
        mo_calc_producao_prod ?= /qaps/cl_custo_calculate_base=>get_instance(
                        iv_modalidade  = 'S'
                        iv_moeda_local = iv_moeda_local
                        iv_moeda_final = iv_moeda_final
                        it_taxa_cambio = it_taxa_cambio
                    ).
        when 'C'.
          mo_calc_producao_conv ?= /qaps/cl_custo_calculate_base=>get_instance(
                        iv_modalidade  = 'C'
                        iv_moeda_local = iv_moeda_local
                        iv_moeda_final = iv_moeda_final
                        it_taxa_cambio = it_taxa_cambio
                    ).
    ENDCASE.

  ENDMETHOD.


  METHOD criar_programa.

    DATA: lv_programa TYPE programm,
          lv_result   TYPE /qaps/valor_absoluto.

    DATA: prog         TYPE string,
          lv_index     TYPE i,
          lt_source    TYPE TABLE OF string,
          lt_code      TYPE /qaps/t_program_code,
          lv_expressao TYPE string,
          mess         TYPE string.

    DATA(lv_simulacao) = |{ is_simulacao-id_simulacao ALPHA = OUT }|.
    CONDENSE lv_simulacao NO-GAPS.
    lv_programa = |Z_{ lv_simulacao }_{ is_expressao-fieldname }|.

    APPEND `PROGRAM ` && lv_programa && `.`                        TO lt_code.

    SORT it_parametros BY fieldname ASCENDING.

    LOOP AT it_parametros INTO DATA(ls_parameters).

      CHECK is_expressao-expressao CS ls_parameters-fieldname.

      CASE ls_parameters-tipo_dado.
        WHEN '1' OR '0'.
          APPEND `PARAMETERS ` && ls_parameters-parameter_name  && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO lt_code.
        WHEN '2'.
*          APPEND `PARAMETERS ` && ls_parameters-parameter_name && ` TYPE /QAPS/PERCENTUAL.` TO lt_code.
          APPEND `PARAMETERS ` && ls_parameters-parameter_name && ` TYPE /QAPS/PERCENTUAL_CALC.` TO lt_code.
      ENDCASE.
    ENDLOOP.

    APPEND `START-OF-SELECTION.`                        TO lt_code.
    APPEND `FREE MEMORY ID 'QAPS_VALOR'.`                        TO lt_code.

    APPEND `DATA ` && is_expressao-parameter_name && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO lt_code.

    LOOP AT it_parametros INTO ls_parameters.

      CHECK ls_parameters-tipo_dado = '2'.
      APPEND ls_parameters-parameter_name  && ` = ` && ls_parameters-parameter_name && ` / 100.` TO lt_code.

    ENDLOOP.

    lv_expressao = is_expressao-expressao.

    LOOP AT it_parametros INTO ls_parameters.
      REPLACE ALL OCCURRENCES OF ls_parameters-fieldname
          IN lv_expressao WITH ls_parameters-parameter_name.
    ENDLOOP.

    if lv_expressao is INITIAL.
      APPEND is_expressao-parameter_name && ` = 0.` TO lt_code.
    else.
      APPEND is_expressao-parameter_name && ` = ` && lv_expressao && `.` TO lt_code.
    endif.

    APPEND `EXPORT LV_RESULT = ` && is_expressao-parameter_name && ` TO MEMORY ID 'QAPS_VALOR' .` TO lt_code.

    READ REPORT lv_programa INTO lt_source.

    IF sy-subrc = 0.
      DELETE REPORT lv_programa.
    ENDIF.

    INSERT REPORT lv_programa FROM lt_code.
    IF sy-subrc = 0.
*      WRITE: / 'Programa criado com sucesso.'.
    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD execute.

    DATA: lv_times   TYPE sy-index,
          ls_trajeto TYPE /QAPS/DIST_TRAJ."/qaps/s_v_prm_trj.

    ms_simulacao = is_simulacao.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @it_origem
      WHERE id_ponto = @it_origem-id_origem
      INTO TABLE @DATA(lt_ponto).

    LOOP AT it_origem INTO DATA(ls_origem).

       IF lines( ls_origem-t_trajetos ) > 0.
        lv_times = lines( ls_origem-t_trajetos ).
      ELSE.
        lv_times = 1.
      ENDIF.

*      IF lines( ls_origem-t_traj_trecho ) > 0.
*        lv_times = lines( ls_origem-t_traj_trecho ).
*      ELSE.
*        lv_times = 1.
*      ENDIF.


      DO lv_times TIMES.

        ls_trajeto = VALUE #( ls_origem-t_trajetos[ sy-index ] OPTIONAL ).

        REFRESH mt_expressao.
        "Expressao
        get_expressao( ).

        "Carregar variáveis elementares
        fill_variaveis_elementares( is_material_centro = is_material_centro
                                    is_origem = ls_origem
                                    is_trajeto = ls_trajeto ).

        "Resolver expressão
        solve_expressao( ).

        APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
        <fs_return> = CORRESPONDING #( ls_origem ).
        <fs_return>-material = is_material_centro-matnr.
        <fs_return>-plant = is_material_centro-werks.
        <fs_return>-cod_trajeto = ls_trajeto-cod_trajeto.
        <fs_return>-desc_trajeto = ls_trajeto-descricao.

        CASE <fs_return>-tipo_origem.
          WHEN 'F'.
*            break c060863.
            DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs_return>-id_origem ] OPTIONAL ).
            <fs_return>-lifnr = ls_ponto-codigo.
            <fs_return>-dsc_lifnr = ls_ponto-descricao.
        ENDCASE.

        fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

        <fs_return>-t_expressao = mt_expressao.
        SORT <fs_return>-t_expressao BY price_field DESCENDING fieldname ASCENDING.

      ENDDO.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_moeda_saida.

    IF mv_moeda_local <> mv_moeda_final.

      LOOP AT ct_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).
        LOOP AT <fs_expressao>-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).

          DATA(ls_taxa_cambio) = mt_taxa_cambio[ periodo = <fs_valores>-periodo
                                                 moeda_final = mv_moeda_final ].
          <fs_valores>-moeda_final = mv_moeda_final.

          IF <fs_valores>-valor > 0.
            if ls_taxa_cambio-taxa > 0.
              <fs_valores>-valor_moeda_final = <fs_valores>-valor / ls_taxa_cambio-taxa.
            else.
              <fs_valores>-valor_moeda_final = <fs_valores>-valor.
            endif.
          ELSE.
            <fs_valores>-valor_moeda_final = <fs_valores>-valor.
          ENDIF.

*          IF <fs_valores>-valor_conversao_moeda > 0.
*            <fs_valores>-valor_conversao_moeda_final = <fs_valores>-valor_conversao_moeda / ls_taxa_cambio-taxa.
*          ELSE.
*            <fs_valores>-valor_moeda_final = <fs_valores>-valor_conversao_moeda.
*          ENDIF.

        ENDLOOP.
      ENDLOOP.

    ELSE.

      LOOP AT ct_expressao ASSIGNING <fs_expressao>.
        LOOP AT <fs_expressao>-t_valores ASSIGNING <fs_valores>.
          <fs_valores>-moeda_final = mv_moeda_final.
          <fs_valores>-valor_moeda_final = <fs_valores>-valor.
*          <fs_valores>-valor_conversao_moeda_final = <fs_valores>-valor_conversao_moeda.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD fill_parameter_name.

    DATA: lv_elem(3) TYPE n,
          lv_calc(3) TYPE n,
          lv_vlr(3)  TYPE n.

    SORT ct_data BY fieldname ASCENDING.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-tipo_node.
        WHEN 'E'.
          lv_elem = lv_elem + 1.
          <fs>-parameter_name = |E_{ lv_elem ALPHA = IN WIDTH = 3 }|.
        WHEN 'C'.
          IF <fs>-price_field = 'X'.
            lv_vlr = lv_vlr + 1.
            <fs>-parameter_name = |V_{ lv_vlr ALPHA = IN WIDTH = 3 }|.
          ELSE.
            lv_calc = lv_calc + 1.
            <fs>-parameter_name = |C_{ lv_calc ALPHA = IN WIDTH = 3 }|.
          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_valores_elementares.

    DATA lr_id_cais TYPE RANGE OF /qaps/cais-id_cais.

    SELECT DISTINCT id_custo_elementar,id_var_input,tipo_regra,matnr,
           id_grupo_produto,agregador,mat_planejado,
           tipo_origem,id_origem,tipo_destino,id_destino, ponto~codigo
      FROM /qaps/var_input
      LEFT OUTER JOIN /qaps/v_ponto AS ponto
      ON /qaps/var_input~id_destino = ponto~id_ponto
      WHERE id_custo_elementar = @is_custo_elementar-id_custo_elementar
      AND   ( id_origem = @is_origem-id_origem OR id_origem = @is_origem-id_origem_matriz )
      and   /qaps/var_input~id_simulacao = @ms_simulacao-id_simulacao
*      AND   ( ( tipo_destino = 'G' and ponto~codigo = @is_material_centro-cod_grp_planta )
*              or ( tipo_destino = 'W' and ponto~codigo = @is_material_centro-werks ) )
      INTO TABLE @DATA(lt_var_input).


    IF lines( lt_var_input ) = 0.
*      BREAK-POINT.
      SELECT DISTINCT id_custo_elementar,id_var_input,tipo_regra,matnr,
           id_grupo_produto,agregador,mat_planejado,
           tipo_origem,id_origem,tipo_destino,id_destino
      FROM /qaps/var_input
      WHERE id_custo_elementar = @is_custo_elementar-id_custo_elementar
      and   id_simulacao = @ms_simulacao-id_simulacao
*      AND   ( id_origem = @is_origem-id_origem OR id_origem = @is_origem-id_origem_matriz )
      INTO TABLE @lt_var_input.
    ENDIF.

    IF line_exists( lt_var_input[ matnr = is_material_centro-matnr ] ).
*                      AND NOT is_material_centro-matnr IS INITIAL.
      DATA(lt_var_input_aux) = lt_var_input.
      DELETE lt_var_input_aux WHERE matnr <> is_material_centro-matnr .

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> is_material_centro-cod_grp_planta.
      ENDIF.

      DATA(ls_var_input) = lt_var_input_aux[ matnr = is_material_centro-matnr ].

    ELSEIF line_exists( lt_var_input[ agregador = is_material_centro-agregador ] ).
*            and not is_material_centro-agregador is INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE agregador <> is_material_centro-agregador.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> is_material_centro-cod_grp_planta.
      ENDIF.

      ls_var_input = lt_var_input_aux[ agregador = is_material_centro-agregador ].
    ELSEIF line_exists( lt_var_input[ mat_planejado = is_material_centro-mat_planejado ] ).
*                      and not is_material_centro-mat_planejado is INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE mat_planejado <> is_material_centro-mat_planejado.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> is_material_centro-cod_grp_planta.
      ENDIF.

      ls_var_input = lt_var_input_aux[ mat_planejado = is_material_centro-mat_planejado ].
    ELSEIF line_exists( lt_var_input[ id_grupo_produto = is_material_centro-id_grupo_produto ] ).
*              and is_material_centro-id_grupo_produto is INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE id_grupo_produto <> is_material_centro-id_grupo_produto.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> is_material_centro-cod_grp_planta.
      ENDIF.

      ls_var_input = value #( lt_var_input_aux[ id_grupo_produto = is_material_centro-id_grupo_produto ] OPTIONAL ).

    ELSE.
      ls_var_input = VALUE #( lt_var_input[ tipo_regra = 'GE' ] OPTIONAL ).
    ENDIF.

    CHECK NOT ls_var_input IS INITIAL.

    SELECT *
     FROM /qaps/var_input
     WHERE id_custo_elementar = @is_custo_elementar-id_custo_elementar
     AND   id_var_input = @ls_var_input-id_var_input
     and   id_simulacao = @ms_simulacao-id_simulacao
     INTO TABLE @DATA(lt_valores).

*    if is_custo_elementar-tipo_dado = '1'.
*      break c060863.
*    endif.

    LOOP AT lt_valores INTO DATA(ls_valores).

      ASSIGN ct_values[ periodo = ls_valores-periodo ] TO FIELD-SYMBOL(<fs_value>).

      CASE is_custo_elementar-tipo_dado.
        WHEN '1'. "Valor

          IF mv_moeda_local <> is_custo_elementar-moeda.
            DATA(ls_taxa_cambio) = mt_taxa_cambio[ periodo     = ls_valores-periodo
                                             moeda_final = is_custo_elementar-moeda ].
          ELSE.
            ls_taxa_cambio-moeda_local = mv_moeda_local.
          ENDIF.

          ASSIGN COMPONENT 'VALOR_VAR_ELEMENTAR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_value_elm>).
          <fv_value_elm> = ls_valores-valor.

          ASSIGN COMPONENT 'MOEDA_VAR_ELEMENTAR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_moeda_elm>).
          <fv_moeda_elm> = is_custo_elementar-moeda.

          ASSIGN COMPONENT 'VALOR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_value>).
          IF mv_moeda_local <> is_custo_elementar-moeda.
            <fv_value> = ls_valores-valor * ls_taxa_cambio-taxa.
          ELSE.
            <fv_value> = ls_valores-valor.
          ENDIF.

          <fs_value>-moeda = mv_moeda_local.
          <fs_value>-taxa = ls_taxa_cambio-taxa.
          <fs_value>-data = ls_taxa_cambio-data.

        WHEN '2'. "Percentual
          ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE <fs_value> TO <fv_value>.
          <fv_value> = ls_valores-percentual.
      ENDCASE.

    ENDLOOP.



  ENDMETHOD.


  METHOD fill_valores_elem_com_trajeto.

    SELECT *
      FROM /qaps/traj_trech
      WHERE id_trajeto = @is_trajeto-id_trajeto
      INTO TABLE @DATA(lt_traj_trech).

    CHECK lines( lt_traj_trech ) > 0.

    SELECT *
     FROM /qaps/var_input
     FOR ALL ENTRIES IN @lt_traj_trech
     WHERE id_custo_elementar = @is_custo_elementar-id_custo_elementar
     AND   id_trecho = @lt_traj_trech-id_trecho
     AND (    ( tipo_regra = 'MA' AND matnr = @is_material_centro-matnr )
           OR ( tipo_regra = 'AG' AND agregador = @is_material_centro-agregador )
           OR ( tipo_regra = 'MP' AND mat_planejado = @is_material_centro-mat_planejado )
           OR ( tipo_regra = 'GP' AND id_grupo_produto = @is_material_centro-id_grupo_produto )
           OR tipo_regra = 'GE' )
     AND  id_simulacao = @ms_simulacao-id_simulacao
     INTO TABLE @DATA(lt_valores).

    LOOP AT lt_valores INTO DATA(ls_valores).

      ASSIGN ct_values[ periodo = ls_valores-periodo ] TO FIELD-SYMBOL(<fs_value>).

      CASE is_custo_elementar-tipo_dado.
        WHEN '1'. "Valor
          ASSIGN COMPONENT 'VALOR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_value>).

          IF is_custo_elementar-moeda <> 'BRL'.
            DATA(ls_taxa_cambio) = mt_taxa_cambio[ periodo = ls_valores-periodo ].
          ELSE.
            ls_taxa_cambio-taxa = '1'.
          ENDIF.

          <fv_value> = <fv_value> + ( ls_valores-valor * ls_taxa_cambio-taxa ).
        WHEN '2'. "Percentual
          ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE <fs_value> TO <fv_value>.
          <fv_value> = <fv_value> + ls_valores-percentual.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_variaveis_elementares.

    DATA lv_where TYPE string.
    DATA ls_expressao TYPE /qaps/s_expressao.

    CASE mv_modalidade.
      WHEN 'I'. lv_where = ` IMPORTACAO = 'X'`.
      WHEN 'N'. lv_where = ` NACIONAL = 'X'`.
      WHEN 'P'. lv_where = ` PRODUCAO = 'X'`.
      WHEN 'T'. lv_where = ` TRANSFERENCIA = 'X'`.
    ENDCASE.

    SELECT *
      FROM /qaps/custo_elm
      FOR ALL ENTRIES IN @mt_expressao
      WHERE id_custo_elementar = @mt_expressao-id_custo_elementar
      AND (lv_where)
      INTO TABLE @DATA(lt_custo_elementar).

    CHECK lines( lt_custo_elementar ) > 0.


    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).
      CHECK <fs_expressao>-tipo_node = 'E'.

      DATA(lt_custo_individual) = lt_custo_elementar.
      DELETE lt_custo_individual WHERE id_custo_elementar <> <fs_expressao>-id_custo_elementar.

      CHECK lines( lt_custo_individual ) > 0.

      IF <fs_expressao>-tipo_variavel <> 'F'. "FRETE

          fill_valores_elementares( EXPORTING is_material_centro = is_material_centro
                                              is_custo_elementar = lt_custo_individual[ 1 ]
                                              is_origem          = is_origem
                                      CHANGING ct_values = <fs_expressao>-t_valores ).
      ELSE.

        fill_valores_elem_com_trajeto( EXPORTING is_material_centro = is_material_centro
                                                 is_custo_elementar = lt_custo_individual[ 1 ]
                                                 is_origem          = is_origem
                                                 is_trajeto         = is_trajeto
                                       CHANGING ct_values = <fs_expressao>-t_valores ).
      ENDIF.
    ENDLOOP.

    ls_expressao-calculated = 'X'.

    MODIFY mt_expressao
    FROM ls_expressao
    TRANSPORTING calculated
    WHERE tipo_node = 'E'.

    MODIFY mt_expressao
    FROM ls_expressao
    TRANSPORTING calculated
    WHERE tipo_node = 'R'.

  ENDMETHOD.


  METHOD get_expressao.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @ms_simulacao-id_tp_lista
      AND   tipo_node      = 'R'
      INTO CORRESPONDING FIELDS OF @ms_expressao.

    SELECT *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @ms_simulacao-id_tp_lista
*      AND   tipo_node      = 'R'
      INTO CORRESPONDING FIELDS OF TABLE @mt_expressao.

    CHECK lines( mt_expressao ) > 0.

    DATA(lt_values) = get_initial_table_value( ).

    DATA(lt_custo_elm) = NEW /qaps/cl_mdl_custo_elementar(
                          )->get_variaveis_by_tipo_lista( ms_simulacao-id_tp_lista ).

    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(ls_custo_elm) = VALUE #( lt_custo_elm[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).
      <fs>-dsc_custo_elementar = ls_custo_elm-descricao.
      <fs>-tipo_dado = ls_custo_elm-tipo_dado.
      <fs>-tipo_variavel = ls_custo_elm-tipo_variavel.
      <fs>-t_valores = lt_values.
    ENDLOOP.

    fill_parameter_name( CHANGING ct_data = mt_expressao ).


  ENDMETHOD.


  METHOD get_initial_table_value.

    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval,
          ls_data       TYPE /qaps/s_values.

    DATA(lv_periodo_inicial) = ms_simulacao-periodo_inicial.

    ls_periodo-year = ms_simulacao-periodo_inicial(4).
    ls_periodo-month = ms_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = ms_simulacao-periodo_inicial
        final   = ms_simulacao-periodo_final    ).

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

  ENDMETHOD.


  METHOD get_instance.

    CASE iv_modalidade.
      WHEN 'I'.
        return ?= NEW /qaps/cl_custo_calc_importacao( iv_modalidade = iv_modalidade
                                                      iv_moeda_local = iv_moeda_local
                                                      iv_moeda_final = iv_moeda_final
                                                      it_taxa_cambio = it_taxa_cambio ).
      WHEN 'N'.
        return ?= NEW /qaps/cl_custo_calc_nacional( iv_modalidade = iv_modalidade
                                                    iv_moeda_local = iv_moeda_local
                                                    iv_moeda_final = iv_moeda_final
                                                    it_taxa_cambio = it_taxa_cambio ).
      WHEN 'P'.
        return ?= NEW /qaps/cl_custo_calc_std_prd( iv_modalidade = iv_modalidade
                                                   iv_moeda_local = iv_moeda_local
                                                   iv_moeda_final = iv_moeda_final
                                                   it_taxa_cambio = it_taxa_cambio
                                                   iv_std_prd_type = iv_std_prd_type ).
      WHEN 'T'.
        return ?= NEW /qaps/cl_custo_calc_transf( iv_modalidade = iv_modalidade
                                                  iv_moeda_local = iv_moeda_local
                                                  iv_moeda_final = iv_moeda_final
                                                  it_taxa_cambio = it_taxa_cambio ).
      WHEN 'S'. "Somatório de Std PRD - Produção
        return ?= NEW /qaps/cl_custo_calc_sum_prd_p( iv_modalidade = iv_modalidade
                                                   iv_moeda_local = iv_moeda_local
                                                   iv_moeda_final = iv_moeda_final
                                                   it_taxa_cambio = it_taxa_cambio ).
      WHEN 'C'. "Somatório de Std PRD - Conversão
        return ?= NEW /qaps/cl_custo_calc_sum_prd_c( iv_modalidade = iv_modalidade
                                                   iv_moeda_local = iv_moeda_local
                                                   iv_moeda_final = iv_moeda_final
                                                   it_taxa_cambio = it_taxa_cambio ).
      WHEN 'A'. "Somatório Outros
        return ?= NEW /qaps/cl_custo_calc_sum_outros( iv_modalidade = iv_modalidade
                                                      iv_moeda_local = iv_moeda_local
                                                      iv_moeda_final = iv_moeda_final
                                                      it_taxa_cambio = it_taxa_cambio ).
    ENDCASE.

  ENDMETHOD.


  METHOD GET_STD_PRODUCAO.



  ENDMETHOD.


  METHOD GET_STD_PRODUCAO_CONVERSAO.



  ENDMETHOD.


  METHOD GET_STD_PRODUCAO_PRODUCAO.



  ENDMETHOD.


  METHOD get_transferencias.


  ENDMETHOD.


  METHOD GET_TRANSFERENCIAS_STD_PRD.


  ENDMETHOD.


  METHOD pre_generate_expressoes.

    DATA lt_expressao TYPE /qaps/t_expressao.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    CHECK NOT ls_simulacao IS INITIAL.

    SELECT *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @ls_simulacao-id_tp_lista
      INTO CORRESPONDING FIELDS OF TABLE @lt_expressao.

    DATA(lt_custo_elm) = NEW /qaps/cl_mdl_custo_elementar(
                         )->get_variaveis_by_tipo_lista( ls_simulacao-id_tp_lista ).

    SORT lt_expressao BY fieldname ASCENDING.

    LOOP AT lt_expressao ASSIGNING FIELD-SYMBOL(<fs>).
      DATA(ls_custo_elm) = VALUE #( lt_custo_elm[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).
      <fs>-dsc_custo_elementar = ls_custo_elm-descricao.
      <fs>-tipo_dado = ls_custo_elm-tipo_dado.
      <fs>-tipo_variavel = ls_custo_elm-tipo_variavel.
*      <fs>-t_valores = lt_values.
    ENDLOOP.

    fill_parameter_name( CHANGING ct_data = lt_expressao ).

    DATA(lt_calc_expressao) = lt_expressao.
    DELETE lt_calc_expressao WHERE tipo_node <> 'C'.

    LOOP AT lt_calc_expressao INTO DATA(ls_expressao).

      DATA(lt_param_express) = lt_expressao.
      DELETE lt_param_express WHERE
        ( id_parent_node <> ls_expressao-id_calc_node AND price_field = '' ).

      criar_programa( is_simulacao  = ls_simulacao
                      is_expressao  = ls_expressao
                      it_parametros = lt_param_express ).

    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD solve_expressao.


    "Calcula nós  calculados com exceção de preços de saída
    WHILE line_exists( mt_expressao[ calculated = '' price_field = '' ] ).

      LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>)
        WHERE tipo_node <> 'E' AND tipo_node <> 'R' AND price_field = ''.

        CHECK <fs_expressao>-calculated = '' .

        DATA(lt_parametros) = mt_expressao.
        DELETE lt_parametros WHERE id_parent_node <> <fs_expressao>-id_calc_node.

        "só calcula se todos nós filho estiverem calculados
        IF NOT line_exists( lt_parametros[ calculated = '' ] ).

          calcular_parametro( EXPORTING it_parametros = lt_parametros
                              CHANGING  cs_expressao  = <fs_expressao>  ).

          <fs_expressao>-calculated = 'X'.
        ENDIF.

      ENDLOOP.

    ENDWHILE.

*    BREAK-POINT.

    "Calcula nós  calculados com exceção de preços de saída
    WHILE line_exists( mt_expressao[ calculated = '' price_field = 'X' ] ).

      LOOP AT mt_expressao ASSIGNING <fs_expressao>.
        CHECK <fs_expressao>-calculated = '' AND <fs_expressao>-price_field = 'X'.

        lt_parametros = mt_expressao.
        DELETE lt_parametros WHERE id_parent_node <> <fs_expressao>-id_calc_node.

        DATA(lt_price_field) = mt_expressao.
        DELETE lt_price_field WHERE price_field = ''.

        APPEND LINES OF lt_price_field TO lt_parametros.

        DELETE lt_parametros WHERE fieldname = <fs_expressao>-fieldname.

        "só calcula se todos nós filho estiverem calculados
        IF NOT line_exists( lt_parametros[ calculated = '' price_field = '' ] ).
          calcular_parametro( EXPORTING it_parametros = lt_parametros
                              CHANGING  cs_expressao  = <fs_expressao>  ).
          <fs_expressao>-calculated = 'X'.
        ENDIF.

      ENDLOOP.

    ENDWHILE.

*    BREAK-POINT.

  ENDMETHOD.
ENDCLASS.
