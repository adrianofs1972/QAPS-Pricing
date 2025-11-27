class /QAPS/CL_CUSTO_CALC_SUM_PRD_C definition
  public
  inheriting from /QAPS/CL_CUSTO_CALCULATE_BASE
  final
  create public .

public section.

  methods SOLVE_STD_PRD_TOTAL_EXP_CONV_2
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
    changing
      !CS_EXPRESSAO type /QAPS/S_RETORNO_CALCULO .
  methods SOLVE_STD_PRD_TOTAL_EXP_CONV
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
    changing
      !CS_EXPRESSAO type /QAPS/S_PONDERACAO .

  methods EXECUTE
    redefinition .
protected section.
private section.

  methods CALCULAR_PRODUCAO
    importing
      !IV_QUANTIDADE type KMPMG
    changing
      !CT_EXPRESSAO type /QAPS/T_EXPRESSAO .
  methods CONVERTER_UNIDADE
    importing
      !IV_MATNR type MATNR
      !IV_QUANTIDADE type KMPMG
      !IV_UNIDADE_INPUT type MSEHI
      !IV_UNIDADE_OUTPUT type MSEHI
    returning
      value(RETURN) type KMPMG .
  methods FILL_CUSTO_ELEMENTAR_PRODUCAO
    importing
      !IV_COD_GRP_PLANTA type /QAPS/ED_COD_GRP_PLANTA
      !IV_WERKS type WERKS_D
      !IV_MATNR type MATNR
      !IV_TIPO_ORIGEM type /QAPS/ED_TIPO_PONTO
      !IV_ID_ORIGEM type /QAPS/ID_PONTO
    changing
      !CS_EXPRESSAO type /QAPS/S_EXPRESSAO .
  methods GET_STD_PRD_COMPONENTS
    changing
      !CT_DATA type /QAPS/T_PREMISSA_MATRIZ .
ENDCLASS.



CLASS /QAPS/CL_CUSTO_CALC_SUM_PRD_C IMPLEMENTATION.


  METHOD CALCULAR_PRODUCAO.

    REFRESH mt_expressao.
    mt_expressao = ct_expressao.
    refresh ct_expressao.

    "Clear Calcution Nodes e ajusta valores Elementares
    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-tipo_node.
        WHEN 'E'.
          LOOP AT <fs>-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).
            <fs_valores>-valor = <fs_valores>-valor * iv_quantidade.
          ENDLOOP.
        WHEN OTHERS.
          CLEAR <fs>-calculated.
          LOOP AT <fs>-t_valores ASSIGNING <fs_valores>.
            CLEAR <fs_valores>-valor.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    solve_expressao( ).

    ct_expressao = mt_expressao.

  ENDMETHOD.


  METHOD CONVERTER_UNIDADE.

    DATA: lv_input  TYPE menge_d,
          lv_output TYPE menge_d,
          lv_matnr  TYPE matnr,
          lv_me_in  TYPE msehi,
          lv_me_out TYPE msehi.

    lv_input = iv_quantidade.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = iv_matnr
        i_in_me              = iv_unidade_input
        i_out_me             = iv_unidade_output
        i_menge              = lv_input
      IMPORTING
        e_menge              = lv_output
      EXCEPTIONS
        error_in_application = 1
        OTHERS               = 2.

    return = lv_output.

  ENDMETHOD.


  METHOD EXECUTE.

    BREAK c060863.
*    DATA(lt_origem) = it_origem.
*
*    LOOP AT lt_origem ASSIGNING FIELD-SYMBOL(<fs>).
*      <fs>-std_producao_parent = abap_true.
*    ENDLOOP.
*
*    get_std_prd_components( CHANGING ct_data = lt_origem ).

    CALL METHOD super->execute
      EXPORTING
        is_simulacao       = is_simulacao
        it_origem          = it_origem
        is_material_centro = is_material_centro
      RECEIVING
        return             = return.



  ENDMETHOD.


  METHOD FILL_CUSTO_ELEMENTAR_PRODUCAO.

    DATA lr_id_cais TYPE RANGE OF /qaps/cais-id_cais.

    DATA(lt_material) = NEW /qaps/cl_mdl_material( )->get_materiais_by_material( iv_matnr ).
    DATA(ls_material) = lt_material[ 1 ].

*    select SINGLE *
*      from /qaps/v_ponto
*      where id_ponto = @iv_id_origem
*      into @data(ls_ponto).

    SELECT DISTINCT id_custo_elementar,id_var_input,tipo_regra,matnr,
           id_grupo_produto,agregador,mat_planejado,
           tipo_origem,id_origem,tipo_destino,id_destino, ponto~codigo
      FROM /qaps/var_input
      LEFT OUTER JOIN /qaps/v_ponto AS ponto
      ON /qaps/var_input~id_destino = ponto~id_ponto
      WHERE id_custo_elementar = @cs_expressao-id_custo_elementar
      AND   ( id_origem = @iv_id_origem )
      AND   /qaps/var_input~id_simulacao = @ms_simulacao-id_simulacao
      AND  ( ponto~codigo = @iv_cod_grp_planta
           OR ponto~codigo = @iv_werks )
      INTO TABLE @DATA(lt_var_input).

    IF lines( lt_var_input ) = 0.
*      BREAK-POINT.
      SELECT DISTINCT id_custo_elementar,id_var_input,tipo_regra,matnr,
           id_grupo_produto,agregador,mat_planejado,
           tipo_origem,id_origem,tipo_destino,id_destino, ponto~codigo
      FROM /qaps/var_input
      LEFT OUTER JOIN /qaps/v_ponto AS ponto
      ON /qaps/var_input~id_destino = ponto~id_ponto
      WHERE id_custo_elementar = @cs_expressao-id_custo_elementar
      AND   id_simulacao = @ms_simulacao-id_simulacao
      AND  ( ponto~codigo = @iv_cod_grp_planta
           OR ponto~codigo = @iv_werks )
*      AND   ( id_origem = @is_origem-id_origem OR id_origem = @is_origem-id_origem_matriz )
      INTO TABLE @lt_var_input.
    ENDIF.

    IF line_exists( lt_var_input[ matnr = iv_matnr ] ).
*                      AND NOT iv_matnr IS INITIAL.
      DATA(lt_var_input_aux) = lt_var_input.
      DELETE lt_var_input_aux WHERE matnr <> iv_matnr .

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> iv_cod_grp_planta.
      ENDIF.

      DATA(ls_var_input) = lt_var_input_aux[ matnr = iv_matnr ].
      DATA(lt_source) = lt_var_input_aux.
      DELETE lt_var_input WHERE matnr <> iv_matnr.

    ELSEIF line_exists( lt_var_input[ agregador = ls_material-agregador ] )
      AND NOT ls_material-agregador IS INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE agregador <> ls_material-agregador.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> iv_cod_grp_planta.
      ENDIF.

      ls_var_input = lt_var_input_aux[ agregador = ls_material-agregador ].
      DELETE lt_var_input WHERE agregador <> ls_material-agregador.
    ELSEIF line_exists( lt_var_input[ mat_planejado = ls_material-mat_planejado ] ).
*                      and not is_material_centro-mat_planejado is INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE mat_planejado <> ls_material-mat_planejado.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> iv_cod_grp_planta.
      ENDIF.

      ls_var_input = lt_var_input_aux[ mat_planejado = ls_material-mat_planejado ].
      DELETE lt_var_input WHERE mat_planejado <> ls_material-mat_planejado.
    ELSEIF line_exists( lt_var_input[ id_grupo_produto = ls_material-id_grupo_produto ] ).
*              and is_material_centro-id_grupo_produto is INITIAL.
      lt_var_input_aux = lt_var_input.
      DELETE lt_var_input_aux WHERE id_grupo_produto <> ls_material-id_grupo_produto.

      IF lines( lt_var_input_aux ) > 1.
        DELETE lt_var_input_aux WHERE codigo <> iv_cod_grp_planta.
      ENDIF.

      ls_var_input = VALUE #( lt_var_input_aux[ id_grupo_produto = ls_material-id_grupo_produto ] OPTIONAL ).
      DELETE lt_var_input WHERE id_grupo_produto <> ls_material-id_grupo_produto.
    ELSE.
      ls_var_input = VALUE #( lt_var_input[ tipo_regra = 'GE' ] OPTIONAL ).
      DELETE lt_var_input WHERE tipo_regra <> 'GE'.
    ENDIF.



    CHECK lines( lt_var_input ) > 0."NOT ls_var_input IS INITIAL.


    SELECT *
     FROM /qaps/var_input
     FOR ALL ENTRIES IN @lt_var_input
     WHERE id_custo_elementar = @lt_var_input-id_custo_elementar
     AND   id_var_input = @lt_var_input-id_var_input
     AND   tipo_regra   = @lt_var_input-tipo_regra
     AND   id_simulacao = @ms_simulacao-id_simulacao
     INTO TABLE @DATA(lt_valores).

    SORT lt_valores BY periodo.

    DATA(ls_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( )->get_variavel_by_id( cs_expressao-id_custo_elementar ).

*    break c060863.
    LOOP AT lt_valores INTO DATA(ls_valores).

      ASSIGN cs_expressao-t_valores[ periodo = ls_valores-periodo ] TO FIELD-SYMBOL(<fs_value>).

      CASE ls_custo_elementar-tipo_dado.
        WHEN '1'. "Valor

          IF mv_moeda_local <> mv_moeda_final.
            DATA(ls_taxa_cambio) = mt_taxa_cambio[ periodo     = ls_valores-periodo
                                                   moeda_final = mv_moeda_final ].
          ELSE.
            ls_taxa_cambio-moeda_local = mv_moeda_local.
          ENDIF.

          ASSIGN COMPONENT 'VALOR_VAR_ELEMENTAR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_value_elm>).
          <fv_value_elm> = ls_valores-valor.

          ASSIGN COMPONENT 'MOEDA_VAR_ELEMENTAR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_moeda_elm>).
          <fv_moeda_elm> = ls_custo_elementar-moeda.

          ASSIGN COMPONENT 'VALOR' OF STRUCTURE <fs_value> TO FIELD-SYMBOL(<fv_value>).
          IF mv_moeda_local <> ls_custo_elementar-moeda.
            <fv_value> = <fv_value> + ( ls_valores-valor * ls_taxa_cambio-taxa ).
          ELSE.
            <fv_value> = <fv_value> + ls_valores-valor.
          ENDIF.

*          BREAK c060863.
          ASSIGN COMPONENT 'VALOR_MOEDA_FINAL' OF STRUCTURE <fs_value>
            TO FIELD-SYMBOL(<fv_value_final>).
          IF mv_moeda_local <> mv_moeda_final.
            <fv_value_final> = <fv_value_final> + ( ls_valores-valor / ls_taxa_cambio-taxa ).
          ELSE.
            <fv_value_final> = <fv_value_final> + ls_valores-valor.
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


  METHOD GET_STD_PRD_COMPONENTS.

    BREAK C060863.

  ENDMETHOD.


  METHOD SOLVE_STD_PRD_TOTAL_EXP_CONV.

    data lv_fator_conversao type /qaps/s_values-fator_conversao.

    ms_simulacao = is_simulacao.

    "grp planta/werks - TODO
    SELECT SINGLE pa~id_std_prod_pa,pa~id_std_producao,pa~matnr,pa~categoria,
      cp~id_std_prod_cp,cp~componente,cp~menge,cp~meins,cp~rendimento
      FROM /qaps/std_prd_pa AS pa
      INNER JOIN /qaps/std_prd_cp AS cp
      ON pa~id_std_prod_pa = cp~id_std_prod_pa
      WHERE pa~id_std_producao = @ms_simulacao-id_std_producao
      AND   pa~matnr = @cs_expressao-matnr
      AND categoria = 'C'
      INTO @DATA(ls_std_prd).

    IF ls_std_prd-meins = 'KG'.
      lv_fator_conversao = ls_std_prd-menge / 1000.
    ENDIF.

    "1ª Etapa - Calcular Fator Conversão
    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).

      IF <fs_expressao>-tipo_node <> 'E' AND <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
      ELSEIF <fs_expressao>-tipo_node = 'R'.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

    DATA(lt_expressao_conversao) = mt_expressao.
    DELETE lt_expressao_conversao WHERE price_field = ''.

    LOOP AT lt_expressao_conversao ASSIGNING <fs_expressao>.

      LOOP AT <fs_expressao>-t_valores ASSIGNING FIELD-SYMBOL(<fs_c>).

        CHECK <fs_c>-valor > 0.
        <fs_c>-fator_conversao = lv_fator_conversao.
        <fs_c>-valor_conversao_moeda = <fs_c>-valor - ( <fs_c>-valor * <fs_c>-fator_conversao ).

      ENDLOOP.

    ENDLOOP.

    "2ª Etapa - Calcular Total
    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

    LOOP AT mt_expressao ASSIGNING <fs_expressao>.

      IF <fs_expressao>-tipo_node <> 'E' AND <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
      ELSEIF <fs_expressao>-tipo_variavel = 'P' AND <fs_expressao>-tipo_node = 'E'.

        fill_custo_elementar_producao( EXPORTING iv_cod_grp_planta = cs_expressao-cod_grp_planta
                                                 iv_werks = cs_expressao-werks
                                                 iv_matnr = cs_expressao-matnr
                                                 iv_id_origem = cs_expressao-id_origem
                                                 iv_tipo_origem = cs_expressao-tipo_origem
                                       CHANGING cs_expressao = <fs_expressao> ).
      ELSEIF <fs_expressao>-tipo_node = 'R'.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    "APLICAR CONVERSÃO
*    BREAK c060863.
    loop at mt_expressao ASSIGNING <fs_expressao>.
      check <fs_expressao>-price_field = 'X'.
      data(ls_exp_conv) = lt_expressao_conversao[ id_calc_node = <fs_expressao>-id_calc_node ].
      loop at <fs_expressao>-t_valores ASSIGNING <fs_c>.

        check <fs_c>-valor > 0.
        data(ls_value) = ls_exp_conv-t_valores[ periodo = <fs_c>-periodo ].
        <fs_c>-valor = <fs_c>-valor - ls_value-valor_conversao_moeda.
        <fs_c>-fator_conversao = lv_fator_conversao.
        <fs_c>-valor_conversao_moeda = ls_value-valor_conversao_moeda.
      endloop.
    ENDLOOP.

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

*    break c060863.
    cs_expressao-t_expressao = mt_expressao.



  ENDMETHOD.


  METHOD solve_std_prd_total_exp_conv_2.

    DATA lv_fator_conversao TYPE /qaps/s_values-fator_conversao.

    BREAK abap.
    ms_simulacao = is_simulacao.

    "grp planta/werks - TODO
    SELECT SINGLE pa~id_std_prod_pa,pa~id_std_producao,pa~matnr,pa~categoria,
      cp~id_std_prod_cp,cp~componente,cp~menge,cp~meins,cp~rendimento
      FROM /qaps/std_prd_pa AS pa
      INNER JOIN /qaps/std_prd_cp AS cp
      ON pa~id_std_prod_pa = cp~id_std_prod_pa
      WHERE pa~id_std_producao = @ms_simulacao-id_std_producao
      AND   pa~matnr = @cs_expressao-matnr
      AND categoria = 'C'
      INTO @DATA(ls_std_prd).

    IF ls_std_prd-meins = 'KG'.
      lv_fator_conversao = ls_std_prd-menge / 1000.
    ENDIF.

    "1ª Etapa - Calcular Fator Conversão
    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).

      IF <fs_expressao>-tipo_node <> 'E' AND <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
      ELSEIF <fs_expressao>-tipo_node = 'R'.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

    DATA(lt_expressao_conversao) = mt_expressao.
    DELETE lt_expressao_conversao WHERE price_field = ''.

*    DELETE lt_expressao_conversao WHERE tipo_variavel <> 'C'.
*    break c060863.
    LOOP AT lt_expressao_conversao ASSIGNING <fs_expressao>.

      LOOP AT <fs_expressao>-t_valores ASSIGNING FIELD-SYMBOL(<fs_c>).

        CHECK <fs_c>-valor > 0.
        <fs_c>-fator_conversao = lv_fator_conversao.
        <fs_c>-valor_conversao_moeda = <fs_c>-valor - ( <fs_c>-valor * <fs_c>-fator_conversao ).

      ENDLOOP.

    ENDLOOP.

    "2ª Etapa - Calcular Total
    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

    LOOP AT mt_expressao ASSIGNING <fs_expressao>.

      IF <fs_expressao>-tipo_node <> 'E' AND <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
      ELSEIF <fs_expressao>-tipo_variavel = 'P' AND <fs_expressao>-tipo_node = 'E'.

        fill_custo_elementar_producao( EXPORTING iv_cod_grp_planta = cs_expressao-cod_grp_planta
                                                 iv_werks          = cs_expressao-werks
                                                 iv_matnr          = cs_expressao-matnr
                                                 iv_id_origem      = cs_expressao-id_origem
                                                 iv_tipo_origem    = cs_expressao-tipo_origem
                                       CHANGING  cs_expressao      = <fs_expressao> ).
      ELSEIF <fs_expressao>-tipo_node = 'R'.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    "APLICAR CONVERSÃO
    LOOP AT mt_expressao ASSIGNING <fs_expressao>.
      CHECK <fs_expressao>-price_field = 'X'.
      DATA(ls_exp_conv) = lt_expressao_conversao[ id_calc_node = <fs_expressao>-id_calc_node ].
      LOOP AT <fs_expressao>-t_valores ASSIGNING <fs_c>.

        CHECK <fs_c>-valor > 0.
        DATA(ls_value) = ls_exp_conv-t_valores[ periodo = <fs_c>-periodo ].
        <fs_c>-valor = <fs_c>-valor - ls_value-valor_conversao_moeda.
        <fs_c>-fator_conversao = lv_fator_conversao.
        <fs_c>-valor_conversao_moeda = ls_value-valor_conversao_moeda.
      ENDLOOP.
    ENDLOOP.

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

*    break c060863.
    cs_expressao-t_expressao = mt_expressao.

  ENDMETHOD.
ENDCLASS.
