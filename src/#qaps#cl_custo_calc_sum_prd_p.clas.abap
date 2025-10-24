class /QAPS/CL_CUSTO_CALC_SUM_PRD_P definition
  public
  inheriting from /QAPS/CL_CUSTO_CALCULATE_BASE
  final
  create public .

public section.

  methods SOLVE_STD_PRD_TOTAL_EXP_PROD_2
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
    changing
      !CS_EXPRESSAO type /QAPS/S_RETORNO_CALCULO .
  methods SOLVE_STD_PRD_TOTAL_EXP_PROD
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
    changing
      !CS_EXPRESSAO type /QAPS/S_PONDERACAO .

  methods EXECUTE
    redefinition .
  methods GET_STD_PRODUCAO_CONVERSAO
    redefinition .
  methods GET_STD_PRODUCAO_PRODUCAO
    redefinition .
protected section.

  methods CALCULAR_PARAMETRO
    redefinition .
  methods SOLVE_EXPRESSAO
    redefinition .
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



CLASS /QAPS/CL_CUSTO_CALC_SUM_PRD_P IMPLEMENTATION.


  METHOD CALCULAR_PARAMETRO.

    DATA: lt_parametros TYPE /qaps/t_calc_parameter,
          lv_valor      TYPE /qaps/valor_absoluto.
*    break c060863.
    LOOP AT cs_expressao-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).

      CHECK NOT cs_expressao-expressao IS INITIAL.

      REFRESH: lt_parametros.

      LOOP AT it_parametros INTO DATA(ls_param).

        DATA(ls_valor) = ls_param-t_valores[ periodo = <fs_valores>-periodo ].

        data(ls_item_param) = VALUE /qaps/s_calc_parameter(
            fieldname  = ls_param-parameter_name
            tipo_dado  = COND #( WHEN ls_param-tipo_dado = 1 OR ls_param-tipo_dado = 2 THEN ls_param-tipo_dado
                                 ELSE 1 )
            valor      = ls_valor-valor
            percentual = ls_valor-percentual
        ).

        if ls_item_param-tipo_dado = 2.
          ls_item_param-percentual = 100.
        endif.

        append ls_item_param TO lt_parametros.

      ENDLOOP.

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

      data(lv_periodo) = |{ cs_expressao-t_valores[ 1 ]-periodo }|.

      LOG-POINT ID /qaps/pricing_lista_custo
          SUBKEY |{ cs_expressao-descricao } Período: { lv_periodo } valor:{ lv_valor }|
          FIELDS ms_simulacao cs_expressao lt_parametros lv_valor.

      <fs_valores>-valor = lv_valor.

    ENDLOOP.

  ENDMETHOD.


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

*    BREAK c060863.
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


  METHOD GET_STD_PRODUCAO_CONVERSAO.

*    DATA: lv_matnr      TYPE matnr,
*          lv_componente TYPE matnr.
*
*    DATA: lt_return TYPE /qaps/t_retorno_calculo,
*          ls_total  TYPE /qaps/s_retorno_calculo.
*
*
*    ms_simulacao = is_simulacao.
*
*    SELECT SINGLE *
*      FROM /qaps/std_prd_pa
*      WHERE matnr = @is_material_centro-matnr
*      AND   categoria = 'C'
*      AND   ( werks         = @is_material_centro-werks
*      OR      id_grp_planta = @is_material_centro-id_grp_planta )
*      INTO @DATA(ls_prod_acabado).
*
*    lv_matnr = |{ is_material_centro-matnr ALPHA = OUT }|.
*
**    CHECK lines( lt_prod_acabado ) > 0.
*
*    SELECT *
*      FROM /qaps/std_prd_cp
**      FOR ALL ENTRIES IN @lt_prod_acabado
*      WHERE id_std_prod_pa = @ls_prod_acabado-id_std_prod_pa
*      INTO TABLE @DATA(lt_componentes).
*
*
*    LOOP AT lt_componentes INTO DATA(ls_componentes).
*
*      REFRESH lt_return.
*
*      lv_componente = |{ ls_componentes-componente ALPHA = IN WIDTH = 18 }|.
*
*      DATA(lt_importado) = it_importado.
*      DATA(lt_nacional) = it_nacional.
*      DATA(lt_transf_importacao) = it_transf_importacao.
*      DATA(lt_transf_nacional) = it_transf_nacional.
*
*      DELETE lt_importado WHERE material <> lv_componente
*                          OR plant <> is_material_centro-werks.
*
*      DELETE lt_nacional WHERE material <> lv_componente
*                         OR plant <> is_material_centro-werks.
*
*      DELETE lt_transf_importacao WHERE material <> lv_componente
*                                  OR plant <> is_material_centro-werks.
*
*      DELETE lt_transf_nacional WHERE material <> lv_componente
*                                OR plant <> is_material_centro-werks.
*
*      APPEND LINES OF lt_importado TO lt_return.
*      APPEND LINES OF lt_nacional TO lt_return.
**      APPEND LINES OF lt_transf_importacao TO lt_return.
**      APPEND LINES OF lt_transf_nacional TO lt_return.
*
*      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs>).
*        <fs>-std_prd_pa = lv_matnr.
*        <fs>-std_prd_cp = lv_componente.
*        <fs>-std_prd_werks = is_material_centro-werks.
*
*        IF ls_componentes-meins = 'TO'.
*          <fs>-std_prd_menge = ls_componentes-menge.
*          <fs>-std_prd_meins = ls_componentes-meins.
*        ELSE.
*          <fs>-std_prd_menge = converter_unidade( iv_matnr          = lv_componente
*                                                  iv_quantidade     = ls_componentes-menge
*                                                  iv_unidade_input  = ls_componentes-meins
*                                                  iv_unidade_output = 'TO' ).
*          <fs>-std_prd_meins = 'TO'.
*        ENDIF.
*
*
**        IF ls_prod_acabado-categoria = 'C'.
*
*        LOOP AT <fs>-t_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).
*          CHECK <fs_expressao>-price_field = 'X'.
*          LOOP AT <fs_expressao>-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).
*            <fs_valores>-valor = <fs_valores>-valor * <fs>-std_prd_menge.
*          ENDLOOP.
*        ENDLOOP.
*
*        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).
*
*      ENDLOOP.
*
*      APPEND LINES OF lt_return TO return.
*
*    ENDLOOP.
*
*    CHECK lines( return ) > 0.
*    BREAK c060863.
*
*    DATA(ls_source) = return[ 1 ].
*    ls_total = CORRESPONDING #( ls_source ).
*    ls_total-matnr = |{ ls_source-std_prd_pa ALPHA = IN WIDTH = 18 }|..
*    ls_total-std_prd_total = abap_true.
*
*    mo_calc_producao->solve_std_prd_total_exp_conv_2(
*          EXPORTING is_simulacao = ms_simulacao
*          CHANGING cs_expressao = ls_total
*    ).
*
*    APPEND ls_total TO return.
*

  ENDMETHOD.


  METHOD GET_STD_PRODUCAO_PRODUCAO.

*    DATA: lv_matnr      TYPE matnr,
*          lv_componente TYPE matnr.
*
*    DATA lt_return TYPE /qaps/t_retorno_calculo.
*
*    ms_simulacao = is_simulacao.
*
*    SELECT SINGLE *
*      FROM /qaps/std_prd_pa
*      WHERE matnr = @is_material_centro-matnr
*      AND   categoria = 'P'
*      AND   ( werks         = @is_material_centro-werks
*      OR      id_grp_planta = @is_material_centro-id_grp_planta )
*      INTO @DATA(ls_prod_acabado).
*
*    lv_matnr = |{ is_material_centro-matnr ALPHA = OUT }|.
*
**    CHECK lines( lt_prod_acabado ) > 0.
*
*    SELECT *
*      FROM /qaps/std_prd_cp
**      FOR ALL ENTRIES IN @lt_prod_acabado
*      WHERE id_std_prod_pa = @ls_prod_acabado-id_std_prod_pa
*      INTO TABLE @DATA(lt_componentes).
*
**    IF is_material_centro-matnr = '000000001000006142'
**       AND is_material_centro-werks = 'RIG1'.
**      BREAK c060863.
**    ENDIF.
*
*    LOOP AT lt_componentes INTO DATA(ls_componentes).
*
*      REFRESH lt_return.
*
*      lv_componente = |{ ls_componentes-componente ALPHA = IN WIDTH = 18 }|.
*
*      DATA(lt_importado) = it_importado.
*      DATA(lt_nacional) = it_nacional.
*      DATA(lt_transf_importacao) = it_transf_importacao.
*      DATA(lt_transf_nacional) = it_transf_nacional.
*      DATA(lt_transf_std_prd_conversao) = it_transf_std_prd_conversao.
*
*      DELETE lt_importado WHERE material <> lv_componente
*                          OR plant <> is_material_centro-werks.
*
*      DELETE lt_nacional WHERE material <> lv_componente
*                         OR plant <> is_material_centro-werks.
*
*      DELETE lt_transf_importacao WHERE material <> lv_componente
*                                  OR plant <> is_material_centro-werks.
*
*      DELETE lt_transf_nacional WHERE material <> lv_componente
*                                OR plant <> is_material_centro-werks.
*
*      DELETE lt_transf_std_prd_conversao WHERE std_prd_pa <> lv_componente
*                                         OR plant <> is_material_centro-werks.
*
*      APPEND LINES OF lt_importado TO lt_return.
*      APPEND LINES OF lt_nacional TO lt_return.
**      APPEND LINES OF lt_transf_std_prd_conversao TO lt_return.
*
*      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs>).
*
*        <fs>-std_prd_pa = lv_matnr.
*        <fs>-std_prd_cp = lv_componente.
*        <fs>-std_prd_werks = is_material_centro-werks.
*
*        if ls_componentes-meins = 'TO'.
*          <fs>-std_prd_menge = ls_componentes-menge.
*          <fs>-std_prd_meins = ls_componentes-meins.
*        else.
*          <fs>-std_prd_menge = converter_unidade( iv_matnr          = lv_componente
*                                                  iv_quantidade     = ls_componentes-menge
*                                                  iv_unidade_input  = ls_componentes-meins
*                                                  iv_unidade_output = 'TO' ).
*          <fs>-std_prd_meins = 'TO'.
*        endif.
*
*
*        calcular_producao( EXPORTING iv_quantidade = <fs>-std_prd_menge
*                           CHANGING ct_expressao = <fs>-t_expressao ).
*
*        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).
*
*      ENDLOOP.
*
*      APPEND LINES OF lt_return TO return.
*
*      LOOP AT lt_transf_std_prd_conversao ASSIGNING <fs>.
*
*        <fs>-std_prd_pa = lv_matnr.
*        <fs>-std_prd_cp = lv_componente.
*        <fs>-std_prd_werks = is_material_centro-werks.
*
*        "Calcula proporcional
*        if ls_componentes-meins = 'TO'.
*          <fs>-std_prd_menge = ls_componentes-menge * <fs>-std_prd_menge.
*          <fs>-std_prd_meins = ls_componentes-meins.
*        else.
*          ls_componentes-menge = converter_unidade( iv_matnr          = lv_componente
*                                                  iv_quantidade     = ls_componentes-menge
*                                                  iv_unidade_input  = ls_componentes-meins
*                                                  iv_unidade_output = 'TO' ).
*          <fs>-std_prd_menge = ls_componentes-menge * <fs>-std_prd_menge.
*          <fs>-std_prd_meins = ls_componentes-meins.
*        endif.
*
*
*
*        calcular_producao( EXPORTING iv_quantidade = <fs>-std_prd_menge
*                           CHANGING ct_expressao = <fs>-t_expressao ).
*
*        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).
*
*        "Retorna valor original
*        <fs>-std_prd_menge = ls_componentes-menge.
*
*      ENDLOOP.
*
*      APPEND LINES OF lt_transf_std_prd_conversao TO return.
*
*    ENDLOOP.
*
*
*
  ENDMETHOD.


  method SOLVE_EXPRESSAO.

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


  endmethod.


  METHOD SOLVE_STD_PRD_TOTAL_EXP_PROD.

*    BREAK c060863.
    ms_simulacao = is_simulacao.

    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

*    break c060863.
    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).

      IF <fs_expressao>-tipo_node <> 'E' and <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
*      ELSEIF <fs_expressao>-tipo_variavel = 'P' AND <fs_expressao>-tipo_node = 'E'.
*
*        fill_custo_elementar_producao( EXPORTING iv_cod_grp_planta = cs_expressao-cod_grp_planta
*                                                 iv_werks = cs_expressao-werks
*                                                 iv_matnr = cs_expressao-matnr
*                                                 iv_id_origem = cs_expressao-id_origem
*                                                 iv_tipo_origem = cs_expressao-tipo_origem
*                                       CHANGING cs_expressao = <fs_expressao> ).
      elseIF <fs_expressao>-tipo_node = 'R'.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

*    break c060863.
    cs_expressao-t_expressao = mt_expressao.

  ENDMETHOD.


  METHOD SOLVE_STD_PRD_TOTAL_EXP_PROD_2.

    FIELD-SYMBOLS <fs_expressao> TYPE /qaps/s_expressao.
    ms_simulacao = is_simulacao.

    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

*    BREAK c060863.
    LOOP AT mt_expressao ASSIGNING <fs_expressao>.

      IF <fs_expressao>-tipo_node <> 'E' AND <fs_expressao>-tipo_node <> 'R'.
        IF <fs_expressao>-tipo_node = 'C' AND <fs_expressao>-price_field = 'X'.
          CLEAR <fs_expressao>-calculated.
        ENDIF.
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

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

*    break c060863.
    cs_expressao-t_expressao = mt_expressao.



  ENDMETHOD.
ENDCLASS.
