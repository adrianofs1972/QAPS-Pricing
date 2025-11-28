class /QAPS/CL_CUSTO_CALC_STD_PRD definition
  public
  inheriting from /QAPS/CL_CUSTO_CALCULATE_BASE
  final
  create public .

public section.

  methods EXECUTE
    redefinition .
  methods GET_STD_PRODUCAO_CONVERSAO
    redefinition .
  methods GET_STD_PRODUCAO_PRODUCAO
    redefinition .
protected section.

  methods SOLVE_EXPRESSAO
    redefinition .
  methods CHECK_ERRORS_RETORNO_CALCULO
    redefinition .
private section.

  methods FILL_TOTAL_STD_PRD_PROD
    importing
      !IT_SOURCE type /QAPS/T_RETORNO_CALCULO
    changing
      !CS_TOTAL type /QAPS/S_RETORNO_CALCULO .
  methods CALCULAR_PRODUCAO
    importing
      !IV_QUANTIDADE type KMPMG
      !IV_QUANTIDADE_FULL type KMPMG
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
  methods GET_STD_PRD_COMPONENTS
    changing
      !CT_DATA type /QAPS/T_PREMISSA_MATRIZ .
ENDCLASS.



CLASS /QAPS/CL_CUSTO_CALC_STD_PRD IMPLEMENTATION.


  METHOD calcular_producao.

    REFRESH mt_expressao.
    mt_expressao = ct_expressao.
    REFRESH ct_expressao.

    "Clear Calcution Nodes e ajusta valores Elementares
*    BREAK c060863.
    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-tipo_node.
        WHEN 'E'.
          IF <fs>-tipo_variavel = 'P'.
            LOOP AT <fs>-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).
              <fs_valores>-valor = <fs_valores>-valor * iv_quantidade_full.
            ENDLOOP.
          ELSE.

            LOOP AT <fs>-t_valores ASSIGNING <fs_valores>.
              <fs_valores>-valor = <fs_valores>-valor * iv_quantidade.
            ENDLOOP.
          ENDIF.
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


  METHOD check_errors_retorno_calculo.

    DATA: lv_matnr TYPE matnr,
          lv_componente TYPE matnr,
          lv_message    TYPE bapi_msg.


    ASSIGN ct_data[ std_prd_total = 'X' ] TO FIELD-SYMBOL(<fs_total>).

    CHECK sy-subrc IS INITIAL.

    SELECT *
      FROM /qaps/v_stp_all
      WHERE id_std_producao = @ms_simulacao-id_std_producao
      AND   matnr = @<fs_total>-matnr
      AND   cod_grp_planta = @<fs_total>-cod_grp_planta
      INTO TABLE @DATA(lt_std_prd).

    SELECT *
      FROM /qaps/v_stp_all
      WHERE id_std_producao = @ms_simulacao-id_std_producao
      AND   matnr = @<fs_total>-matnr
      AND   werks = @<fs_total>-werks
      APPENDING TABLE @lt_std_prd.

    lv_matnr = |{ <fs_total>-matnr ALPHA = OUT }|.

    LOOP AT lt_std_prd INTO DATA(ls_std_prd).

      CHECK NOT line_exists( ct_data[ std_prd_cp = ls_std_prd-componente ] ).
      lv_componente = |{ ls_std_prd-componente ALPHA = OUT }|.
      lv_message = |PA: { lv_matnr } - Componente { lv_componente } - { <fs_total>-cod_grp_planta }/{ <fs_total>-werks } não possui custo|.
      APPEND VALUE bapiret2( type =  'E' message = lv_message ) TO <fs_total>-t_erros.

    ENDLOOP.

  ENDMETHOD.


  METHOD converter_unidade.

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


  METHOD execute.

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


  METHOD fill_total_std_prd_prod.

    "Limpar calculadas
    LOOP AT cs_total-t_expressao ASSIGNING FIELD-SYMBOL(<fs_exp_clean>).
      LOOP AT <fs_exp_clean>-t_valores ASSIGNING FIELD-SYMBOL(<fs_val_clean>).
        DATA(lv_periodo) = <fs_val_clean>-periodo.
        CLEAR: <fs_val_clean>.
        <fs_val_clean>-periodo = lv_periodo.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_source INTO DATA(ls_source).

      LOOP AT ls_source-t_expressao INTO DATA(ls_exp_source).

        CHECK ls_exp_source-tipo_node = 'E' or ls_exp_source-tipo_node = 'C'.

        ASSIGN cs_total-t_expressao[ id_calc_node = ls_exp_source-id_calc_node ]
            TO FIELD-SYMBOL(<fs_exp_target>).

        LOOP AT ls_exp_source-t_valores INTO DATA(ls_valores).

*          DATA(ls_distribuicao) = it_distribuicao[ matnr = ls_source-matnr
*                                                    ].
*          DATA(ls_trajeto) = it_trajeto[ matnr = ls_source-matnr ].

          ASSIGN <fs_exp_target>-t_valores[ periodo = ls_valores-periodo ]
              TO FIELD-SYMBOL(<fs_val_target>).

          IF ls_valores-valor > 0.

            <fs_val_target>-valor = <fs_val_target>-valor + ls_valores-valor.
            <fs_val_target>-valor_var_elementar = <fs_val_target>-valor_var_elementar +
                      ls_valores-valor_var_elementar.
            <fs_val_target>-valor_moeda_final = <fs_val_target>-valor_moeda_final +
                  ls_valores-valor_moeda_final.
          ENDIF.

*          IF ls_valores-percentual > 0.
**            break c060863.
*            <fs_val_target>-percentual = ls_valores-percentual.
*          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_std_prd_components.

    BREAK C060863.

  ENDMETHOD.


  METHOD get_std_producao_conversao.

    DATA: lv_matnr       TYPE matnr,
          lv_componente  TYPE matnr,
          lv_premissa    TYPE /qaps/percentual,
          lv_trajeto     TYPE /qaps/percentual,
          lv_coeficiente TYPE /qaps/percentual.

    DATA: lt_return TYPE /qaps/t_retorno_calculo,
          ls_total  TYPE /qaps/s_retorno_calculo.

    ms_simulacao = is_simulacao.

    SELECT SINGLE *
      FROM /qaps/std_prd_pa
      WHERE matnr = @is_material_centro-matnr
      AND   categoria = 'C'
      AND   ( werks         = @is_material_centro-werks
      OR      id_grp_planta = @is_material_centro-id_grp_planta )
      AND   id_std_producao = @ms_simulacao-id_std_producao
      INTO @DATA(ls_prod_acabado).

    lv_matnr = |{ is_material_centro-matnr ALPHA = OUT }|.

*    CHECK lines( lt_prod_acabado ) > 0.

    SELECT *
      FROM /qaps/std_prd_cp
*      FOR ALL ENTRIES IN @lt_prod_acabado
      WHERE id_std_prod_pa = @ls_prod_acabado-id_std_prod_pa
      AND   id_std_producao = @ms_simulacao-id_std_producao
      INTO TABLE @DATA(lt_componentes).


    LOOP AT lt_componentes INTO DATA(ls_componentes).

      REFRESH lt_return.

      lv_componente = |{ ls_componentes-componente ALPHA = IN WIDTH = 18 }|.

      DATA(lt_importado) = it_importado.
      DATA(lt_nacional) = it_nacional.
      DATA(lt_transf_importacao) = it_transf_importacao.
      DATA(lt_transf_nacional) = it_transf_nacional.

      DELETE lt_importado WHERE material <> lv_componente
                          OR plant <> is_material_centro-werks.

      DELETE lt_nacional WHERE material <> lv_componente
                         OR plant <> is_material_centro-werks.

      DELETE lt_transf_importacao WHERE material <> lv_componente
                                  OR plant <> is_material_centro-werks.

      DELETE lt_transf_nacional WHERE material <> lv_componente
                                OR plant <> is_material_centro-werks.

      APPEND LINES OF lt_importado TO lt_return.
      APPEND LINES OF lt_nacional TO lt_return.
*      APPEND LINES OF lt_transf_importacao TO lt_return.
*      APPEND LINES OF lt_transf_nacional TO lt_return.

      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-std_prd_pa = lv_matnr.
        <fs>-std_prd_cp = lv_componente.
        <fs>-std_prd_werks = is_material_centro-werks.

        IF ls_componentes-meins = 'TO'.
          <fs>-std_prd_menge = ls_componentes-menge.
          <fs>-std_prd_meins = ls_componentes-meins.
        ELSE.
          <fs>-std_prd_menge = converter_unidade( iv_matnr          = lv_componente
                                                  iv_quantidade     = ls_componentes-menge
                                                  iv_unidade_input  = ls_componentes-meins
                                                  iv_unidade_output = 'TO' ).
          <fs>-std_prd_meins = 'TO'.
        ENDIF.


*        IF ls_prod_acabado-categoria = 'C'.

        LOOP AT <fs>-t_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).
          CHECK <fs_expressao>-price_field = 'X'.
          LOOP AT <fs_expressao>-t_valores ASSIGNING FIELD-SYMBOL(<fs_valores>).
            <fs_valores>-valor = <fs_valores>-valor * <fs>-std_prd_menge.
          ENDLOOP.
        ENDLOOP.

        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).

      ENDLOOP.

      APPEND LINES OF lt_return TO return.

    ENDLOOP.

    CHECK lines( return ) > 0.

*    BREAK abap.
    DATA lv_first TYPE abap_bool.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs_return>).

      IF sy-tabix = 1.
        ls_total = CORRESPONDING #( <fs_return> ).
        ls_total-matnr = |{ <fs_return>-std_prd_pa ALPHA = IN WIDTH = 18 }|.
        ls_total-std_prd_total = abap_true.
        lv_first = abap_true.
      ELSE.
        lv_first = abap_false.
      ENDIF.

      LOOP AT <fs>-t_expressao ASSIGNING <fs_expressao>.
        CHECK <fs_expressao>-price_field = 'X'.

        ASSIGN ls_total-t_expressao[ id_custo_elementar = <fs_expressao>-id_custo_elementar ]
              TO FIELD-SYMBOL(<fs_total_expr>).

        LOOP AT <fs_expressao>-t_valores ASSIGNING <fs_valores>.

          ASSIGN <fs_total_expr>-t_valores[ periodo = <fs_valores>-periodo ] TO
             FIELD-SYMBOL(<fs_total_valores>).

          lv_premissa = ( <fs>-t_dist_premissa[ periodo = <fs_valores>-periodo ]-percentual ) / 100.
          lv_trajeto  = ( <fs>-trajeto-t_dist_trajeto[ periodo = <fs_valores>-periodo ]-percentual ) / 100.

          lv_coeficiente = lv_premissa * lv_trajeto.

          IF lv_first = abap_true.
            <fs_total_valores>-valor = ( <fs_valores>-valor * lv_coeficiente ).
          ELSE.
            <fs_total_valores>-valor = <fs_total_valores>-valor + ( <fs_valores>-valor * lv_coeficiente ).
          ENDIF.

        ENDLOOP.
      ENDLOOP.

      fill_moeda_saida( CHANGING ct_expressao = ls_total-t_expressao ).

    ENDLOOP.

*    DATA(ls_source) = return[ 1 ].
*    ls_total = CORRESPONDING #( ls_source ).
*    ls_total-matnr = |{ ls_source-std_prd_pa ALPHA = IN WIDTH = 18 }|.
*    ls_total-std_prd_total = abap_true.

    break abap.
    mo_calc_producao_conv->solve_std_prd_total_exp_conv_2(
      EXPORTING
        is_simulacao = ms_simulacao
      CHANGING
        cs_expressao = ls_total
    ).

    APPEND ls_total TO return.


  ENDMETHOD.


  METHOD get_std_producao_producao.

    DATA: lv_matnr      TYPE matnr,
          lv_componente TYPE matnr,
          lv_where      type string.

    DATA: lt_return TYPE /qaps/t_retorno_calculo,
          ls_total  TYPE /qaps/s_retorno_calculo.

    ms_simulacao = is_simulacao.

*    break abap.
    if not is_material_centro-werks is INITIAL and not is_material_centro-id_grp_planta is INITIAL.
      lv_where = ` werks = @is_material_centro-werks  `.
      lv_where = lv_where && ` and id_grp_planta = @is_material_centro-id_grp_planta `.
    elseif not is_material_centro-werks is INITIAL and is_material_centro-id_grp_planta is INITIAL.
      lv_where = ` werks = @is_material_centro-werks  `.
    elseif is_material_centro-werks is INITIAL and not is_material_centro-id_grp_planta is INITIAL.
      lv_where =  ` id_grp_planta = @is_material_centro-id_grp_planta  `.
    endif.

    SELECT SINGLE *
      FROM /qaps/std_prd_pa
      WHERE matnr = @is_material_centro-matnr
      AND   categoria = 'P'
      AND   (lv_where)
      and   id_std_producao = @ms_simulacao-id_std_producao
      INTO @DATA(ls_prod_acabado).

    lv_matnr = |{ is_material_centro-matnr ALPHA = OUT }|.

*    CHECK lines( lt_prod_acabado ) > 0.

    SELECT *
      FROM /qaps/std_prd_cp
*      FOR ALL ENTRIES IN @lt_prod_acabado
      WHERE id_std_prod_pa = @ls_prod_acabado-id_std_prod_pa
      and    id_std_producao = @ms_simulacao-id_std_producao
      INTO TABLE @DATA(lt_componentes).

*    IF is_material_centro-matnr = '000000001000006142'
*       AND is_material_centro-werks = 'RIG1'.
*      BREAK c060863.
*    ENDIF.

    LOOP AT lt_componentes INTO DATA(ls_componentes).

      REFRESH lt_return.

      lv_componente = |{ ls_componentes-componente ALPHA = IN WIDTH = 18 }|.

      DATA(lt_importado) = it_importado.
      DATA(lt_nacional) = it_nacional.
      DATA(lt_transf_importacao) = it_transf_importacao.
      DATA(lt_transf_nacional) = it_transf_nacional.
      DATA(lt_transf_std_prd_conversao) = it_transf_std_prd_conversao.

      DELETE lt_importado WHERE material <> lv_componente
                          OR plant <> is_material_centro-werks.

      DELETE lt_nacional WHERE material <> lv_componente
                         OR plant <> is_material_centro-werks.

      DELETE lt_transf_importacao WHERE material <> lv_componente
                                  OR plant <> is_material_centro-werks.

      DELETE lt_transf_nacional WHERE material <> lv_componente
                                OR plant <> is_material_centro-werks.

      DELETE lt_transf_std_prd_conversao WHERE std_prd_pa <> lv_componente
                                         OR plant <> is_material_centro-werks.

      APPEND LINES OF lt_importado TO lt_return.
      APPEND LINES OF lt_nacional TO lt_return.

*      BREAK c060863.

      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs>).

        <fs>-std_prd_pa = lv_matnr.
        <fs>-std_prd_cp = lv_componente.
        <fs>-std_prd_werks = is_material_centro-werks.

        IF ls_componentes-meins = 'TO'.
          <fs>-std_prd_menge = ls_componentes-menge.
          <fs>-std_prd_meins = ls_componentes-meins.
        ELSE.
          <fs>-std_prd_menge = converter_unidade( iv_matnr          = lv_componente
                                                  iv_quantidade     = ls_componentes-menge
                                                  iv_unidade_input  = ls_componentes-meins
                                                  iv_unidade_output = 'TO' ).
          <fs>-std_prd_meins = 'TO'.
        ENDIF.

        calcular_producao( EXPORTING iv_quantidade = <fs>-std_prd_menge
                                     iv_quantidade_full = <fs>-std_prd_menge
                           CHANGING ct_expressao = <fs>-t_expressao ).

        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).

      ENDLOOP.

      APPEND LINES OF lt_return TO return.

      DELETE lt_transf_std_prd_conversao WHERE std_prd_total = ''.

      LOOP AT lt_transf_std_prd_conversao ASSIGNING <fs>.
*        <fs>-std_prd_menge = 1.
        <fs>-std_prd_pa = lv_matnr.
        <fs>-std_prd_cp = lv_componente.
        <fs>-std_prd_werks = is_material_centro-werks.

        "Calcula proporcional
        IF ls_componentes-meins = 'TO'.
          <fs>-std_prd_menge = ls_componentes-menge * <fs>-std_prd_menge.
          <fs>-std_prd_meins = ls_componentes-meins.
        ELSE.
          ls_componentes-menge = converter_unidade( iv_matnr          = lv_componente
                                                  iv_quantidade     = ls_componentes-menge
                                                  iv_unidade_input  = ls_componentes-meins
                                                  iv_unidade_output = 'TO' ).
          <fs>-std_prd_menge = ls_componentes-menge * <fs>-std_prd_menge.
          <fs>-std_prd_meins = ls_componentes-meins.
        ENDIF.

        calcular_producao( EXPORTING iv_quantidade = <fs>-std_prd_menge
                                     iv_quantidade_full = ls_componentes-menge
                           CHANGING ct_expressao = <fs>-t_expressao ).

        fill_moeda_saida( CHANGING ct_expressao = <fs>-t_expressao ).

        "Retorna valor original
        <fs>-std_prd_menge = ls_componentes-menge.

      ENDLOOP.

      APPEND LINES OF lt_transf_std_prd_conversao TO return.

    ENDLOOP.

    CHECK lines( return ) > 0.

    DATA(ls_source) = return[ 1 ].
    ls_total = CORRESPONDING #( ls_source ).
    ls_total-matnr = |{ ls_source-std_prd_pa ALPHA = IN WIDTH = 18 }|.
    ls_total-std_prd_total = abap_true.

    CLEAR: ls_total-id_grupo_produto.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-std_prd_total = abap_false.
    ENDLOOP.

    fill_total_std_prd_prod( EXPORTING it_source = return
                             CHANGING  cs_total  = ls_total ).


    mo_calc_producao_prod->solve_std_prd_total_exp_prod_2( EXPORTING is_simulacao = ms_simulacao
                                                           CHANGING cs_expressao = ls_total ).

    ls_total-plant = ls_total-werks.
    APPEND ls_total TO return.

    check_errors_retorno_calculo( CHANGING ct_data = return ).

  ENDMETHOD.


  METHOD solve_expressao.

    DATA ls_expressao TYPE /qaps/s_expressao.
*    BREAK c060863.

    ls_expressao-calculated = 'X'.

    MODIFY mt_expressao
    FROM ls_expressao
    TRANSPORTING calculated
    WHERE tipo_node = 'R'.

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

  ENDMETHOD.
ENDCLASS.
