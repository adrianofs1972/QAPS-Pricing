class /QAPS/CL_LISTA_CUSTO_GENERATOR definition
  public
  final
  create public .

public section.

  methods JOB_EXECUTE
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
    returning
      value(RETURN) type /QAPS/S_RETORNO_FINAL
    raising
      /QAPS/CX_LISTA_CUSTO_GENERATOR .
  methods UPDATE_PONDERACAO
    importing
      value(IV_COD_LISTA_CUSTO) type /QAPS/ED_COD_LISTA_CUSTO .
protected section.

  constants MC_GUID_NULL type GUID16 value '00000000000000000000000000000000' ##NO_TEXT.
private section.

  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MS_STD_PRODUCAO type /QAPS/STD_PRD_H .
  data MS_TIPO_LISTA type /QAPS/TP_LISTA .
  data MS_SIMULACAO type /QAPS/SIMULACAO .

  methods FILL_TOTAL_OUTROS
    importing
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IT_TRAJETO type /QAPS/T_PREM_TRAJ
      !IT_SOURCE type /QAPS/T_RETORNO_CALCULO
      !IT_PRODUCAO type /QAPS/T_PONDERACAO
    changing
      !CS_PONDERACAO type /QAPS/S_PONDERACAO .
  methods FILL_TOTAL_STD_PRD_CONV
    importing
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IT_TRAJETO type /QAPS/T_PREM_TRAJ
      !IT_SOURCE type /QAPS/T_RETORNO_CALCULO
    changing
      !CS_PONDERACAO type /QAPS/S_PONDERACAO .
  methods FILL_TOTAL_STD_PRD_PROD
    importing
      !IT_DISTRIBUICAO type /QAPS/T_PREM_DISTR
      !IT_TRAJETO type /QAPS/T_PREM_TRAJ
      !IT_SOURCE type /QAPS/T_RETORNO_CALCULO
    changing
      !CS_PONDERACAO type /QAPS/S_PONDERACAO .
  methods FILL_PONDERACAO_STD_PRD
    importing
      value(IT_SOURCE) type /QAPS/T_RETORNO_CALCULO
    changing
      !CS_PONDERACAO type /QAPS/S_PONDERACAO .
  methods FILL_PONDERACAO_OUTROS
    importing
      !IT_SOURCE type /QAPS/T_RETORNO_CALCULO
      !IT_STD_PRD type /QAPS/T_PONDERACAO
    changing
      !CS_PONDERACAO type /QAPS/S_PONDERACAO .
  methods GET_FULL_TRAJETO
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
    returning
      value(RETURN) type /QAPS/T_FULL_TRJ .
  methods GET_DISTRIBUICAO_TRAJETO
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
      !IV_ID_TRAJETO type /QAPS/ED_ID_TRAJETO
    returning
      value(RETURN) type /QAPS/T_PREM_TRAJ .
  methods GET_DISTRIBUICAO_PREMISSA
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
    returning
      value(RETURN) type /QAPS/T_PREM_DISTR .
  methods GET_PREMISSA_TRAJETO
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO
    returning
      value(RETURN) type /QAPS/T_PRM_TRJ .
  methods FILL_PONDERACAO
    changing
      !CS_DATA type /QAPS/S_RETORNO_FINAL .
  methods COMPILE_ERRORS
    changing
      !CS_DATA type /QAPS/S_RETORNO_FINAL .
  methods NUMBERING_ITEMS
    changing
      !CS_DATA type /QAPS/S_RETORNO_FINAL .
  methods GET_TAXA_CAMBIO
    importing
      !IV_FONTE type /QAPS/ED_DSC_FONTE_CAMBIO
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_MOEDA_FINAL type /QAPS/ED_MOEDA_FINAL
      !IS_SIMULACAO type /QAPS/SIMULACAO
    returning
      value(RETURN) type /QAPS/T_TAXA_CAMBIO_PERIODO .
  methods HAS_MODALIDADE_TRANSFERENCIA
    importing
      !IS_MATERIAL type /QAPS/V_PLNT_MAT
    returning
      value(RETURN) type ABAP_BOOL .
  methods HAS_MODALIDADE
    importing
      !IS_MATERIAL type /QAPS/V_PLNT_MAT
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_ORIGENS_TRANSFERENCIA
    importing
      !IS_MATERIAL type /QAPS/V_PLNT_MAT
      !IS_SIMULACAO type /QAPS/SIMULACAO
    returning
      value(RETURN) type /QAPS/T_PREMISSA_MATRIZ .
  methods GET_ORIGENS_OLD
    importing
      !IS_MATERIAL type /QAPS/V_PLNT_MAT
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
    returning
      value(RETURN) type /QAPS/T_PREMISSA_MATRIZ .
  methods GET_ORIGENS
    importing
      !IS_MATERIAL type /QAPS/V_PLNT_MAT
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
    returning
      value(RETURN) type /QAPS/T_PREMISSA_MATRIZ .
  methods GET_PLANTA_MATERIAL
    returning
      value(RETURN) type /QAPS/T_PLANTA_MATERIAL
    raising
      /QAPS/CX_LISTA_CUSTO_GENERATOR .
  methods GET_PLANTA_MATERIAL_BY_TYPE
    importing
      !IV_TIPO_REGRA type /QAPS/TIPO_REGRA
    returning
      value(RETURN) type /QAPS/T_PLANTA_MATERIAL .
  methods GET_TRAJETOS_TRANSFERENCIA
    importing
      !IS_DATA type /QAPS/S_V_PRM_MAT
    returning
      value(RETURN) type /QAPS/T_V_PRM_TRJ .
  methods GET_TRAJETOS
    importing
      !IS_DATA type /QAPS/S_V_PRM_MAT
      !IV_MODALIDADE type /QAPS/ED_MODALIDADE
    returning
      value(RETURN) type /QAPS/T_V_PRM_TRJ .
  methods RAISE_EXCEPTION
    importing
      !IV_MESSAGE type STRING
    raising
      /QAPS/CX_LISTA_CUSTO_GENERATOR .
  methods SET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    raising
      /QAPS/CX_LISTA_CUSTO_GENERATOR .
ENDCLASS.



CLASS /QAPS/CL_LISTA_CUSTO_GENERATOR IMPLEMENTATION.


  METHOD COMPILE_ERRORS.

    LOOP AT cs_data-t_importado ASSIGNING FIELD-SYMBOL(<fs>).
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_nacional ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_producao_conversao ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_producao_producao  ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_transf_importacao  ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_transf_nacional  ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_transf_std_producao  ASSIGNING <fs>.
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

    LOOP AT cs_data-t_ponderacao ASSIGNING FIELD-SYMBOL(<fs_ponderacao>).
      APPEND LINES OF <fs>-t_erros to cs_data-t_erros_compilados.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_ponderacao.

    DATA: lt_data     TYPE /qaps/t_retorno_calculo,
          lv_producao TYPE abap_bool.

    APPEND LINES OF: cs_data-t_importado           TO lt_data,
                     cs_data-t_nacional            TO lt_data,
                     cs_data-t_producao_conversao  TO lt_data,
                     cs_data-t_producao_producao   TO lt_data,
                     cs_data-t_transf_importacao   TO lt_data,
                     cs_data-t_transf_nacional     TO lt_data,
                     cs_data-t_transf_std_producao TO lt_data.

    FIELD-SYMBOLS: <fs_ponderacao> TYPE /qaps/s_ponderacao,
                   <fs_expressao>  TYPE /qaps/s_expressao,
                   <fs_valores>    TYPE /qaps/s_values.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-matnr = |{ <fs>-matnr ALPHA = IN WIDTH = 18 }|.
      <fs>-std_prd_pa  = |{ <fs>-std_prd_pa ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/material
      FOR ALL ENTRIES IN @lt_data
      WHERE matnr = @lt_data-matnr
      INTO TABLE @DATA(lt_material).

    SELECT *
      FROM /qaps/material
      FOR ALL ENTRIES IN @lt_data
      WHERE matnr = @lt_data-std_prd_pa
      APPENDING TABLE @lt_material.

    TRY.
        DATA(lt_std_prd) = NEW /qaps/cl_mdl_std_producao( )->get_produtos_acabados( ms_simulacao-id_std_producao ).
      CATCH /qaps/cx_pricing_error.  " .
    ENDTRY.

    "Criar Linhas

*    break abap.
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      <fs_data>-matnr = |{ <fs_data>-matnr ALPHA = IN WIDTH = 18 }|.

      IF NOT <fs_data>-std_prd_pa IS INITIAL.

        lv_producao = 'X'.

        IF line_exists( cs_data-t_ponderacao[ cod_grp_planta = <fs_data>-cod_grp_planta
                                            werks          = <fs_data>-werks
                                            matnr          = <fs_data>-std_prd_pa
                                            producao       = lv_producao ] ).
          ASSIGN cs_data-t_ponderacao[ cod_grp_planta        = <fs_data>-cod_grp_planta
                                               werks         = <fs_data>-werks
                                               matnr         = <fs_data>-std_prd_pa
                                               producao      = lv_producao    ]
          TO <fs_ponderacao>.
        ELSE.

*          BREAK c060863.
          DATA(ls_std_prd) = lt_std_prd[ matnr = <fs_data>-std_prd_pa ].

          APPEND INITIAL LINE TO cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.
          <fs_ponderacao>-cod_grp_planta = <fs_data>-cod_grp_planta.
          <fs_ponderacao>-werks          = <fs_data>-werks.
          <fs_ponderacao>-matnr          = <fs_data>-std_prd_pa.
          <fs_ponderacao>-agregador      =
                VALUE #( lt_material[ matnr = |{ <fs_data>-std_prd_pa ALPHA = IN WIDTH = 18 }| ]-agregador OPTIONAL ).
          <fs_ponderacao>-producao       = lv_producao.
          <fs_ponderacao>-categoria = ls_std_prd-categoria.
          CASE ls_std_prd-categoria.
            WHEN 'C'.
              <fs_ponderacao>-tipo           = 'Prod. - Conversão'.
            WHEN 'P'.
              <fs_ponderacao>-tipo           = 'Prod. - Produção'.
          ENDCASE.

          <fs_ponderacao>-tipo_origem    = <fs_data>-tipo_origem.
          <fs_ponderacao>-id_origem      = <fs_data>-id_origem.
          <fs_ponderacao>-t_expressao    = <fs_data>-t_expressao.

*          DELETE <fs_ponderacao>-t_expressao WHERE price_field IS INITIAL.
          LOOP AT <fs_ponderacao>-t_expressao ASSIGNING <fs_expressao>.
            LOOP AT <fs_expressao>-t_valores ASSIGNING <fs_valores>.
              CLEAR: <fs_valores>-valor,
                     <fs_valores>-valor_moeda_final,
                     <fs_valores>-valor_var_elementar,
                     <fs_valores>-percentual.
            ENDLOOP.
          ENDLOOP.
        ENDIF.

      ELSE.
        lv_producao = ''.

        IF line_exists( cs_data-t_ponderacao[ cod_grp_planta = <fs_data>-cod_grp_planta
                                            werks          = <fs_data>-werks
                                            matnr          = <fs_data>-material
                                            producao       = lv_producao ] ).
          ASSIGN cs_data-t_ponderacao[ cod_grp_planta        = <fs_data>-cod_grp_planta
                                               werks         = <fs_data>-werks
                                               matnr         = <fs_data>-material
                                               producao      = lv_producao    ]
          TO <fs_ponderacao>.
        ELSE.
          APPEND INITIAL LINE TO cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.
          <fs_ponderacao>-cod_grp_planta = <fs_data>-cod_grp_planta.
          <fs_ponderacao>-werks          = <fs_data>-werks.
          <fs_ponderacao>-matnr          = <fs_data>-matnr.
          <fs_ponderacao>-agregador      =
              VALUE #( lt_material[ matnr = |{ <fs_data>-matnr ALPHA = IN WIDTH = 18 }| ]-agregador OPTIONAL ).
          <fs_ponderacao>-producao       = lv_producao.
          <fs_ponderacao>-tipo_origem    = <fs_data>-tipo_origem.
          <fs_ponderacao>-id_origem      = <fs_data>-id_origem.
          <fs_ponderacao>-t_expressao    = <fs_data>-t_expressao.

*        DELETE <fs_ponderacao>-t_expressao WHERE price_field IS INITIAL.
          LOOP AT <fs_ponderacao>-t_expressao ASSIGNING <fs_expressao>.
            LOOP AT <fs_expressao>-t_valores ASSIGNING <fs_valores>.
              CLEAR: <fs_valores>-valor,
                     <fs_valores>-valor_moeda_final,
                     <fs_valores>-valor_var_elementar,
                     <fs_valores>-percentual.
            ENDLOOP.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDLOOP.

*    break abap.

    "Preenchar Linhas
    DATA(lt_data_outros) = lt_data.
    DATA(lt_data_producao) = lt_data.

    DELETE lt_data_outros WHERE NOT std_prd_pa IS INITIAL.
    DELETE lt_data_producao WHERE std_prd_pa IS INITIAL.

*    break c060863.

    "1º Std Prd Conversão
    LOOP AT cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.

      CHECK <fs_ponderacao>-producao = 'X' AND <fs_ponderacao>-categoria = 'C'.

      DATA(lt_source_prd_conv) = lt_data_producao.
      DELETE lt_source_prd_conv WHERE cod_grp_planta <> <fs_ponderacao>-cod_grp_planta
                          OR werks          <> <fs_ponderacao>-werks
                          OR std_prd_pa     <> <fs_ponderacao>-matnr
                          OR modalidade     =  'P'.

      fill_ponderacao_std_prd( EXPORTING it_source     = lt_source_prd_conv
                               CHANGING  cs_ponderacao = <fs_ponderacao> ).
      <fs_ponderacao>-no_out = abap_true.

    ENDLOOP.

    "2º Std Produção
    LOOP AT cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.

      CHECK <fs_ponderacao>-producao = 'X' AND <fs_ponderacao>-categoria = 'P'.

      DATA(lt_source_prd_prod) = lt_data_producao.
      DELETE lt_source_prd_prod WHERE cod_grp_planta <> <fs_ponderacao>-cod_grp_planta
                          OR werks          <> <fs_ponderacao>-werks
                          OR std_prd_pa     <> <fs_ponderacao>-matnr
                          OR modalidade     =  'P'.

      fill_ponderacao_std_prd( EXPORTING it_source     = lt_source_prd_prod
                               CHANGING  cs_ponderacao = <fs_ponderacao> ).
      <fs_ponderacao>-no_out = abap_true.


    ENDLOOP.

    "3º Demais
    LOOP AT cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.

      CHECK <fs_ponderacao>-producao IS INITIAL.

      DATA(lt_producao) = cs_data-t_ponderacao.
      DELETE lt_producao WHERE producao IS INITIAL.
      DELETE lt_producao  WHERE matnr  <> <fs_ponderacao>-matnr.

      DATA(lt_source_other) = lt_data_outros.
      DELETE lt_source_other WHERE cod_grp_planta <> <fs_ponderacao>-cod_grp_planta
                          OR werks          <> <fs_ponderacao>-werks
                          OR matnr          <> <fs_ponderacao>-matnr
                          OR modalidade     =  'P'.

      fill_ponderacao_outros( EXPORTING it_source     = lt_source_other
                                        it_std_prd    = lt_producao
                              CHANGING  cs_ponderacao = <fs_ponderacao> ).

    ENDLOOP.

    SORT cs_data-t_ponderacao BY cod_grp_planta werks matnr.

    LOOP AT cs_data-t_ponderacao ASSIGNING <fs_ponderacao>.
      <fs_ponderacao>-id_ponderacao = sy-tabix.
    ENDLOOP.

    "Marcar Ponderacao
    DATA(lt_ponderacao_full) = cs_data-t_ponderacao.
    DELETE lt_ponderacao_full WHERE items > 1 OR producao = 'X'.

    DEFINE set_ponderacao.
      LOOP AT &1 ASSIGNING <fs_data>.

        <fs_data>-matnr = |{ <fs_data>-matnr ALPHA = IN WIDTH = 18 }|.

        IF line_exists( lt_ponderacao_full[ cod_grp_planta = <fs_data>-cod_grp_planta
                                            werks          = <fs_data>-werks
                                            matnr          = <fs_data>-matnr
                                            producao       = '' ] ).

          <fs_data>-ponderacao = 'X'.

        ENDIF.

      ENDLOOP.
    END-OF-DEFINITION.

    DEFINE set_ponderacao_prd.
      LOOP AT &1 ASSIGNING <fs_data>.

        IF <fs_data>-std_prd_total = 'X'.

          <fs_data>-ponderacao = 'X'.

        ENDIF.

      ENDLOOP.
    END-OF-DEFINITION.

    set_ponderacao cs_data-t_importado.
    set_ponderacao cs_data-t_nacional.
    set_ponderacao cs_data-t_transf_importacao.
    set_ponderacao cs_data-t_transf_nacional.
    set_ponderacao cs_data-t_transf_std_producao.

    set_ponderacao_prd cs_data-t_producao_conversao.
    set_ponderacao_prd cs_data-t_producao_producao.


  ENDMETHOD.


  METHOD fill_ponderacao_outros.

    DATA: lv_matnr_in  TYPE matnr,
          lv_matnr_out TYPE matnr,
          lv_where     TYPE string.

    DATA: lt_distribuicao TYPE /qaps/t_prem_distr,
          lt_trajeto      TYPE /qaps/t_prem_traj.

    cs_ponderacao-items = lines( it_source ).

    IF cs_ponderacao-items = 1.
      RETURN.
    ENDIF.

    lv_matnr_in = |{ cs_ponderacao-matnr ALPHA = IN WIDTH = 18 }|.
    lv_matnr_out = |{ cs_ponderacao-matnr ALPHA = OUT }|.

    if not cs_ponderacao-cod_grp_planta is INITIAL and not cs_ponderacao-werks is INITIAL.
      lv_where = ` grp_planta = @cs_ponderacao-cod_grp_planta  `.
      lv_where = lv_where && ` and werks      = @cs_ponderacao-werks `.
    elseif cs_ponderacao-cod_grp_planta is INITIAL and not cs_ponderacao-werks is INITIAL.
      lv_where = ` werks      = @cs_ponderacao-werks `.
    elseif not cs_ponderacao-cod_grp_planta is INITIAL and cs_ponderacao-werks is INITIAL.
      lv_where = ` grp_planta = @cs_ponderacao-cod_grp_planta  `.
    endif.

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @it_source
      WHERE id_simulacao = @it_source-id_simulacao
      AND   id_premissa  = @it_source-id_premissa
      AND   tipo_regra = 'MA'
      AND   (lv_where)
      AND   ( matnr    = @lv_matnr_in OR  matnr    = @lv_matnr_out )
      INTO TABLE @DATA(lt_prm_full).

    CHECK lines( lt_prm_full ) > 0.

    LOOP AT lt_prm_full ASSIGNING FIELD-SYMBOL(<fs_lt_prm_full>).
      <fs_lt_prm_full>-matnr = |{ <fs_lt_prm_full>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_prm_full
      WHERE id_distribuicao = @lt_prm_full-id_distribuicao
      INTO TABLE @lt_distribuicao.

    IF lines( lt_distribuicao ) > 0.

      SELECT *
        FROM /qaps/prem_traj
        FOR ALL ENTRIES IN @lt_distribuicao
        WHERE id_distribuicao = @lt_distribuicao-id_distribuicao
        INTO TABLE @lt_trajeto.

    ENDIF.

    fill_total_outros( EXPORTING it_distribuicao = lt_distribuicao
                                 it_producao     = it_std_prd
                                 it_trajeto      = lt_trajeto
                                 it_source       = it_source
                       CHANGING  cs_ponderacao   = cs_ponderacao ).

  ENDMETHOD.


  METHOD fill_ponderacao_std_prd.

    DATA: lr_componente   TYPE RANGE OF matnr,
          lt_distribuicao TYPE /qaps/t_prem_distr,
          lt_trajeto      TYPE /qaps/t_prem_traj.

    cs_ponderacao-items = lines( it_source ).

    LOOP AT it_source ASSIGNING FIELD-SYMBOL(<fs_source>).
*      <fs_source>-std_prd_cp = |{
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = |{ <fs_source>-std_prd_cp ALPHA = OUT }| )
        TO lr_componente.
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = |{ <fs_source>-std_prd_cp ALPHA = IN WIDTH = 18 }| )
        TO lr_componente.
    ENDLOOP.

    SELECT *
      FROM /qaps/v_prm_full
      FOR ALL ENTRIES IN @it_source
      WHERE id_simulacao = @it_source-id_simulacao
      AND   id_premissa  = @it_source-id_premissa
      AND   tipo_regra = 'MA'
      AND   grp_planta = @cs_ponderacao-cod_grp_planta
      AND   werks      = @cs_ponderacao-werks
      AND   matnr      IN @lr_componente
      INTO TABLE @DATA(lt_prm_full).

    CHECK lines( lt_prm_full ) > 0.

    LOOP AT lt_prm_full ASSIGNING FIELD-SYMBOL(<fs_lt_prm_full>).
      <fs_lt_prm_full>-matnr = |{ <fs_lt_prm_full>-matnr ALPHA = IN WIDTH = 18 }|.
    ENDLOOP.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @lt_prm_full
      WHERE id_distribuicao = @lt_prm_full-id_distribuicao
      INTO TABLE @lt_distribuicao.

    CHECK lines( lt_distribuicao ) > 0.

    SELECT *
      FROM /qaps/prem_traj
      FOR ALL ENTRIES IN @lt_distribuicao
      WHERE id_distribuicao = @lt_distribuicao-id_distribuicao
      INTO TABLE @lt_trajeto.

    DATA(lt_std_prod) = NEW /qaps/cl_mdl_std_producao(
            )->get_produtos_acabados( iv_id_std_producao = ms_simulacao-id_std_producao ).

    DATA(ls_std_prod) = lt_std_prod[ matnr = cs_ponderacao-matnr ] .

    CASE ls_std_prod-categoria.

      WHEN 'C'.
        fill_total_std_prd_conv( EXPORTING it_distribuicao = lt_distribuicao
                                      it_trajeto      = lt_trajeto
                                      it_source       = it_source
                            CHANGING  cs_ponderacao   = cs_ponderacao  ).

      WHEN 'P'.
        fill_total_std_prd_prod( EXPORTING it_distribuicao = lt_distribuicao
                                      it_trajeto      = lt_trajeto
                                      it_source       = it_source
                            CHANGING  cs_ponderacao   = cs_ponderacao  ).

    ENDCASE.



  ENDMETHOD.


  METHOD fill_total_outros.

    DATA lo_calc_outros TYPE REF TO /qaps/cl_custo_calc_sum_outros.
    DATA: lv_premissa    TYPE /qaps/prem_distr-percentual,
          lv_trajeto     TYPE /qaps/prem_distr-percentual,
          lv_coeficiente TYPE /qaps/prem_distr-percentual.

    IF lines( it_producao ) = 0.

      LOOP AT it_source INTO DATA(ls_source).

        LOOP AT ls_source-t_expressao INTO DATA(ls_exp_source).

          CHECK ls_exp_source-tipo_node = 'E'.

          ASSIGN cs_ponderacao-t_expressao[ id_calc_node = ls_exp_source-id_calc_node ]
              TO FIELD-SYMBOL(<fs_exp_target>).

          LOOP AT ls_exp_source-t_valores INTO DATA(ls_valores).

            ASSIGN <fs_exp_target>-t_valores[ periodo = ls_valores-periodo ]
                TO FIELD-SYMBOL(<fs_val_target>).

            DATA(ls_premissa) = ls_source-t_dist_premissa[ periodo = ls_valores-periodo ].
            DATA(ls_trajeto)  = ls_source-trajeto-t_dist_trajeto[ periodo = ls_valores-periodo ].

            lv_premissa = ls_premissa-percentual / 100 .
            lv_trajeto = ls_trajeto-percentual / 100 .
*            IF lv_trajeto =  0.
*              lv_trajeto = 1.
*            ENDIF.

            lv_coeficiente = lv_premissa * lv_trajeto.

            <fs_val_target>-valor = <fs_val_target>-valor + ( ls_valores-valor * lv_coeficiente ).
            <fs_val_target>-valor_var_elementar = <fs_val_target>-valor_var_elementar
                      + ( ls_valores-valor_var_elementar * lv_coeficiente ).
            <fs_val_target>-valor_moeda_final = <fs_val_target>-valor_moeda_final
                + ( ls_valores-valor_moeda_final * lv_coeficiente ).
            <fs_val_target>-percentual = <fs_val_target>-percentual
                + ( ls_valores-percentual  * lv_coeficiente ).

          ENDLOOP.

        ENDLOOP.

      ENDLOOP.

    ELSE.
*      break c060863.
      LOOP AT it_producao INTO DATA(ls_producao).

        LOOP AT ls_producao-t_expressao INTO ls_exp_source.

          CHECK ls_exp_source-tipo_node = 'E'.

          ASSIGN cs_ponderacao-t_expressao[ id_calc_node = ls_exp_source-id_calc_node ]
              TO <fs_exp_target>.

          LOOP AT ls_exp_source-t_valores INTO ls_valores.

*          DATA(ls_distribuicao) = it_distribuicao[ matnr = ls_source-matnr
*                                                    ].
*          DATA(ls_trajeto) = it_trajeto[ matnr = ls_source-matnr ].

            ASSIGN <fs_exp_target>-t_valores[ periodo = ls_valores-periodo ]
                TO <fs_val_target>.

            <fs_val_target>-valor = ls_valores-valor + <fs_val_target>-valor.
            <fs_val_target>-valor_var_elementar = ls_valores-valor_var_elementar + <fs_val_target>-valor_var_elementar.
            <fs_val_target>-valor_moeda_final = ls_valores-valor_moeda_final + <fs_val_target>-valor_moeda_final.
            <fs_val_target>-percentual = ls_valores-percentual + <fs_val_target>-percentual.

          ENDLOOP.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    DATA(lt_taxa_cambio) = get_taxa_cambio( iv_fonte       = ms_lista_custo-dsc_fonte
                                            iv_moeda_local = ms_lista_custo-moeda_calculo
                                            iv_moeda_final = ms_lista_custo-moeda_lista
                                            is_simulacao   = ms_simulacao ).

    BREAK abap.

    lo_calc_outros ?= /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade  = 'A'
                                                                   iv_moeda_local = ms_lista_custo-moeda_calculo
                                                                   iv_moeda_final = ms_lista_custo-moeda_lista
                                                                   it_taxa_cambio = lt_taxa_cambio ).

    lo_calc_outros->solve_outros_total_expressao(
      EXPORTING
        is_simulacao = ms_simulacao
      CHANGING
        cs_expressao = cs_ponderacao
    ).


  ENDMETHOD.


  METHOD fill_total_std_prd_conv.

    DATA lo_calc_producao TYPE REF TO /qaps/cl_custo_calc_sum_prd_c.

    LOOP AT it_source INTO DATA(ls_source).

      LOOP AT ls_source-t_expressao INTO DATA(ls_exp_source).

        CHECK ls_exp_source-tipo_node = 'E'.

        ASSIGN cs_ponderacao-t_expressao[ id_calc_node = ls_exp_source-id_calc_node ]
            TO FIELD-SYMBOL(<fs_exp_target>).

        LOOP AT ls_exp_source-t_valores INTO DATA(ls_valores).

          ASSIGN <fs_exp_target>-t_valores[ periodo = ls_valores-periodo ]
              TO FIELD-SYMBOL(<fs_val_target>).

          CHECK ls_valores-valor > 0 OR ls_valores-percentual > 0.

          <fs_val_target>-valor = <fs_val_target>-valor + ls_valores-valor.
          <fs_val_target>-valor_var_elementar = <fs_val_target>-valor_var_elementar +
                    ls_valores-valor_var_elementar.
          <fs_val_target>-valor_moeda_final = <fs_val_target>-valor_moeda_final +
                ls_valores-valor_moeda_final.
          <fs_val_target>-percentual = <fs_val_target>-percentual + ls_valores-percentual.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    DATA(lt_taxa_cambio) = get_taxa_cambio( iv_fonte       =  ms_lista_custo-dsc_fonte
                                            iv_moeda_local =  ms_lista_custo-moeda_calculo
                                            iv_moeda_final =  ms_lista_custo-moeda_lista
                                            is_simulacao   =  ms_simulacao  ).

    lo_calc_producao ?= /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade = 'C'
                                                                     iv_moeda_local = ms_lista_custo-moeda_calculo
                                                                     iv_moeda_final = ms_lista_custo-moeda_lista
                                                                     it_taxa_cambio = lt_taxa_cambio  ).

    lo_calc_producao->solve_std_prd_total_exp_conv(
          EXPORTING is_simulacao = ms_simulacao
          CHANGING cs_expressao = cs_ponderacao
    ).

  ENDMETHOD.


  METHOD FILL_TOTAL_STD_PRD_PROD.

    DATA lo_calc_producao TYPE REF TO /qaps/cl_custo_calc_sum_prd_p.

*    BREAK c060863.
    LOOP AT it_source INTO DATA(ls_source).

      LOOP AT ls_source-t_expressao INTO DATA(ls_exp_source).

        CHECK ls_exp_source-tipo_node = 'E'.

        ASSIGN cs_ponderacao-t_expressao[ id_calc_node = ls_exp_source-id_calc_node ]
            TO FIELD-SYMBOL(<fs_exp_target>).

        LOOP AT ls_exp_source-t_valores INTO DATA(ls_valores).

*          DATA(ls_distribuicao) = it_distribuicao[ matnr = ls_source-matnr
*                                                    ].
*          DATA(ls_trajeto) = it_trajeto[ matnr = ls_source-matnr ].

          ASSIGN <fs_exp_target>-t_valores[ periodo = ls_valores-periodo ]
              TO FIELD-SYMBOL(<fs_val_target>).

          check ls_valores-valor > 0 or ls_valores-percentual > 0.

          <fs_val_target>-valor = <fs_val_target>-valor + ls_valores-valor.
          <fs_val_target>-valor_var_elementar = <fs_val_target>-valor_var_elementar +
                    ls_valores-valor_var_elementar.
          <fs_val_target>-valor_moeda_final = <fs_val_target>-valor_moeda_final +
                ls_valores-valor_moeda_final.
          <fs_val_target>-percentual = <fs_val_target>-percentual + ls_valores-percentual.

*          <fs_val_target>-valor = <fs_val_target>-valor + ( ls_valores-valor * ls_source-std_prd_menge ).
*          <fs_val_target>-valor_var_elementar = <fs_val_target>-valor_var_elementar +
*                    ( ls_valores-valor_var_elementar * ls_source-std_prd_menge ).
*          <fs_val_target>-valor_moeda_final = <fs_val_target>-valor_moeda_final +
*                ( ls_valores-valor_moeda_final * ls_source-std_prd_menge ).
*          <fs_val_target>-percentual = <fs_val_target>-percentual + ls_valores-percentual.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    DATA(lt_taxa_cambio) = get_taxa_cambio( iv_fonte       =  ms_lista_custo-dsc_fonte
                                            iv_moeda_local =  ms_lista_custo-moeda_calculo
                                            iv_moeda_final =  ms_lista_custo-moeda_lista
                                            is_simulacao   =  ms_simulacao  ).

    lo_calc_producao ?= /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade = 'S'
                                                                          iv_moeda_local = ms_lista_custo-moeda_calculo
                                                                          iv_moeda_final = ms_lista_custo-moeda_lista
                                                                          it_taxa_cambio = lt_taxa_cambio
                                                                           ).

    lo_calc_producao->solve_std_prd_total_exp_prod(
          EXPORTING is_simulacao = ms_simulacao
          CHANGING cs_expressao = cs_ponderacao
    ).

  ENDMETHOD.


  METHOD get_full_trajeto.

    SELECT *
      FROM /qaps/v_full_trj
      WHERE id_distribuicao = @iv_id_distribuicao
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_origens.

    DATA lt_return TYPE /qaps/t_premissa_matriz.
    DATA lv_matnr TYPE matnr.

    CHECK has_modalidade( is_material   = is_material
                          iv_modalidade = iv_modalidade ).

    IF NOT is_material-matnr IS INITIAL.

      lv_matnr = |{ is_material-matnr ALPHA = OUT }|.

      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   ( matnr = @is_material-matnr OR matnr = @lv_matnr )
        AND   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_return.

      IF sy-subrc NE 0.
        SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
*        AND   werks = ''"@is_material-werks
        AND   ( matnr = @is_material-matnr OR matnr = @lv_matnr )
        AND   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_return.
      ENDIF.

    ENDIF.

    IF iv_modalidade = 'I'.
      SELECT *
        FROM /qaps/v_prt_cais
        INTO TABLE @DATA(lt_porto_cais).
*
      LOOP AT lt_return INTO DATA(ls_return).

        CASE ls_return-tipo_origem.
          WHEN 'I'.
            DATA(ls_porto_cais) = VALUE #( lt_porto_cais[ cod_cais = ls_return-cod_origem ] OPTIONAL ).
            ls_return-id_origem = ls_porto_cais-id_ponto_cais.
          WHEN 'P'.
            ls_porto_cais = VALUE #( lt_porto_cais[ cod_porto = ls_return-cod_origem ] OPTIONAL ).
            ls_return-id_origem = ls_porto_cais-id_ponto_porto.
        ENDCASE.

        ls_return-cod_cais = ls_porto_cais-cod_cais.
        ls_return-cod_porto = ls_porto_cais-cod_porto.

        APPEND ls_return TO return.

      ENDLOOP.

    ELSE.
      APPEND LINES OF lt_return TO return.
    ENDIF.

    DELETE return WHERE id_origem = mc_guid_null.
*    break abap.
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-t_trajetos = get_premissa_trajeto( <fs>-id_distribuicao ).
      <fs>-t_dist_premissa = get_distribuicao_premissa( <fs>-id_distribuicao ).
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_ORIGENS_OLD.

    DATA lt_return TYPE /qaps/t_premissa_matriz.

    CHECK has_modalidade( is_material   = is_material
                          iv_modalidade = iv_modalidade ).

    IF NOT is_material-matnr IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   matnr = @is_material-matnr
        INTO CORRESPONDING FIELDS OF TABLE @lt_return.

      IF sy-subrc NE 0.
        SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
*        AND   werks = ''"@is_material-werks
        AND   matnr = @is_material-matnr
        INTO CORRESPONDING FIELDS OF TABLE @lt_return.
      ENDIF.

    ENDIF.


    IF NOT is_material-agregador IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   agregador = @is_material-agregador
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_return.

*      IF sy-subrc NE 0.
*        SELECT *
*        FROM /qaps/v_prm_mat
*        WHERE id_grp_planta = @is_material-id_grp_planta
*        AND   modalidade = @iv_modalidade
**      AND   werks = ''"@is_material-werks
*        AND   agregador = @is_material-agregador
*        APPENDING CORRESPONDING FIELDS OF TABLE @LT_RETURN.
*      ENDIF.

    ENDIF.
*
*
*
    IF NOT is_material-mat_planejado IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   mat_planejado = @is_material-mat_planejado
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_return.
    ENDIF.

*    IF sy-subrc NE 0.
*      SELECT *
*      FROM /qaps/v_prm_mat
*      WHERE id_grp_planta = @is_material-id_grp_planta
*      AND   modalidade = @iv_modalidade
**      AND   werks = ''"@is_material-werks
*      AND   mat_planejado = @is_material-mat_planejado
*      APPENDING CORRESPONDING FIELDS OF TABLE @LT_RETURN.
*    ENDIF.

    IF NOT is_material-id_grupo_produto IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   id_grupo_produto = @is_material-id_grupo_produto
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_return.
    ENDIF.

    IF sy-subrc NE 0.
      SELECT *
      FROM /qaps/v_prm_mat
      WHERE id_grp_planta = @is_material-id_grp_planta
      AND   modalidade = @iv_modalidade
*      AND   werks = '' "@is_material-werks
      AND   id_grupo_produto = @is_material-id_grupo_produto
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_return.
    ENDIF.

    IF iv_modalidade = 'I'.
*      BREAK c060863.
      SELECT *
        FROM /qaps/v_prt_cais
        INTO TABLE @DATA(lt_porto_cais).
*
      LOOP AT lt_return INTO DATA(ls_return).

        CASE ls_return-tipo_origem.
          WHEN 'I'.
            DATA(ls_porto_cais) = VALUE #( lt_porto_cais[ cod_cais = ls_return-cod_origem ] OPTIONAL ).
            ls_return-id_origem = ls_porto_cais-id_ponto_cais.
          WHEN 'P'.
            ls_porto_cais = VALUE #( lt_porto_cais[ cod_porto = ls_return-cod_origem ] OPTIONAL ).
            ls_return-id_origem = ls_porto_cais-id_ponto_porto.
        ENDCASE.

        ls_return-cod_cais = ls_porto_cais-cod_cais.
        ls_return-cod_porto = ls_porto_cais-cod_porto.

        APPEND ls_return TO return.

      ENDLOOP.

    ELSE.
      APPEND LINES OF lt_return TO return.
    ENDIF.

*    IF iv_modalidade = 'N'.
*    BREAK c060863.
*    ENDIF.

    DELETE return WHERE id_origem = mc_guid_null.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
*      <fs>-t_trajetos = get_trajetos( is_data = <fs>
*                                      iv_modalidade = iv_modalidade ).
    ENDLOOP.

*    DELETE return WHERE tipo_regra <> 'MA'.

  ENDMETHOD.


  METHOD get_origens_transferencia.

    DATA lr_grupo_planta TYPE RANGE OF /qaps/ed_cod_grp_planta.
    DATA lt_return TYPE /qaps/t_premissa_matriz.
    DATA lv_matnr TYPE matnr.

    CHECK has_modalidade_transferencia( is_material   = is_material ).

    IF NOT is_material-matnr IS INITIAL.

      lv_matnr = |{ is_material-matnr ALPHA = OUT }|.

      SELECT *
        FROM /qaps/v_prm_mat
        WHERE modalidade = 'T'
        AND   ( cod_origem = @is_material-cod_grp_planta OR
                cod_origem = @is_material-werks )
        AND   ( matnr = @is_material-matnr OR matnr = @lv_matnr )
        and   id_simulacao = @is_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    DELETE return WHERE id_origem = mc_guid_null.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      CHECK <fs>-werks IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs>-cod_grp_planta ) TO lr_grupo_planta.
    ENDLOOP.

    IF lines( lr_grupo_planta ) > 0.
      SELECT *
        FROM /qaps/v_centro
        WHERE codigo IN @lr_grupo_planta
        INTO TABLE @DATA(lt_centro).
    ENDIF.

    APPEND LINES OF lt_return TO return.
    DELETE return WHERE werks IS INITIAL.
    "Expandir para centros


    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-t_trajetos = get_premissa_trajeto( <fs_return>-id_distribuicao ).
      <fs_return>-t_dist_premissa = get_distribuicao_premissa( <fs>-id_distribuicao ).
    ENDLOOP.


  ENDMETHOD.


  METHOD get_planta_material.

    "Busca dados de Premissa
    DATA(lt_result_ma) = get_planta_material_by_type( 'MA' ).
    DATA(lt_result_ag) = get_planta_material_by_type( 'AG' ).
*    DATA(lt_result_mp) = get_planta_material_by_type( 'MP' ).
*    DATA(lt_result_gp) = get_planta_material_by_type( 'GP' ).

    APPEND LINES OF: lt_result_ma TO return,
                     lt_result_ag TO return.
*                     lt_result_gp TO return,
*                     lt_result_mp TO return.


    SORT return BY werks matnr.
    DELETE ADJACENT DUPLICATES FROM return COMPARING werks matnr.

  ENDMETHOD.


  METHOD get_planta_material_by_type.

    DATA: lv_where TYPE string.

    SELECT *
      FROM /qaps/v_mat_type
      WHERE id_simulacao = @ms_simulacao-id_simulacao
      INTO TABLE @DATA(lt_premissa_full).

    DATA(lt_premissa) = lt_premissa_full.
    DELETE lt_premissa WHERE tipo_regra <> iv_tipo_regra.

    if iv_tipo_regra = 'MA'.
      data(lt_comp) = lt_premissa.
      loop at lt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>).
        <fs_comp>-matnr = |{ <fs_comp>-matnr ALPHA = in WIDTH = 18 }|.
      ENDLOOP.
      APPEND LINES OF lt_comp to lt_premissa.

      loop at lt_premissa ASSIGNING FIELD-SYMBOL(<fs_premissa>).
        <fs_premissa>-matnr = |{ <fs_premissa>-matnr ALPHA = in WIDTH = 18 }|.
      ENDLOOP.

    ENDIF.

    CHECK lines( lt_premissa ) > 0.

    CASE iv_tipo_regra.
      WHEN 'MA'. lv_where = ` matnr = @lt_premissa_aux-matnr `.
      WHEN 'AG'. lv_where = ` agregador = @lt_premissa_aux-agregador `.
      WHEN 'GP'. lv_where = ` id_grupo_produto = @lt_premissa_aux-id_grupo_produto `.
      WHEN 'MP'. lv_where = ` mat_planejado = @lt_premissa_aux-mat_planejado `.
    ENDCASE.

    DATA(lt_premissa_aux) = lt_premissa.
    DELETE lt_premissa_aux WHERE werks = ''.

    IF lines( lt_premissa_aux ) > 0.

      SELECT *
        FROM /qaps/v_plnt_mat
        FOR ALL ENTRIES IN @lt_premissa_aux
        WHERE (lv_where)
        AND werks = @lt_premissa_aux-werks
        INTO TABLE @DATA(lt_result_werks).

    ENDIF.

    lt_premissa_aux = lt_premissa.
    DELETE lt_premissa_aux WHERE werks <> ''.

    IF lines( lt_premissa_aux ) > 0.
      SELECT *
        FROM /qaps/v_plnt_mat
        FOR ALL ENTRIES IN @lt_premissa_aux
        WHERE (lv_where)
        AND   id_grp_planta       = @lt_premissa_aux-id_grp_planta
        INTO TABLE @DATA(lt_result_grp_planta).
    ENDIF.

    APPEND LINES OF lt_result_werks TO return.
    APPEND LINES OF lt_result_grp_planta TO return.

    sort return by werks matnr.

  ENDMETHOD.


  METHOD get_premissa_trajeto.

    SELECT *
      FROM /qaps/dist_traj
      WHERE id_distribuicao = @iv_id_distribuicao
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-t_dist_trajeto = get_distribuicao_trajeto( iv_id_distribuicao = <fs>-id_distribuicao
                                                      iv_id_trajeto      = <fs>-id_trajeto ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_taxa_cambio.

    DATA lr_moeda TYPE RANGE OF /qaps/moeda.

    IF iv_moeda_local <> iv_moeda_final.
      SELECT *
        FROM /qaps/v_tx_cmb
        WHERE fonte = @iv_fonte
        AND   moeda_local = @iv_moeda_local
        AND   moeda_final = @iv_moeda_final
        AND   periodo >= @is_simulacao-periodo_inicial
        AND   periodo <= @is_simulacao-periodo_final
        INTO TABLE @return.
    ELSE.

      SELECT DISTINCT moeda
        FROM /qaps/custo_elm
        WHERE ( ( id_tp_lista = @ms_simulacao-id_tp_lista AND escopo = 'T' )
        OR   ( id_tp_lista = @mc_guid_null AND escopo = 'G' ) )
        AND tipo_dado = '1'
        AND moeda <> @iv_moeda_local
        INTO TABLE @DATA(lt_moeda).

      lr_moeda = VALUE #( FOR wa IN lt_moeda
                          ( sign = 'I' option = 'EQ' low = wa-moeda ) ).

      IF lines( lr_moeda ) > 0.
        "Carrega para conversões, quando houver variáveis em outra moeda
        SELECT *
          FROM /qaps/v_tx_cmb
          WHERE fonte = @iv_fonte
          AND   moeda_local = @iv_moeda_local
          AND   moeda_final IN @lr_moeda
          AND   periodo >= @is_simulacao-periodo_inicial
          AND   periodo <= @is_simulacao-periodo_final
          INTO TABLE @return.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_trajetos.

    CASE iv_modalidade.
      WHEN 'I'.
        CASE is_data-tipo_origem.
          WHEN 'P'.
            SELECT *
              FROM /qaps/v_prm_trj
              WHERE id_premissa     = @is_data-id_premissa
              AND   id_item         = @is_data-id_item
              AND   cod_origem      = @is_data-cod_porto
*          AND   id_distribuicao = @is_data-id_distribuicao
              INTO CORRESPONDING FIELDS OF TABLE @return.
          WHEN 'I'.

            SELECT *
              FROM /qaps/v_prm_trj
              WHERE id_premissa     = @is_data-id_premissa
              AND   id_item         = @is_data-id_item
              AND   cod_origem    = @is_data-cod_cais
              INTO CORRESPONDING FIELDS OF TABLE @return.

            IF lines( return ) = 0.

              SELECT SINGLE cod_cais,cod_porto
                FROM /qaps/cais
                INNER JOIN /qaps/porto
                ON /qaps/cais~id_porto = /qaps/porto~id_porto
                WHERE /qaps/cais~cod_cais = @is_data-cod_cais
                INTO @DATA(ls_cais).

              SELECT *
                FROM /qaps/v_prm_trj
                WHERE id_premissa   = @is_data-id_premissa
                AND   id_item       = @is_data-id_item
                AND   cod_origem    = @is_data-cod_porto
                INTO CORRESPONDING FIELDS OF TABLE @return.

            ENDIF.

        ENDCASE.
      WHEN 'N'.
        SELECT *
          FROM /qaps/v_prm_trj
          WHERE id_premissa     = @is_data-id_premissa
          AND   id_item         = @is_data-id_item
*      AND   cod_origem      = @is_data-cod_porto
*      AND   id_distribuicao = @is_data-id_distribuicao
          INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDCASE.

    CHECK lines( return ) > 0.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      SELECT *
        FROM /qaps/v_trj_trc
        WHERE id_trajeto = @<fs>-id_trajeto
        INTO TABLE @<fs>-t_trechos.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_trajetos_transferencia.


    SELECT *
      FROM /qaps/v_prm_trj
      WHERE id_premissa     = @is_data-id_premissa
      AND   id_item         = @is_data-id_item
      AND   modalidade      = 'T'
*      AND   id_distribuicao = @is_data-id_distribuicao
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      SELECT *
        FROM /qaps/v_trj_trc
        WHERE id_trajeto = @<fs>-id_trajeto
        INTO TABLE @<fs>-t_trechos.

    ENDLOOP.

  ENDMETHOD.


  METHOD has_modalidade.

    DATA lt_data TYPE /qaps/t_premissa_matriz.

    IF NOT is_material-matnr IS INITIAL.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   matnr = @is_material-matnr
        and   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.

      IF sy-subrc NE 0.
        SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
*        AND   werks = ''"@is_material-werks
        AND   matnr = @is_material-matnr
          and   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.
      ENDIF.

    ENDIF.

    IF NOT is_material-agregador IS INITIAL and lines( lt_data ) = 0.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
        AND   werks = @is_material-werks
        AND   agregador = @is_material-agregador
        and   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.

     IF sy-subrc NE 0.
        SELECT *
        FROM /qaps/v_prm_mat
        WHERE id_grp_planta = @is_material-id_grp_planta
        AND   modalidade = @iv_modalidade
*        AND   werks = ''"@is_material-werks
         AND   agregador = @is_material-agregador
         and   id_simulacao = @ms_simulacao-id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.
      ENDIF.

    ENDIF.


    CHECK lines( lt_data ) > 0.

    return = abap_true.


  ENDMETHOD.


  METHOD has_modalidade_transferencia.

    DATA lt_data TYPE /qaps/t_premissa_matriz.
    DATA lv_matnr TYPE matnr.

    IF NOT is_material-matnr IS INITIAL.

      lv_matnr = |{ is_material-matnr ALPHA = OUT }|.

      SELECT *
        FROM /qaps/v_prm_mat
        WHERE modalidade = 'T'
        AND   ( matnr = @is_material-matnr OR matnr = @lv_matnr )
        AND   ( cod_origem = @is_material-cod_grp_planta OR
                cod_origem = @is_material-werks )
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.

    ENDIF.

    IF NOT is_material-agregador IS INITIAL AND lines( lt_data ) = 0.
      SELECT *
        FROM /qaps/v_prm_mat
        WHERE modalidade = 'T'
        AND   ( cod_origem = @is_material-cod_grp_planta OR
                cod_origem = @is_material-werks )
        AND   agregador = @is_material-agregador
        INTO CORRESPONDING FIELDS OF TABLE @lt_data.

    ENDIF.

    CHECK lines( lt_data ) > 0.

    return = abap_true.


  ENDMETHOD.


  METHOD job_execute.

    DATA: lv_id_simulacao	TYPE /qaps/ed_id_simulacao,
          lv_fonte        TYPE /qaps/ed_dsc_fonte_cambio,
          lv_moeda_local  TYPE /qaps/ed_moeda_local,
          lv_moeda_final  TYPE /qaps/ed_moeda_final.

    DATA: lt_origem_transf_nac TYPE /qaps/t_premissa_matriz,
          lt_origem_transf_imp TYPE /qaps/t_premissa_matriz.

    DATA(lo_model_lista) = NEW /qaps/cl_mdl_lista_custo( ).
    ms_lista_custo = lo_model_lista->get_single_lista_custo( iv_cod_lista_custo ).

    lv_id_simulacao = ms_lista_custo-id_simulacao.
    lv_fonte        = ms_lista_custo-dsc_fonte.
    lv_moeda_local  = ms_lista_custo-moeda_calculo.
    lv_moeda_final  = ms_lista_custo-moeda_lista.

    "Colocar simulação em memória
    set_simulacao( ms_lista_custo-id_simulacao ).

    DATA(lt_taxa_cambio) = get_taxa_cambio( iv_fonte       = lv_fonte
                                            iv_moeda_local = lv_moeda_local
                                            iv_moeda_final = lv_moeda_final
                                            is_simulacao   = ms_simulacao ).

    IF lines( lt_taxa_cambio ) = 0.
      RAISE EXCEPTION TYPE /qaps/cx_lista_custo_generator
        EXPORTING
          message = 'Taxa de Câmbio não encontrada'.
    ENDIF.

    return-tipo_lista = ms_tipo_lista.
    return-simulacao = ms_simulacao.
    return-std_producao = ms_std_producao.
    return-t_taxa_cambio = lt_taxa_cambio.

*********************************************************
*       Salvamento Temporário para exibição em cabeçalho, durante geração da lista
*********************************************************
    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_data = REF #( return ) ).

    lo_model_lista->update_content(
      EXPORTING
        iv_cod_lista_custo = ms_lista_custo-cod_lista_custo    " Lista Header
        iv_content         = lv_xml
        iv_status          = 'G'
    ).

*********************************************************

    "Buscar Centro/Planta - Destinos
*    BREAK abap.
    DATA(lt_planta_material) = get_planta_material( ).

    "Base apenas para a pre geração do reports
    DATA(lo_base) = NEW /qaps/cl_custo_calculate_base( iv_modalidade  = ''
                                                       iv_moeda_local = lv_moeda_local
                                                       iv_moeda_final = lv_moeda_final
                                                       it_taxa_cambio = lt_taxa_cambio ).
    lo_base->pre_generate_expressoes( ms_lista_custo-id_simulacao ).

    DATA(lo_calc_importacao) = /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade  = 'I'
                                                                            iv_moeda_local = lv_moeda_local
                                                                            iv_moeda_final = lv_moeda_final
                                                                            it_taxa_cambio = lt_taxa_cambio ).
    DATA(lo_calc_nacional) = /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade  = 'N'
                                                                          iv_moeda_local = lv_moeda_local
                                                                          iv_moeda_final = lv_moeda_final
                                                                          it_taxa_cambio = lt_taxa_cambio ).
    DATA(lo_calc_producao_conv) = /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade   = 'P'
                                                                               iv_moeda_local  = lv_moeda_local
                                                                               iv_moeda_final  = lv_moeda_final
                                                                               it_taxa_cambio  = lt_taxa_cambio
                                                                               iv_std_prd_type = 'C' ).
    DATA(lo_calc_producao_prod) = /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade   = 'P'
                                                                               iv_moeda_local  = lv_moeda_local
                                                                               iv_moeda_final  = lv_moeda_final
                                                                               it_taxa_cambio  = lt_taxa_cambio
                                                                               iv_std_prd_type = 'P' ).
    DATA(lo_calc_transferencia) = /qaps/cl_custo_calculate_base=>get_instance( iv_modalidade  = 'T'
                                                                               iv_moeda_local = lv_moeda_local
                                                                               iv_moeda_final = lv_moeda_final
                                                                               it_taxa_cambio = lt_taxa_cambio ).

    "Buscar origens por modalidade
    LOOP AT lt_planta_material INTO DATA(ls_planta_material).

      "IMPORTADO
      DATA(lt_origem_importacao) = get_origens( is_material = ls_planta_material iv_modalidade = 'I' ).
      DELETE lt_origem_importacao WHERE werks <> ls_planta_material-werks.
      IF lines( lt_origem_importacao ) > 0.
        DATA(lt_imp) = lo_calc_importacao->execute( is_simulacao       = ms_simulacao
                                                    it_origem          = lt_origem_importacao
                                                    is_material_centro = ls_planta_material ).
        APPEND LINES OF lt_imp TO return-t_importado.

        "Transferência
        REFRESH lt_origem_transf_imp.
        lt_origem_transf_imp = get_origens_transferencia( is_material  = ls_planta_material
                                                          is_simulacao = ms_simulacao ).
        IF lines( lt_imp ) > 0 AND lines( lt_origem_transf_imp ) > 0.
          DATA(lt_trans_imp) = lo_calc_transferencia->get_transferencias( is_simulacao         = ms_simulacao
                                                                          it_origem            = lt_origem_transf_imp
                                                                          it_origem_processada = lt_imp
                                                                          is_material_centro   = ls_planta_material ).
          APPEND LINES OF lt_trans_imp TO return-t_transf_importacao.
        ENDIF.

      ENDIF.

      "NACIONAL
      DATA(lt_origem_nacional) = get_origens( is_material = ls_planta_material iv_modalidade = 'N' ).
      DELETE lt_origem_nacional WHERE werks <> ls_planta_material-werks.
      IF lines( lt_origem_nacional ) > 0.
*        BREAK abap.
        DATA(lt_nac) = lo_calc_nacional->execute( is_simulacao       = ms_simulacao
                                                  it_origem          = lt_origem_nacional
                                                  is_material_centro = ls_planta_material ).
        APPEND LINES OF lt_nac TO return-t_nacional.

        "Transferência
*        BREAK c060863.
        REFRESH lt_origem_transf_nac.
        lt_origem_transf_nac = get_origens_transferencia( is_material  = ls_planta_material
                                                          is_simulacao = ms_simulacao ).
        IF lines( lt_nac ) > 0 AND lines( lt_origem_transf_nac ) > 0.
          DATA(lt_trans_nac) = lo_calc_transferencia->get_transferencias( is_simulacao         = ms_simulacao
                                                                          it_origem            = lt_origem_transf_nac
                                                                          it_origem_processada = lt_nac
                                                                          is_material_centro   = ls_planta_material ).
          APPEND LINES OF lt_trans_nac TO return-t_transf_nacional.
        ENDIF.

      ENDIF.

    ENDLOOP.

    "Std Produção - Conversão

    LOOP AT lt_planta_material INTO ls_planta_material.
      DATA(lt_origem_producao) = get_origens( is_material = ls_planta_material iv_modalidade = 'P' ).
      DELETE lt_origem_producao WHERE werks <> ls_planta_material-werks.

      IF lines( lt_origem_producao ) > 0.

        DATA(lt_prod) = lo_calc_producao_conv->get_std_producao_conversao(
          is_simulacao         = ms_simulacao
          it_origem            = lt_origem_producao
          is_material_centro   = ls_planta_material
          it_importado         = return-t_importado
          it_nacional          = return-t_nacional
          it_transf_importacao = return-t_transf_importacao
          it_transf_nacional   = return-t_transf_nacional ).
        APPEND LINES OF lt_prod TO return-t_producao_conversao.

        LOOP AT return-t_producao_conversao ASSIGNING FIELD-SYMBOL(<fs>).
          <fs>-std_prd_pa = |{ <fs>-std_prd_pa ALPHA  = IN WIDTH = 18 }|.
        ENDLOOP.

        "Transf
        DATA(lt_origem_transf_std) = get_origens_transferencia( is_material  = ls_planta_material
                                                                is_simulacao = ms_simulacao ).
        IF lines( lt_prod ) > 0 AND lines( lt_origem_transf_std ) > 0.

          DATA(lt_trans_std) = lo_calc_transferencia->get_transferencias_std_prd( is_simulacao         = ms_simulacao
                                                                                  it_origem            = lt_origem_transf_std
                                                                                  it_origem_processada = lt_prod
                                                                                  is_material_centro   = ls_planta_material ).
          APPEND LINES OF lt_trans_std TO return-t_transf_std_producao.
        ENDIF.


      ENDIF.

    ENDLOOP.

    "Std Produção - Produção
*    BREAK abap.
    LOOP AT lt_planta_material INTO ls_planta_material.

      lt_origem_producao = get_origens( is_material = ls_planta_material iv_modalidade = 'P' ).
      DELETE lt_origem_producao WHERE werks <> ls_planta_material-werks.

      IF lines( lt_origem_producao ) > 0.

        lt_prod = lo_calc_producao_prod->get_std_producao_producao(
          is_simulacao                = ms_simulacao
          it_origem                   = lt_origem_producao
          is_material_centro          = ls_planta_material
          it_importado                = return-t_importado
          it_nacional                 = return-t_nacional
          it_transf_importacao        = return-t_transf_importacao
          it_transf_nacional          = return-t_transf_nacional
          it_transf_std_prd_conversao = return-t_producao_conversao ).
        APPEND LINES OF lt_prod TO return-t_producao_producao.

*        "Transf
        lt_origem_transf_std = get_origens_transferencia( is_material  = ls_planta_material
                                                          is_simulacao = ms_simulacao ).
        IF lines( lt_prod ) > 0 AND lines( lt_origem_transf_std ) > 0.
*          break c060863.
          lt_trans_std = lo_calc_transferencia->get_transferencias_std_prd( is_simulacao         = ms_simulacao
                                                                            it_origem            = lt_origem_transf_std
                                                                            it_origem_processada = lt_prod
                                                                            is_material_centro   = ls_planta_material ).
          APPEND LINES OF lt_trans_std TO return-t_transf_std_producao.
        ENDIF.


      ENDIF.

    ENDLOOP.

    SORT return-t_importado BY material werks cod_porto cod_cais cod_trajeto.
    SORT return-t_nacional BY material werks cod_porto cod_cais cod_trajeto.
    SORT return-t_producao_conversao BY std_prd_pa std_prd_cp werks cod_porto cod_cais cod_trajeto.
    SORT return-t_producao_producao BY std_prd_pa std_prd_cp werks cod_porto cod_cais cod_trajeto.

    SORT return-t_transf_importacao BY material werks cod_porto cod_cais cod_trajeto.
    SORT return-t_transf_nacional BY material werks cod_porto cod_cais cod_trajeto.
    SORT return-t_transf_std_producao BY material werks cod_porto cod_cais cod_trajeto.

    fill_ponderacao( CHANGING cs_data = return ).

    numbering_items( CHANGING cs_data = return ).

    compile_errors( CHANGING cs_data = return ).

    lv_xml = /qaps/cl_serialization=>serialize( ir_data = REF #( return ) ).

    lo_model_lista->update_content(
      EXPORTING
        iv_cod_lista_custo = ms_lista_custo-cod_lista_custo    " Lista Header
        iv_content         = lv_xml
        iv_status          = 'A'
    ).


  ENDMETHOD.


  METHOD numbering_items.

    DATA lv_id      TYPE /qaps/ed_id_item_lista.

    LOOP AT cs_data-t_importado ASSIGNING FIELD-SYMBOL(<fs>).
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_nacional ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_producao_conversao ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_producao_producao  ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_transf_importacao  ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_transf_nacional  ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_transf_std_producao  ASSIGNING <fs>.
      lv_id = lv_id + 1.
      <fs>-id_item_lista_custo = lv_id.
    ENDLOOP.

    LOOP AT cs_data-t_ponderacao ASSIGNING FIELD-SYMBOL(<fs_ponderacao>).
      lv_id = lv_id + 1.
      <fs_ponderacao>-id_item_lista_custo = lv_id.
    ENDLOOP.

  ENDMETHOD.


  METHOD raise_exception.

    RAISE EXCEPTION TYPE /qaps/cx_lista_custo_generator
      EXPORTING
        message = iv_message.

  ENDMETHOD.


  METHOD set_simulacao.

    CLEAR: ms_simulacao,
           ms_tipo_lista,
           ms_std_producao.

    SELECT SINGLE *
      FROM /qaps/simulacao
      INTO @ms_simulacao
      WHERE id_simulacao = @iv_id_simulacao.

    IF sy-subrc NE 0.

      raise_exception( iv_message = `Simulação não existe` ).

    ELSE.

      SELECT SINGLE *
        FROM /qaps/tp_lista
        WHERE id_tp_lista = @ms_simulacao-id_tp_lista
        INTO @ms_tipo_lista.

      IF ms_simulacao-id_std_producao <> mc_guid_null.

        SELECT SINGLE *
          FROM /qaps/std_prd_h
          WHERE id_std_producao = @ms_simulacao-id_std_producao
          INTO @ms_std_producao.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update_ponderacao.

    DATA: ls_content TYPE /qaps/s_retorno_final,
          lr_data type ref to data.

    DATA(lo_model_lista) = NEW /qaps/cl_mdl_lista_custo( ).
    ms_lista_custo = lo_model_lista->get_single_lista_custo( iv_cod_lista_custo ).

    lr_data = ref #( ls_content ).

     /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_lista_custo-content
                                          CHANGING  cr_data = lr_data ).

    refresh ls_content-t_ponderacao.

    fill_ponderacao( CHANGING cs_data = ls_content  ).

    numbering_items( CHANGING cs_data = ls_content ).

    data(lv_xml) = /qaps/cl_serialization=>serialize( ir_data = REF #( ls_content ) ).

    lo_model_lista->update_content(
      EXPORTING
        iv_cod_lista_custo = ms_lista_custo-cod_lista_custo    " Lista Header
        iv_content         = lv_xml
        iv_status          = 'A'
    ).

  ENDMETHOD.


  METHOD get_distribuicao_premissa.

    SELECT *
      FROM /qaps/prem_distr
      WHERE id_distribuicao = @iv_id_distribuicao
      INTO TABLE @return.

  ENDMETHOD.


  METHOD get_distribuicao_trajeto.

    SELECT *
      FROM /qaps/prem_traj
      WHERE id_distribuicao = @iv_id_distribuicao
      AND   id_trajeto      = @iv_id_trajeto
      INTO TABLE @return.

  ENDMETHOD.
ENDCLASS.
