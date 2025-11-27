class /QAPS/CL_CUSTO_CALC_TRANSF definition
  public
  inheriting from /QAPS/CL_CUSTO_CALCULATE_BASE
  final
  create public .

public section.

  methods GET_TRANSFERENCIAS_STD_PRD_2
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IT_ORIGEM type /QAPS/T_PREMISSA_MATRIZ
      !IS_MATERIAL_CENTRO type /QAPS/V_PLNT_MAT
      !IT_ORIGEM_PROCESSADA type /QAPS/T_RETORNO_CALCULO optional
    returning
      value(RETURN) type /QAPS/T_RETORNO_CALCULO .

  methods GET_TRANSFERENCIAS
    redefinition .
  methods GET_TRANSFERENCIAS_STD_PRD
    redefinition .
protected section.

  methods FILL_VARIAVEIS_ELEMENTARES
    redefinition .
private section.

  data MT_MERGE type /QAPS/T_EXPRESSAO .
  data MT_VALUES_CHANGE type /QAPS/T_VALUES .
ENDCLASS.



CLASS /QAPS/CL_CUSTO_CALC_TRANSF IMPLEMENTATION.


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

      IF <fs_expressao>-dsc_custo_elementar = 'FOB Transferência'.
        <fs_expressao>-t_valores = mt_merge[ fieldname = 'VLR_CUSTO_GERENCIAL' ]-t_valores.
      ELSE.

        IF <fs_expressao>-tipo_variavel <> 'F'. "FRETE

          fill_valores_elementares( EXPORTING is_material_centro = is_material_centro
                                              is_custo_elementar = lt_custo_individual[ 1 ]
                                              is_origem          = is_origem
                                      CHANGING ct_values = <fs_expressao>-t_valores ).
        ELSE.
          break c060863.
          fill_valores_elem_com_trajeto( EXPORTING is_material_centro = is_material_centro
                                                   is_custo_elementar = lt_custo_individual[ 1 ]
                                                   is_origem          = is_origem
                                                   is_trajeto         = is_trajeto
                                         CHANGING ct_values = <fs_expressao>-t_valores ).
        ENDIF.
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


  METHOD get_transferencias.

    DATA: lv_times   TYPE sy-index,
          ls_trajeto TYPE /qaps/s_dist_traj. "/qaps/s_v_prm_trj.

    ms_simulacao = is_simulacao.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @it_origem
      WHERE id_ponto = @it_origem-id_origem
      INTO TABLE @DATA(lt_ponto).

    LOOP AT it_origem INTO DATA(ls_origem).

*      CHECK ls_origem-std_prd_total = abap_true.

      DATA(ls_ponto) = lt_ponto[ id_ponto = ls_origem-id_origem ].

      DATA(lt_processada) = it_origem_processada.

      LOOP AT lt_processada INTO DATA(ls_processada).

        mt_merge = ls_processada-t_expressao.
        DELETE mt_merge WHERE fieldname <> 'VLR_CUSTO_GERENCIAL'.

*        IF lines( ls_origem-t_traj_trecho ) > 0.
*          lv_times = lines( ls_origem-t_traj_trecho ).
*        ELSE.
*          lv_times = 1.
*        ENDIF.

        IF lines( ls_origem-t_trajetos ) > 0.
          lv_times = lines( ls_origem-t_trajetos ).
        ELSE.
          lv_times = 1.
        ENDIF.

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
*          <fs_return>-cod_origem = ls_origem-cod_origem.
          <fs_return>-cod_trajeto = ls_trajeto-cod_trajeto.
          <fs_return>-desc_trajeto = ls_trajeto-descricao.

          CASE <fs_return>-tipo_origem.
            WHEN 'F'.
*            break c060863.
              ls_ponto = VALUE #( lt_ponto[ id_ponto = <fs_return>-id_origem ] OPTIONAL ).
              <fs_return>-lifnr = ls_ponto-codigo.
              <fs_return>-dsc_lifnr = ls_ponto-descricao.
          ENDCASE.

          <fs_return>-t_expressao = mt_expressao.
          SORT <fs_return>-t_expressao BY price_field DESCENDING fieldname ASCENDING.

        ENDDO.

      ENDLOOP.

    ENDLOOP.

    LOOP AT return ASSIGNING <fs_return>.
      fill_moeda_saida( CHANGING ct_expressao = <fs_return>-t_expressao ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_transferencias_std_prd.

    DATA: lv_times   TYPE sy-index,
          ls_trajeto TYPE /qaps/s_dist_traj. "/qaps/s_v_prm_trj.

    ms_simulacao = is_simulacao.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @it_origem
      WHERE id_ponto = @it_origem-id_origem
      INTO TABLE @DATA(lt_ponto).

    LOOP AT it_origem INTO DATA(ls_origem).

*      CHECK ls_origem-std_prd_total = abap_true.

      DATA(ls_ponto) = lt_ponto[ id_ponto = ls_origem-id_origem ].

      DATA(lt_processada) = it_origem_processada.
      DELETE lt_processada WHERE std_prd_total = abap_false.

      LOOP AT lt_processada INTO DATA(ls_processada).

        mt_merge = ls_processada-t_expressao.
        DELETE mt_merge WHERE fieldname <> 'VLR_CUSTO_GERENCIAL'.

*        IF lines( ls_origem-t_traj_trecho ) > 0.
*          lv_times = lines( ls_origem-t_traj_trecho ).
*        ELSE.
*          lv_times = 1.
*        ENDIF.

        IF lines( ls_origem-t_trajetos ) > 0.
          lv_times = lines( ls_origem-t_trajetos ).
        ELSE.
          lv_times = 1.
        ENDIF.

        DO lv_times TIMES.

          ls_trajeto = VALUE #( ls_origem-t_trajetos[ sy-index ] OPTIONAL ).

          REFRESH mt_expressao.
          "Expressao
          get_expressao( ).

          "Carregar variáveis elementares
          fill_variaveis_elementares( is_material_centro = is_material_centro
                                      is_origem          = ls_origem
                                      is_trajeto         = ls_trajeto ).

          "Resolver expressão
          solve_expressao( ).

          APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
          <fs_return> = CORRESPONDING #( ls_origem ).
          <fs_return>-material = is_material_centro-matnr.
          <fs_return>-plant = is_material_centro-werks.
*          <fs_return>-cod_origem = ls_origem-cod_origem.
          <fs_return>-cod_trajeto = ls_trajeto-cod_trajeto.
          <fs_return>-desc_trajeto = ls_trajeto-descricao.

          CASE <fs_return>-tipo_origem.
            WHEN 'F'.
*            break c060863.
              ls_ponto = VALUE #( lt_ponto[ id_ponto = <fs_return>-id_origem ] OPTIONAL ).
              <fs_return>-lifnr = ls_ponto-codigo.
              <fs_return>-dsc_lifnr = ls_ponto-descricao.
          ENDCASE.

          <fs_return>-t_expressao = mt_expressao.
          SORT <fs_return>-t_expressao BY price_field DESCENDING fieldname ASCENDING.

        ENDDO.

      ENDLOOP.

    ENDLOOP.

    LOOP AT return ASSIGNING <fs_return>.
      fill_moeda_saida( CHANGING ct_expressao = <fs_return>-t_expressao ).
    ENDLOOP.



  ENDMETHOD.


  METHOD get_transferencias_std_prd_2.

    DATA: lv_times   TYPE sy-index,
          ls_trajeto TYPE /qaps/s_dist_traj. "/qaps/s_v_prm_trj.

    ms_simulacao = is_simulacao.

    SELECT *
      FROM /qaps/v_ponto
      FOR ALL ENTRIES IN @it_origem
      WHERE id_ponto = @it_origem-id_origem
      INTO TABLE @DATA(lt_ponto).

    BREAK abap.

    LOOP AT it_origem INTO DATA(ls_origem).

      DATA(ls_ponto) = lt_ponto[ id_ponto = ls_origem-id_origem ].

      DATA(lt_processada) = it_origem_processada.
      DELETE lt_processada WHERE std_prd_total = abap_false.

      LOOP AT lt_processada INTO DATA(ls_processada).

        mt_merge = ls_processada-t_expressao.
        DELETE mt_merge WHERE fieldname <> 'VLR_CUSTO_GERENCIAL'.

*        IF lines( ls_origem-t_traj_trecho ) > 0.
*          lv_times = lines( ls_origem-t_traj_trecho ).
*        ELSE.
*          lv_times = 1.
*        ENDIF.

        IF lines( ls_origem-t_trajetos ) > 0.
          lv_times = lines( ls_origem-t_trajetos ).
        ELSE.
          lv_times = 1.
        ENDIF.

        DO lv_times TIMES.

          ls_trajeto = VALUE #( ls_origem-t_trajetos[ sy-index ] OPTIONAL ).

          REFRESH mt_expressao.
          "Expressao
          get_expressao( ).

          "Carregar variáveis elementares
          fill_variaveis_elementares( is_material_centro = is_material_centro
                                      is_origem          = ls_origem
                                      is_trajeto         = ls_trajeto ).

          "Resolver expressão
          solve_expressao( ).

          APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
          <fs_return> = CORRESPONDING #( ls_origem ).
          <fs_return>-material = is_material_centro-matnr.
          <fs_return>-plant = is_material_centro-werks.
*          <fs_return>-cod_origem = ls_origem-cod_origem.
          <fs_return>-cod_trajeto = ls_trajeto-cod_trajeto.
          <fs_return>-desc_trajeto = ls_trajeto-descricao.
          <fs_return>-t_dist_premissa = ls_origem-t_dist_premissa.
          <fs_return>-trajeto      = ls_trajeto.

          CASE <fs_return>-tipo_origem.
            WHEN 'F'.
*            break c060863.
              ls_ponto = VALUE #( lt_ponto[ id_ponto = <fs_return>-id_origem ] OPTIONAL ).
              <fs_return>-lifnr = ls_ponto-codigo.
              <fs_return>-dsc_lifnr = ls_ponto-descricao.
          ENDCASE.

          <fs_return>-t_expressao = mt_expressao.
          SORT <fs_return>-t_expressao BY price_field DESCENDING fieldname ASCENDING.

        ENDDO.

      ENDLOOP.

    ENDLOOP.

    LOOP AT return ASSIGNING <fs_return>.
      fill_moeda_saida( CHANGING ct_expressao = <fs_return>-t_expressao ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
