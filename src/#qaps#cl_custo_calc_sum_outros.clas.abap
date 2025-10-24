class /QAPS/CL_CUSTO_CALC_SUM_OUTROS definition
  public
  inheriting from /QAPS/CL_CUSTO_CALCULATE_BASE
  final
  create public .

public section.

  methods SOLVE_OUTROS_TOTAL_EXPRESSAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
    changing
      !CS_EXPRESSAO type /QAPS/S_PONDERACAO .

  methods EXECUTE
    redefinition .
protected section.
private section.

  methods CONVERTER_UNIDADE
    importing
      !IV_MATNR type MATNR
      !IV_QUANTIDADE type KMPMG
      !IV_UNIDADE_INPUT type MSEHI
      !IV_UNIDADE_OUTPUT type MSEHI
    returning
      value(RETURN) type KMPMG .
ENDCLASS.



CLASS /QAPS/CL_CUSTO_CALC_SUM_OUTROS IMPLEMENTATION.


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


  METHOD SOLVE_OUTROS_TOTAL_EXPRESSAO.

*    BREAK c060863.
    ms_simulacao = is_simulacao.

    REFRESH mt_expressao.
    mt_expressao = cs_expressao-t_expressao.

    LOOP AT mt_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).

      IF <fs_expressao>-tipo_node <> 'E' and <fs_expressao>-tipo_node <> 'R'.
        CLEAR <fs_expressao>-calculated.
      else.
        <fs_expressao>-calculated = 'X'.
      ENDIF.

    ENDLOOP.

    solve_expressao( ).

    fill_moeda_saida( CHANGING ct_expressao = mt_expressao ).

*    break c060863.
    cs_expressao-t_expressao = mt_expressao.

  ENDMETHOD.
ENDCLASS.
