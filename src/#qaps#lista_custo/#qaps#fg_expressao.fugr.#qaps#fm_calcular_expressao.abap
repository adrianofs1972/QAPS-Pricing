FUNCTION /qaps/fm_calcular_expressao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_SIMULACAO) TYPE  /QAPS/SIMULACAO
*"     VALUE(IS_EXPRESSAO) TYPE  /QAPS/S_EXPRESSAO
*"     VALUE(IT_PARAMETROS) TYPE  /QAPS/T_CALC_PARAMETER
*"  EXPORTING
*"     VALUE(EV_VALOR) TYPE  /QAPS/VALOR_ABSOLUTO
*"     VALUE(EV_PERCENTUAL) TYPE  /QAPS/PERCENTUAL
*"----------------------------------------------------------------------

  DATA: lt_params TYPE STANDARD TABLE OF rsparams,
        ls_param  TYPE rsparams.

  DATA: lv_report TYPE programm,
        lv_result TYPE /qaps/valor_absoluto.

  DATA(lv_simulacao) = |{ is_simulacao-id_simulacao ALPHA = OUT }|.
  CONDENSE lv_simulacao NO-GAPS.
  lv_report = |Z_{ lv_simulacao }_{ is_expressao-fieldname }|.

  LOOP AT it_parametros INTO DATA(ls_expressao).

    CLEAR ls_param.
    ls_param-selname = ls_expressao-fieldname.
    ls_param-kind    = 'P'.        " 'P' para PARAMETERS
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = COND #( WHEN ls_expressao-tipo_dado = '1' THEN ls_expressao-valor
                               ELSE ls_expressao-percentual ).
    APPEND ls_param TO lt_params.

  ENDLOOP.

  SUBMIT (lv_report)
  WITH SELECTION-TABLE lt_params
  AND RETURN.

  IMPORT lv_result TO lv_result FROM MEMORY ID 'QAPS_VALOR'.

  ev_valor = lv_result.

ENDFUNCTION.
