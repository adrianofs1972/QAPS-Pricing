class /QAPS/CL_VIEW_TX_CAMBIO_HDR definition
  public
  inheriting from /QAPS/CL_VIEW_HTML_BASE
  final
  create public .

public section.
protected section.

  methods MERGE_DATA_TO_HTML
    redefinition .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_TX_CAMBIO_HDR IMPLEMENTATION.


  METHOD merge_data_to_html.

    FIELD-SYMBOLS: <fs> TYPE tcurt,
                   <fs_periodo> type /qaps/s_periodo_interval.
    DATA ls_html TYPE ts_html.
    DATA lv_line TYPE char255.

    DEFINE add_to_html.
      CLEAR ls_html.
      ls_html-line = &1.
      APPEND ls_html TO et_html.
    END-OF-DEFINITION.

    ASSIGN ir_data->* TO <fs>.
    ASSIGN ir_addtional_data->* TO <fs_periodo>.

    lv_line = '<html><body>'.
    add_to_html lv_line.

    lv_line = '<table>'.
    add_to_html lv_line.

    lv_line = `<tr><td>Moeda Local:</td><td>` && <fs>-waers && ` - ` && <fs>-ktext && `</td></tr>`.
    add_to_html lv_line.

    DATA(lv_ini) = <fs_periodo>-inicial.
    DATA(lv_fim) = <fs_periodo>-final.

    DATA(lv_periodo) = |{ lv_ini+4(2) }.{ lv_ini(4) } a { lv_fim+4(2) }.{ lv_fim(4) } |.
    lv_line = `<tr><td>Período:</td><td>` && lv_periodo && `</td></tr>`.
    add_to_html lv_line.
*    lv_line = `<tr><td>&nbsp;</td><td>&nbsp;</td></tr>`.
*    add_to_html lv_line.

*    data(lv_aprovacao) = cond #( when <fs>-aprovacao = 'X' then 'Sim'
*                                 else 'Não' ).
*
*    lv_line = `<tr><td>Necessita Aprovação:</td><td>` && lv_aprovacao && `</td></tr>`.
*    add_to_html lv_line.
*
*    data(lv_ativo) = cond char10( when <fs>-ativo = 'X' then 'Ativo'
*                                 else 'Inativo' ).
*    lv_line = `<tr><td>Status:</td><td>` && lv_ativo && `</td></tr>`.
*    add_to_html lv_line.

    lv_line = '</table>'.
    add_to_html lv_line.

    lv_line = '</body></html>'.
    add_to_html lv_line.


  ENDMETHOD.
ENDCLASS.
