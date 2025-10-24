class /QAPS/CL_VIEW_MATRIZ_SIMUL_HDR definition
  public
  inheriting from /QAPS/CL_VIEW_HTML_BASE
  final
  create public .

public section.

  methods REFRESH
    redefinition .
protected section.

  methods MERGE_DATA_TO_HTML
    redefinition .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_MATRIZ_SIMUL_HDR IMPLEMENTATION.


  METHOD merge_data_to_html.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_matriz_abast_simulacao.
    DATA ls_html TYPE ts_html.
    DATA lv_line TYPE char255.

    DEFINE add_to_html.
      CLEAR ls_html.
      ls_html-line = &1.
      APPEND ls_html TO et_html.
    END-OF-DEFINITION.

    ASSIGN ir_data->* TO <fs>.

    lv_line = '<html><body>'.
    add_to_html lv_line.

    lv_line = '<table>'.
    add_to_html lv_line.

    DATA(lv_simulacao) = |{ <fs>-id_simulacao ALPHA = OUT } - { <fs>-descricao }|.

    lv_line = `<tr><td>Simulação:</td><td>` && lv_simulacao && `</td></tr>`.
    add_to_html lv_line.

*    lv_line = `<tr><td>&nbsp;</td><td>&nbsp;</td></tr>`.
*    add_to_html lv_line.

    lv_line = `<tr><td>Tipo de Lista:</td><td>` && <fs>-dsc_tp_lista && `</td></tr>`.
    add_to_html lv_line.

    DATA(lv_ini) = <fs>-periodo_inicial.
    DATA(lv_fim) = <fs>-periodo_final.

    DATA(lv_periodo) = |{ lv_ini+4(2) }.{ lv_ini(4) } a { lv_fim+4(2) }.{ lv_fim(4) } |.
    lv_line = `<tr><td>Período:</td><td>` && lv_periodo && `</td></tr>`.
    add_to_html lv_line.

    lv_line = `<tr><td>Status:</td><td>` && <fs>-dsc_status && `</td></tr>`.
    add_to_html lv_line.

    lv_line = '</table>'.
    add_to_html lv_line.

    lv_line = '</body></html>'.
    add_to_html lv_line.


  ENDMETHOD.


  method REFRESH.
    DATA lt_html TYPE tt_html.

    APPEND VALUE ts_html( line = '<html><body></body></html>' ) TO lt_html.
    display_html_data( lt_html ).
  endmethod.
ENDCLASS.
