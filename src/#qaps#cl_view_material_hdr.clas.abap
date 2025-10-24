class /QAPS/CL_VIEW_MATERIAL_HDR definition
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



CLASS /QAPS/CL_VIEW_MATERIAL_HDR IMPLEMENTATION.


  METHOD MERGE_DATA_TO_HTML.

    FIELD-SYMBOLS <fs> type /qaps/s_tp_lista.
    DATA ls_html TYPE ts_html.
    data lv_line TYPE char255.

    DEFINE add_to_html.
      clear ls_html.
      ls_html-line = &1.
      APPEND ls_html TO et_html.
    END-OF-DEFINITION.

    assign ir_data->* to <fs>.

    lv_line = '<html><body>'.
    add_to_html lv_line.

    lv_line = '<table>'.
    add_to_html lv_line.

    lv_line = `<tr><td>Tipo de Lista:</td><td>` && <fs>-descricao && `</td></tr>`.
    add_to_html lv_line.
    lv_line = `<tr><td>&nbsp;</td><td>&nbsp;</td></tr>`.
    add_to_html lv_line.

    data(lv_aprovacao) = cond #( when <fs>-aprovacao = 'X' then 'Sim'
                                 else 'Não' ).

    lv_line = `<tr><td>Necessita Aprovação:</td><td>` && lv_aprovacao && `</td></tr>`.
    add_to_html lv_line.

    data(lv_ativo) = cond char10( when <fs>-ativo = 'X' then 'Ativo'
                                 else 'Inativo' ).
    lv_line = `<tr><td>Status:</td><td>` && lv_ativo && `</td></tr>`.
    add_to_html lv_line.

    lv_line = '</table>'.
    add_to_html lv_line.

    lv_line = '</body></html>'.
    add_to_html lv_line.


  ENDMETHOD.
ENDCLASS.
