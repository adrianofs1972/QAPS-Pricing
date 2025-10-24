class /QAPS/CL_VIEW_TIPO_LISTA_HDR definition
  public
  inheriting from /QAPS/CL_VIEW_HTML_BASE
  final
  create public .

public section.

  methods SET_FULL_HEADER
    importing
      !IV_FULL type ABAP_BOOL default ABAP_TRUE .

  methods REFRESH
    redefinition .
protected section.

  methods MERGE_DATA_TO_HTML
    redefinition .
private section.

  data MV_FULL type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
ENDCLASS.



CLASS /QAPS/CL_VIEW_TIPO_LISTA_HDR IMPLEMENTATION.


  METHOD merge_data_to_html.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_tp_lista.
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

    lv_line = `<tr><td style="width:180px;">Código:</td><td>` && <fs>-cod_tp_lista && `</td></tr>`.
    add_to_html lv_line.

    lv_line = `<tr><td>Tipo de Lista:</td><td>` && <fs>-descricao && `</td></tr>`.
    add_to_html lv_line.

    IF mv_full = abap_true.

      lv_line = `<tr style="height:10px;"><td></td><td></td></tr>`.
      add_to_html lv_line.

      DATA(lv_aprovacao) = COND #( WHEN <fs>-aprovacao = 'X' THEN 'Sim'
                                   ELSE 'Não' ).

      lv_line = `<tr><td>Necessita Aprovação:</td><td>` && lv_aprovacao && `</td></tr>`.
      add_to_html lv_line.

      DATA(lv_ativo) = COND char10( WHEN <fs>-ativo = 'X' THEN 'Ativo'
                                   ELSE 'Inativo' ).
      lv_line = `<tr><td>Status:</td><td>` && lv_ativo && `</td></tr>`.
      add_to_html lv_line.

    ENDIF.

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


  METHOD set_full_header.
    mv_full = iv_full.
  ENDMETHOD.
ENDCLASS.
