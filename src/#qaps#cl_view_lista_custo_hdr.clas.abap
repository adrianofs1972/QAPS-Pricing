class /QAPS/CL_VIEW_LISTA_CUSTO_HDR definition
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



CLASS /QAPS/CL_VIEW_LISTA_CUSTO_HDR IMPLEMENTATION.


  METHOD merge_data_to_html.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_lista_header.
    DATA ls_html TYPE ts_html.
    DATA lv_line TYPE char255.
    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    DEFINE add_to_html.
      CLEAR ls_html.
      ls_html-line = &1.
      APPEND ls_html TO et_html.
    END-OF-DEFINITION.


    ASSIGN ir_data->* TO <fs>.

*    BREAK c060863.

    lr_data = REF #( ls_data ).
    IF NOT <fs>-content IS INITIAL.
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  =  <fs>-content
                                           CHANGING  cr_data = lr_data ).
    ENDIF.

    lv_line = '<html><body style="margin-top: 1px;margin-bottom: 0px">'.
    add_to_html lv_line.

    "container
    lv_line = '<table style="width:95%;boder:10px">'.
    add_to_html lv_line.
    "Célula 1
**********************************************************************
*    Lado Esquerdo
************************************************************************
    lv_line = `<tr><td style="width:50%;vertical-align:top;">`.
    add_to_html lv_line.

    lv_line = '<table style="width:100%;font-size:12px;">'.
    add_to_html lv_line.

    DATA(lv_tipo_lista) = ls_data-tipo_lista-cod_tp_lista && ` - ` && ls_data-tipo_lista-descricao.
    lv_line = `<tr><td style="width:15%;">Tipo Lista:</td><td style="width:45%;">` && lv_tipo_lista && `</td></trd>`.
    add_to_html lv_line.

    DATA(lv_id_simulacao) = |{ ls_data-simulacao-id_simulacao ALPHA = OUT }|.
    CONDENSE lv_id_simulacao NO-GAPS.
    DATA(lv_simulacao) = lv_id_simulacao && ` - ` && ls_data-simulacao-descricao.
    lv_line = `<tr><td>Simulação:</td><td>` && lv_simulacao && `</td></tr>`.
    add_to_html lv_line.

    IF NOT ls_data-std_producao IS INITIAL.
      DATA(lv_id_std_producao) = |{ ls_data-std_producao-codigo ALPHA = OUT }|.
      CONDENSE lv_id_std_producao NO-GAPS.
      DATA(lv_std_producao) = lv_id_std_producao && ` - ` && ls_data-std_producao-descricao.
      lv_line = `<tr><td>Std Produção:</td><td>` && lv_std_producao && `</td></tr>`.
      add_to_html lv_line.
    ENDIF.

    lv_line = `<tr><td>Fonte Câmbio:</td><td>` && <fs>-dsc_fonte && `</td></tr>`.
    add_to_html lv_line.

    DATA(lv_moeda) = <fs>-moeda_calculo && ` / ` && <fs>-moeda_lista.
    lv_line = `<tr><td>Moeda (Local/Final):</td><td>` && lv_moeda && `</td></tr>`.
    add_to_html lv_line.

    DATA(lv_metodo) = <fs>-metodo_custeio && ` - ` && <fs>-dsc_metodo_custeio.
    lv_line = `<tr><td>Método Custeio:</td><td>` && lv_metodo && `</td></tr>`.
    add_to_html lv_line.


    lv_line = '</table>'.
    add_to_html lv_line.

    ""Célula 2
***********************************************************************
*    Lado Direito
************************************************************************
*    BREAK c060863.
    lv_line = `</td><td td style="width:50%;vertical-align:top;">`.
    add_to_html lv_line.

    "Taxas de Cãmbio por período
    lv_line = `<table style="font-size:12px;width:100%;"><thead><tr>`.
    add_to_html lv_line.

*    lv_line = `<th colspan="6">Taxa de Câmbio</th></tr>`.
*    add_to_html lv_line.

    lv_line = `<tr style="padding-left:3px;;"><th>Período</th><th>Taxa</th><th>Período</th><th>Taxa</th><th>Período</th><th>Taxa</th></tr>`.
    add_to_html lv_line.

*    DATA(lv_times) = lines( ls_data-t_taxa_cambio ) / 3.
*    BREAK c060863.

    SORT ls_data-t_taxa_cambio BY periodo.
    DO 3 TIMES.

      DATA(lv_index) = sy-index.

      lv_line = `<tr style="padding-left:3px;text-align:center;">`.
      add_to_html lv_line.

      DATA(ls_periodo) = VALUE #( ls_data-t_taxa_cambio[ lv_index ] OPTIONAL ).
*
      IF NOT ls_periodo IS INITIAL.
        DATA(lv_periodo) = ls_periodo-periodo+4(2) && `.` && ls_periodo-periodo(4).
        DATA(lv_taxa) = ls_periodo-taxa.
        lv_line = `<td style="text-align:center;">` && lv_periodo && `</td><td>` && lv_taxa && `</td>`.
      ENDIF.
      add_to_html lv_line.

      lv_index = lv_index + 4.
      ls_periodo = VALUE #( ls_data-t_taxa_cambio[ lv_index  ] OPTIONAL ).
*
      IF NOT ls_periodo IS INITIAL.
        lv_periodo = ls_periodo-periodo+4(2) && `.` && ls_periodo-periodo(4).
        lv_taxa = ls_periodo-taxa.
        lv_line = `<td style="text-align:center;">` && lv_periodo && `</td><td>` && lv_taxa && `</td>`.
      ELSE.
        lv_line = `<td style="text-align:center;">&nbsp;</td><td>&nbsp;</td>`.
      ENDIF.
      add_to_html lv_line.

      lv_index = lv_index + 4.
      ls_periodo = VALUE #( ls_data-t_taxa_cambio[ lv_index  ] OPTIONAL ).
*
      IF NOT ls_periodo IS INITIAL.
        lv_periodo = ls_periodo-periodo+4(2) && `.` && ls_periodo-periodo(4).
        lv_taxa = ls_periodo-taxa.
        lv_line = `<td style="text-align:center;">` && lv_periodo && `</td><td>` && lv_taxa && `</td>`.
      ELSE.
        lv_line = `<td style="text-align:center;">&nbsp;</td><td>&nbsp;</td>`.
      ENDIF.
      add_to_html lv_line.

      lv_line = `</tr>`.
      add_to_html lv_line.
    ENDDO.

*    ENDLOOP.

    lv_line = `</td></tr></table>`.
    add_to_html lv_line.


*************************************************************************
    lv_line = '</body></html>'.
    add_to_html lv_line.


  ENDMETHOD.


  method REFRESH.
    DATA lt_html TYPE tt_html.

    APPEND VALUE ts_html( line = '<html><body></body></html>' ) TO lt_html.
    display_html_data( lt_html ).
  endmethod.


  METHOD SET_FULL_HEADER.
    mv_full = iv_full.
  ENDMETHOD.
ENDCLASS.
