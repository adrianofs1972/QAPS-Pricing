class /QAPS/CL_VIEW_REL_0001 definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods SET_DATA
    redefinition .
protected section.

  methods CUSTOMIZE_CATALOG
    redefinition .
  methods DISPLAY_ALV
    redefinition .
  methods GET_LAYOUT
    redefinition .
private section.

  data MT_DATA type /QAPS/T_REL_0001 .
ENDCLASS.



CLASS /QAPS/CL_VIEW_REL_0001 IMPLEMENTATION.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                     OR fieldname = 'MESSAGE'
                     OR fieldname = 'STYLE'
                     OR fieldname = 'COLOR'
                     or fieldname ='TIPO_NODE'
                     OR datatype = 'RAW'.

    define set_text.
      <fs>-coltext = &1.
      <fs>-tooltip = &1.
    end-OF-DEFINITION.

*    break c060863.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-fieldname.
        WHEN 'WERKS'.
        WHEN 'COD_GRP_PLANTA'.
        WHEN 'MATERIAL'.
          <fs>-outputlen = 12.
        WHEN 'AGREGADOR'.
          <fs>-outputlen = 12.
        WHEN 'MES'.
          set_text 'Mês Geração Lista'.
        WHEN 'ANO'.
          set_text 'Ano Geração Lista'.
        WHEN 'DSC_GRUPO_PRODUTO'.
          <fs>-outputlen = 12.
        WHEN 'DSC_CUSTO_ELEMENTAR'.
          <fs>-outputlen = 15.
        WHEN 'PERIODO'.
        WHEN 'COD_LISTA_CUSTO_REF'.
          <fs>-outputlen = 8.
        WHEN 'DESCRICAO_REF'.
          <fs>-outputlen = 15.
        WHEN 'PERIODO_REF'.
          SET_TEXT 'Período Referencia'.
        WHEN 'PERIODO_COMP'.
          SET_TEXT 'Periodo Comparativo'.
        WHEN 'STATUS_REF'.
        WHEN 'TAXA_REF'.
        WHEN 'VALOR_REF'.
          <fs>-outputlen = 15.
          set_text 'Valor Referência (BRL)'.
        WHEN 'VALOR_MOEDA_FINAL_REF'.
          set_text 'Valor Referência (USD)'.
          <fs>-outputlen = 15.
        WHEN 'COD_LISTA_CUSTO_COMP'.
          <fs>-outputlen = 8.
        WHEN 'DESCRICAO_COMP'.
          <fs>-outputlen = 15.
        WHEN 'STATUS_COMP'.
        WHEN 'TAXA_COMP'.
        WHEN 'VALOR_COMP'.
          <fs>-outputlen = 15.
          set_text 'Valor Comparação(BRL)'.
        WHEN 'VALOR_MOEDA_FINAL_COMP'.
          <fs>-outputlen = 15.
          set_text 'Valor Comparação(USD)'.
        WHEN 'VALOR_VAR'.
          <fs>-outputlen = 15.
          set_text 'Variação(BRL)'.
        WHEN 'VALOR_MOEDA_FINAL_VAR'.
          set_text 'Variação(USD)'.
          <fs>-outputlen = 15.
        WHEN 'PERCENTUAL_VAR'.
          set_text 'Variação %'.
          <fs>-outputlen = 15.
      ENDCASE.

      endloop.

*      CASE <fs>-fieldname.
*        WHEN 'MATNR'.
*          <fs>-col_pos = 1.
*          <fs>-hotspot = 'X'.
*        WHEN 'DSC_MAKTX'.
*          <fs>-col_pos = 2.
*        WHEN 'MEINS'.
*          <fs>-col_pos = 3.
**          <fs>-coltext = 'Teor Real'.
*        WHEN 'MEINS'.
*          <fs>-col_pos = 4.
*        WHEN 'TIPO_DESTINO'.
*          <fs>-coltext = 'Tipo Destino'.
**          <fs>-col_pos = 5.
*          <fs>-outputlen = 12.
*        WHEN 'COD_DESTINO'.
*          <fs>-coltext = 'Código'.
**          <fs>-col_pos = 5.
*          <fs>-outputlen = 8.
**        WHEN 'DSC_DESTINO'.
***          <fs>-coltext = 'Descrição'.
***          <fs>-col_pos = 5.
**          <fs>-outputlen = 20.
*      ENDCASE.

*    ENDLOOP.

        endmethod.


  METHOD DISPLAY_ALV.

*    ASSIGN mr_outtab->* TO FIELD-SYMBOL(<fs>).

    mo_alv->set_table_for_first_display(
      EXPORTING
*       i_buffer_active =
*       i_bypassing_buffer            =
*       i_consistency_check           =
*       i_structure_name              =
*       is_variant      =
*       i_save          =
*       i_default       = 'X'
        is_layout       = is_layout
*       is_print        =
*       it_special_groups             =
*       it_toolbar_excluding          =
*       it_hyperlink    =
*       it_alv_graphics =
*       it_except_qinfo =
*       ir_salv_adapter =
      CHANGING
        it_outtab       = mt_data
        it_fieldcatalog = it_catalog
       it_sort         = it_sort
*       it_filter       =
*      EXCEPTIONS
*       invalid_parameter_combination = 1
*       program_error   = 2
*       too_many_lines  = 3
*       others          = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  method GET_LAYOUT.

    return-zebra = 'X'.
    return-sel_mode = 'A'.
    return-ctab_fname = 'COLOR'.
    return-stylefname = 'STYLE'.
    return-grid_title = 'Comparativo'.

  endmethod.


  METHOD set_data.

    FIELD-SYMBOLS <fs> TYPE /qaps/t_rel_0001.
    mr_outtab = ir_outtab.

    ASSIGN mr_outtab->* TO <fs>.
    mt_data = <fs>.

    mo_alv->refresh_table_display(
      EXPORTING
        is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
        i_soft_refresh = iv_soft_refresh
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).


  ENDMETHOD.
ENDCLASS.
