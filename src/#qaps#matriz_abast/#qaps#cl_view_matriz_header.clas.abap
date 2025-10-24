class /QAPS/CL_VIEW_MATRIZ_HEADER definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods RESET
    redefinition .
  methods SET_DATA
    redefinition .
  methods SET_GRID_TITLE
    redefinition .
protected section.

  methods CUSTOMIZE_CATALOG
    redefinition .
  methods DISPLAY_ALV
    redefinition .
  methods GET_LAYOUT
    redefinition .
  methods HOTSPOT_CLICK
    redefinition .
  methods USER_COMMAND
    redefinition .
  methods BUTTON_CLICK
    redefinition .
  PRIVATE SECTION.

    DATA mt_data TYPE /qaps/t_matriz_item."/QAPS/T_MATRIZ_ABAST_HEADER .
ENDCLASS.



CLASS /QAPS/CL_VIEW_MATRIZ_HEADER IMPLEMENTATION.


  METHOD button_click.

    DATA ls_data TYPE /qaps/s_matriz_item.

    DATA(ls_line) = mt_data[ es_row_no-row_id ].
    ls_data = CORRESPONDING #( ls_line ).

    RAISE EVENT on_button_click
      EXPORTING
        is_col_id = es_col_id
        is_row_no = es_row_no
        ir_data   = REF #( ls_data )
    .

  ENDMETHOD.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE datatype = 'RAW'.

*    ICON
    DELETE ct_catalog WHERE fieldname = 'MANDT'
        OR fieldname = 'ID_MATRIZ_ABAST'
        OR fieldname = 'ID_SIMULACAO'
        OR fieldname = 'ID_ITEM'
        OR fieldname = 'SEM_DESTINO'
        OR fieldname = 'ORD'
        OR fieldname = 'MATNR'
        OR fieldname = 'ID_GRUPO_PRODUTO'
        OR fieldname = 'MAT_PLANEJADO'
        OR fieldname = 'AGREGADOR'
        OR fieldname = 'BTN_LINK'
        OR fieldname = 'CREATED_BY'
        OR fieldname = 'CREATED_IN'
        OR fieldname = 'CREATED_ON'
        OR fieldname = 'MODIFIED_BY'
        OR fieldname = 'MODIFIED_IN'
        OR fieldname = 'MODIFIED_ON'
        OR fieldname = 'MESSAGE_TYPE'
        OR fieldname = 'MESSAGE'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'ICON'.
          <fs>-col_pos = 1.
          <fs>-icon = 'X'.
          <fs>-outputlen = '5'.
        WHEN 'TIPO_REGRA'.
          <fs>-col_pos = 2.
          <fs>-hotspot = 'X'.
          <fs>-coltext = 'Tipo Regra'.
          <fs>-outputlen = '8'.
        WHEN 'DSC_TIPO_REGRA'.
          <fs>-coltext = 'Dsc Tipo Regra'.
          <fs>-col_pos = 3.
          <fs>-outputlen = '15'.
        WHEN 'KEY_INPUT'.
          <fs>-col_pos = 4.
          <fs>-hotspot = 'X'.
          <fs>-coltext = 'Material'.
          <fs>-outputlen = 30.
        WHEN 'DSC_INPUT'.
          <fs>-col_pos = 5.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = 45.
*        WHEN 'BTN_LINK'.
*          <fs>-col_pos = 6.
*          <fs>-outputlen = 8.
*          <fs>-coltext = 'Vínculo'.
*          <fs>-tooltip = 'Vincular/Desvincular'.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.


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


  METHOD get_layout.
    return = super->get_layout( ).
    return-grid_title = 'Grupo Produto / Material'.
    return-sel_mode = 'A'.
    return-no_toolbar = 'X'.
  ENDMETHOD.


  METHOD hotspot_click.

    DATA lr_data TYPE REF TO data.

    lr_data = REF #( mt_data[ e_row_id-index ] ).

    RAISE EVENT on_hotspot_click
      EXPORTING
        iv_source    = 'CUSTO_ELEMENTAR'
        is_row_id    = e_row_id
        is_column_id = e_column_id
        is_row_no    = es_row_no
        ir_data      = lr_data.

  ENDMETHOD.


  METHOD reset.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_data.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_matriz_item."/qaps/t_matriz_abast_header.
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


  METHOD set_grid_title.

    mo_alv->set_gridtitle( i_gridtitle = `Grupo Produto / Material - ` && iv_title ).

  ENDMETHOD.


  METHOD user_command.

    DATA: lt_data TYPE /qaps/t_matriz_item,
          lr_data TYPE REF TO data.

    mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

    LOOP AT lt_selected INTO DATA(ls_selected).
      APPEND mt_data[ ls_selected-index ] TO lt_data.
    ENDLOOP.

    IF lines( lt_data ) > 0.
      lr_data = REF #( lt_data ).
      DATA(lv_xml_data) = /qaps/cl_serialization=>serialize( lr_data ).
    ENDIF.

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm  = e_ucomm
        iv_source = 'CUSTO_ELEMENTAR'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
