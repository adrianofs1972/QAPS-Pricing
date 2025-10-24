class /QAPS/CL_VIEW_CST_ELEM_INPUT definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods RESET
    redefinition .
  methods SET_DATA
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
  methods GET_SORT
    redefinition .
private section.

  data MT_DATA type /QAPS/T_CUSTO_ELEMENTAR .
ENDCLASS.



CLASS /QAPS/CL_VIEW_CST_ELEM_INPUT IMPLEMENTATION.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                    OR fieldname = 'STYLE'
                    OR fieldname = 'COLOR'
                    OR fieldname = 'ORD'
                    OR fieldname = 'MESSAGE'
                    OR fieldname = 'CREATED_BY'
                    OR fieldname = 'CREATED_IN'
                    OR fieldname = 'CREATED_ON'
                    OR fieldname = 'MODIFIED_BY'
                    OR fieldname = 'MODIFIED_IN'
                    OR fieldname = 'MODIFIED_ON'
                    OR fieldname = 'ID_CUSTO_ELEMENTAR'
                    OR fieldname = 'ID_TP_LISTA'
                    OR fieldname = 'ID_AREA'
                    OR fieldname = 'ESCOPO'
                    OR fieldname = 'TIPO_DADO'
                    OR fieldname = 'TIPO_VARIAVEL'
                    OR fieldname = 'ORIGEM_DADO'
                    OR fieldname = 'UTILIZACOES'.
*    break c060863.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'ICON'.
          <fs>-col_pos = 1.
          <fs>-icon = 'X'.
          <fs>-outputlen = '5'.
        WHEN 'DSC_TIPO_VARIAVEL'.
          <fs>-col_pos = 2.
          <fs>-outputlen = '17'.
        WHEN 'DESCRICAO'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '25'.
        WHEN 'DSC_ESCOPO'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_AREA'.
          <fs>-outputlen = '17'.
        WHEN 'DSC_TP_LISTA'.
          <fs>-outputlen = '17'.
        WHEN 'DSC_TIPO_DADO'.
          <fs>-outputlen = '17'.
        WHEN 'DSC_ORIGEM_DADO'.
          <fs>-outputlen = '12'.
        WHEN 'UTILIZACOES'.
          <fs>-outputlen = '10'.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD DISPLAY_ALV.


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
    return-grid_title = 'Variáveis de Custo Elementar'.
    return-sel_mode = 'A'.
    return-no_toolbar = 'X'.
  ENDMETHOD.


  METHOD get_sort.

    APPEND VALUE lvc_s_sort(
        spos       = '1'
        fieldname  = 'DSC_TIPO_VARIAVEL'
        up = 'X' ) TO return.

*    APPEND VALUE lvc_s_sort(
*        spos       = '2'
*        fieldname  = 'DESCRICAO'
*        up = 'X' ) TO return.


  ENDMETHOD.


  METHOD HOTSPOT_CLICK.

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


  METHOD RESET.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD SET_DATA.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_custo_elementar.
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


  METHOD USER_COMMAND.

    DATA: lt_data TYPE /qaps/t_custo_elementar,
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
