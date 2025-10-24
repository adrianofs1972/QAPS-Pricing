class /QAPS/CL_VIEW_LISTA_AREA definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods SET_DATA
    redefinition .
  methods RESET
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
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MT_DATA type /QAPS/T_AREA .
ENDCLASS.



CLASS /QAPS/CL_VIEW_LISTA_AREA IMPLEMENTATION.


  METHOD CUSTOMIZE_CATALOG.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
                     OR fieldname = 'STYLE'
                     OR fieldname = 'COLOR'
                     OR fieldname = 'ORD'
                     OR fieldname = 'MESSAGE'
                     OR fieldname = 'ID_AREA'
                     OR fieldname = 'CREATED_BY'
                     OR fieldname = 'CREATED_IN'
                     OR fieldname = 'CREATED_ON'
                     OR fieldname = 'MODIFIED_BY'
                     OR fieldname = 'MODIFIED_IN'
                     OR fieldname = 'MODIFIED_ON'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-fieldname.
*        WHEN 'ID_AREA'.
*          <fs>-hotspot = 'X'.
*          <fs>-outputlen = 10.
        WHEN 'DESCRICAO'.
          <fs>-outputlen = 30.
          <fs>-hotspot = 'X'.
        WHEN 'ATIVO'.
          <fs>-checkbox = 'X'.
          <fs>-outputlen = 8.
        WHEN OTHERS.
          IF <fs>-outputlen < 10.
            <fs>-outputlen = 10.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.

*    FIELD-SYMBOLS <fs> TYPE /qaps/t_area.
*
**    ASSIGN mr_outtab->* TO FIELD-SYMBOL(<fs>).
*    ASSIGN mr_outtab->* TO <fs>.
*    BREAK-POINT.

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

    return = super->get_layout( ).
    return-grid_title = 'Área'.
    return-sel_mode = 'A'.

  endmethod.


  METHOD hotspot_click.

    RAISE EVENT on_hotspot_click
      EXPORTING
        iv_source    = 'LISTA_AREA'
        is_row_id    = e_row_id
        is_column_id = e_column_id
        is_row_no    = es_row_no
        ir_data      = ref #( mt_data[ e_row_id-index ] ).

  ENDMETHOD.


  METHOD reset.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  method SET_DATA.
    FIELD-SYMBOLS <fs> type /qaps/t_area.
    mr_outtab = ir_outtab.

    ASSIGN mr_outtab->* to <fs>.
    mt_data = <fs>.

    IF iv_soft_refresh = abap_true.
      mo_alv->refresh_table_display(
        EXPORTING
          is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
          i_soft_refresh = abap_true
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
    ELSE.
      mo_alv->refresh_table_display( ).
    ENDIF.

  endmethod.


  METHOD toolbar.

    REFRESH e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ADD'
        icon      = icon_insert_row
        quickinfo = 'Criar Área'
        text      = 'Criar Área'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&REMOVE'
        icon      = icon_delete_row
        quickinfo = 'Excluir Área'
        text      = 'Excluir Área'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&&SEP00'
        butn_type = 3
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ACTIVE'
        icon      = icon_activate
        quickinfo = 'Ativar Área'
        text      = 'Ativar Área'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&INACTIVE'
        icon      = icon_deactivate
        quickinfo = 'Desativar Área'
        text      = 'Desativar Área'
    ) TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD user_command.

    DATA: lt_data TYPE /qaps/t_area,
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
        iv_source = 'LISTA_AREA'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
