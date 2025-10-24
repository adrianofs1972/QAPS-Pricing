class /QAPS/CL_VIEW_LISTA_APROV definition
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
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MT_DATA type /QAPS/T_LISTA_APROV .
ENDCLASS.



CLASS /QAPS/CL_VIEW_LISTA_APROV IMPLEMENTATION.


  METHOD CUSTOMIZE_CATALOG.

     DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                     OR fieldname = 'ID_TP_LISTA'
                     OR fieldname = 'ID_AREA'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-fieldname.
        WHEN 'UNAME'.
          <fs>-outputlen = 12.
        WHEN 'FULLNAME'.
          <fs>-outputlen = 40.
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
    return-grid_title = 'Aprovadores'.
    return-sel_mode = 'A'.
  endmethod.


  METHOD reset.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_data.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_lista_aprov.
    mr_outtab = ir_outtab.

    ASSIGN mr_outtab->* TO <fs>.
    mt_data = <fs>.

    SORT <fs> BY uname ASCENDING.

    mo_alv->refresh_table_display(
      EXPORTING
        is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  ENDMETHOD.


  method TOOLBAR.

    REFRESH e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ADD'
        icon      = icon_insert_row
        quickinfo = 'Criar Aprovador'
        text      = 'Criar Aprovador'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&REMOVE'
        icon      = icon_delete_row
        quickinfo = 'Excluir Aprovador'
        text      = 'Excluir Aprovador'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&&SEP00'
        butn_type = 3
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ACTIVE'
        icon      = icon_activate
        quickinfo = 'Ativar Aprovador'
        text      = 'Ativar Aprovador'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&INACTIVE'
        icon      = icon_deactivate
        quickinfo = 'Desativar Aprovador'
        text      = 'Desativar Aprovador'
    ) TO e_object->mt_toolbar.

  endmethod.


  method USER_COMMAND.

    DATA: lt_data TYPE /qaps/t_lista_aprov,
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
        iv_source = 'LISTA_APROV'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  endmethod.
ENDCLASS.
