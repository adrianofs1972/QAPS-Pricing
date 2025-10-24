class /QAPS/CL_VIEW_STD_PRD_CP definition
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

  data MT_DATA type /QAPS/T_STD_PRODUCAO_CP .
ENDCLASS.



CLASS /QAPS/CL_VIEW_STD_PRD_CP IMPLEMENTATION.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                     OR fieldname = 'MESSAGE'
                     OR fieldname = 'STYLE'
                     OR fieldname = 'COLOR'
                     OR fieldname = 'ID_STD_PROD_PA'
                     OR fieldname = 'ID_STD_PROD_CP'
                     OR fieldname = 'ID_STD_PRODUCAO'
                     OR fieldname = 'RENDIMENTO'
                     OR fieldname = 'CREATED_BY'
                     OR fieldname = 'CREATED_IN'
                     OR fieldname = 'CREATED_ON'
                     OR fieldname = 'MODIFIED_BY'
                     OR fieldname = 'MODIFIED_IN'
                     OR fieldname = 'MODIFIED_ON'.


    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-fieldname.
        WHEN 'COMPONENTE'.
          <fs>-col_pos = 1.
          <fs>-coltext = 'Componente'.
        WHEN 'DSC_MAKTX'.
          <fs>-col_pos = 2.
        WHEN 'MENGE'.
          <fs>-col_pos = 3.
*          <fs>-coltext = 'Teor Real'.
        WHEN 'MEINS'.
          <fs>-col_pos = 4.
*        WHEN 'RENDIMENTO'.
*          <fs>-col_pos = 5.
*          <fs>-outputlen = 12.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


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

  endmethod.


  METHOD reset.

    REFRESH mt_data.
    mo_alv->set_gridtitle( i_gridtitle = '' ).
    mo_alv->refresh_table_display( ).

  ENDMETHOD.


  METHOD set_data.

    FIELD-SYMBOLS <fs> TYPE /qaps/t_std_producao_cp.
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


  method TOOLBAR.

    REFRESH e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ADD'
        icon      = icon_insert_row
        quickinfo = 'Criar Componente'
        text      = 'Criar Componente'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&EDITAR'
        icon      = icon_edit_file
        quickinfo = 'Editar Material'
        text      = 'Editar Material'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&REMOVE'
        icon      = icon_delete_row
        quickinfo = 'Excluir Material'
        text      = 'Excluir Material'
    ) TO e_object->mt_toolbar.


  endmethod.


  METHOD USER_COMMAND.

    DATA lr_data TYPE REF TO data.
    DATA lt_data TYPE /qaps/t_std_producao_cp.
    DATA lr_index TYPE RANGE OF lvc_index.
*    FIELD-SYMBOLS  <ft> TYPE /qaps/t_std_producao_item.

    IF e_ucomm = '&EDITAR' OR e_ucomm = '&REMOVE'.

      mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

      IF lines( lt_selected ) = 0.
        MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lines( lt_selected ) > 1 AND e_ucomm = '&EDITAR'.
        MESSAGE 'Selecione apenas um item para edição' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lr_index = VALUE #( FOR wa IN lt_selected
                           ( sign  = 'I'  option = 'EQ' low = wa-index ) ).

*      ASSIGN mr_outtab->* TO <ft>.

      LOOP AT mt_data INTO DATA(ls_data).
        CHECK sy-tabix IN lr_index.
        APPEND ls_data TO lt_data.
      ENDLOOP.

      lr_data = REF #( lt_data ).
      DATA(lv_xml_data) = /qaps/cl_serialization=>serialize( lr_data ).

    ENDIF.

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm    = e_ucomm
        iv_source   = 'CP'
        iv_action   = CONV #( e_ucomm )
        iv_xml_data = lv_xml_data
    .
  ENDMETHOD.
ENDCLASS.
