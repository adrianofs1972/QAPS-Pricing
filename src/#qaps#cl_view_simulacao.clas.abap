class /QAPS/CL_VIEW_SIMULACAO definition
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
  methods TOOLBAR
    redefinition .
private section.

  data MT_DATA type /QAPS/T_SIMULACAO .
ENDCLASS.



CLASS /QAPS/CL_VIEW_SIMULACAO IMPLEMENTATION.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                    OR fieldname = 'STYLE'
                    OR fieldname = 'COLOR'
                    OR fieldname = 'ORD'
                    OR fieldname = 'MANDT'
                    OR fieldname = 'MESSAGE'
                    OR fieldname = 'CREATED_BY'
                    OR fieldname = 'CREATED_IN'
                    OR fieldname = 'CREATED_ON'
                    OR fieldname = 'MODIFIED_BY'
                    OR fieldname = 'MODIFIED_IN'
                    OR fieldname = 'MODIFIED_ON'
                    OR fieldname = 'ID_TP_LISTA'
                    OR fieldname = 'ID_STD_PRODUCAO'
                    OR fieldname = 'STATUS'
                    OR fieldname = 'DSC_TP_LISTA'.


    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'ICON'.
          <fs>-col_pos = 1.
          <fs>-icon = 'X'.
          <fs>-outputlen = '5'.
        WHEN 'DSC_STATUS'.
          <fs>-col_pos = 2.
          <fs>-outputlen = '10'.
          <fs>-just = 'C'.
        WHEN 'ID_SIMULACAO'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '10'.
          <fs>-col_pos = 3.
        WHEN 'DESCRICAO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = '18'.
          <fs>-hotspot = 'X'.
        WHEN 'ID_ORIGINAL'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '12'.
          <fs>-col_pos = 5.
        WHEN 'DSC_REFERENCIA'.
          <fs>-col_pos = 6.
          <fs>-outputlen = '15'.
        WHEN 'PERIODO_INICIAL'.
          <fs>-col_pos = 7.
          <fs>-outputlen = '12'.
        WHEN 'PERIODO_FINAL'.
          <fs>-col_pos = 8.
          <fs>-outputlen = '12'.
        WHEN 'COD_STD_PRODUCAO'.
          <fs>-col_pos = 9.
          <fs>-outputlen = '10'.
        WHEN 'DSC_STD_PRODUCAO'.
          <fs>-col_pos = 10.
          <fs>-outputlen = '18'.

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


  METHOD GET_LAYOUT.
    return = super->get_layout( ).
    return-grid_title = 'Simulações'.
    return-sel_mode = 'A'.
*    return-no_toolbar = 'X'.
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
    FIELD-SYMBOLS <fs> TYPE /qaps/t_simulacao.
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


  METHOD toolbar.

    REFRESH e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ADD'
        icon      = icon_insert_row
        quickinfo = 'Criar Simulação'
        text      = 'Criar Simulação'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ADD_COPY'
        icon      = icon_copy_object
        quickinfo = 'Criar Simulação com cópia'
        text      = 'Criar Simulação com cópia'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&EDIT'
        icon      = icon_change_text
        quickinfo = 'Editar Simulação'
        text      = 'Editar Simulação'

    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&REMOVE'
        icon      = icon_delete_row
        quickinfo = 'Excluir Simulação'
        text      = 'Excluir Simulação'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&&SEP00'
        butn_type = 3
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&ASSIGN'
        icon      = icon_allow
        quickinfo = 'Vincular Std Produção'
        text      = 'Vincular Std Produção'
    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&UNASSIGN'
        icon      = icon_reject
        quickinfo = 'Desvincular Std Produção'
        text      = 'Desvincular Std Produção'
    ) TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD user_command.

    DATA: lt_data TYPE /qaps/t_simulacao,
          lr_data TYPE REF TO data.

    mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

    LOOP AT lt_selected INTO DATA(ls_selected).
      APPEND mt_data[ ls_selected-index ] TO lt_data.
    ENDLOOP.

    IF lines( lt_data ) > 0.
      lr_data = REF #( lt_data ).
      DATA(lv_xml_data) = /qaps/cl_serialization=>serialize( lr_data ).
    ENDIF.

    IF lines( lt_data ) = 0 AND ( e_ucomm = '&EDIT' OR e_ucomm = '&DELETE'
                                  OR e_ucomm = '&ASSIGN' OR e_ucomm = '&UNASSIGN' ).

      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lines( lt_data ) > 1 AND ( e_ucomm = '&EDIT' OR e_ucomm = '&ASSIGN' ).

      MESSAGE 'Selecione apenas 1 item' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm  = e_ucomm
        iv_source = 'SIMULACAO'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
