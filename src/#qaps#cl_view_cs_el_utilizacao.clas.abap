class /QAPS/CL_VIEW_CS_EL_UTILIZACAO definition
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
  methods USER_COMMAND
    redefinition .
private section.

  data MT_DATA type /QAPS/T_MATERIAL .
ENDCLASS.



CLASS /QAPS/CL_VIEW_CS_EL_UTILIZACAO IMPLEMENTATION.


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
                     OR fieldname = 'ID_CATEGORIA'
                     OR fieldname = 'ID_GRUPO_PRODUTO'
                     .

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'MATNR'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 12.
        WHEN 'DSC_MATNR'.
          <fs>-col_pos = 2.
          <fs>-outputlen = '20'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Desc Material'.
        WHEN 'MAT_PLANEJADO'.
          <fs>-col_pos = 3.
        WHEN 'DSC_MAT_PLANEJADO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = '20'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Desc Planejado'.
*        WHEN 'ID_CATEGORIA'.
*          <fs>-col_pos = 5.
        WHEN 'DSC_CATEGORIA'.
          <fs>-col_pos = 6.
          <fs>-outputlen = '18'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Categoria Transporte'.
        WHEN 'AGREGADOR'.
          <fs>-col_pos = 7.
          <fs>-outputlen = '18'.
        WHEN 'CLASSIF_PERIGOSA'.
          <fs>-col_pos = 8.
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
    return-grid_title = 'Utilizações'.
*    return-sel_mode = 'A'.
    return-no_toolbar = 'X'.
  ENDMETHOD.


  METHOD RESET.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD SET_DATA.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_material.
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

    DATA: lt_data TYPE /qaps/t_material,
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
        iv_source = 'MATERIAL'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
