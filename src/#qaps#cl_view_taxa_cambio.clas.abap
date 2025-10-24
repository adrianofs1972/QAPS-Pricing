class /QAPS/CL_VIEW_TAXA_CAMBIO definition
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
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MT_DATA type /QAPS/T_TAXA_CAMBIO_VERSAO .
ENDCLASS.



CLASS /QAPS/CL_VIEW_TAXA_CAMBIO IMPLEMENTATION.


  METHOD CUSTOMIZE_CATALOG.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                     OR fieldname = 'MESSAGE'
                     OR fieldname = 'STYLE'
                     OR fieldname = 'COLOR'
                     OR fieldname = 'MOEDA_LOCAL'
                     OR fieldname = 'MOEDA_FINAL'
                     OR fieldname = 'ID_TAXA_CAMBIO'
                     OR fieldname = 'ID_FONTE'
                     OR fieldname = 'ID_STD_PRODUCAO'
                     OR fieldname = 'CREATED_BY'
                     OR fieldname = 'CREATED_IN'
                     OR fieldname = 'CREATED_ON'
                     OR fieldname = 'MODIFIED_BY'
                     OR fieldname = 'MODIFIED_IN'
                     OR fieldname = 'MODIFIED_ON'.


    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-fieldname.
        WHEN 'DATA'.
          <fs>-col_pos = 1.
        WHEN 'HORA'.
          <fs>-col_pos = 2.
        WHEN 'VERSAO'.
          <fs>-col_pos = 3.
          <fs>-outputlen = '10'.
        WHEN 'PERIODO'.
          <fs>-col_pos = 4.
        WHEN 'TAXA'.
          <fs>-col_pos = 5.
        WHEN 'ATIVO'.
          <fs>-col_pos = 6.
          <fs>-outputlen = '10'.
          <fs>-checkbox = 'X'.
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


  METHOD set_data.

    FIELD-SYMBOLS <fs> TYPE /qaps/t_taxa_cambio_versao.
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
        function  = '&ATIVAR'
        icon      = icon_activate
        quickinfo = 'Ativar Taxa'
        text      = 'Ativar Taxa' ) TO e_object->mt_toolbar.



*    APPEND VALUE stb_button(
*        function  = '&REMOVE'
*        icon      = icon_delete_row
*        quickinfo = 'Excluir Cotação'
*        text      = 'Excluir Cotação'
*    ) TO e_object->mt_toolbar.


  endmethod.


  METHOD user_command.

    DATA lr_data TYPE REF TO data.
    DATA lt_data TYPE /qaps/t_taxa_cambio_versao.
    DATA lr_index TYPE RANGE OF lvc_index.
*    FIELD-SYMBOLS  <ft> TYPE /qaps/t_std_producao_item.

    IF e_ucomm = '&ATIVAR'.

      mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

      IF lines( lt_selected ) = 0.
        MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lines( lt_selected ) > 1 AND e_ucomm = '&ATIVAR'.
        MESSAGE 'Selecione apenas um item para ativação' TYPE 'S' DISPLAY LIKE 'E'.
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
        iv_source   = 'TAXA_CAMBIO'
        iv_action   = CONV #( e_ucomm )
        iv_xml_data = lv_xml_data
    .
  ENDMETHOD.
ENDCLASS.
