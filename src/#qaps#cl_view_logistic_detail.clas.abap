class /QAPS/CL_VIEW_LOGISTIC_DETAIL definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods REFRESH .
  methods SET_TRAJETO
    importing
      !IS_TRAJETO type /QAPS/S_TRAJETO .

  methods INITIALIZE
    redefinition .
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
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MS_SERIALIZABLE type STRING .
  data MS_TRAJETO type /QAPS/S_TRAJETO .
  data MT_DATA type /QAPS/T_REGIAO .

  methods CUSTOMIZE_CAT_TRAJETO
    changing
      !CT_CATALOG type LVC_T_FCAT .
ENDCLASS.



CLASS /QAPS/CL_VIEW_LOGISTIC_DETAIL IMPLEMENTATION.


  METHOD CUSTOMIZE_CATALOG.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                       OR fieldname = 'STYLE'
                       OR fieldname = 'COLOR'
                       OR fieldname = 'MESSAGE'
                       OR fieldname = 'CREATED_BY'
                       OR fieldname = 'CREATED_IN'
                       OR fieldname = 'CREATED_ON'
                       OR fieldname = 'MODIFIED_BY'
                       OR fieldname = 'MODIFIED_IN'
                       OR fieldname = 'MODIFIED_ON'.

    CASE mv_source.
      WHEN 'TRAJETO'.  customize_cat_trajeto( CHANGING ct_catalog = ct_catalog ).
    ENDCASE.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_TRAJETO.

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
                      OR fieldname = 'ID_TRAJ_TRECHO'
                      OR fieldname = 'ID_TRECHO'
                      OR fieldname = 'ID_ORIGEM'
                      OR fieldname = 'ID_DESTINO'
                      OR fieldname = 'ID_MODAL'
                      OR fieldname = 'TIPO_ORIGEM'
                      OR fieldname = 'TIPO_DESTINO'
                      OR fieldname = 'ID_TRAJETO'.
*    BREAK-POINT.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'ORDEM'.
          <fs>-col_pos = 1.
          <fs>-coltext = 'Ordem'.
          <fs>-outputlen = 6.
          <fs>-hotspot = 'X'.
        WHEN 'COD_TRECHO'.
          <fs>-col_pos = 2.
          <fs>-coltext = 'Trecho'.
          <fs>-outputlen = 6.
          <fs>-hotspot = 'X'.
        WHEN 'DSC_TIPO_ORIGEM'.
          <fs>-col_pos = 3.
          <fs>-coltext = 'Tipo Origem'.
          <fs>-outputlen = 10.
        WHEN 'DSC_ORIGEM'.
          <fs>-col_pos = 4.
          <fs>-coltext = 'Cod Origem'.
          <fs>-outputlen = 10.
        WHEN 'ORIGEM'.
          <fs>-col_pos = 5.
          <fs>-coltext = 'Origem'.
          <fs>-outputlen = 22.
        WHEN 'DSC_TIPO_DESTINO'.
          <fs>-col_pos = 6.
          <fs>-coltext = 'Tipo Destino'.
          <fs>-outputlen = 10.
        WHEN 'DSC_DESTINO'.
          <fs>-col_pos = 7.
          <fs>-coltext = 'Cod Destino'.
          <fs>-outputlen = 10.
        WHEN 'DESTINO'.
          <fs>-col_pos = 8.
          <fs>-coltext = 'Destino'.
          <fs>-outputlen = 22.
        WHEN 'DSC_MODAL'.
          <fs>-col_pos = 9.
          <fs>-coltext = 'Modal'.
          <fs>-outputlen = 12.
        WHEN 'DESLOCAMENTO'.
          <fs>-col_pos = 10.
          <fs>-coltext = 'Deslocamento'.
          <fs>-outputlen = 12.
          <fs>-do_sum = 'X'.
        WHEN 'DISTANCIA'.
          <fs>-col_pos = 11.
          <fs>-coltext = 'Distância (km)'.
          <fs>-outputlen = 12.
          <fs>-do_sum = 'X'.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD DISPLAY_ALV.

    FIELD-SYMBOLS <fs> TYPE ANY TABLE.

    ASSIGN mr_outtab->* TO <fs>.

    ms_serializable = /qaps/cl_serialization=>serialize( ir_data = mr_outtab ).

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
        it_outtab       = <fs>"mt_data
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

    CASE mv_source.
      WHEN 'TRAJETO'.
        IF ms_trajeto IS INITIAL.
          return-grid_title = 'Trechos'.
        ELSE.
          return-grid_title = `Trechos - ` && ms_trajeto-codigo.
        ENDIF.
    ENDCASE.

    return-sel_mode = 'A'.
  ENDMETHOD.


  method INITIALIZE.

    mr_outtab = ir_outtab.
    mv_action = iv_action.

    "Cria instânica
    create_instance( io_container ).

*    "Catálogo de Campo
*    DATA(lt_catalog) = get_catalog( is_catalog_structure ).
*    customize_catalog( CHANGING ct_catalog = lt_catalog ).
*
*    "Layout
*    DATA(ls_layo) = get_layout( ).
*
*    "Sort
*    DATA(lt_sort) = get_sort( ).
*
*    "Eventos
*    set_events( ).
*
*    "Display ALV
*    display_alv( is_layout  = ls_layo
*                 it_catalog = lt_catalog
*                 it_sort    = lt_sort ).

  endmethod.


  METHOD REFRESH.

    mo_alv->refresh_table_display( ).

  ENDMETHOD.


  METHOD RESET.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD SET_DATA.

    mr_outtab = ir_outtab.
*    mv_action = iv_action.
    mv_source = iv_source.

*    "Cria instânica
*    create_instance( io_container ).

    set_content( ir_outtab ).

    DATA(lv_struct) = get_structure_name( ir_outtab ).

*    "Catálogo de Campo
    DATA(lt_catalog) = get_catalog( lv_struct ).
    customize_catalog( CHANGING ct_catalog = lt_catalog ).

    "Layout
    DATA(ls_layo) = get_layout( ).

    "Sort
    DATA(lt_sort) = get_sort( ).

    "Eventos
    set_events( ).

    "Display ALV
    display_alv( is_layout  = ls_layo
                 it_catalog = lt_catalog
                 it_sort    = lt_sort ).

  ENDMETHOD.


  METHOD set_trajeto.
    ms_trajeto = is_trajeto.
  ENDMETHOD.


  METHOD TOOLBAR.

*    BREAK c060863.
    CASE mv_source.

      WHEN 'CENTRO_PORTO'.
*        DATA(lt_toolbar) = e_object->mt_toolbar.
*        DELETE lt_toolbar WHERE function <> '&&SEP00'
*                          AND function <> '&SORT_ASC'
*                          AND function <> '&SORT_DSC'
*                          AND function <> '&MB_FILTER'.
        REFRESH e_object->mt_toolbar.
      WHEN OTHERS.
        REFRESH e_object->mt_toolbar.
    ENDCASE.



    DEFINE add_button.
      APPEND VALUE stb_button(
            function  = &1
            icon      = &2
            quickinfo = &3
            text      = &3

        ) TO e_object->mt_toolbar.
    END-OF-DEFINITION.

    CASE mv_source.

      WHEN 'TRAJETO'.
        add_button '&ADD' icon_create 'Novo Trecho'.
*        add_button '&EDIT' icon_change_text 'Editar'.
        add_button '&REMOVE' icon_delete 'Excluir Trecho'.
    ENDCASE.



  ENDMETHOD.


  METHOD user_command.

    DATA: lr_data     TYPE REF TO data,
          lv_xml_data TYPE string,
          lr_index    TYPE RANGE OF i.

    DATA: lt_trajeto_trecho     TYPE /qaps/t_trajeto_trecho,
          lt_trajeto_trecho_out TYPE /qaps/t_trajeto_trecho.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    IF e_ucomm = '&EDIT' OR e_ucomm = '&REMOVE' OR e_ucomm = '&ACTIVE'
        OR e_ucomm = '&INACTIVE'.

      mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

      IF lines( lt_selected ) = 0.
        MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lines( lt_selected ) > 1 AND e_ucomm = '&EDIT'.
        MESSAGE 'Selecione apenas 1 item para edição' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lr_index = VALUE #( FOR wa IN lt_selected
                          ( sign = 'I' option = 'EQ' low = wa-index ) ).

      CASE mv_source.
        WHEN 'TRAJETO'.
          lr_data = REF #( lt_trajeto_trecho ).
          get_content( CHANGING cr_data = lr_data ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_trajeto_trecho INTO DATA(ls_trecho).
            CHECK sy-tabix IN lr_index.
            APPEND ls_trecho TO lt_trajeto_trecho_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_trajeto_trecho_out ) ).
      ENDCASE.

    ENDIF.

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm  = e_ucomm
        iv_source = 'TRECHO_BY_TRAJETO'
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
