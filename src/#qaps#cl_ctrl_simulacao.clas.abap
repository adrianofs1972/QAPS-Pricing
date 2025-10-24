class /QAPS/CL_CTRL_SIMULACAO definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      tipo_lista     TYPE REF TO cl_gui_container,
      tipo_lista_hdr TYPE REF TO cl_gui_container,
      simulacao      TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      tipo_lista     TYPE abap_bool,
      tipo_lista_hdr TYPE abap_bool,
      simulacao      TYPE abap_bool,
    END OF ts_update_controls .

  data MO_MODEL type ref to /QAPS/CL_MDL_SIMULACAO .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_TIPO_LISTA_HDR type ref to /QAPS/CL_VIEW_TIPO_LISTA_HDR .
  data MO_VIEW_TIPO_LISTA type ref to /QAPS/CL_VIEW_TP_LIS_CST_ELEM .
  data MO_VIEW_SIMULACAO type ref to /QAPS/CL_VIEW_SIMULACAO .
  data MT_SIMULACAO type /QAPS/T_SIMULACAO .
  data MV_ID_TP_LISTA type /QAPS/ED_TP_LISTA .
  data MV_INITIALIZED type ABAP_BOOL .

  methods ON_MENU_BUTTON
    for event ON_MENU_BUTTON of /QAPS/CL_VIEW_ALV_BASE
    importing
      !E_OBJECT
      !E_UCOMM .
  methods REFRESH
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS .
  methods ON_HOTSPOT_CLICK
    for event ON_HOTSPOT_CLICK of /QAPS/CL_VIEW_ALV_BASE
    importing
      !IV_SOURCE
      !IS_ROW_ID
      !IS_COLUMN_ID
      !IS_ROW_NO
      !IR_DATA .
  methods ON_USER_COMMAND_ASSIGN
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_EDIT
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_COPY
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_UNASSIGN
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_REMOVE
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_CREATE
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1 .
  methods ON_USER_COMMAND
    for event ON_USER_COMMAND of /QAPS/CL_VIEW_ALV_BASE
    importing
      !IV_UCOMM
      !IV_SOURCE
      !IV_ACTION
      !IV_XML_DATA .
  methods ON_NODE_DOUBLE_CLICK
    for event ON_NODE_DOUBLE_CLICK of /QAPS/CL_VIEW_SIMPLE_TREE_BASE
    importing
      !IV_NODE_KEY
      !IV_XML_DATA .
  methods UPDATE_SIMULACAO
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE_TIPO_LISTA_HEADER
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
      !IR_DATA type ref to DATA optional .
  methods ON_FUNCTION_SELECTED
    for event ON_FUNCTION_SELECTED of /QAPS/CL_VIEW_SIMPLE_TREE_BASE
    importing
      !IV_FUNCTION
      !IV_XML_DATA .
  methods CREATE_MAIN_CONTAINER
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER
    returning
      value(RETURN) type ref to CL_GUI_SPLITTER_CONTAINER .
  methods INITIALIZE_COMPONENTES
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  methods GET_CONTAINERS
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER
    returning
      value(RETURN) type /QAPS/CL_CTRL_SIMULACAO=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_SIMULACAO IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_simulacao( iv_action = 'C' ).
  ENDMETHOD.


  METHOD CREATE_MAIN_CONTAINER.

    mo_splitter_parent = NEW cl_gui_splitter_container(
*        link_dynnr              =
*        link_repid              =
*        shellstyle              =
*        left                    =
*        top                     =
*        width                   =
*        height                  =
*        metric                  = CNTL_METRIC_DYNPRO
*        align                   = 15
           parent                  = io_container
           rows                    = 1
           columns                 = 2
*        no_autodef_progid_dynnr =
*        name                    =
       ).


    mo_splitter_parent->set_column_width( id = 1 width = 15 ).
    mo_splitter_parent->set_border(
      EXPORTING
        border            = ''    " Draw Frame (gfw_true); Do Not Draw Frame (gfw_false)
*      EXCEPTIONS
*        cntl_error        = 1
*        cntl_system_error = 2
*        others            = 3
    ).


    return = mo_splitter_parent.


  ENDMETHOD.


  METHOD GET_CONTAINERS.

    "treview
    return-tipo_lista = io_splitter->get_container( row       = 1    " Row
                                                    column    = 1     ).

    DATA(lo_parent_child) = io_splitter->get_container( row       = 1    " Row
                                                        column    = 2     ).

    mo_splitter_child = NEW cl_gui_splitter_container(
        parent                  = lo_parent_child
        rows                    = 2
        columns                 = 1 ).

    mo_splitter_child->set_row_height( id = 1 height = 14 ).

    "Html Header
    return-tipo_lista_hdr = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    return-simulacao = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).

  ENDMETHOD.


  METHOD INITIALIZE.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_tipo_lista = NEW /qaps/cl_view_tp_lis_cst_elem( ).
    SET HANDLER: on_node_double_click FOR mo_view_tipo_lista.
    mo_view_tipo_lista->initialize( io_container = ls_containers-tipo_lista
                                       iv_root_text = 'Tipo de Lista'
                                       iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_tipo_lista_hdr = NEW /qaps/cl_view_tipo_lista_hdr( ).
    mo_view_tipo_lista_hdr->initialize( ls_containers-tipo_lista_hdr ).

    "Material
    mo_view_simulacao = NEW /qaps/cl_view_simulacao( ).
    SET HANDLER: on_user_command FOR mo_view_simulacao.

    mo_view_simulacao->initialize( ir_outtab            = REF #( mt_simulacao )
                                         io_container         = ls_containers-simulacao
                                         is_catalog_structure = '/QAPS/S_SIMULACAO'
                                         iv_action = 'C' ).


  ENDMETHOD.


  METHOD ON_FUNCTION_SELECTED.

*    DATA ls_data TYPE /qaps/s_grupo_produto.
*    DATA lr_data TYPE REF TO data.
*
*    CASE iv_function.
*      WHEN 'INSERT'.
*        DATA(lv_id_grupo_produto) = mo_model->create_grupo_produto( is_data = VALUE #( ) ).
*        IF NOT lv_id_grupo_produto IS INITIAL.
*          update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true )
*                            iv_id_grupo_produto = lv_id_grupo_produto ).
*        ENDIF.
*      WHEN 'REMOVE'.
*
*        lr_data =  REF #( ls_data ).
*        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                             CHANGING  cr_data = lr_data ).
*
*        DATA(lv_return) = mo_model->delete_grupo_produto( ls_data ).
*
*        IF lv_return = abap_true.
*          update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true ) ).
*          refresh( EXPORTING is_controls   = VALUE #( tipo_lista_hdr = abap_true
*                                                      input = abap_true ) ).
*        ENDIF.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD ON_HOTSPOT_CLICK.

    FIELD-SYMBOLS <fs_area> TYPE /qaps/s_area.

    CASE iv_source.
      WHEN 'LISTA_AREA'.

*        ASSIGN ir_data->* TO <fs_area>.
*
*        update( EXPORTING is_controls   = VALUE #( area_user      = abap_true )
*                          iv_tipo_lista = mv_id_tp_lista
**                          iv_id_area    = mv_id_area
*                          ir_data       = ir_data ).
*
*        mv_id_area = <fs_area>-id_area.

    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD ON_NODE_DOUBLE_CLICK.
    update( EXPORTING is_controls   = VALUE #( tipo_lista     = abap_false
                                               tipo_lista_hdr = abap_true
                                               simulacao    = abap_true

                                              )

                      iv_id_tipo_lista = CONV #( iv_node_key ) ).

    mv_id_tp_lista = iv_node_key.

  ENDMETHOD.


  METHOD on_user_command.

    CASE iv_ucomm.
      WHEN '&ADD'.
        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action ).
      WHEN '&ADD_COPY'.
        on_user_command_copy( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
      WHEN '&EDIT'.
        on_user_command_edit( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
      WHEN '&REMOVE'.
        on_user_command_remove( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
      WHEN '&ASSIGN'.
        on_user_command_assign( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
      WHEN '&UNASSIGN'.
        on_user_command_unassign( EXPORTING iv_ucomm  = iv_ucomm
                                            iv_source = iv_source
                                            iv_action = iv_action
                                            iv_xml_data = iv_xml_data ).


    ENDCASE.

  ENDMETHOD.


  METHOD ON_USER_COMMAND_ASSIGN.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'SIMULACAO'.

        DATA lt_simulacao TYPE /qaps/t_simulacao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_simulacao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).

          DATA(ls_data) = lt_simulacao[ 1 ].
        ENDIF.

        lv_return = mo_model->assign_std_producao( iv_id_tp_lista = mv_id_tp_lista
                                                   is_data = ls_data ).
        ls_update_controls-simulacao = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).
  ENDMETHOD.


  METHOD on_user_command_copy.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'SIMULACAO'.
        DATA lt_simulacao TYPE /qaps/t_simulacao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_simulacao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).

          DATA(ls_data) = lt_simulacao[ 1 ].
        ENDIF.


        lv_return = mo_model->copy_simulacao( iv_id_tp_lista = mv_id_tp_lista
                                              is_data = ls_data ).
        ls_update_controls-simulacao = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_create.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'SIMULACAO'.
        lv_return = mo_model->create_simulacao( mv_id_tp_lista ).
        ls_update_controls-simulacao = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_edit.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'SIMULACAO'.

        DATA lt_simulacao TYPE /qaps/t_simulacao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_simulacao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).

          DATA(ls_data) = lt_simulacao[ 1 ].
        ENDIF.

        lv_return = mo_model->edit_simulacao( iv_id_tp_lista = mv_id_tp_lista
                                              is_data = ls_data ).
        ls_update_controls-simulacao = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).
  ENDMETHOD.


  METHOD on_user_command_remove.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'SIMULACAO'.
        DATA lt_simulacao TYPE /qaps/t_simulacao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_simulacao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->delete_simulacao( lt_simulacao ).
        ls_update_controls-simulacao = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_UNASSIGN.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'SIMULACAO'.
        DATA lt_simulacao TYPE /qaps/t_simulacao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_simulacao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->UNASSIGN_STD_PRODUCAO( lt_simulacao ).
        ls_update_controls-simulacao = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_tipo_lista = mv_id_tp_lista
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD REFRESH.

    IF is_controls-tipo_lista = abap_true.
*      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
      mo_view_tipo_lista_hdr->refresh( ).
    ENDIF.

    IF is_controls-simulacao = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
      mo_view_simulacao->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD update.

    IF is_controls-tipo_lista = abap_true.
      update_tipo_lista( iv_id_tipo_lista ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
      update_tipo_lista_header( iv_id_tipo_lista ).
    ENDIF.

    IF is_controls-simulacao = abap_true.
      update_simulacao( iv_id_tipo_lista ).
    ENDIF.

  ENDMETHOD.


  METHOD update_simulacao.

    DATA(lt_data) = mo_model->get_simulacao( iv_id_tipo_lista ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_simulacao->set_data( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_TIPO_LISTA.

    DATA(lt_data) = mo_model->get_tipo_lista( ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_tipo_lista->update( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_TIPO_LISTA_HEADER.

    DATA(lt_data) = mo_model->get_tipo_lista( iv_id_tipo_lista ).

    IF lines( lt_data ) > 0.
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_tipo_lista_hdr->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
