class /QAPS/CL_CTRL_CUSTO_ELEMENTAR definition
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
      custo_elementar TYPE REF TO cl_gui_container,
      utilizacoes     TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      custo_elementar TYPE abap_bool,
      utilizacoes     TYPE abap_bool,
    END OF ts_update_controls .

  data MO_MODEL type ref to /QAPS/CL_MDL_CUSTO_ELEMENTAR .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_CUSTO_ELEMENTAR type ref to /QAPS/CL_VIEW_CUSTO_ELEMENTAR .
  data MO_VIEW_UTILIZACOES type ref to /QAPS/CL_VIEW_CS_EL_UTILIZACAO .
  data MV_INITIALIZED type ABAP_BOOL .
  data MT_CUSTO_ELEMENTAR type /QAPS/T_CUSTO_ELEMENTAR .

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
  methods ON_USER_COMMAND_EDIT
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
  methods UPDATE_VARIAVEL .
  methods UPDATE_UTILIZACOES
    importing
      !IV_ID_CUSTO_ELEMENTAR type /QAPS/ID_CUSTO_ELEMENTAR optional .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_ID_CUSTO_ELEMENTAR type /QAPS/ID_CUSTO_ELEMENTAR optional
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
      value(RETURN) type /QAPS/CL_CTRL_CUSTO_ELEMENTAR=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_CUSTO_ELEMENTAR IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_custo_elementar( iv_action = 'C' ).
  ENDMETHOD.


  METHOD create_main_container.

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
           rows                    = 2
           columns                 = 1
*        no_autodef_progid_dynnr =
*        name                    =
       ).


    mo_splitter_parent->set_row_height( id = 1 height = 70 ).
*    mo_splitter_parent->set_border(
*      EXPORTING
*        border            = ''    " Draw Frame (gfw_true); Do Not Draw Frame (gfw_false)
**      EXCEPTIONS
**        cntl_error        = 1
**        cntl_system_error = 2
**        others            = 3
*    ).


    return = mo_splitter_parent.


  ENDMETHOD.


  METHOD GET_CONTAINERS.


    return-custo_elementar = io_splitter->get_container( row       = 1    " Row
                                                         column    = 1     ).

    return-utilizacoes = io_splitter->get_container( row       = 2    " Row
                                                     column    = 1     ).

  ENDMETHOD.


  METHOD INITIALIZE.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( custo_elementar = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    "Custo Elementar
    mo_view_custo_elementar = NEW /qaps/cl_view_custo_elementar( ).
    SET HANDLER: on_user_command FOR mo_view_custo_elementar,
                 on_hotspot_click FOR mo_view_custo_elementar.
    mo_view_custo_elementar->initialize( ir_outtab            = REF #( mt_custo_elementar )
                                  io_container         = ls_containers-custo_elementar
                                  is_catalog_structure = '/QAPS/S_CUSTO_ELEMENTAR'
                                  iv_action = 'C' ).

    mo_view_utilizacoes = NEW /qaps/cl_view_cs_el_utilizacao( ).
    mo_view_utilizacoes->initialize( ir_outtab            = REF #( mt_custo_elementar )
                                    io_container         = ls_containers-utilizacoes
                                    is_catalog_structure = '/QAPS/S_CUSTO_ELEMENTAR'
                                    iv_action = 'C' ).


  ENDMETHOD.


  METHOD ON_FUNCTION_SELECTED.

    DATA ls_data TYPE /qaps/s_grupo_produto.
    DATA lr_data TYPE REF TO data.

    CASE iv_function.
      WHEN 'INSERT'.
*        DATA(lv_id_grupo_produto) = mo_model->create_grupo_produto( is_data = VALUE #( ) ).
*        IF NOT lv_id_grupo_produto IS INITIAL.
*          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true )
*                            iv_id_grupo_produto = lv_id_grupo_produto ).
*        ENDIF.
      WHEN 'REMOVE'.

*        lr_data =  REF #( ls_data ).
*        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                             CHANGING  cr_data = lr_data ).
*
*        DATA(lv_return) = mo_model->delete_grupo_produto( ls_data ).
*
*        IF lv_return = abap_true.
*          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true ) ).
*          refresh( EXPORTING is_controls   = VALUE #( grp_produto_hdr = abap_true
*                                                      material = abap_true ) ).
*        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD on_hotspot_click.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_custo_elementar.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.

        ASSIGN ir_data->* TO <fs>.

        update( EXPORTING is_controls   = VALUE #( utilizacoes      = abap_true )
                          iv_id_custo_elementar = <fs>-id_custo_elementar
*                          iv_id_area    = mv_id_area
*                          ir_data       = ir_data
                          ).

*        mv_id_area = <fs_area>-id_area.

    ENDCASE.

  ENDMETHOD.


  METHOD ON_NODE_DOUBLE_CLICK.
*    update( EXPORTING is_controls   = VALUE #( grupo_produto     = abap_false
*                                               grp_produto_hdr = abap_true
*                                               material    = abap_true
*
*                                              )
*
*                      iv_id_grupo_produto = CONV #( iv_node_key ) ).
*
*    mv_id_grupo_produto = iv_node_key.

  ENDMETHOD.


  METHOD ON_USER_COMMAND.

    CASE iv_ucomm.
      WHEN '&ADD'.
        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action ).
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


    ENDCASE.

  ENDMETHOD.


  METHOD ON_USER_COMMAND_CREATE.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.
        lv_return = mo_model->create_variavel( ).
        ls_update_controls-custo_elementar = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
*        iv_id_grupo_produto = mv_id_grupo_produto
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_edit.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.
        DATA lt_data TYPE /qaps/t_custo_elementar.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_data ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->edit_variavel( lt_data ).
        ls_update_controls-custo_elementar = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
*        iv_id_grupo_produto = mv_id_grupo_produto
*        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_remove.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.
        DATA lt_data TYPE /qaps/t_custo_elementar.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_data ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->delete_variavel( lt_data ).
        ls_update_controls-custo_elementar = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
*        iv_id_grupo_produto = mv_id_grupo_produto
*        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD REFRESH.

*    IF is_controls-grupo_produto = abap_true.
**      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
*    ENDIF.
*
*    IF is_controls-grp_produto_hdr = abap_true.
*      mo_view_grp_produto_hdr->refresh( ).
*    ENDIF.
*
*    IF is_controls-material = abap_true.
**      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
*      mo_view_material->reset( ).
*    ENDIF.

  ENDMETHOD.


  METHOD update.

    IF is_controls-custo_elementar = abap_true.
      update_variavel( ).
    ENDIF.

    IF is_controls-utilizacoes = abap_true.
      update_utilizacoes( iv_id_custo_elementar = iv_id_custo_elementar ).
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_UTILIZACOES.

*    DATA(lt_data) = mo_model->get_grupo_produto( ).
*    DATA(lr_data) = REF #( lt_data ).
*    mo_view_grupo_produto->update( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_VARIAVEL.

    DATA(lt_data) = mo_model->get_variavel( ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_custo_elementar->set_data( lr_data ).

  ENDMETHOD.
ENDCLASS.
