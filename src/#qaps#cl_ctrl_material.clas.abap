class /QAPS/CL_CTRL_MATERIAL definition
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
      grupo_produto   TYPE REF TO cl_gui_container,
      grp_produto_hdr TYPE REF TO cl_gui_container,
      material        TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      grupo_produto   TYPE abap_bool,
      grp_produto_hdr TYPE abap_bool,
      material        TYPE abap_bool,
    END OF ts_update_controls .

  data MO_MODEL type ref to /QAPS/CL_MDL_MATERIAL .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_GRP_PRODUTO_HDR type ref to /QAPS/CL_VIEW_GRP_PRODUTO_HDR .
  data MO_VIEW_GRUPO_PRODUTO type ref to /QAPS/CL_VIEW_SIMP_GRP_PRODUTO .
  data MO_VIEW_MATERIAL type ref to /QAPS/CL_VIEW_MATERIAL .
  data MT_MATERIAL type /QAPS/T_MATERIAL .
  data MV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO .
  data MV_INITIALIZED type ABAP_BOOL .

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
  methods ON_USER_COMMAND_CATEG_TRANSP
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
  methods UPDATE_MATERIAIS
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO optional .
  methods UPDATE_GRP_PRODUTO_HEADER
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO optional .
  methods UPDATE_GRUPO_PRODUTO
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO optional .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO optional
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
      value(RETURN) type /QAPS/CL_CTRL_MATERIAL=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_MATERIAL IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_material( iv_action = 'C' ).
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


    mo_splitter_parent->set_column_width( id = 1 width = 20 ).
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
    return-grupo_produto = io_splitter->get_container( row       = 1    " Row
                                                    column    = 1     ).

    data(lo_parent_child) = io_splitter->get_container( row       = 1    " Row
                                                        column    = 2     ).

    mo_splitter_child = new cl_gui_splitter_container(
        parent                  = lo_parent_child
        rows                    = 2
        columns                 = 1 ).

    mo_splitter_child->set_row_height( id = 1 height = 8 ).

    "Html Header
    return-grp_produto_hdr = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    return-material = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).


  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_grupo_produto = NEW /qaps/cl_view_simp_grp_produto( ).
    SET HANDLER: on_function_selected FOR mo_view_grupo_produto,
                 on_node_double_click FOR mo_view_grupo_produto.
    mo_view_grupo_produto->initialize( io_container = ls_containers-grupo_produto
                                       iv_root_text = 'Grupo de Produtos'
                                       iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_grp_produto_hdr = NEW /qaps/cl_view_grp_produto_hdr( ).
    mo_view_grp_produto_hdr->initialize( ls_containers-grp_produto_hdr ).

    "Material
    mo_view_material = NEW /qaps/cl_view_material( ).
    SET HANDLER on_user_command FOR mo_view_material.
    mo_view_material->initialize( ir_outtab            = REF #( mt_material )
                                  io_container         = ls_containers-material
                                  is_catalog_structure = '/QAPS/S_MATERIAL'
                                  iv_action = 'C' ).


  ENDMETHOD.


  METHOD on_function_selected.

    DATA ls_data TYPE /qaps/s_grupo_produto.
    DATA lr_data TYPE REF TO data.

    CASE iv_function.
      WHEN 'INSERT'.
        DATA(lv_id_grupo_produto) = mo_model->create_grupo_produto( is_data = VALUE #( ) ).
        IF NOT lv_id_grupo_produto IS INITIAL.
          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true )
                            iv_id_grupo_produto = lv_id_grupo_produto ).
        ENDIF.
      WHEN 'REMOVE'.

        lr_data =  REF #( ls_data ).
        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                             CHANGING  cr_data = lr_data ).

        DATA(lv_return) = mo_model->delete_grupo_produto( ls_data ).

        IF lv_return = abap_true.
          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true ) ).
          refresh( EXPORTING is_controls   = VALUE #( grp_produto_hdr = abap_true
                                                      material = abap_true ) ).
        ENDIF.

    ENDCASE.

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


  METHOD on_node_double_click.

    data: lr_data type ref to data,
          ls_data type /qaps/s_grupo_produto.

    lr_data = ref #( ls_data ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    update( EXPORTING is_controls   = VALUE #( grupo_produto     = abap_false
                                               grp_produto_hdr = abap_true
                                               material    = abap_true )
                      iv_id_grupo_produto = ls_data-id_grupo_produto ).

    mv_id_grupo_produto = ls_data-id_grupo_produto.

  ENDMETHOD.


  METHOD on_user_command.

    CASE iv_ucomm.
      WHEN '&ADD'.
        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action ).
      WHEN '&REMOVE'.
        on_user_command_remove( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
      WHEN '&ASSIGN' OR '&UNASSIGN'.
        on_user_command_categ_transp( EXPORTING iv_ucomm  = iv_ucomm
                                                  iv_source = iv_source
                                                  iv_action = iv_action
                                                  iv_xml_data = iv_xml_data ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_user_command_categ_transp.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'MATERIAL'.
        DATA lt_materiais TYPE /qaps/t_material.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_materiais ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        IF iv_ucomm = '&ASSIGN'.
          DATA(lv_return) = mo_model->vincular_categ_transporte( lt_materiais ).
        ELSEIF iv_ucomm = '&UNASSIGN'.
          lv_return = mo_model->desvincular_categ_transporte( lt_materiais ).
        ENDIF.
        ls_update_controls-material = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_grupo_produto = mv_id_grupo_produto
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_create.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'MATERIAL'.
        lv_return = mo_model->create_materiais( mv_id_grupo_produto ).
        ls_update_controls-material = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_grupo_produto = mv_id_grupo_produto
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_remove.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'MATERIAL'.
        DATA lt_materiais TYPE /qaps/t_material.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_materiais ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->delete_materiais( lt_materiais ).
        ls_update_controls-material = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_id_grupo_produto = mv_id_grupo_produto
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD refresh.

    IF is_controls-grupo_produto = abap_true.
*      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-grp_produto_hdr = abap_true.
      mo_view_grp_produto_hdr->refresh( ).
    ENDIF.

    IF is_controls-material = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
      mo_view_material->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD update.

    IF is_controls-grupo_produto = abap_true.
      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-grp_produto_hdr = abap_true.
      update_grp_produto_header( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-material = abap_true.
      update_materiais( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

  ENDMETHOD.


  METHOD update_grp_produto_header.

    DATA(lt_data) = mo_model->get_grupo_produto( iv_id_grupo_produto ).

    IF lines( lt_data ) > 0.
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_grp_produto_hdr->update( lr_data ).

  ENDMETHOD.


  METHOD update_grupo_produto.

    DATA(lt_data) = mo_model->get_grupo_produto( ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_grupo_produto->update( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_MATERIAIS.

    DATA(lt_data) = mo_model->get_materiais( iv_id_grupo_produto ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_material->set_data( lr_data ).

  ENDMETHOD.
ENDCLASS.
