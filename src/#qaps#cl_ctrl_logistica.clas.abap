CLASS /qaps/cl_ctrl_logistica DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    METHODS initialize
      IMPORTING
        !io_container TYPE REF TO cl_gui_container .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ts_container,
        tree     TYPE REF TO cl_gui_container,
        cadastro TYPE REF TO cl_gui_container,
      END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
        tree     TYPE abap_bool,
        cadastro TYPE abap_bool,
        detail   TYPE abap_bool,
      END OF ts_update_controls .

  data MS_TRAJETO type /QAPS/S_TRAJETO .
  data MV_PREVIOUS_SOURCE type STRING .
  data MV_PREVIOUS_SHOW_DETAIL type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data MV_TEXT type STRING .
  data MO_MODEL type ref to /QAPS/CL_MDL_LOGISTICA .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_TREE type ref to /QAPS/CL_VIEW_TREE_LOGISTICA .
  data MO_LOGISTICA type ref to /QAPS/CL_VIEW_LOGISTICA .
  data MT_MATERIAL type /QAPS/T_MATERIAL .
  data MV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_DETAIL type ref to /QAPS/CL_VIEW_LOGISTIC_DETAIL .

  methods SET_CONTAINER_OBJECTS
    importing
      !IV_SHOW_ALV_DETAIL type ABAP_BOOL default ABAP_FALSE
      !IT_DATA type ref to DATA .
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
  methods ON_USER_COMMAND_ACTIVATE
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
      !IV_XML_DATA
      !IV_SOURCE
      !IV_GUID
      !IV_TEXTO
      !IV_ADDITIONAL_DATA .
  methods UPDATE_DETAIL
    importing
      !IV_SOURCE type STRING .
  methods UPDATE_CADASTROS
    importing
      !IV_SOURCE type STRING .
  methods UPDATE_TREE .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_SOURCE type STRING optional .
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
      value(RETURN) type /QAPS/CL_CTRL_LOGISTICA=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_LOGISTICA IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_logistica( iv_action = 'C' ).
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


  METHOD get_containers.

    "treview
    return-tree = io_splitter->get_container( row       = 1    " Row
                                              column    = 1     ).

    return-cadastro = io_splitter->get_container( row       = 1   " Row
                                                  column    = 2     ).

  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    mo_splitter_parent = create_main_container( io_container  ).
    initialize_componentes( mo_splitter_parent ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( tree = abap_true
                                               cadastro = abap_true )
                      iv_source = 'REGIAO' ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_tree = NEW /qaps/cl_view_tree_logistica( ).
    SET HANDLER: on_node_double_click FOR mo_tree.
    mo_tree->initialize( io_container = ls_containers-tree
                                       iv_root_text = 'Logística'
                                       iv_toolbar = abap_false ).

*    "Html - Tipo Lista
*    mo_view_grp_produto_hdr = NEW /qaps/cl_view_grp_produto_hdr( ).
*    mo_view_grp_produto_hdr->initialize( ls_containers-grp_produto_hdr ).

    "Material
    DATA lt_initial TYPE /qaps/t_regiao.
    mo_logistica = NEW /qaps/cl_view_logistica( ).
    SET HANDLER: on_user_command FOR mo_logistica,
                 on_hotspot_click FOR mo_logistica.
    mo_logistica->initialize( ir_outtab            = REF #( lt_initial )
                                  io_container         = ls_containers-cadastro
                                  is_catalog_structure = VALUE #( ) "'/QAPS/S_MATERIAL'
                                  iv_action = 'C' ).


  ENDMETHOD.


  METHOD on_function_selected.

    DATA ls_data TYPE /qaps/s_grupo_produto.
    DATA lr_data TYPE REF TO data.

*    CASE iv_function.
*      WHEN 'INSERT'.
*        DATA(lv_id_grupo_produto) = mo_model->create_grupo_produto( is_data = VALUE #( ) ).
*        IF NOT lv_id_grupo_produto IS INITIAL.
*          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true )
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
*          update( EXPORTING is_controls   = VALUE #( grupo_produto = abap_true ) ).
*          refresh( EXPORTING is_controls   = VALUE #( grp_produto_hdr = abap_true
*                                                      material = abap_true ) ).
*        ENDIF.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD on_hotspot_click.

    FIELD-SYMBOLS <fs_trajeto> TYPE /qaps/s_trajeto.

    CASE iv_source.
      WHEN 'TRAJETO'.
*        BREAK-POINT.
        ASSIGN ir_data->* TO <fs_trajeto>.
        ms_trajeto = <fs_trajeto>.
        mo_detail->set_trajeto( ms_trajeto ).
        DATA(lt_data) = mo_model->get_trechos_by_trajeto( <fs_trajeto> ).
        mo_detail->set_data( ir_outtab = ref #( lt_data )
                             iv_source = conv #( iv_source ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_node_double_click.

    mv_text = iv_additional_data.

    update( EXPORTING is_controls   = VALUE #( cadastro  = abap_true )
                      iv_source = iv_source ).

  ENDMETHOD.


  METHOD on_user_command.

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
      WHEN '&ACTIVE' OR '&INACTIVE'.
        on_user_command_activate( EXPORTING iv_ucomm  = iv_ucomm
                                            iv_source = iv_source
                                            iv_action = iv_action
                                            iv_xml_data = iv_xml_data ).



    ENDCASE.

  ENDMETHOD.


  METHOD on_user_command_activate.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    DATA: lt_centro_porto      TYPE /qaps/t_centro_porto.

    CASE iv_source.

      WHEN 'CENTRO_PORTO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_centro_porto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        CASE iv_ucomm.
          WHEN '&ACTIVE'.
            lv_return = mo_model->active_centro_porto( lt_centro_porto ).
          WHEN '&INACTIVE'.
            lv_return = mo_model->deactive_centro_porto( lt_centro_porto ).
        ENDCASE.
        ls_update_controls-cadastro = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING iv_source     = CONV #( iv_source )
                      is_controls   = ls_update_controls ).


  ENDMETHOD.


  METHOD on_user_command_create.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'REGIAO'.
        lv_return = mo_model->create_regiao( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CIDADE'.
        lv_return = mo_model->create_cidade( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'GRP_PLANTA'.
        lv_return = mo_model->create_grp_planta( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CENTRO'.
        lv_return = mo_model->create_centro( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'PORTO'.
        lv_return = mo_model->create_porto( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CAIS'.
        lv_return = mo_model->create_cais( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'GRP_CLIENTE'.
        lv_return = mo_model->create_grp_cliente( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CLIENTE'.
        lv_return = mo_model->create_cliente( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'FORNECEDOR'.
        lv_return = mo_model->create_fornecedor( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRECHO'.
        lv_return = mo_model->create_trecho( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRAJETO'.
        lv_return = mo_model->create_trajeto( ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRECHO_BY_TRAJETO'.
        lv_return = mo_model->create_trecho_by_trajeto( ms_trajeto  ).
        ls_update_controls-detail = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING iv_source     = CONV #( iv_source )
                      is_controls   = ls_update_controls ).

  ENDMETHOD.


  METHOD on_user_command_edit.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'REGIAO'.
        DATA lt_regiao TYPE /qaps/t_regiao.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_regiao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->edit_regiao( lt_regiao[ 1 ] ).
      WHEN 'CIDADE'.
        DATA lt_cidade TYPE /qaps/t_cidade.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cidade ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_cidade( lt_cidade[ 1 ] ).
      WHEN 'GRP_PLANTA'.
        DATA lt_grp_planta TYPE /qaps/t_grp_planta.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_grp_planta ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_grp_planta( lt_grp_planta[ 1 ] ).
      WHEN 'CENTRO'.
        DATA lt_centro TYPE /qaps/t_centro.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_centro ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_centro( lt_centro[ 1 ] ).
      WHEN 'PORTO'.
        DATA lt_porto TYPE /qaps/t_porto.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_porto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_porto( lt_porto[ 1 ] ).
      WHEN 'CAIS'.
        DATA lt_cais TYPE /qaps/t_cais.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cais ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_cais( lt_cais[ 1 ] ).
      WHEN 'GRP_CLIENTE'.
        DATA lt_grp_cliente TYPE /qaps/t_grp_cliente.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_grp_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_grp_cliente( lt_grp_cliente[ 1 ] ).
      WHEN 'CLIENTE'.
        DATA lt_cliente TYPE /qaps/t_cliente.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_cliente( lt_cliente[ 1 ] ).
      WHEN 'FORNECEDOR'.
        DATA lt_fornecedor TYPE /qaps/t_fornecedor.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_fornecedor ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_fornecedor( lt_fornecedor[ 1 ] ).
      WHEN 'TRAJETO'.
        DATA lt_trajeto TYPE /qaps/t_trajeto.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_trajeto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->edit_trajeto( lt_trajeto[ 1 ] ).

    ENDCASE.

    CHECK lv_return = abap_true.

    ls_update_controls-cadastro = abap_true.

    update( EXPORTING is_controls = ls_update_controls
                      iv_source   = CONV #( iv_source ) ).

  ENDMETHOD.


  METHOD on_user_command_remove.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    DATA: lt_regiao         TYPE /qaps/t_regiao,
          lt_cidade         TYPE /qaps/t_cidade,
          lt_grp_planta     TYPE /qaps/t_grp_planta,
          lt_centro         TYPE /qaps/t_centro,
          lt_porto          TYPE /qaps/t_porto,
          lt_cais           TYPE /qaps/t_cais,
          lt_grp_cliente    TYPE /qaps/t_grp_cliente,
          lt_cliente        TYPE /qaps/t_cliente,
          lt_fornecedor     TYPE /qaps/t_fornecedor,
          lt_trajeto        TYPE /qaps/t_trajeto,
          lt_trecho         TYPE /qaps/t_trecho,
          lt_trajeto_trecho TYPE /qaps/t_trajeto_trecho.

    CASE iv_source.
      WHEN 'REGIAO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_regiao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_regiao( lt_regiao ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CIDADE'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cidade ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_cidade( lt_cidade ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'GRP_PLANTA'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_grp_planta ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_grp_planta( lt_grp_planta ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CENTRO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_centro ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_centro( lt_centro ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'PORTO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_porto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_porto( lt_porto ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CAIS'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cais ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_cais( lt_cais ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'GRP_CLIENTE'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_grp_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_grp_cliente( lt_grp_cliente ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'CLIENTE'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_cliente( lt_cliente ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'FORNECEDOR'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_fornecedor ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_fornecedor( lt_fornecedor ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRAJETO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_trajeto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_trajeto( lt_trajeto  ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRECHO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_trecho ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_trecho( lt_trecho ).
        ls_update_controls-cadastro = abap_true.
      WHEN 'TRECHO_BY_TRAJETO'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_trajeto_trecho ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_trecho_by_trajeto( lt_trajeto_trecho ).
        ls_update_controls-detail = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING iv_source     = CONV #( iv_source )
                      is_controls   = ls_update_controls ).

*    DATA: ls_update_controls TYPE ts_update_controls,
*          lr_data            TYPE REF TO data.
*
*    CASE iv_source.
*      WHEN 'MATERIAL'.
*        DATA lt_materiais TYPE /qaps/t_material.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_materiais ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*        DATA(lv_return) = mo_model->delete_materiais( lt_materiais ).
*        ls_update_controls-material = abap_true.
*
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update(
*      EXPORTING
*        is_controls   = ls_update_controls
*        iv_id_grupo_produto = mv_id_grupo_produto
*        ir_data       = lr_data
*    ).

  ENDMETHOD.


  METHOD refresh.

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


  METHOD set_container_objects.

    FREE: mo_splitter_child,
          mo_logistica.
*    BREAK-POINT.

    mo_splitter_parent->remove_control( row               = 1
                                        column            =  2  ).
    DATA(lo_plitter_container) = mo_splitter_parent->get_container(
                                 row       = 1
                                 column    = 2
                             ).

    IF iv_show_alv_detail = abap_false.

      mo_splitter_child = NEW cl_gui_splitter_container(
                                            parent                  = lo_plitter_container
                                            rows                    = 1
                                            columns                 = 1 ).

    ELSE.

      mo_splitter_child = NEW cl_gui_splitter_container(
                                            parent                  = lo_plitter_container
                                            rows                    = 2
                                            columns                 = 1 ).

      mo_splitter_child->set_row_height(
        EXPORTING
          id                = 2
          height            = 30  ).

    ENDIF.

    DATA(lo_cont_1) = mo_splitter_child->get_container( row       = 1
                                                        column    = 1 ).

    mo_logistica = NEW /qaps/cl_view_logistica( ).
    SET HANDLER: on_user_command FOR mo_logistica,
                 on_hotspot_click FOR mo_logistica.
    mo_logistica->initialize( ir_outtab            = it_data
                              io_container         = lo_cont_1
                              is_catalog_structure = VALUE #( ) "'/QAPS/S_MATERIAL'
                              iv_action = 'C' ).

    IF iv_show_alv_detail = abap_true.

      DATA(lo_cont_2) = mo_splitter_child->get_container( row       = 2
                                                          column    = 1 ).

      mo_detail = NEW /qaps/cl_view_logistic_detail( ).
      SET HANDLER: on_user_command FOR mo_detail.

      DATA lt_data TYPE /qaps/t_trajeto_trecho.
      mo_detail->initialize( ir_outtab            = REF #( lt_data )
                             io_container         = lo_cont_2
                             is_catalog_structure = '/QAPS/S_TRAJETO_TRECHO'
                             iv_action = 'C' ).
    ENDIF.

  ENDMETHOD.


  METHOD update.

    IF is_controls-tree = abap_true.
      update_tree( ).
    ENDIF.
*
*    IF is_controls-grp_produto_hdr = abap_true.
*      update_grp_produto_header( iv_id_grupo_produto = iv_id_grupo_produto ).
*    ENDIF.
*
    IF is_controls-cadastro = abap_true.
      update_cadastros( iv_source ).
    ENDIF.

    IF is_controls-detail = abap_true.
      update_detail( iv_source ).
    ENDIF.

  ENDMETHOD.


  METHOD update_cadastros.
    DATA: lv_werks TYPE string,
          lv_texto TYPE string.

    DATA: lr_data        TYPE REF TO data,
          lv_show_detail TYPE abap_bool.

    CASE iv_source.
      WHEN 'TRAJETO'.
        lv_show_detail = abap_true.
      WHEN OTHERS.
        lv_show_detail = abap_false.
    ENDCASE.

    CASE iv_source.
      WHEN 'REGIAO'.
        DATA(lt_regiao) = mo_model->get_regioes( iv_force_update = abap_true ).
        lr_data = REF #( lt_regiao ).
      WHEN 'CIDADE'.
        DATA(lt_cidade) = mo_model->get_cidades( iv_force_update = abap_true ).
        lr_data = REF #( lt_cidade ).
      WHEN 'GRP_PLANTA'.
        DATA(lt_grp_planta) = mo_model->get_grp_planta( iv_force_update = abap_true ).
        lr_data = REF #( lt_grp_planta ).
      WHEN 'CENTRO'.
        DATA(lt_centro) = mo_model->get_centros( iv_force_update = abap_true ).
        lr_data = REF #( lt_centro ).
      WHEN 'PORTO'.
        DATA(lt_porto) = mo_model->get_portos( iv_force_update = abap_true ).
        lr_data = REF #( lt_porto ).
      WHEN 'CAIS'.
        DATA(lt_cais) = mo_model->get_cais( iv_force_update = abap_true ).
        lr_data = REF #( lt_cais ).
      WHEN 'GRP_CLIENTE'.
        DATA(lt_grp_cliente) = mo_model->get_grp_cliente( iv_force_update = abap_true ).
        lr_data = REF #( lt_grp_cliente ).
      WHEN 'CLIENTE'.
        DATA(lt_cliente) = mo_model->get_clientes( iv_force_update = abap_true ).
        lr_data = REF #( lt_cliente ).
      WHEN 'FORNECEDOR'.
        DATA(lt_fornecedor) = mo_model->get_fornecedor( iv_force_update = abap_true ).
        lr_data = REF #( lt_fornecedor ).
      WHEN 'CENTRO_PORTO'.
        SPLIT mv_text AT '-' INTO lv_werks lv_texto.
        mo_model->initialize_centro_porto( ).
        DATA(lt_centro_porto) = mo_model->get_centro_porto( iv_werks = CONV #( lv_werks )
                                                            iv_update = abap_true ).
        lr_data = REF #( lt_centro_porto ).
      WHEN 'TRECHO'.
        DATA(lt_trecho) = mo_model->get_trechos( ).
        lr_data = REF #( lt_trecho ).
      WHEN 'TRAJETO'.
        DATA(lt_trajeto) = mo_model->get_trajetos( ).
        lr_data = REF #( lt_trajeto ).
    ENDCASE.

    IF mv_previous_show_detail <> lv_show_detail.
      CASE iv_source.
        WHEN 'TRAJETO'.
          set_container_objects( iv_show_alv_detail = abap_true
                                 it_data            = lr_data ).
        WHEN OTHERS.
          set_container_objects( iv_show_alv_detail = abap_false
                                 it_data            = lr_data ).
      ENDCASE.

    ENDIF.

    mv_previous_show_detail = lv_show_detail.

    CHECK NOT lr_data IS INITIAL.

    mo_logistica->set_data( ir_outtab =  lr_data
                            iv_source = iv_source ).

    CHECK lv_show_detail = abap_true.

    DATA: lt_det_trajeto TYPE /qaps/t_trajeto_trecho,
          lr_data_detail TYPE REF TO data.

    CASE iv_source.
      WHEN 'TRAJETO'.
        lr_data_detail = REF #( lt_det_trajeto ).
    ENDCASE.

    mo_detail->set_data( ir_outtab =  lr_data_detail
                         iv_source = iv_source ).


  ENDMETHOD.


  METHOD update_detail.

    CASE iv_source.
      WHEN 'TRAJETO' OR 'TRECHO_BY_TRAJETO'.
        DATA(lt_data) = mo_model->get_trechos_by_trajeto( ms_trajeto ).
        mo_detail->set_trajeto( ms_trajeto ).
        mo_detail->set_data( ir_outtab = REF #( lt_data )
                             iv_source = 'TRAJETO' ).
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD update_tree.

    DATA lr_data TYPE REF TO data.

    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
    DATA(lt_centro) = lo_logistica->get_centros( ).

    mo_tree->set_planta_menu_distribuicao( lt_centro ).
    mo_tree->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
