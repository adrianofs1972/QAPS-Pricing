class /QAPS/CL_CTRL_INPUT_CUSTO_CALC definition
  public
  final
  create public .

public section.

  methods SET_TIPO_LISTA
    importing
      !IS_DATA type /QAPS/S_TP_LISTA .
  methods SHOW_TREEVIEW .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods HIDE_TREEVIEW .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      tipo_lista     TYPE REF TO cl_gui_container,
      tipo_lista_hdr TYPE REF TO cl_gui_container,
      input          TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      tipo_lista     TYPE abap_bool,
      tipo_lista_hdr TYPE abap_bool,
      input          TYPE abap_bool,
    END OF ts_update_controls .

  data MS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR .
  data MS_TIPO_LISTA type /QAPS/S_TP_LISTA .
  data MT_CUSTO_ELEMENTAR type /QAPS/T_CUSTO_ELEMENTAR .
  data MO_MODEL type ref to /QAPS/CL_MDL_INPUT_CUSTO_CALC .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_TIPO_LISTA_HDR type ref to /QAPS/CL_VIEW_TIPO_LISTA_HDR .
  data MO_VIEW_EXPRESSAO type ref to /QAPS/CL_VIEW_TREE_EXPRESSAO .
  data MO_VIEW_INPUT type ref to /QAPS/CL_VIEW_INPUT_VARIABLE .
  data MV_INITIALIZED type ABAP_BOOL .

  methods ON_CHANGE_PARENT
    for event ON_CHANGE_PARENT of /QAPS/CL_VIEW_TREE_EXPRESSAO
    importing
      !IV_ID_NEW_PARENT
      !IV_XML_DATA .
  methods ON_NODE_REMOVE
    for event ON_NODE_REMOVE of /QAPS/CL_VIEW_TREE_EXPRESSAO
    importing
      !IV_XML_DATA
      !IV_ACTION
      !IV_TIPO
      !IV_FCODE .
  methods ON_SUBMIT_CHANGE_MODALIDADE
    for event ON_SUBMIT_CHANGE_MODALIDADE of /QAPS/CL_VIEW_INPUT_VARIABLE
    importing
      !IV_ACTION
      !IS_DATA .
  methods ON_SUBMIT_EXPRESSAO
    for event ON_SUBMIT_EXPRESSAO of /QAPS/CL_VIEW_INPUT_VARIABLE
    importing
      !IV_ACTION
      !IS_DATA
      !IT_PARAMETERS .
  methods ON_NODE_ADDED
    for event ON_NODE_ADDED of /QAPS/CL_VIEW_TREE_EXPRESSAO
    importing
      !IV_XML_DATA
      !IV_ACTION
      !IV_TIPO .
  methods ON_DATA_CHANGED_FINISHED
    for event ON_DATA_CHANGED_FINISHED of /QAPS/CL_VIEW_INPUT_CST_ELEM
    importing
      !IT_CHANGED_DATA .
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
  methods UPDATE_INPUT
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA optional
      !IR_DATA type ref to DATA .
  methods UPDATE_TIPO_LISTA_HEADER
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE_TIPO_LISTA
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA optional
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
      value(RETURN) type /QAPS/CL_CTRL_INPUT_CUSTO_CALC=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_INPUT_CUSTO_CALC IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_input_custo_calc( iv_action = 'C' ).
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


    mo_splitter_parent->set_column_width( id = 1 width = 25 ).
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

    mo_splitter_child->set_row_height( id = 1 height = 8 ).

    "Html Header
    return-tipo_lista_hdr = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    return-input = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).


  ENDMETHOD.


  METHOD HIDE_TREEVIEW.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 0   ).

  ENDMETHOD.


  METHOD INITIALIZE.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true
                                               tipo_lista_hdr = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_expressao = NEW /qaps/cl_view_tree_expressao( ).
    SET HANDLER: on_node_double_click FOR mo_view_expressao,
                 on_function_selected FOR mo_view_expressao,
                 on_node_added        FOR mo_view_expressao,
                 on_node_remove       FOR mo_view_expressao,
                 on_change_parent     FOR mo_view_expressao.
    mo_view_expressao->initialize( io_container = ls_containers-tipo_lista
                                   iv_root_text = 'Expressão'
                                   iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_tipo_lista_hdr = NEW /qaps/cl_view_tipo_lista_hdr( ).
    mo_view_tipo_lista_hdr->initialize( ls_containers-tipo_lista_hdr ).

*    "Input
    mo_view_input = NEW /qaps/cl_view_input_variable( ).
    SET HANDLER: on_submit_expressao FOR mo_view_input,
                 on_submit_change_modalidade  FOR mo_view_input .

    mo_view_input->initialize(
      EXPORTING
        io_container = ls_containers-input
        iv_title     = 'Variáveis'
    ).


  ENDMETHOD.


  METHOD on_change_parent.

    DATA: ls_data TYPE /qaps/calc_node,
          lr_data TYPE REF TO data.

    lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    DATA(lv_return) = mo_model->change_parent( is_data = ls_data
                                               iv_id_new_parent = iv_id_new_parent ).

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls    = VALUE ts_update_controls(
        tipo_lista     = 'X'
*        tipo_lista_hdr =
*        input          =
    )
*        iv_id_tp_lista =     " QAPS: Id Tipo de Lista Pricing
*        ir_data        =
    ).

  ENDMETHOD.


  METHOD ON_DATA_CHANGED_FINISHED.

*    mo_model->update_input( it_changed_data ).
*
*    MESSAGE 'Dados atualizados com sucesso' TYPE 'S'.

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


  METHOD on_hotspot_click.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_custo_elementar.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.

        ASSIGN ir_data->* TO <fs>.
        ms_custo_elementar = <fs>.

        update( EXPORTING is_controls   = VALUE #( input      = abap_true )
                          iv_id_tp_lista = ms_tipo_lista-id_tp_lista
                          ir_data       = ir_data ).



    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD on_node_added.

    DATA lr_data TYPE REF TO data.
    DATA ls_data TYPE /qaps/s_calculation_node.

    CHECK NOT iv_xml_data IS INITIAL.

    lr_data = REF #( ls_data ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data  ).

    CASE iv_tipo.
      WHEN 'CALCULADA'.
        DATA(ls_new) = mo_model->create_variavel_calculada( is_parent = ls_data ).
      WHEN 'ELEMENTAR'.
        ls_new = mo_model->assign_variavel_elementar( is_parent = ls_data ).
    ENDCASE.

    CHECK NOT ls_new IS INITIAL.
    lr_data = REF #( ls_new ).

    mo_view_expressao->add_new_child_node( lr_data ).

  ENDMETHOD.


  METHOD on_node_double_click.

    DATA: lr_data TYPE REF TO data,
          ls_node TYPE /qaps/s_calculation_node.

    lr_data = REF #( ls_node ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    update( EXPORTING is_controls   = VALUE #( tipo_lista     = abap_false
                                               tipo_lista_hdr = abap_true
                                               input         = abap_true )
                      iv_id_tp_lista = CONV #( iv_node_key )
                      ir_data = lr_data ).

  ENDMETHOD.


  METHOD on_node_remove.

    DATA: ls_data TYPE /qaps/calc_node,
          lr_data TYPE REF TO data.

    lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    CASE iv_fcode.
      WHEN 'EXCLUIR'.
        DATA(lv_return) = mo_model->excluir_node_calculado( ls_data ).
      WHEN 'DESVINCULAR'.
        lv_return = mo_model->desvicular_node_elementar( ls_data ).
    ENDCASE.

    CHECK lv_return = abap_true.

    mo_view_expressao->remove_node( lr_data ).

  ENDMETHOD.


  METHOD on_submit_change_modalidade.

    mo_model->update_node( is_data = is_data
                           iv_action = iv_action ).

    mo_view_expressao->update_node_content( is_data ).

  ENDMETHOD.


  METHOD on_submit_expressao.

    CASE iv_action.
      WHEN 'SAVE'.
        mo_model->save_expressao( is_data       = is_data
                                  it_parameters = it_parameters ).
        mo_view_expressao->update_node_content( is_data ).
      WHEN 'CHECK' .
        mo_model->check_expressao( is_data       = is_data
                                   it_parameters = it_parameters ).
    ENDCASE.

  ENDMETHOD.


  METHOD ON_USER_COMMAND.

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


    ENDCASE.

  ENDMETHOD.


  METHOD ON_USER_COMMAND_CREATE.

*    DATA ls_update_controls TYPE ts_update_controls.
*    DATA lr_data TYPE REF TO data.
*    DATA lv_return TYPE abap_bool.
*
*    DATA(ls_periodo) = VALUE /qaps/s_periodo_interval( inicial = ms_simulacao-periodo_inicial
*                                                      final   = ms_simulacao-periodo_final ).
*    mo_model->set_periodo( is_periodo = ls_periodo ).
*
*    CASE iv_source.
*      WHEN 'CUSTO_ELEMENTAR'.
*        lv_return = mo_model->create_input( is_simulacao = ms_simulacao
*                                            is_data = ms_custo_elementar  ).
*        ls_update_controls-input = abap_true.
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update(
*      EXPORTING
*        is_controls   = ls_update_controls
*        iv_id_simulacao = ms_simulacao-id_simulacao ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_REMOVE.
*    DATA: ls_update_controls TYPE ts_update_controls,
*          lr_data            TYPE REF TO data,
*          lt_selected        TYPE /qaps/t_var_input_selected.
*
*    CASE iv_source.
*      WHEN 'CUSTO_ELEMENTAR'.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        DATA(lv_return) = mo_model->delete_input( lt_selected ).
*          ls_update_controls-input = abap_true.
*
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update(
*      EXPORTING
*        is_controls   = ls_update_controls
**        iv_id_grupo_produto = mv_id_grupo_produto
**        ir_data       = lr_data
*    ).

  ENDMETHOD.


  METHOD REFRESH.

    IF is_controls-tipo_lista = abap_true.
*      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
      mo_view_tipo_lista_hdr->refresh( ).
    ENDIF.

    IF is_controls-input = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
*      mo_view_input->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_TIPO_LISTA.
    ms_tipo_lista = is_data.
  ENDMETHOD.


  METHOD SHOW_TREEVIEW.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 18   ).

  ENDMETHOD.


  METHOD update.

    IF is_controls-tipo_lista = abap_true.
      update_tipo_lista( iv_id_tp_lista ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
      update_tipo_lista_header( iv_id_tp_lista ).
    ENDIF.

    IF is_controls-input = abap_true.
      update_input( iv_id_tp_lista = iv_id_tp_lista
                    ir_data = ir_data ).
    ENDIF.

  ENDMETHOD.


  METHOD update_input.

    FIELD-SYMBOLS <fs> type /qaps/s_calculation_node.
    assign ir_data->* to <fs>.

    if <fs>-tipo_node = 'C'.
      data(lt_nodes) = mo_model->get_child_nodes( iv_id_tp_lista   = <fs>-id_tp_lista
                                                  iv_id_parent_node = <fs>-id_calc_node ).
    endif.

    mo_view_input->set_child_nodes( lt_nodes ).
    mo_view_input->update( ir_data = ir_data ).

  ENDMETHOD.


  METHOD UPDATE_TIPO_LISTA.

    DATA(lt_data) = mo_model->get_nodes( iv_id_tp_lista = ms_tipo_lista-id_tp_lista ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_expressao->update( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_TIPO_LISTA_HEADER.

*    DATA(lt_data) = mo_model->get_simulacao( ms_simulacao-id_simulacao ).
    DATA(lt_data) = mo_model->get_tipo_lista( ms_tipo_lista-id_tp_lista ).

    IF lines( lt_data ) > 0.
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_tipo_lista_hdr->set_full_header( abap_false ).
    mo_view_tipo_lista_hdr->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
