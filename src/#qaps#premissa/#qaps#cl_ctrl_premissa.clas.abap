class /QAPS/CL_CTRL_PREMISSA definition
  public
  final
  create public .

public section.

  methods ADD_TRANSFER .
  methods UPDATE_PREMISSA .
  methods SHOW_TREEVIEW .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods HIDE_TREEVIEW .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ts_container,
      simulacao     TYPE REF TO cl_gui_container,
      simulacao_hdr TYPE REF TO cl_gui_container,
      variaveis     TYPE REF TO cl_gui_container,
      input         TYPE REF TO cl_gui_container,
      trajeto       TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      simulacao     TYPE abap_bool,
      simulacao_hdr TYPE abap_bool,
      variaveis     TYPE abap_bool,
      input         TYPE abap_bool,
      trajeto       TYPE abap_bool,
    END OF ts_update_controls .

  data MS_ITEM type /QAPS/S_PREMISSA_ITEM .
  data MS_SIMULACAO type /QAPS/S_SIMULACAO .
  data MV_TEXTO type STRING .
  data MV_TIPO_DESTINO type /QAPS/ED_TIPO_DESTINO .
  data MV_ID_DESTINO type GUID16 .
  data MS_PREMISSA_HEADER type /QAPS/S_PREMISSA_HEADER .
  data MT_HEADER type /QAPS/T_PREMISSA_HEADER .
  data MO_MODEL type ref to /QAPS/CL_MDL_PREMISSA .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_HEADER type ref to /QAPS/CL_VIEW_PRM_SIMUL_HDR .
  data MO_TREE type ref to /QAPS/CL_VIEW_TREE_PRM_SIMUL .
  data MO_ITEM type ref to /QAPS/CL_VIEW_PREMISSA_HEADER .
  data MO_INPUT type ref to /QAPS/CL_VIEW_PREMISSA_INPUT .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_TRAJETO type ref to /QAPS/CL_VIEW_PREMISSA_TRAJETO .
  data MS_DISTRIB type /QAPS/S_PREMISSA_DISTRIB .

  methods RESET .
  methods ON_PREM_TRAJ_CHANGED_FINISHED
    for event ON_PREM_TRAJ_CHANGED_FINISHED of /QAPS/CL_VIEW_PREMISSA_TRAJETO
    importing
      !IT_CHANGED_DATA .
  methods ON_DATA_CHANGED_FINISHED
    for event ON_PREMISSA_CHANGED_FINISHED of /QAPS/CL_VIEW_PREMISSA_INPUT
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
  methods ON_USER_COMMAND_DEL_TRANSFER
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_ADD_TRANSFER
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1 .
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
  methods UPDATE_ITEM .
  methods UPDATE_TRAJETO .
  methods UPDATE_INPUT .
  methods UPDATE_HEADER .
  methods UPDATE_TREE .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IR_DATA type ref to DATA optional
      !IV_GUID type GUID16 optional .
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
      value(RETURN) type /QAPS/CL_CTRL_PREMISSA=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_PREMISSA IMPLEMENTATION.


  METHOD add_transfer.

    DATA ls_header TYPE /qaps/s_premissa_header.
    DATA lr_data TYPE REF TO data.

    IF ms_simulacao IS INITIAL.
      MESSAGE 'Selecionar uma simulação' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF ms_simulacao-STATUS <> 'A'.
      MESSAGE 'Transferência só é possível em simulação aberta' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_content) = mo_tree->get_selected_node( ).

    IF NOT lv_content IS INITIAL.

      lr_data = REF #( ls_header ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = lv_content
                                           CHANGING  cr_data = lr_data ).

      DATA(lt_premissa_header) = mo_model->get_header( iv_id_simulacao = ms_simulacao-id_simulacao ).

      DATA(ls_premissa_header) = lt_premissa_header[ id_premissa = ls_header-id_premissa ].

      ls_header-dsc_tipo_destino =  ls_premissa_header-dsc_tipo_ponto.
      ls_header-dsc_destino =  ls_premissa_header-codigo.

      DATA(lv_return) = mo_model->add_transfer( is_simulacao = ms_simulacao
                                                is_header    = ls_header ).

      CHECK lv_return = abap_true.

      update( EXPORTING is_controls = VALUE #( simulacao = 'X'
                                               variaveis = 'X'
                                               input = 'X' ) ).

    ELSE.

      lv_return = mo_model->add_transfer( is_simulacao = ms_simulacao
                                          is_header    = VALUE #( ) ).

      CHECK lv_return = abap_true.
      update( EXPORTING is_controls = VALUE #( variaveis = 'X'
                                               input = 'X' ) ).


    ENDIF.


  ENDMETHOD.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_premissa( ).
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


    mo_splitter_parent->set_column_width( id = 1 width = 18 ).
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
    return-simulacao = io_splitter->get_container( row       = 1    " Row
                                                    column    = 1     ).

    DATA(lo_parent_child) = io_splitter->get_container( row       = 1    " Row
                                                        column    = 2     ).

    mo_splitter_child = NEW cl_gui_splitter_container(
        parent                  = lo_parent_child
        rows                    = 4
        columns                 = 1 ).

    "Html Header
    mo_splitter_child->set_row_height( id = 1 height = 13 ).
    return-simulacao_hdr = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    "Materiais
    mo_splitter_child->set_row_height( id = 2 height = 36 ).
    return-variaveis = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).

    "Input
    return-input = mo_splitter_child->get_container( row       = 3    " Row
                                                               column    = 1     ).

    mo_splitter_child->set_row_height( id = 4 height = 20 ).
    return-trajeto = mo_splitter_child->get_container( row       = 4
                                                       column    = 1     ).


  ENDMETHOD.


  METHOD hide_treeview.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 0   ).

  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( simulacao = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_tree = NEW /qaps/cl_view_tree_prm_simul( ).
    SET HANDLER: on_node_double_click FOR mo_tree.
    mo_tree->initialize( io_container = ls_containers-simulacao
                                   iv_root_text = 'Simulações'
                                   iv_toolbar = abap_false ).

    "Html - Tipo Lista
    mo_header = NEW /qaps/cl_view_prm_simul_hdr( ).
    mo_header->initialize( ls_containers-simulacao_hdr ).

    "Header - Premissa
    mo_item = NEW /qaps/cl_view_premissa_header( ).
    SET HANDLER: on_hotspot_click FOR mo_item.
    mo_item->initialize( ir_outtab            = REF #( mt_header )
                                         io_container         = ls_containers-variaveis
                                         is_catalog_structure = '/QAPS/S_PREMISSA_ITEM'
                                         iv_action = 'C' ).

    "Input - Premissa
    mo_input = NEW /qaps/cl_view_premissa_input( ).
    SET HANDLER: on_user_command FOR mo_input,
                 on_data_changed_finished FOR mo_input,
                 on_hotspot_click FOR mo_input.

    DATA lt_data TYPE /qaps/t_premissa_distrib.

    mo_input->initialize( ir_outtab            = REF #( lt_data )
                          io_container         = ls_containers-input
                          is_catalog_structure = '/QAPS/S_PREMISSA_DISTRIB'
                          iv_action = 'C' ).

    "Trajeo - Premissa
    mo_trajeto = NEW /qaps/cl_view_premissa_trajeto( ).
    SET HANDLER: on_prem_traj_changed_finished FOR mo_trajeto.

*    DATA lt_data TYPE /qaps/t_premissa_distrib.

    mo_trajeto->initialize( ir_outtab            = REF #( lt_data )
                            io_container         = ls_containers-trajeto
                            is_catalog_structure = '/QAPS/S_PREMISSA_DISTRIB'
                            iv_action = 'C' ).


  ENDMETHOD.


  METHOD on_data_changed_finished.

    mo_model->update_input( it_changed_data ).

    update( EXPORTING is_controls     = VALUE #( variaveis    = abap_true )
                      iv_guid         = mv_id_destino ).

    MESSAGE 'Dados atualizados com sucesso' TYPE 'S'.

  ENDMETHOD.


  METHOD on_function_selected.

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

    DATA ls_item TYPE /qaps/s_premissa_item.

    FIELD-SYMBOLS: <fs>       TYPE /qaps/s_premissa_item.

    CASE iv_source.
      WHEN 'CUSTO_ELEMENTAR'.

        ASSIGN ir_data->* TO <fs>.
        ms_item = <fs>.

        update( EXPORTING is_controls   = VALUE #( input      = abap_true )
                          ir_data       = ir_data ).

        mo_trajeto->reset( ).

      WHEN 'INPUT'.

        ASSIGN ir_data->* TO FIELD-SYMBOL(<fs_input>).

        ASSIGN COMPONENT 'ID_PREMISSA' OF STRUCTURE <fs_input> TO FIELD-SYMBOL(<fv_premissa>).
        ASSIGN COMPONENT 'ID_ITEM' OF STRUCTURE <fs_input> TO FIELD-SYMBOL(<fv_item>).
        ASSIGN COMPONENT 'ID_DISTRIBUICAO' OF STRUCTURE <fs_input> TO FIELD-SYMBOL(<fv_distrib>).

        ls_item-id_premissa = <fv_premissa>.
        ls_item-id_item = <fv_item>.

        DATA(lt_data) = mo_model->get_distribuicao( ls_item ).
        ms_distrib = lt_data[ id_distribuicao = <fv_distrib> ].

        update( EXPORTING is_controls   = VALUE #(  trajeto      = abap_true )
                          ir_data       = ir_data ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_menu_button.
    BREAK-POINT .
  ENDMETHOD.


  METHOD on_node_double_click.

    DATA: lr_data      TYPE REF TO data.

    lr_data = REF #( ms_premissa_header ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    ms_simulacao = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ms_premissa_header-id_simulacao ).

    mv_id_destino = iv_guid.
    mv_texto = iv_texto.

    update( EXPORTING is_controls   = VALUE #( simulacao     = abap_false
                                               simulacao_hdr = abap_true
                                               variaveis    = abap_true )
                      iv_guid = iv_guid ).

    mo_input->reset( ).
    mo_trajeto->reset( ).

  ENDMETHOD.


  METHOD on_prem_traj_changed_finished.

    mo_model->update_trajeto( it_changed_data ).

    update( EXPORTING is_controls     = VALUE #( trajeto    = abap_true )
                      iv_guid         = mv_id_destino ).

    MESSAGE 'Dados atualizados com sucesso' TYPE 'S'.

  ENDMETHOD.


  METHOD on_user_command.

    CASE iv_ucomm.
*      WHEN '&ADD'.
*        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
*                                          iv_source = iv_source
*                                          iv_action = iv_action ).
*      WHEN '&REMOVE'.
*        on_user_command_remove( EXPORTING iv_ucomm  = iv_ucomm
*                                          iv_source = iv_source
*                                          iv_action = iv_action
*                                          iv_xml_data = iv_xml_data ).

      WHEN '&ADD_TRANSFER'.
        on_user_command_add_transfer( EXPORTING iv_ucomm  = iv_ucomm
                                                iv_source = iv_source
                                                iv_action = iv_action ).
      WHEN '&DEL_TRANSFER'.
        on_user_command_del_transfer( EXPORTING iv_ucomm  = iv_ucomm
                                                iv_source = iv_source
                                                iv_action = iv_action
                                                iv_xml_data = iv_xml_data ).




    ENDCASE.

  ENDMETHOD.


  METHOD on_user_command_add_transfer.

    DATA: ls_update_controls TYPE ts_update_controls.

    CASE iv_source.
      WHEN 'PREMISSA'.

        DATA(lv_return) = mo_model->add_transfer( is_simulacao       = ms_simulacao
                                                  is_header    = ms_premissa_header
                                                  is_item = ms_item ).
        ls_update_controls-input = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

*    BREAK c060863.
    update( EXPORTING is_controls   = VALUE #( input      = abap_true )
*                          iv_id_simulacao = ms_simulacao-id_simulacao
*                          ir_data       = ref #( ms_tree_destino )
                          ).

  ENDMETHOD.


  METHOD on_user_command_create.

*    DATA ls_update_controls TYPE ts_update_controls.
*    DATA lr_data TYPE REF TO data.
*    DATA lv_return TYPE abap_bool.
*
*    DATA(ls_periodo) = VALUE /qaps/s_periodo_interval( inicial = ms_simulacao-periodo_inicial
*                                                      final   = ms_simulacao-periodo_final ).
*    mo_model->set_periodo( is_periodo = ls_periodo ).
*
**    CASE iv_source.
**      WHEN 'CUSTO_ELEMENTAR'.
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


  METHOD on_user_command_del_transfer.
    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data,
          lt_selected        TYPE /qaps/t_prm_input_selected.

    CASE iv_source.
      WHEN 'PREMISSA'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_selected ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.

        DATA(lv_return) = mo_model->delete_input( it_data =  lt_selected
                                                  is_simulacao = ms_simulacao ).
        ls_update_controls-simulacao = abap_true.
        ls_update_controls-input = abap_true.

    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls   = VALUE #( input      = abap_true ) ).

  ENDMETHOD.


  METHOD on_user_command_remove.
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


  METHOD refresh.

    IF is_controls-simulacao = abap_true.
*      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-simulacao_hdr = abap_true.
      mo_header->refresh( ).
    ENDIF.

    IF is_controls-input = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
      mo_input->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD reset.

    mo_tree->reset( ).
    mo_header->refresh( ).
    mo_item->reset( ).
    mo_trajeto->reset( ).

  ENDMETHOD.


  METHOD show_treeview.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 18   ).

  ENDMETHOD.


  METHOD update.

    IF is_controls-simulacao = abap_true.
      update_tree( ).
    ENDIF.

    IF is_controls-simulacao_hdr = abap_true.
      update_header(  ).
    ENDIF.

    IF is_controls-variaveis = abap_true.
      update_item( ).
    ENDIF.

    IF is_controls-input = abap_true.
      update_input( ).
    ENDIF.

    IF is_controls-trajeto = abap_true.
      update_trajeto( ).
    ENDIF.

  ENDMETHOD.


  METHOD update_header.

    DATA(lr_data) = REF #( ms_simulacao ).
    mo_header->update( lr_data ).
*    DATA(lt_data) = mo_model->get_simulacao( ms_premissa_header-id_simulacao ).
*
*    IF lines( lt_data ) > 0.
*      DATA(lr_data) = REF #( lt_data[ 1 ] ).
*    ENDIF.
*
*    mo_header->update( lr_data ).

  ENDMETHOD.


  METHOD update_input.

    DATA(lt_data) = mo_model->get_distribuicao( ms_item ).

    DATA(lr_data) = REF #( lt_data ).
    DATA(ls_periodo) = VALUE /qaps/s_periodo_interval( inicial = ms_simulacao-periodo_inicial
                                                       final   = ms_simulacao-periodo_final ).

    mo_input->set_session_data( is_periodo = ls_periodo
                                iv_tipo_destino = mv_tipo_destino
                                is_premissa_header = ms_premissa_header
                                is_item            = ms_item
                                is_simulacao       = ms_simulacao ).

    mo_input->set_data( lr_data ).
    mo_input->set_grid_title( iv_title = CONV #( ms_item-key_input ) ).

  ENDMETHOD.


  METHOD update_item.

    DATA(lt_data) = mo_model->get_items( ms_premissa_header ).

    DATA(lr_data) = REF #( lt_data ).
    mo_item->set_grid_title( iv_title = CONV #( mv_texto ) ).
    mo_item->set_data( lr_data ).

  ENDMETHOD.


  METHOD update_premissa.

    DATA(lo_update) = NEW /qaps/cl_rule_sincronizacao( ).
    lo_update->execute(
*        iv_id_simulacao =
    ).

    reset( ).

    update( EXPORTING is_controls   = VALUE #( simulacao = abap_true ) ).
  ENDMETHOD.


  METHOD update_trajeto.

    DATA(lt_data) = mo_model->get_trajeto_by_distribuicao( ms_distrib ).

    DATA(lr_data) = REF #( lt_data ).
    DATA(ls_periodo) = VALUE /qaps/s_periodo_interval( inicial = ms_simulacao-periodo_inicial
                                                       final   = ms_simulacao-periodo_final ).

    mo_trajeto->set_session_data( is_periodo = ls_periodo
                                  iv_tipo_destino = mv_tipo_destino
                                  is_simulacao = ms_simulacao ).

    mo_trajeto->set_data( lr_data ).
    mo_trajeto->set_grid_title( iv_title = CONV #( ms_item-key_input ) ).

  ENDMETHOD.


  METHOD update_tree.


    DATA(lt_data) = mo_model->get_header( ).

    DATA(lr_data) = REF #( lt_data ).
    IF ms_simulacao IS INITIAL.
      mo_tree->update( lr_data ).
    ELSE.
      mo_tree->update( ir_data = lr_data
                       ir_expanded = REF #( ms_simulacao ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
