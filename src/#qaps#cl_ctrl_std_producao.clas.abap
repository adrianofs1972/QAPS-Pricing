class /QAPS/CL_CTRL_STD_PRODUCAO definition
  public
  final
  create public .

public section.

  methods EXPORT_FILE
    raising
      /QAPS/CX_PRICING_ERROR .
  methods CREATE_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_HEADER
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods DELETE_STD_PRODUCAO
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods SET_HEADER
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods IMPORT_FILE
    raising
      /QAPS/CX_PRICING_ERROR .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      produto_acabado TYPE REF TO cl_gui_container,
      componente      TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      produto_acabado TYPE abap_bool,
      componente      TYPE abap_bool,
    END OF ts_update_controls .

  data MS_PA type /QAPS/S_STD_PRODUCAO_PA .
  data MS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER .
  data MO_MODEL type ref to /QAPS/CL_MDL_STD_PRODUCAO .
  data MO_DOCKING type ref to CL_GUI_DOCKING_CONTAINER .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_VIEW_COMPONENTES type ref to /QAPS/CL_VIEW_STD_PRD_CP .
  data MO_VIEW_PROD_ACABADO type ref to /QAPS/CL_VIEW_STD_PRD_PA .
  data MT_PA type /QAPS/T_STD_PRODUCAO_PA .
  data MT_CP type /QAPS/T_STD_PRODUCAO_CP .

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
  methods ON_USER_COMMAND_EDITAR
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
  methods UPDATE_PRODUTOS_ACABADOS
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
      !IR_DATA type ref to DATA .
  methods UPDATE_COMPONENTES
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
      !IR_DATA type ref to DATA .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
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
      value(RETURN) type ref to CL_GUI_DOCKING_CONTAINER .
  methods INITIALIZE_COMPONENTES
    importing
      !IO_CONTAINER type ref to CL_GUI_DOCKING_CONTAINER .
  methods GET_CONTAINERS
    importing
      !IO_DOCKING type ref to CL_GUI_DOCKING_CONTAINER
    returning
      value(RETURN) type /QAPS/CL_CTRL_STD_PRODUCAO=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_STD_PRODUCAO IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_std_producao( ).
  ENDMETHOD.


  METHOD create_main_container.

    mo_docking = NEW cl_gui_docking_container(
        parent                      = io_container
*        repid                       =
*        dynnr                       =
        side                        = cl_gui_docking_container=>dock_at_bottom
*        extension                   = 50
*        style                       =
*        lifetime                    = LIFETIME_DEFAULT
*        caption                     =
*        metric                      = 0
*        ratio                       =
*        no_autodef_progid_dynnr     =
*        name                        =
    ).

    mo_docking->set_height( EXPORTING height     = 404  ).

    return = mo_docking.


  ENDMETHOD.


  METHOD create_std_producao.

    return = mo_model->create_std_producao( ).

  ENDMETHOD.


  METHOD delete_std_producao.

    mo_model->delete_std_producao( is_header ).

  ENDMETHOD.


  METHOD export_file.
      mo_model->export_file( ms_header-codigo ).
  ENDMETHOD.


  METHOD get_containers.

    mo_splitter = NEW cl_gui_splitter_container(
*        link_dynnr              =
*        link_repid              =
*        shellstyle              =
*        left                    =
*        top                     =
*        width                   =
*        height                  =
*        metric                  = CNTL_METRIC_DYNPRO
*        align                   = 15
        parent                  = io_docking
        rows                    = 2
        columns                 = 1
*        no_autodef_progid_dynnr =
*        name                    =
    ).

    "treview
    return-produto_acabado = mo_splitter->get_container( row       = 1    " Row
                                                        column    = 1     ).

    return-componente = mo_splitter->get_container( row       = 2    " Row
                                                   column    = 1     ).

  ENDMETHOD.


  METHOD get_header.

    return = mo_model->get_header(  iv_codigo              = iv_codigo ).

  ENDMETHOD.


  METHOD import_file.
    mo_model->import_file( ms_header-codigo ).

    update( EXPORTING is_controls   = VALUE #( produto_acabado = 'X' )
                          iv_id_std_producao = ms_header-id_std_producao ).

    mo_view_componentes->reset( ).

  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    DATA(lo_docking) = create_main_container( io_container  ).
    initialize_componentes( lo_docking ).

    mv_initialized = abap_true.

    update( EXPORTING iv_id_std_producao = ms_header-id_std_producao
                      is_controls   = VALUE #( produto_acabado = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_container ).

    "Produto Acabado
    mo_view_prod_acabado = NEW /qaps/cl_view_std_prd_pa( ).
    SET HANDLER: on_hotspot_click  FOR mo_view_prod_acabado.
    SET HANDLER on_user_command FOR mo_view_prod_acabado.
    mo_view_prod_acabado->initialize( ir_outtab            = REF #( mt_pa )
                                      io_container         = ls_containers-produto_acabado
                                      is_catalog_structure = '/QAPS/S_STD_PRODUCAO_PA'
                                      iv_action = 'C' ).

    "Componentes
    mo_view_componentes = NEW /qaps/cl_view_std_prd_cp( ).
    SET HANDLER: on_hotspot_click  FOR mo_view_componentes.
    SET HANDLER on_user_command FOR mo_view_componentes.
    mo_view_componentes->initialize( ir_outtab            = REF #( mt_cp )
                                     io_container         = ls_containers-componente
                                     is_catalog_structure = '/QAPS/S_STD_PRODUCAO_CP'
                                     iv_action = 'C' ).

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

    FIELD-SYMBOLS <fs> TYPE /qaps/s_std_producao_pa.

    CASE iv_source.
      WHEN 'PA'.

        ASSIGN ir_data->* TO <fs>.
        ms_pa = <fs>.

        update( EXPORTING is_controls   = VALUE #( componente      = abap_true )
                          iv_id_std_producao = ms_header-id_std_producao
                          ir_data       = ir_data
                          ).

    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD on_user_command.

    CASE iv_ucomm.
      WHEN '&ADD'.
        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action ).

      WHEN '&EDITAR'.
        on_user_command_editar( EXPORTING iv_ucomm  = iv_ucomm
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


  METHOD on_user_command_create.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'PA'.
        lv_return = mo_model->create_std_producao_pa( ms_header-id_std_producao ).
        ls_update_controls-produto_acabado = abap_true.
      WHEN 'CP'.
        if not ms_pa is INITIAL.
          lv_return = mo_model->create_std_producao_cp( ms_pa ).
          ls_update_controls-componente = abap_true.
        else.
          message 'Nenhum produto acabado foi selecionado' type 'S' DISPLAY LIKE 'E'.
        endif.
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls   = ls_update_controls
                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD on_user_command_editar.
    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data,
          lt_selected_pa     TYPE /qaps/t_std_producao_pa,
          lt_selected_cp     TYPE /qaps/t_std_producao_cp.

    CASE iv_source.
      WHEN 'PA'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_selected_pa ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.

        DATA(lv_return) = mo_model->edit_std_producao_pa( lt_selected_pa[ 1 ] ).
        ls_update_controls-produto_acabado = abap_true.
      WHEN 'CP'.
        IF NOT ms_pa IS INITIAL.
          IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_selected_cp ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.

        lv_return = mo_model->edit_std_producao_cp( is_data = lt_selected_cp[ 1 ]
                                                    is_produto_acabado = ms_pa ).
        ls_update_controls-componente = abap_true.
        ELSE.
          MESSAGE 'Nenhum produto acabado foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls   = ls_update_controls
                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD on_user_command_remove.
    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data,
          lt_selected_pa     TYPE /qaps/t_std_producao_pa,
          lt_selected_cp     TYPE /qaps/t_std_producao_cp.

    CASE iv_source.
      WHEN 'PA'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_selected_pa ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.

        DATA(lv_return) = mo_model->delete_std_producao_pa( lt_selected_pa ).
        ls_update_controls-produto_acabado = abap_true.
      WHEN 'CP'.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_selected_cp ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.

        lv_return = mo_model->delete_std_producao_cp( lt_selected_cp ).
        ls_update_controls-componente = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls   = ls_update_controls
                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD REFRESH.

*    IF is_controls-tipo_lista = abap_true.
**      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
*    ENDIF.
*
*    IF is_controls-tipo_lista_hdr = abap_true.
*      mo_view_tipo_lista_hdr->refresh( ).
*    ENDIF.
*
*    IF is_controls-input = abap_true.
**      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
**      mo_view_input->reset( ).
*    ENDIF.

  ENDMETHOD.


  METHOD set_header.
    ms_header = is_header.
  ENDMETHOD.


  METHOD update.

    IF is_controls-produto_acabado = abap_true.
      update_produtos_acabados( iv_id_std_producao = iv_id_std_producao
                                ir_data = ir_data ).
    ENDIF.

    IF is_controls-componente = abap_true.
      update_componentes( iv_id_std_producao = iv_id_std_producao
                          ir_data = ir_data ).
    ENDIF.


  ENDMETHOD.


  METHOD update_componentes.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE /qaps/s_std_producao_pa.
    ASSIGN ir_data->* TO <fs>.

    CHECK NOT ms_pa IS INITIAL.

    DATA(lt_items) = mo_model->get_componentes( iv_id_std_producao = ms_pa-id_std_producao
                                                iv_id_std_prod_pa  = ms_pa-id_std_prod_pa ).


    lr_data = REF #( lt_items ).

    mo_view_componentes->set_grid_title( iv_title = `Componentes de ` && ms_pa-matnr ).

    mo_view_componentes->set_data(
      EXPORTING
        ir_outtab       = lr_data
*        iv_soft_refresh = ABAP_TRUE
*        ir_parent       =
*        iv_source       =
    ).


  ENDMETHOD.


  METHOD UPDATE_PRODUTOS_ACABADOS.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_std_producao_pa.
    ASSIGN ir_data->* TO <fs>.

    DATA(lt_items) = mo_model->get_produtos_acabados( iv_id_std_producao ).


    lr_data = REF #( lt_items ).

    mo_view_prod_acabado->set_data( EXPORTING ir_outtab       = lr_data
*        iv_soft_refresh = ABAP_TRUE
*        ir_parent       =
*        iv_source       =
    ).


  ENDMETHOD.
ENDCLASS.
