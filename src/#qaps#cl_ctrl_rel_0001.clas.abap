class /QAPS/CL_CTRL_REL_0001 definition
  public
  final
  create public .

public section.

  methods VALIDATE
    importing
      !IS_HEADER type /QAPS/S_REL_0001_HEADER
    returning
      value(RETURN) type ABAP_BOOL
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_HEADER
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods SET_HEADER
    importing
      !IS_HEADER type /QAPS/S_REL_0001_HEADER .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      items TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      items TYPE abap_bool,
    END OF ts_update_controls .

  data MS_PA type /QAPS/S_STD_PRODUCAO_PA .
  data MS_HEADER type /QAPS/S_REL_0001_HEADER .
  data MO_MODEL type ref to /QAPS/CL_MDL_REL_0001 .
  data MO_DOCKING type ref to CL_GUI_DOCKING_CONTAINER .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_VIEW_ITEMS type ref to /QAPS/CL_VIEW_REL_0001 .
  data MT_ITEMS type /QAPS/T_REL_0001 .

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
  methods UPDATE_ITEMS
    importing
      !IR_DATA type ref to DATA .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
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
      !IO_CONTAINER type ref to CL_GUI_SPLITTER_CONTAINER .
  methods GET_CONTAINERS
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER
    returning
      value(RETURN) type /QAPS/CL_CTRL_REL_0001=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_REL_0001 IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_rel_0001( ).
  ENDMETHOD.


  METHOD create_main_container.

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
        parent                  = io_container
        rows                    = 1
        columns                 = 1
*        no_autodef_progid_dynnr =
*        name                    =
    ).


    return = mo_splitter.


  ENDMETHOD.


  METHOD GET_CONTAINERS.

    "treview
    return-items = mo_splitter->get_container(
                   row       =  1
                   column    =  1
               ).

  ENDMETHOD.


  METHOD GET_HEADER.

*    return = mo_model->get_header(  iv_codigo              = iv_codigo ).

  ENDMETHOD.


  METHOD INITIALIZE.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( items = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_container ).

    "Items
    mo_view_items = NEW /qaps/cl_view_rel_0001( ).
    SET HANDLER: on_hotspot_click  FOR mo_view_items.

    mo_view_items->initialize( ir_outtab            = REF #( mt_items )
                               io_container         = ls_containers-items
                               is_catalog_structure = '/QAPS/S_REL_0001'
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


  METHOD ON_HOTSPOT_CLICK.

*    FIELD-SYMBOLS <fs> TYPE /qaps/s_std_producao_pa.
*
*    CASE iv_source.
*      WHEN 'PA'.
*
*        ASSIGN ir_data->* TO <fs>.
*        ms_pa = <fs>.
*
*        update( EXPORTING is_controls   = VALUE #( componente      = abap_true )
*                          iv_id_std_producao = ms_header-id_std_producao
*                          ir_data       = ir_data
*                          ).
*
*    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD ON_USER_COMMAND.

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


  METHOD ON_USER_COMMAND_CREATE.

*    DATA ls_update_controls TYPE ts_update_controls.
*    DATA lr_data TYPE REF TO data.
*    DATA lv_return TYPE abap_bool.
*
*    CASE iv_source.
*      WHEN 'PA'.
*        lv_return = mo_model->create_std_producao_pa( ms_header-id_std_producao ).
*        ls_update_controls-produto_acabado = abap_true.
*      WHEN 'CP'.
*        if not ms_pa is INITIAL.
*          lv_return = mo_model->create_std_producao_cp( ms_pa ).
*          ls_update_controls-componente = abap_true.
*        else.
*          message 'Nenhum produto acabado foi selecionado' type 'S' DISPLAY LIKE 'E'.
*        endif.
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update( EXPORTING is_controls   = ls_update_controls
*                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_EDITAR.
*    DATA: ls_update_controls TYPE ts_update_controls,
*          lr_data            TYPE REF TO data,
*          lt_selected_pa     TYPE /qaps/t_std_producao_pa,
*          lt_selected_cp     TYPE /qaps/t_std_producao_cp.
*
*    CASE iv_source.
*      WHEN 'PA'.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected_pa ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        DATA(lv_return) = mo_model->edit_std_producao_pa( lt_selected_pa[ 1 ] ).
*        ls_update_controls-produto_acabado = abap_true.
*      WHEN 'CP'.
*        IF NOT ms_pa IS INITIAL.
*          IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected_cp ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        lv_return = mo_model->edit_std_producao_cp( is_data = lt_selected_cp[ 1 ]
*                                                    is_produto_acabado = ms_pa ).
*        ls_update_controls-componente = abap_true.
*        ELSE.
*          MESSAGE 'Nenhum produto acabado foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update( EXPORTING is_controls   = ls_update_controls
*                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_REMOVE.
*    DATA: ls_update_controls TYPE ts_update_controls,
*          lr_data            TYPE REF TO data,
*          lt_selected_pa     TYPE /qaps/t_std_producao_pa,
*          lt_selected_cp     TYPE /qaps/t_std_producao_cp.
*
*    CASE iv_source.
*      WHEN 'PA'.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected_pa ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        DATA(lv_return) = mo_model->delete_std_producao_pa( lt_selected_pa ).
*        ls_update_controls-produto_acabado = abap_true.
*      WHEN 'CP'.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected_cp ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        lv_return = mo_model->delete_std_producao_cp( lt_selected_cp ).
*        ls_update_controls-componente = abap_true.
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update( EXPORTING is_controls   = ls_update_controls
*                      iv_id_std_producao = ms_header-id_std_producao ).

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


  METHOD SET_HEADER.
    ms_header = is_header.
  ENDMETHOD.


  METHOD update.

    IF is_controls-items = abap_true.
      update_items( ir_data = ir_data ).
    ENDIF.



  ENDMETHOD.


  METHOD update_items.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_rel_0001.
    ASSIGN ir_data->* TO <fs>.

    DATA(lt_items) = mo_model->get_items( ms_header ).

    lr_data = REF #( lt_items ).

    mo_view_items->set_data( EXPORTING ir_outtab       = lr_data
*        iv_soft_refresh = ABAP_TRUE
*        ir_parent       =
*        iv_source       =
    ).


  ENDMETHOD.


  METHOD validate.
    mo_model->validate( is_header ).
  ENDMETHOD.
ENDCLASS.
