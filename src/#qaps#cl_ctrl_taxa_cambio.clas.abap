class /QAPS/CL_CTRL_TAXA_CAMBIO definition
  public
  final
  create public .

public section.

  methods GET_HEADER
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
    returning
      value(RETURN) type TCURT
    raising
      /QAPS/CX_PRICING_ERROR .
  methods SET_HEADER
    importing
      !IS_HEADER type TCURT .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
  methods SET_PERIODO
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      taxa_cambio_hdr TYPE REF TO cl_gui_container,
      input           TYPE REF TO cl_gui_container,
      taxa_cambio     TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      taxa_cambio_hdr TYPE abap_bool,
      input           TYPE abap_bool,
      taxa_cambio     TYPE abap_bool,
    END OF ts_update_controls .

  data MS_HEADER type TCURT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MS_TAXA_CAMBIO type /QAPS/S_TAXA_CAMBIO .
  data MT_TAXA_CAMBIO type /QAPS/S_TAXA_CAMBIO_VERSAO .
  data MO_MODEL type ref to /QAPS/CL_MDL_TAXA_CAMBIO .
  data MO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_VIEW_TAXA_CAMBIO type ref to /QAPS/CL_VIEW_TAXA_CAMBIO .
  data MO_VIEW_INPUT type ref to /QAPS/CL_VIEW_INPUT_TX_CAMBIO .
  data MO_VIEW_TAXA_CAMBIO_HDR type ref to /QAPS/CL_VIEW_TX_CAMBIO_HDR .

  methods ON_DOUBLE_CLICK_TX_CAMBIO
    for event ON_DOUBLE_CLICK_TX_CAMBIO of /QAPS/CL_VIEW_INPUT_TX_CAMBIO
    importing
      !IV_ID_TAXA_CAMBIO
      !IV_PERIODO .
  methods ON_DATA_CHANGED_FINISHED
    for event ON_DATA_CHANGED_FINISHED of /QAPS/CL_VIEW_INPUT_TX_CAMBIO
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
  methods ON_USER_COMMAND_ATIVAR
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
  methods UPDATE_TAXA_CAMBIO_HDR
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IR_DATA type ref to DATA .
  methods UPDATE_TAXA_CAMBIO
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IR_DATA type ref to DATA
      !IV_PERIODO type /QAPS/ED_PERIODO
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO .
  methods UPDATE_INPUT
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IR_DATA type ref to DATA .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IR_DATA type ref to DATA optional
      !IV_PERIODO type /QAPS/ED_PERIODO optional
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO optional .
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
      value(RETURN) type /QAPS/CL_CTRL_TAXA_CAMBIO=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_TAXA_CAMBIO IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_taxa_cambio( ).
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
        rows                    = 3
        columns                 = 1
*        no_autodef_progid_dynnr =
*        name                    =
    ).


    mo_splitter->set_row_height( id = 1 height = 8 ).
    mo_splitter->set_row_height( id = 2 height = 30 ).

    return = mo_splitter.


  ENDMETHOD.


  METHOD GET_CONTAINERS.

    "treview
    return-taxa_cambio_hdr = io_splitter->get_container( row       = 1    " Row
                                                         column    = 1     ).

    return-input = io_splitter->get_container( row       = 2    " Row
                                               column    = 1     ).

    return-taxa_cambio = io_splitter->get_container( row       = 3    " Row
                                               column    = 1     ).

  ENDMETHOD.


  METHOD get_header.

    return = mo_model->get_header_data(  iv_moeda_local ).

  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING iv_moeda_local = ms_header-waers
                      is_controls   = VALUE #( taxa_cambio_hdr = abap_true
                                               input = abap_true ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_container ).

    "Html - Tipo Lista
    mo_view_taxa_cambio_hdr = NEW /qaps/cl_view_tx_cambio_hdr( ).
    mo_view_taxa_cambio_hdr->initialize( ls_containers-taxa_cambio_hdr ).

*    "Input
    mo_view_input = NEW /qaps/cl_view_input_tx_cambio( ).
    SET HANDLER: on_data_changed_finished  FOR mo_view_input,
                 on_double_click_tx_cambio  FOR mo_view_input,
                 on_user_command FOR mo_view_input.

    DATA lt_data TYPE /qaps/t_taxa_cambio.
    mo_view_input->set_session_data( is_periodo = ms_periodo ).
    mo_view_input->initialize( ir_outtab            = REF #( lt_data )
                               io_container         = ls_containers-input
                               is_catalog_structure = '/QAPS/S_TAXA_CAMBIO'
                               iv_action = 'C' ).

    "Taxa de Câmbio
    mo_view_taxa_cambio = NEW /qaps/cl_view_taxa_cambio( ).
    SET HANDLER: on_user_command    FOR mo_view_taxa_cambio.
    mo_view_taxa_cambio->initialize( ir_outtab            = REF #( mt_taxa_cambio  )
                                     io_container         = ls_containers-taxa_cambio
                                     is_catalog_structure = '/QAPS/S_TAXA_CAMBIO_VERSAO'
                                     iv_action = 'C' ).

  ENDMETHOD.


  METHOD on_data_changed_finished.

    mo_model->update_taxa( it_changed_data ).

    IF lines( it_changed_data ) > 0.
      DATA(ls_data) = it_changed_data[ 1 ].

      update( EXPORTING is_controls       = VALUE #( taxa_cambio = abap_true )
                       iv_periodo        = ls_data-periodo
                       iv_moeda_local    = ms_header-waers
                       iv_id_taxa_cambio = ls_data-id_taxa_cambio ).
    ENDIF.

    MESSAGE 'Dados atualizados com sucesso' TYPE 'S'.

  ENDMETHOD.


  METHOD ON_DOUBLE_CLICK_TX_CAMBIO.
*    BREAK-POINT.
    update( EXPORTING is_controls       = VALUE #( taxa_cambio = abap_true )
                      iv_periodo        = iv_periodo
                      iv_moeda_local    = ms_header-waers
                      iv_id_taxa_cambio = iv_id_taxa_cambio ).

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
      WHEN '&ATIVAR'.
        on_user_command_ativar( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).


    ENDCASE.

  ENDMETHOD.


  METHOD on_user_command_ativar.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.
    DATA: lt_data TYPE /qaps/t_taxa_cambio_versao,
          ls_data TYPE /qaps/s_taxa_cambio_versao.

    CASE iv_source.
      WHEN 'TAXA_CAMBIO'.

        lr_data = REF #( lt_data ).
        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                             CHANGING  cr_data = lr_data  ).

        ls_data = lt_data[ 1 ].
        lv_return = mo_model->ativar_taxa_cambio( ls_data ).
        ls_update_controls-input = abap_true.
        ls_update_controls-taxa_cambio = abap_true.
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls       = ls_update_controls
                      iv_moeda_local    = ms_header-waers
                      iv_id_taxa_cambio = ls_data-id_taxa_cambio
                      iv_periodo        = ls_data-periodo ).

  ENDMETHOD.


  METHOD on_user_command_create.

    DATA ls_update_controls TYPE ts_update_controls.
    DATA lr_data TYPE REF TO data.
    DATA lv_return TYPE abap_bool.

    CASE iv_source.
      WHEN 'TAXA_CAMBIO'.
        lv_return = mo_model->create_taxa_cambio( is_moeda_local = ms_header
                                                  is_periodo = ms_periodo ).
        ls_update_controls-input = abap_true.
        refresh( VALUE #( taxa_cambio = abap_true ) ).
    ENDCASE.

    CHECK lv_return = abap_true.

    update( EXPORTING is_controls   = ls_update_controls
                      iv_moeda_local = ms_header-waers ).

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
*        ls_update_controls-componente = abap_true.
*      WHEN 'CP'.
**        IF NOT iv_xml_data IS INITIAL.
**          lr_data = REF #( lt_selected_cp ).
**          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
**                                               CHANGING  cr_data = lr_data ).
**        ENDIF.
**
**        lv_return = mo_model->edit_std_producao_pa( lt_selected_cp[ 1 ] ).
**        ls_update_controls-componente = abap_true.
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
*          lt_selected        TYPE /qaps/t_std_producao_pa.
*
*    CASE iv_source.
*      WHEN 'PA'.
*        IF NOT iv_xml_data IS INITIAL.
*          lr_data = REF #( lt_selected ).
*          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
*                                               CHANGING  cr_data = lr_data ).
*        ENDIF.
*
*        DATA(lv_return) = mo_model->delete_std_producao_pa( lt_selected ).
*        ls_update_controls-produto_acabado = abap_true.
*
*    ENDCASE.
*
*    CHECK lv_return = abap_true.
*
*    update( EXPORTING is_controls   = ls_update_controls
*                      iv_id_std_producao = ms_header-id_std_producao ).

  ENDMETHOD.


  METHOD refresh.

*    IF is_controls-tipo_lista = abap_true.
**      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
*    ENDIF.
*
*    IF is_controls-tipo_lista_hdr = abap_true.
*      mo_view_tipo_lista_hdr->refresh( ).
*    ENDIF.
*
    IF is_controls-input = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
      mo_view_taxa_cambio->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD set_header.
    ms_header = is_header.
  ENDMETHOD.


  METHOD set_periodo.
    ms_periodo = is_periodo.
  ENDMETHOD.


  METHOD update.

    IF is_controls-taxa_cambio_hdr = abap_true.
      update_taxa_cambio_hdr( iv_moeda_local = iv_moeda_local
                              ir_data = ir_data ).
    ENDIF.
    IF is_controls-input = abap_true.
      update_input( iv_moeda_local = iv_moeda_local
                    ir_data = ir_data ).
    ENDIF.

    IF is_controls-taxa_cambio = abap_true.
      update_taxa_cambio( iv_id_taxa_cambio = iv_id_taxa_cambio
                          iv_moeda_local = iv_moeda_local
                          ir_data       = ir_data
                          iv_periodo    = iv_periodo ).
    ENDIF.


  ENDMETHOD.


  METHOD update_input.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE /qaps/t_taxa_cambio.
    ASSIGN ir_data->* TO <fs>.

*    CHECK NOT ms_pa IS INITIAL.

    DATA(lt_items) = mo_model->get_taxa_cambio( is_moeda_local = ms_header ).

    lr_data = REF #( lt_items ).

    mo_view_input->set_grid_title( iv_title = 'Taxas de Câmbio' ).
    mo_view_input->set_data( EXPORTING ir_outtab       = lr_data
                                       iv_soft_refresh = abap_true ).

  ENDMETHOD.


  METHOD update_taxa_cambio.

    DATA lr_data TYPE REF TO data.
    data lv_title TYPE LVC_TITLE.

    DATA(lt_items) = mo_model->get_versoes( iv_id_taxa_cambio = iv_id_taxa_cambio
                                            iv_moeda_local    = iv_moeda_local
                                            iv_periodo        = iv_periodo ).

    lr_data = REF #( lt_items ).

    DATA(ls_cambio) = mo_model->get_single_taxa_cambio( iv_id_taxa_cambio = iv_id_taxa_cambio
                                                        iv_periodo        = iv_periodo ).
    data(lv_periodo) = ls_cambio-periodo+4(2) && `.` && ls_cambio-periodo(4).
    lv_title = |Moeda { ls_cambio-moeda_final } - Período { lv_periodo }|.

    mo_view_taxa_cambio->set_grid_title( lv_title ).

    mo_view_taxa_cambio->set_data(
      EXPORTING
        ir_outtab       = lr_data
*        iv_soft_refresh = ABAP_TRUE
*        ir_parent       =
*        iv_source       =
    ).


  ENDMETHOD.


  METHOD update_taxa_cambio_hdr.

    DATA: lr_data           TYPE REF TO data,
          lr_addtional_data TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE /qaps/s_taxa_cambio.
    ASSIGN ir_data->* TO <fs>.

    DATA(ls_header) = mo_model->get_header_data( iv_moeda_local ).

    lr_data = REF #( ls_header ).
    lr_addtional_data = ref #( ms_periodo ).
    mo_view_taxa_cambio_hdr->update( ir_data           = lr_data
                                     ir_addtional_data = lr_addtional_data ).


  ENDMETHOD.
ENDCLASS.
