class /QAPS/CL_CTRL_REL_0002 definition
  public
  final
  create public .

public section.

  events ON_CHANGE_MODE
    exporting
      value(IV_MODE) type STRING .

  methods SHOW_TREEVIEW .
  methods EXPORTAR .
  methods HIDE_TREEVIEW .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      tree     TYPE REF TO cl_gui_container,
      header   TYPE REF TO cl_gui_container,
      material TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      tree     TYPE abap_bool,
      header   TYPE abap_bool,
      material TYPE abap_bool,
    END OF ts_update_controls .

  data MV_TREEVIEW_VISIBLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MV_MOEDA type STRING value 'MOEDA_FINAL' ##NO_TEXT.
  data MV_VALOR type STRING value 'VLR_CUSTO_GERENCIAL' ##NO_TEXT.
  data MV_ID_LISTA_CUSTO type /QAPS/ED_ID_LISTA_CUSTO .
  data MO_MODEL type ref to /QAPS/CL_MDL_REL_0002 .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_LISTA_CUSTO_HDR type ref to /QAPS/CL_VIEW_LISTA_CUSTO_HDR .
  data MO_VIEW_LISTA_CUSTO type ref to /QAPS/CL_VIEW_TREE_LISTA_CUSTO .
  data MO_VIEW_ITEMS type ref to /QAPS/CL_VIEW_ITEM_REL_0002 .
  data MT_SIMULACAO type /QAPS/T_SIMULACAO .
  data MV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO .
  data MV_INITIALIZED type ABAP_BOOL .
  data MV_PREVIOUS_FCAT type STRING .
  data MT_REL_0002 type /QAPS/T_REL_0002 .

  methods GET_ITEM_VALUES
    importing
      !IV_ID_ITEM_LISTA_CUSTO type /QAPS/ED_ID_ITEM_LISTA
      !IV_PERIODO type SPMON
      !IS_DATA type /QAPS/S_RETORNO_FINAL
    returning
      value(RETURN) type /QAPS/S_RETORNO_CALCULO .
  methods GET_TAXA_CAMBIO
    importing
      !IV_FONTE type /QAPS/ED_DSC_FONTE_CAMBIO
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_MOEDA_FINAL type /QAPS/ED_MOEDA_FINAL
      !IS_SIMULACAO type /QAPS/SIMULACAO
    returning
      value(RETURN) type /QAPS/T_TAXA_CAMBIO_PERIODO .
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
      !IR_DATA
      !IV_XML_DATA
      !IV_ADDITIONAL_DATA_1
      !IV_ADDITIONAL_DATA_2
      !IV_ADDITIONAL_DATA_3
      !IV_ADDITIONAL_DATA_4 .
  methods ON_USER_COMMAND_UPDATE
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_VALOR type STRING
      !IV_MOEDA type STRING
      !IV_PREVIOUS_FCAT type STRING optional .
  methods ON_USER_COMMAND
    for event ON_USER_COMMAND of /QAPS/CL_VIEW_ALV_BASE
    importing
      !IV_UCOMM
      !IV_SOURCE
      !IV_ACTION
      !IV_XML_DATA
      !IV_ADDTIONAL_DATA_1
      !IV_ADDTIONAL_DATA_2
      !IV_ADDTIONAL_DATA_3
      !IV_ADDTIONAL_DATA_4 .
  methods ON_NODE_DOUBLE_CLICK
    for event ON_NODE_DOUBLE_CLICK of /QAPS/CL_VIEW_SIMPLE_TREE_BASE
    importing
      !IV_NODE_KEY
      !IV_XML_DATA .
  methods UPDATE_ITEMS .
  methods UPDATE_HEADER .
  methods UPDATE_TREE .
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
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  methods GET_CONTAINERS
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER
    returning
      value(RETURN) type /QAPS/CL_CTRL_REL_0002=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_REL_0002 IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    mo_model = NEW /qaps/cl_mdl_rel_0002( ).
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
           columns                 = 3
*        no_autodef_progid_dynnr =
*        name                    =
       ).


    mo_splitter_parent->set_column_width( id = 1 width = 15 ).
    mo_splitter_parent->set_column_width( id = 3 width = 0 ).
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


  METHOD EXPORTAR.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_lista_header.

    DATA(lv_node) = mo_view_lista_custo->get_selected_node( ).

    IF NOT lv_node IS INITIAL.

      DATA(lv_return) = mo_model->exportar( ms_lista_custo-cod_lista_custo ).

      IF lv_return = abap_true.
        MESSAGE 'Exportação concuída com sucesso' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'Selecionar Lista de Custo para efetuar operação' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CONTAINERS.

    "treview
    return-tree = io_splitter->get_container( row       = 1    " Row
                                                    column    = 1     ).

    DATA(lo_parent_child) = io_splitter->get_container( row       = 1    " Row
                                                        column    = 2     ).

    mo_splitter_child = NEW cl_gui_splitter_container(
        parent                  = lo_parent_child
        rows                    = 2
        columns                 = 1 ).

    mo_splitter_child->set_row_height( id = 1 height = 14 ).

    "Html Header
    return-header = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    return-material = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).

  ENDMETHOD.


  METHOD GET_ITEM_VALUES.

    DATA lt_data TYPE /qaps/t_retorno_calculo.

    APPEND LINES OF: is_data-t_importado TO lt_data,
                     is_data-t_nacional  TO lt_data,
                     is_data-t_producao_conversao TO lt_data,
                     is_data-t_producao_producao TO lt_data,
                     is_data-t_transf_importacao TO lt_data,
                     is_data-t_transf_nacional TO lt_data,
                     is_data-t_transf_std_producao TO lt_data.

    DATA(ls_return) = VALUE #( lt_data[ id_item_lista_custo = iv_id_item_lista_custo ] OPTIONAL ).

    IF NOT ls_return IS INITIAL.

      LOOP AT ls_return-t_expressao ASSIGNING FIELD-SYMBOL(<fs_expressao>).
        DELETE <fs_expressao>-t_valores WHERE periodo <> iv_periodo.
      ENDLOOP.

      return = ls_return.

    ELSE.
      DATA(ls_ponderacao) = VALUE #( is_data-t_ponderacao[ id_item_lista_custo = iv_id_item_lista_custo ] OPTIONAL ).

      LOOP AT ls_ponderacao-t_expressao ASSIGNING FIELD-SYMBOL(<fs_ponderacao>).
        DELETE <fs_ponderacao>-t_valores WHERE periodo <> iv_periodo.
      ENDLOOP.

      return = CORRESPONDING #( ls_ponderacao ).

    ENDIF.

  ENDMETHOD.


  METHOD GET_TAXA_CAMBIO.

    IF iv_moeda_local <> iv_moeda_final.
      SELECT *
        FROM /qaps/v_tx_cmb
        WHERE fonte = @iv_fonte
        AND   moeda_local = @iv_moeda_local
        AND   moeda_final = @iv_moeda_final
        AND   periodo >= @is_simulacao-periodo_inicial
        AND   periodo <= @is_simulacao-periodo_final
        INTO TABLE @return.
    ELSE.
      "Carrega para conversões, quando houver variáveis em outra moeda
      SELECT *
        FROM /qaps/v_tx_cmb
        WHERE fonte = @iv_fonte
        AND   moeda_local = @iv_moeda_local
*        AND   moeda_final = @iv_moeda_final
        AND   periodo >= @is_simulacao-periodo_inicial
        AND   periodo <= @is_simulacao-periodo_final
        INTO TABLE @return.
    ENDIF.

  ENDMETHOD.


  METHOD HIDE_TREEVIEW.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 0   ).

    mv_treeview_visible = abap_false.

  ENDMETHOD.


  METHOD INITIALIZE.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = VALUE #( tree = abap_true ) ).

  ENDMETHOD.


  METHOD INITIALIZE_COMPONENTES.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_lista_custo = NEW /qaps/cl_view_tree_lista_custo( ).
    SET HANDLER: on_node_double_click FOR mo_view_lista_custo,
                 on_function_selected FOR mo_view_lista_custo.
    mo_view_lista_custo->initialize( io_container = ls_containers-tree
                                     iv_root_text = 'Lista de Custos'
                                     iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_lista_custo_hdr = NEW /qaps/cl_view_lista_custo_hdr( ).
    mo_view_lista_custo_hdr->initialize( ls_containers-header ).

    "Itens
    mo_view_items = NEW /qaps/cl_view_item_rel_0002( ).
    SET HANDLER: on_user_command FOR mo_view_items.

    mo_view_items->initialize( ir_outtab            = REF #( MT_REL_0002 )
                               io_container         = ls_containers-material
                               is_catalog_structure = '/QAPS/S_REL_0002'
                               iv_action = 'C' ).


  ENDMETHOD.


  METHOD ON_FUNCTION_SELECTED.

    DATA ls_data TYPE /qaps/s_grupo_produto.
    DATA lr_data TYPE REF TO data.

*    break c060863.

    CASE iv_function.
      when 'REFRESH'.
        update_tree( ).
      WHEN 'INSERT'.
*        mv_id_lista_custo = mo_model->create_lista_custo(  ).
*        IF NOT mv_id_lista_custo IS INITIAL.
*          update( EXPORTING is_controls   = VALUE #( tree = abap_true ) ).
*        ELSE.
*          CLEAR mv_id_lista_custo.
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

    ENDCASE.

  ENDMETHOD.


  METHOD ON_HOTSPOT_CLICK.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    CASE iv_source.
      WHEN 'LISTA_CUSTO_DETALHE'.

        CHECK NOT ms_lista_custo-content IS INITIAL.
        lr_data = REF #( ls_data ).
        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_lista_custo-content
                                             CHANGING  cr_data = lr_data ).

        DATA(ls_values) = get_item_values( is_data = ls_data
                                           iv_periodo = CONV #( iv_additional_data_1 )
                                           iv_id_item_lista_custo = CONV #( iv_additional_data_2 ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD ON_NODE_DOUBLE_CLICK.
    mv_cod_lista_custo = iv_node_key.
*    CLEAR: mv_moeda,mv_valor.
    update( EXPORTING is_controls   = VALUE #( tree     = abap_false
                                               header   = abap_true
                                               material = abap_true ) ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND.

    on_user_command_update( EXPORTING iv_ucomm  = iv_ucomm
                                      iv_source = iv_source
                                      iv_action = iv_action
                                      iv_valor  = iv_addtional_data_1
                                      iv_moeda  = iv_addtional_data_2
                                      iv_previous_fcat = iv_addtional_data_3 ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_UPDATE.

    mv_valor = iv_valor.
    mv_moeda = iv_moeda.
    mv_previous_fcat = iv_previous_fcat.

    update_items( ).

  ENDMETHOD.


  METHOD REFRESH.

    IF is_controls-tree = abap_true.
*      update_grupo_produto( iv_id_grupo_produto = iv_id_grupo_produto ).
    ENDIF.

    IF is_controls-header = abap_true.
      mo_view_lista_custo_hdr->refresh( ).
    ENDIF.

    IF is_controls-material = abap_true.
*      update_materiais( iv_id_grupo_produto = VALUE #( ) ).
      mo_view_items->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD show_treeview.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 14   ).

    mv_treeview_visible = abap_true.

  ENDMETHOD.


  METHOD UPDATE.

    IF is_controls-tree = abap_true.
      update_tree( ).
    ENDIF.

    IF is_controls-header = abap_true.
      update_header( ).
    ENDIF.

    IF is_controls-material = abap_true.
      update_items(  ).
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_HEADER.

    DATA(lt_data) = mo_model->get_lista_custo( mv_cod_lista_custo ).

    IF lines( lt_data ) > 0.
      ms_lista_custo = lt_data[ 1 ].
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_lista_custo_hdr->update( lr_data ).

  ENDMETHOD.


  METHOD UPDATE_ITEMS.

    DATA: lr_data TYPE REF TO data,
          lt_data TYPE /qaps/t_rel_0002.

    CHECK NOT ms_lista_custo-content IS INITIAL.


    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ms_lista_custo-id_simulacao ).
    mo_view_items->set_session_data( iv_previous_fcat = mv_previous_fcat
                                     is_lista_custo = ms_lista_custo
                                     is_periodo = VALUE /qaps/s_periodo_interval( inicial = ls_simulacao-periodo_inicial
                                                                                  final   = ls_simulacao-periodo_final )
                                     is_exibicao = VALUE /qaps/s_lista_custo_exibicao( valor = mv_valor
                                                                                       moeda = mv_moeda ) ).

    lt_data = mo_model->get_items( iv_cod_lista_custo = ms_lista_custo-cod_lista_custo
                                   is_exibicao = VALUE /qaps/s_lista_custo_exibicao( valor = mv_valor
                                                                                     moeda = mv_moeda ) ).
    lr_data = REF #( lt_data ).

    mo_view_items->set_data( lr_data ).

    CLEAR mv_previous_fcat.

  ENDMETHOD.


  METHOD UPDATE_TREE.

    DATA(lt_data) = mo_model->get_lista_custo( ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_lista_custo->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
