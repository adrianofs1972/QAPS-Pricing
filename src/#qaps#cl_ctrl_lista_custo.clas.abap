class /QAPS/CL_CTRL_LISTA_CUSTO definition
  public
  final
  create public .

public section.

  events ON_CHANGE_MODE
    exporting
      value(IV_MODE) type STRING .

  methods REPROCESSAR_LISTA_CUSTO .
  methods EFETIVAR .
  methods EXPORTAR .
  methods REABRIR_LISTA_CUSTO .
  methods FINALIZAR_LISTA_CUSTO .
  methods CRIAR_LISTA_CUSTO .
  methods CLOSE_DETAIL .
  methods HIDE_TREEVIEW .
  methods SHOW_TREEVIEW .
  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      tree      TYPE REF TO cl_gui_container,
      header    TYPE REF TO cl_gui_container,
      material  TYPE REF TO cl_gui_container,
      expressao TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
      tree      TYPE abap_bool,
      header    TYPE abap_bool,
      material  TYPE abap_bool,
      expressao TYPE abap_bool,
    END OF ts_update_controls .

  data MV_TREEVIEW_VISIBLE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MV_MOEDA type STRING value 'MOEDA_FINAL' ##NO_TEXT.
  data MV_VALOR type STRING value 'VLR_CUSTO_GERENCIAL' ##NO_TEXT.
  data MV_ID_LISTA_CUSTO type /QAPS/ED_ID_LISTA_CUSTO .
  data MO_MODEL type ref to /QAPS/CL_MDL_LISTA_CUSTO .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_LISTA_CUSTO_HDR type ref to /QAPS/CL_VIEW_LISTA_CUSTO_HDR .
  data MO_VIEW_LISTA_CUSTO type ref to /QAPS/CL_VIEW_TREE_LISTA_CUSTO .
  data MO_VIEW_ITEMS type ref to /QAPS/CL_VIEW_ITEM_LISTA_CUSTO .
  data MT_SIMULACAO type /QAPS/T_SIMULACAO .
  data MV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_VIEW_EXPRESSAO type ref to /QAPS/CL_VIEW_EXPRESSAO_VALUES .
  data MV_PREVIOUS_FCAT type STRING .

  methods ON_DOUBLE_CLICK
    for event ON_DOUBLE_CLICK of /QAPS/CL_VIEW_ALV_BASE
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !IV_ADDITIONAL_DATA_1
      !IV_ADDITIONAL_DATA_2
      !IV_ADDITIONAL_DATA_3
      !IV_ADDITIONAL_DATA_4 .
  methods SHOW_ERROR_LOG
    importing
      !IT_ERROR_LOG type BAPIRET2_T .
  methods RESET_ITEMS .
  methods SHOW_EXPRESSAO_VALUES
    importing
      !IS_DATA type /QAPS/S_RETORNO_CALCULO .
  methods GET_ITEM_BY_INDEX
    importing
      !IV_ID_ITEM_LISTA_CUSTO type /QAPS/ED_ID_ITEM_LISTA
      !IS_DATA type /QAPS/S_RETORNO_FINAL
    returning
      value(RETURN) type /QAPS/S_RETORNO_CALCULO .
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
      value(RETURN) type /QAPS/CL_CTRL_LISTA_CUSTO=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_LISTA_CUSTO IMPLEMENTATION.


  METHOD close_detail.

    IF mv_treeview_visible = abap_true.
      mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                      width             = 14   ).
    ENDIF.


*    mo_splitter_parent->set_column_width( EXPORTING id                = 2
*                                                    width             = 70   ).


    mo_splitter_parent->set_column_width( EXPORTING id                = 3
                                                    width             = 0   ).

  ENDMETHOD.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_lista_custo( ).
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


  METHOD criar_lista_custo.

    mo_model->create_lista_custo( ).

    update_tree( ).

  ENDMETHOD.


  METHOD efetivar.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_lista_header.

    DATA(lv_node) = mo_view_lista_custo->get_selected_node( ).

    IF NOT lv_node IS INITIAL.
      lr_data = REF #( ls_data ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = lv_node
                                           CHANGING  cr_data = lr_data ).

      DATA(lv_return) = mo_model->efetivar( ls_data ).

      IF lv_return = abap_true.
        update( is_controls = VALUE ts_update_controls( tree     = 'X'
                                                        header   = 'X'
                                                        material = 'X' ) ).
        MESSAGE 'Operação concuída com sucesso' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'Selecionar Lista de Custo para efetuar operação' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD EXPORTAR.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_lista_header.

    DATA(lv_node) = mo_view_lista_custo->get_selected_node( ).

    IF NOT lv_node IS INITIAL.
      lr_data = REF #( ls_data ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = lv_node
                                           CHANGING  cr_data = lr_data ).

      DATA(lv_return) = mo_model->exportar( ls_data ).

      IF lv_return = abap_true.
        MESSAGE 'Exportação concuída com sucesso' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'Selecionar Lista de Custo para efetuar operação' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD finalizar_lista_custo.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_lista_header.

    DATA(lv_node) = mo_view_lista_custo->get_selected_node( ).

    IF NOT lv_node IS INITIAL.
      lr_data = REF #( ls_data ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = lv_node
                                           CHANGING  cr_data = lr_data ).

      DATA(lv_return) = mo_model->finalizar_lista_custo( ls_data ).

      IF lv_return = abap_true.
        update( is_controls = VALUE ts_update_controls( tree     = 'X'
                                                        header   = 'X'
                                                        material = 'X' ) ).
        MESSAGE 'Operação concuída com sucesso' TYPE 'S'.
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

    mo_splitter_child->set_row_height( id = 1 height = 15 ).

    "Html Header
    return-header = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    return-material = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).

    return-expressao = io_splitter->get_container( row       = 1    " Row
                                                   column    = 3     ).

  ENDMETHOD.


  METHOD get_item_by_index.

    DATA lt_data TYPE /qaps/t_retorno_calculo.

    APPEND LINES OF: is_data-t_importado TO lt_data,
                     is_data-t_nacional  TO lt_data,
                     is_data-t_producao_conversao TO lt_data,
                     is_data-t_producao_producao TO lt_data,
                     is_data-t_transf_importacao TO lt_data,
                     is_data-t_transf_nacional TO lt_data,
                     is_data-t_transf_std_producao TO lt_data.

    return = VALUE #( lt_data[ id_item_lista_custo = iv_id_item_lista_custo ] OPTIONAL ).

  ENDMETHOD.


  METHOD get_item_values.

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


  METHOD get_taxa_cambio.

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


  METHOD hide_treeview.

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


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_lista_custo = NEW /qaps/cl_view_tree_lista_custo( ).
    SET HANDLER: on_node_double_click FOR mo_view_lista_custo,
                 on_function_selected FOR mo_view_lista_custo.
    mo_view_lista_custo->set_display_delete( iv_display_delete = abap_true ).
    mo_view_lista_custo->initialize( io_container = ls_containers-tree
                                     iv_root_text = 'Lista de Custos'
                                     iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_lista_custo_hdr = NEW /qaps/cl_view_lista_custo_hdr( ).
    mo_view_lista_custo_hdr->initialize( ls_containers-header ).

    "Itens
    mo_view_items = NEW /qaps/cl_view_item_lista_custo( ).
    SET HANDLER: on_user_command FOR mo_view_items,
                 on_hotspot_click FOR mo_view_items,
                 on_double_click  FOR mo_view_items.

    mo_view_items->initialize( ir_outtab            = REF #( mt_simulacao )
                               io_container         = ls_containers-material
                               is_catalog_structure = '/QAPS/S_SIMULACAO'
                               iv_action = 'C' ).

    mo_view_expressao = NEW /qaps/cl_view_expressao_values( ).

    mo_view_expressao->initialize(
      EXPORTING
        ir_outtab    = VALUE #( )
        io_container = ls_containers-expressao    " Abstract Container for GUI Controls
        iv_type      = '/QAPS/S_TREE_EXPRESSAO'    " Nome da tabela
        iv_action    = 'C'
    ).


  ENDMETHOD.


  METHOD on_double_click.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    CHECK NOT iv_additional_data_1 IS INITIAL.

    lr_data = REF #( ls_data ).
    CHECK NOT ms_lista_custo-content IS INITIAL.
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  =  ms_lista_custo-content
                                         CHANGING  cr_data = lr_data ).

    DATA(ls_item) = get_item_by_index( iv_id_item_lista_custo = CONV #( iv_additional_data_1 )
                                       is_data                = ls_data ).

    show_error_log( ls_item-t_erros ).



  ENDMETHOD.


  METHOD on_function_selected.

    DATA ls_data TYPE /qaps/s_lista_header.
    DATA lr_data TYPE REF TO data.

*    break c060863.

    CASE iv_function.
      WHEN 'REFRESH'.
        update_tree( ).
      WHEN 'DELETE'.

        lr_data = REF #( ls_data ).

        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml = iv_xml_data
                                             CHANGING  cr_data = lr_data ).

        DATA(lv_return) = mo_model->delete_lista_custo( ls_data-cod_lista_custo ).
        IF lv_return = abap_true.
          update( EXPORTING is_controls   = VALUE #( tree = abap_true ) ).

          mo_view_lista_custo_hdr->refresh( ).
          reset_items( ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD on_hotspot_click.

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

        show_expressao_values( ls_values ).

    ENDCASE.

  ENDMETHOD.


  METHOD ON_MENU_BUTTON.
    BREAK-POINT .
  ENDMETHOD.


  METHOD on_node_double_click.
    mv_cod_lista_custo = iv_node_key.
*    CLEAR: mv_moeda,mv_valor.
    update( EXPORTING is_controls   = VALUE #( tree     = abap_false
                                               header   = abap_true
                                               material = abap_true ) ).

  ENDMETHOD.


  METHOD on_user_command.

    on_user_command_update( EXPORTING iv_ucomm  = iv_ucomm
                                      iv_source = iv_source
                                      iv_action = iv_action
                                      iv_valor  = iv_addtional_data_1
                                      iv_moeda  = iv_addtional_data_2
                                      iv_previous_fcat = iv_addtional_data_3 ).

  ENDMETHOD.


  METHOD on_user_command_update.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    CASE iv_ucomm.
      WHEN 'VLR_CUSTO_GERENCIAL' OR 'VLR_TOTAL_GERENCIAL' OR 'MOEDA_LOCAL' OR 'MOEDA_FINAL'..
        mv_valor = iv_valor.
        mv_moeda = iv_moeda.
        mv_previous_fcat = iv_previous_fcat.

        update_items( ).
      WHEN 'LOG'.

        lr_data = REF #( ls_data ).
        IF NOT ms_lista_custo-content IS INITIAL.
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  =  ms_lista_custo-content
                                               CHANGING  cr_data = lr_data ).

          show_error_log( ls_data-t_erros_compilados ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD reabrir_lista_custo.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_lista_header.

    DATA(lv_node) = mo_view_lista_custo->get_selected_node( ).

    IF NOT lv_node IS INITIAL.
      lr_data = REF #( ls_data ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = lv_node
                                           CHANGING  cr_data = lr_data ).

      DATA(lv_return) = mo_model->reabrir_lista_custo( ls_data ).

      IF lv_return = abap_true.
        update( is_controls = VALUE ts_update_controls( tree     = 'X'
                                                        header   = 'X'
                                                        material = 'X' ) ).
        MESSAGE 'Operação concuída com sucesso' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'Selecionar Lista de Custo para efetuar operação' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD refresh.

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


  METHOD reprocessar_lista_custo.

    TRY.
        DATA(lv_cod_lista_custo) = mo_view_lista_custo->get_selected_node_key( ).

        DATA(lv_return) = mo_model->reprocessar_lista_custo( lv_cod_lista_custo  ).

        IF lv_return = abap_true.
          mv_cod_lista_custo = lv_cod_lista_custo.
          update( EXPORTING is_controls = VALUE #( tree = abap_true
                                                   header = abap_true
                                                   material = abap_true ) ).
        ENDIF.


      CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
        DATA(ls_message) = lo_excep->get_message( ).
        MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  method RESET_ITEMS.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.


    lr_data = REF #( ls_data ).
*    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_lista_custo-content
*                                         CHANGING  cr_data = lr_data ).


*    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ms_lista_custo-id_simulacao ).
*    mo_view_items->set_session_data( iv_previous_fcat = mv_previous_fcat
*                                     is_lista_custo = ms_lista_custo
*                                     is_periodo = VALUE /qaps/s_periodo_interval( inicial = ls_simulacao-periodo_inicial
*                                                                                  final   = ls_simulacao-periodo_final )
*                                     is_exibicao = VALUE /qaps/s_lista_custo_exibicao( valor = mv_valor
*                                                                                       moeda = mv_moeda ) ).

    mo_view_items->set_data( lr_data ).
    mo_view_items->set_grid_title( iv_title = '' ).

    clear ms_lista_custo.

    CLEAR mv_previous_fcat.


  endmethod.


  METHOD show_error_log.

    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = it_error_log.

  ENDMETHOD.


  METHOD show_expressao_values.

    DATA lv_width TYPE i.

    IF mv_treeview_visible = abap_true.
      mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                      width             = 0   ).
    ENDIF.

    mo_splitter_parent->set_column_width( EXPORTING id                = 3
                                                    width             = 40   ).

    RAISE EVENT on_change_mode
      EXPORTING
        iv_mode = 'DETAIL'.


    mo_view_expressao->set_session_data( ms_lista_custo ).
    mo_view_expressao->update( ir_data = REF #( is_data ) ).

  ENDMETHOD.


  METHOD show_treeview.

    mo_splitter_parent->set_column_width( EXPORTING id                = 1
                                                    width             = 18   ).

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


  METHOD update_header.

    DATA(lt_data) = mo_model->get_lista_custo( mv_cod_lista_custo ).

    IF lines( lt_data ) > 0.
      ms_lista_custo = lt_data[ 1 ].
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_lista_custo_hdr->update( lr_data ).

  ENDMETHOD.


  METHOD update_items.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    CHECK NOT ms_lista_custo-content IS INITIAL.
    lr_data = REF #( ls_data ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_lista_custo-content
                                         CHANGING  cr_data = lr_data ).


*    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ms_lista_custo-id_simulacao ).
    mo_view_items->set_session_data( iv_previous_fcat = mv_previous_fcat
                                     is_lista_custo = ms_lista_custo
                                     is_exibicao = VALUE /qaps/s_lista_custo_exibicao( valor = mv_valor
                                                                                       moeda = mv_moeda ) ).

    mo_view_items->set_data( lr_data ).

    CLEAR mv_previous_fcat.

  ENDMETHOD.


  METHOD update_tree.

    DATA(lt_data) = mo_model->get_lista_custo( ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_lista_custo->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
