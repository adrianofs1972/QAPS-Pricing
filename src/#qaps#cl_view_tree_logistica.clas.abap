class /QAPS/CL_VIEW_TREE_LOGISTICA definition
  public
  inheriting from /QAPS/CL_VIEW_SIMPLE_TREE_BASE
  final
  create public .

public section.

  methods SET_PLANTA_MENU_DISTRIBUICAO
    importing
      !IT_DATA type /QAPS/T_CENTRO .

  methods UPDATE
    redefinition .
protected section.

  methods ADD_CHILD_NODE
    redefinition .
  methods CUSTOMIZE_TOOLBAR
    redefinition .
  methods FUNCTION_SELECTED
    redefinition .
  methods NODE_DOUBLE_CLICK
    redefinition .
private section.

  data MT_CENTROS type /QAPS/T_CENTRO .
ENDCLASS.



CLASS /QAPS/CL_VIEW_TREE_LOGISTICA IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE ts_tree_line.

    ASSIGN ir_line->* TO <fs_line>.

    DATA(lv_parent_key) = COND #( WHEN NOT <fs_line>-parent_key IS INITIAL
                                                             THEN <fs_line>-parent_key
                                                             ELSE 'Root' ).

    mo_tree->add_node( EXPORTING node_key          = <fs_line>-node_key
                                 relative_node_key = lv_parent_key
                                 isfolder          = ''
                                 text              = <fs_line>-text
                                 image             = <fs_line>-image
                                 expanded_image    = <fs_line>-image ).

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).
    APPEND VALUE ts_line_key( key     = <fs_line>-node_key
                              text    = <fs_line>-text
                              is_root = abap_false
                              no_event = <fs_line>-no_event
                              source = <fs_line>-source
                              content    = lv_xml ) TO mt_nodes.

    check iv_expand_parent = abap_true.

    mo_tree->expand_node(
     EXPORTING
       node_key            = lv_parent_key ).   " Node key
*        expand_predecessors =     " 'X': Expand Predecessor of Node
*        expand_subtree      =     " 'X': Expand all Subsequent Nodes
*        level_count         =     " Number of Lower Levels to be Expanded
*      EXCEPTIONS
*        node_not_found      = 1
*        others              = 2


  ENDMETHOD.


  METHOD CUSTOMIZE_TOOLBAR.

    mo_toolbar->add_button(
    EXPORTING
      fcode     = 'INSERT'
      icon      = icon_insert_row
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Add Grupo de Produto' ).                    "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'REMOVE'
        icon      = icon_delete_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Remove Grupo de Produto' ).               "#EC NOTEXT

  ENDMETHOD.


  METHOD FUNCTION_SELECTED.

    mo_tree->get_selected_node(
      IMPORTING
        node_key                   =  DATA(lv_key)   " Key of Selected Node
*       EXCEPTIONS
*         control_not_existing       = 1
*         control_dead               = 2
*         cntl_system_error          = 3
*         failed                     = 4
*         single_node_selection_only = 5
*         others                     = 6
    ).

    data(ls_data) = value #( mt_nodes[ key = lv_key ] OPTIONAL ).
    RAISE EVENT on_function_selected
      EXPORTING
        iv_function = fcode
        iv_xml_data = ls_data-content.

  ENDMETHOD.


  METHOD node_double_click.
    CHECK node_key <> 'Root'.

    DATA(ls_node) = mt_nodes[ key = node_key ].

    CHECK NOT ls_node-source IS INITIAL
      and ls_node-no_event = abap_false.

*    BREAK c060863.
    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key         = node_key
        iv_source           = ls_node-source
        iv_additional_data  = conv #( ls_node-text ).

  ENDMETHOD.


  METHOD set_planta_menu_distribuicao.
    mt_centros = it_data.
  ENDMETHOD.


  METHOD update.

    DATA: lt_nodes TYPE tt_tree_line,
          lt_child TYPE tt_tree_line.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '1' )
                               text     = 'Cadastros Básicos'
                               image    = CONV #( icon_gis_promote ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '2' )
                               text     = 'Rotas'
                               image    = CONV #( icon_foreign_trade ) ) TO lt_nodes.

*    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '3' )
*                               text     = 'Fretes'
*                               image    = CONV #( icon_translation_show ) ) TO lt_nodes.


    SORT lt_nodes BY node_key DESCENDING.

    LOOP AT lt_nodes REFERENCE INTO DATA(lr_line).
      add_child_node( lr_line ).
    ENDLOOP.

    REFRESH lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '11' )
                               parent_key = '1'
                               text     = 'Regiões'
                               source   = 'REGIAO'
                               image    = CONV #( icon_gis_promote ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '12' )
                               parent_key = '1'
                               text     = 'Cidades'
                               source   = 'CIDADE'
                               image    = CONV #( icon_foreign_trade ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '13' )
                               parent_key = '1'
                               text     = 'Grupo de Plantas'
                               source   = 'GRP_PLANTA'
                               image    = CONV #( icon_stock ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '14' )
                               parent_key = '1'
                               text     = 'Centros'
                               source   = 'CENTRO'
                               image    = CONV #( icon_plant ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '15' )
                               parent_key = '1'
                               text     = 'Portos'
                               source   = 'PORTO'
                               image    = CONV #( icon_ws_ship ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '16' )
                               parent_key = '1'
                               text     = 'Terminal Portuário'
                               source   = 'CAIS'
                               image    = CONV #( icon_store_location ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '17' )
                               parent_key = '1'
                               text     = 'Grupo de Clientes'
                               source   = 'GRP_CLIENTE'
                               image    = CONV #( icon_partner ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '18' )
                               parent_key = '1'
                               text     = 'Clientes'
                               source   = 'CLIENTE'
                               image    = CONV #( icon_customer ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '19' )
                               parent_key = '1'
                               text     = 'Fornecedores'
                               source   = 'FORNECEDOR'
                               image    = CONV #( icon_employee ) ) TO lt_nodes.

    "----------------------------------------------------------

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '24' )
                           parent_key = '2'
                           text     = 'Distribuição Logística'
                           source   = 'CENTRO_PORTO'
                           image    = CONV #( icon_distribute )
                           no_event = 'X' ) TO lt_nodes.
    "----------------------------------------------------------
*    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '31' )
*                               parent_key = '3'
*                               text     = 'Pontos'
*                               source   = 'PONTO'
*                                image    = CONV #( icon_transport_point ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '25' )
                               parent_key = '2'
                               text     = 'Trechos'
                               source   = 'TRECHO'
                               image    = CONV #( icon_draw_polyline ) ) TO lt_nodes.

    APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( '26' )
                               parent_key = '2'
                               text     = 'Trajetos'
                               source   = 'TRAJETO'
                               image    = CONV #( icon_transportation_mode ) ) TO lt_nodes.





    SORT lt_nodes BY node_key DESCENDING.

    LOOP AT lt_nodes REFERENCE INTO lr_line.
      add_child_node( lr_line ).
    ENDLOOP.

    SORT mt_centros BY werks ASCENDING.
    LOOP AT mt_centros INTO DATA(ls_centro).

      DATA(lv_node_key) = `24` && sy-tabix.

      APPEND VALUE ts_tree_line( node_key = CONV tm_nodekey( lv_node_key )
                           parent_key = '24'
                           text     = ls_centro-werks" && ` - ` && ls_centro-dsc_werks
                           source   = 'CENTRO_PORTO'
                           image    = CONV #( icon_column_right ) ) TO lt_child.

    ENDLOOP.

*    SORT lt_child  BY node_key DESCENDING.
    SORT lt_child  BY text DESCENDING.

    LOOP AT lt_child REFERENCE INTO lr_line.
      add_child_node( ir_line = lr_line
                      iv_expand_parent = abap_false ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
