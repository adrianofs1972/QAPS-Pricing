class /QAPS/CL_VIEW_SIMP_GRP_PRODUTO definition
  public
  inheriting from /QAPS/CL_VIEW_SIMPLE_TREE_BASE
  final
  create public .

public section.

  methods UPDATE
    redefinition .
protected section.

  methods ADD_CHILD_NODE
    redefinition .
  methods FUNCTION_SELECTED
    redefinition .
  methods NODE_DOUBLE_CLICK
    redefinition .
  methods CUSTOMIZE_TOOLBAR
    redefinition .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_SIMP_GRP_PRODUTO IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_grupo_produto.

    ASSIGN ir_line->* TO <fs_line>.

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).
    DATA(lv_lines) = lines( mt_nodes ) + 1.
    APPEND VALUE ts_line_key( key      = lv_lines "<fs_line>-id_grupo_produto
                              node_key = conv #( lv_lines )
                              text     = <fs_line>-descricao
                              is_root  = abap_false
                              content  = lv_xml ) TO mt_nodes.

    mo_tree->add_node( EXPORTING node_key          = conv #( lv_lines ) "CONV #( <fs_line>-id_grupo_produto )
                                 relative_node_key = 'Root'
                                 isfolder          = ''
                                 text              = CONV #( <fs_line>-descricao )
                                 image             = CONV #( icon_packing ) ).


    mo_tree->expand_node(
     EXPORTING
       node_key            = 'Root'    " Node key
*        expand_predecessors =     " 'X': Expand Predecessor of Node
*        expand_subtree      =     " 'X': Expand all Subsequent Nodes
*        level_count         =     " Number of Lower Levels to be Expanded
*      EXCEPTIONS
*        node_not_found      = 1
*        others              = 2
   ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD customize_toolbar.

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'INSERT'
        icon      = icon_insert_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Add Grupo de Produto' ).                   "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'REMOVE'
        icon      = icon_delete_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Remove Grupo de Produto' ).            "#EC NOTEXT

  ENDMETHOD.


  METHOD function_selected.

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

    DATA(ls_data) = VALUE #( mt_nodes[ node_key = lv_key ] OPTIONAL ).
    RAISE EVENT on_function_selected
      EXPORTING
        iv_function = fcode
        iv_xml_data = ls_data-content.

  ENDMETHOD.


  METHOD NODE_DOUBLE_CLICK.
    CHECK node_key <> 'Root'.

    data(ls_data) = mt_nodes[ node_key = node_key ]."-content

    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key = node_key
        iv_xml_data = ls_data-content.

  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS <ft> TYPE /qaps/t_grupo_produto.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    LOOP AT <ft> REFERENCE INTO DATA(lr_line).
      add_child_node( lr_line ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
