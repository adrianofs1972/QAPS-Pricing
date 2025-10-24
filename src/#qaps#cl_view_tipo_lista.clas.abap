class /QAPS/CL_VIEW_TIPO_LISTA definition
  public
  inheriting from /QAPS/CL_VIEW_TREE_BASE
  final
  create public .

public section.
protected section.

  methods ADD_CHILD_NODE
    redefinition .
  methods CUSTOMIZE_CATALOG
    redefinition .
  methods GET_HIERARCHY_HEADER
    redefinition .
  methods ITEM_DOUBLE_CLICK
    redefinition .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_TIPO_LISTA IMPLEMENTATION.


  METHOD add_child_node.

    DATA lv_key TYPE lvc_nkey.
    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_tp_lista.

*    BREAK-POINT.

    DATA: l_node_text TYPE lvc_value.
    DATA: lt_item_layout TYPE lvc_t_layi,
          ls_item_layout TYPE lvc_s_layi.

    ASSIGN ir_line->* TO <fs_line>.

    ls_item_layout-fieldname = mo_tree->c_hierarchy_column_name.
    ls_item_layout-style     = cl_gui_column_tree=>style_default.
    APPEND ls_item_layout TO lt_item_layout.

* add node
    l_node_text =  <fs_line>-descricao.

    DATA: ls_node TYPE lvc_s_layn.
    ls_node-n_image   = space.
    ls_node-exp_image = space.
    ls_node-isfolder = ''.
    ls_node-expander = ''.
*    ls_node-

    CALL METHOD mo_tree->add_node
      EXPORTING
        i_relat_node_key = mv_root_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_outtab_line   = <fs_line>
        is_node_layout   = ls_node
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = lv_key.

    APPEND VALUE ts_line_key( key     = lv_key
                              text    = l_node_text ) TO mt_nodes.

  ENDMETHOD.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname <> 'DESCRICAO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-hotspot = 'X'.
    ENDLOOP.

  ENDMETHOD.


  method GET_HIERARCHY_HEADER.

    return-heading = 'Tipo de Lista'.
    return-tooltip = 'Tipo de Lista'.
    return-width = 50.
    return-width_pix = ''.

  endmethod.


  method ITEM_DOUBLE_CLICK.
    BREAK-POINT.
  endmethod.
ENDCLASS.
