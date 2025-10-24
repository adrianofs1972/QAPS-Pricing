class /QAPS/CL_VIEW_SIMP_TIPO_LISTA definition
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
  methods CUSTOMIZE_TOOLBAR
    redefinition .
  methods FUNCTION_SELECTED
    redefinition .
  methods NODE_DOUBLE_CLICK
    redefinition .
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_SIMP_TIPO_LISTA IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_tp_lista.
    DATA: lv_icon     TYPE tv_image,
          lv_node_key TYPE tm_nodekey.

    ASSIGN ir_line->* TO <fs_line>.

    CASE <fs_line>-ativo.
      WHEN 'X'. lv_icon = icon_green_light.
      WHEN ''. lv_icon = icon_red_light.
    ENDCASE.

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).
    lv_node_key = lines( mt_nodes ) + 1.
    APPEND VALUE ts_line_key( key     = <fs_line>-id_tp_lista
                              text    = <fs_line>-descricao
                              is_root = abap_false
                              node_key = lv_node_key
                              content    = lv_xml ) TO mt_nodes.

    mo_tree->add_node( EXPORTING node_key          = lv_node_key "CONV #( <fs_line>-id_tp_lista )
                                 relative_node_key = 'Root'
                                 isfolder          = ''
                                 text              = CONV #( <fs_line>-descricao )
                                 image             = lv_icon ).



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
      quickinfo = 'Add Tipo de Lista' ).                    "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'REMOVE'
        icon      = icon_delete_row
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Remove Tipo de Lista' ).               "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = '&SEP'
        icon = ''
        butn_type = cntb_btype_sep
        text      = ''  ).                                  "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'ACTIVE'
        icon      = icon_activate
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Ativar Tipo de Lista' ).               "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'INACTIVE'
        icon      = icon_deactivate
        butn_type = cntb_btype_button
        text      = ''
        quickinfo = 'Desativar Tipo de Lista' ).               "#EC NOTEXT

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

    IF ( NOT ls_data IS INITIAL AND ls_data-key <> 'Root' ) OR
        fcode = 'INSERT'.

      RAISE EVENT on_function_selected
        EXPORTING
          iv_function = fcode
          iv_xml_data = ls_data-content.
    ELSE.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD node_double_click.
    CHECK node_key <> 'Root'.

    data(lv_content) = mt_nodes[ node_key = node_key ]-content.

    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key = node_key
        iv_xml_data = lv_content.

  ENDMETHOD.


  method UPDATE.

    FIELD-SYMBOLS <ft> TYPE /qaps/t_tp_lista." ANY TABLE.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    LOOP AT <ft> REFERENCE INTO data(lr_line).
      add_child_node( lr_line ).
    ENDLOOP.

  endmethod.
ENDCLASS.
