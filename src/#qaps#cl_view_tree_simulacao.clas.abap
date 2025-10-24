class /QAPS/CL_VIEW_TREE_SIMULACAO definition
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
private section.
ENDCLASS.



CLASS /QAPS/CL_VIEW_TREE_SIMULACAO IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_simulacao.
    DATA: lv_icon TYPE tv_image.

    ASSIGN ir_line->* TO <fs_line>.

    CASE <fs_line>-status.
      WHEN 'A'. lv_icon = icon_green_light.
      WHEN 'F'. lv_icon = icon_locked.
      WHEN 'E'. lv_icon = icon_complete.
    ENDCASE.

    data(lv_id) = |{ <fs_line>-id_simulacao ALPHA = out }|.
    CONDENSE lv_id.
    DATA(lv_node_text) = |{ lv_id } { <fs_line>-descricao }|.

    mo_tree->add_node( EXPORTING node_key          = CONV #( <fs_line>-id_simulacao )
                                 relative_node_key = 'Root'
                                 isfolder          = ''
                                 text              = CONV #( lv_node_text )
                                 image             = lv_icon ).



    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).

    APPEND VALUE ts_line_key( key     = <fs_line>-id_simulacao
                              text    = lv_node_text "<fs_line>-descricao
                              is_root = abap_false
                              content    = lv_xml ) TO mt_nodes.

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

    DATA(ls_data) = VALUE #( mt_nodes[ key = lv_key ] OPTIONAL ).

    IF ( NOT ls_data IS INITIAL and ls_data-key <> 'Root' ) or
        fcode = 'INSERT'.

      RAISE EVENT on_function_selected
        EXPORTING
          iv_function = fcode
          iv_xml_data = ls_data-content.
    ELSE.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD NODE_DOUBLE_CLICK.
    CHECK node_key <> 'Root'.

    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key = node_key
        iv_xml_data = mt_nodes[ key = node_key ]-content.

  ENDMETHOD.


  method UPDATE.

    FIELD-SYMBOLS <ft> TYPE /qaps/t_simulacao." ANY TABLE.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    sort <ft> by id_simulacao DESCENDING.

    LOOP AT <ft> REFERENCE INTO data(lr_line).
      add_child_node( lr_line ).
    ENDLOOP.

  endmethod.
ENDCLASS.
