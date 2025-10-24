class /QAPS/CL_VIEW_TREE_MAT_SIMUL definition
  public
  inheriting from /QAPS/CL_VIEW_SIMPLE_TREE_BASE
  final
  create public .

public section.

  methods RESET .
  methods UPDATE_NODE
    importing
      !IT_DATA type /QAPS/T_MATRIZ_HEADER .

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

  constants MC_GUID_NULL type GUID16 value '00000000000000000000000000000000' ##NO_TEXT.
  data MV_DESTINO type NUMC3 .

  methods ADD_CHILD_NODE_DESTINO
    importing
      !IR_LINE type ref to DATA
      !IV_EXPAND_PARENT type ABAP_BOOL default ABAP_TRUE
      !IV_PARENT type TM_NODEKEY .
  methods ADD_CHILD_NODE_TIPO
    importing
      !IR_LINE type ref to DATA
      !IV_EXPAND_PARENT type ABAP_BOOL default ABAP_TRUE
      !IV_PARENT type TM_NODEKEY
    returning
      value(RETURN) type TM_NODEKEY .
  methods ADD_CHILD_NODE_SIMULACAO
    importing
      !IR_LINE type ref to DATA
      !IV_EXPAND_PARENT type ABAP_BOOL default ABAP_TRUE
    returning
      value(RETURN) type TM_NODEKEY .
ENDCLASS.



CLASS /QAPS/CL_VIEW_TREE_MAT_SIMUL IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_mat_hdr."/qaps/v_mat_hdr.

    ASSIGN ir_line->* TO <fs_line>.

    DATA(lv_simulacao) = add_child_node_simulacao( EXPORTING ir_line  = ir_line ).

    DATA(lv_grupo)     = add_child_node_tipo( EXPORTING ir_line  = ir_line
                                                        iv_parent = lv_simulacao ).

    IF NOT ( NOT <fs_line>-id_grp_planta IS INITIAL AND <fs_line>-id_centro = mc_guid_null ).
      add_child_node_destino( EXPORTING ir_line  = ir_line
                                        iv_parent = lv_grupo ).
    ENDIF.

  ENDMETHOD.


  METHOD add_child_node_destino.

    DATA lv_nodekey TYPE tm_nodekey.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_mat_hdr."/qaps/v_mat_hdr.
    DATA: lv_icon          TYPE tv_image,
          lv_icon_expanded TYPE tv_image.

    ASSIGN ir_line->* TO <fs_line>.

    IF <fs_line>-id_parent IS INITIAL.
      IF <fs_line>-red = 'X'.
        lv_icon = icon_red_light.
      ELSEIF <fs_line>-yellow = 'X'.
        lv_icon = icon_yellow_light.
      ELSEIF <fs_line>-green = 'X'.
        lv_icon = icon_green_light.
      ENDIF.
    ELSE.
      lv_icon = icon_locked.
    ENDIF.

    DATA(lv_id) = |{ <fs_line>-id_simulacao ALPHA = OUT }|.
    CONDENSE lv_id.

    mv_destino = mv_destino + 1.
    lv_nodekey = |{ iv_parent }_{ mv_destino }|.
    DATA(lv_node_text) = | { <fs_line>-werks }|.

    mo_tree->get_all_node_keys( IMPORTING node_key_table = DATA(lt_keys) ).


    mo_tree->add_node( EXPORTING node_key          = lv_nodekey
                                 relative_node_key = iv_parent
                                 isfolder          = ''
                                 text              = CONV #( lv_node_text )
                                 image             = lv_icon
                                 expanded_image    = lv_icon  ).


    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).

    APPEND VALUE ts_line_key( key     = lv_nodekey
                              node_key = lv_nodekey
                              text    = lv_node_text
                              is_root = abap_false
                              content    = lv_xml
                              trigger_event = abap_true
                              guid = <fs_line>-id_matriz_abast ) TO mt_nodes.


  ENDMETHOD.


  METHOD add_child_node_simulacao.

    DATA lv_nodekey TYPE tm_nodekey.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_mat_hdr."/qaps/s_matriz_header.
    DATA: lv_icon TYPE tv_image.

    ASSIGN ir_line->* TO <fs_line>.

    case <fs_line>-status.
      when 'A'.
        lv_icon = icon_green_light.
      when OTHERS.
        lv_icon = icon_locked.
     ENDCASE.

    DATA(lv_id) = |{ <fs_line>-id_simulacao ALPHA = OUT }|.
    CONDENSE lv_id.
    lv_nodekey = |SIM#{ lv_id }|.
    DATA(lv_node_text) = |{ lv_id } { <fs_line>-dsc_simulacao }|.

    mo_tree->get_all_node_keys( IMPORTING node_key_table = DATA(lt_keys) ).

    IF NOT line_exists( lt_keys[ table_line = lv_nodekey ] ).

      mo_tree->add_node( EXPORTING node_key          = lv_nodekey
                                   relative_node_key = 'Root'
                                   isfolder          = ''
                                   text              = CONV #( lv_node_text )
                                   image             = lv_icon
                                   expanded_image    = lv_icon ).



      DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).

      APPEND VALUE ts_line_key( key     = lv_nodekey
                                text    = lv_node_text "<fs_line>-descricao
                                is_root = abap_false
                                content    = lv_xml ) TO mt_nodes.

      mo_tree->expand_node( EXPORTING node_key = 'Root' ).
      return = lv_nodekey.
    ENDIF.

    return = lv_nodekey.


  ENDMETHOD.


  METHOD add_child_node_tipo.

    DATA: lv_nodekey       TYPE tm_nodekey,
          lv_trigger_event TYPE abap_bool.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_mat_hdr."/qaps/s_matriz_header./qaps/s_matriz_header.
    DATA: lv_icon          TYPE tv_image,
          lv_icon_expanded TYPE tv_image.

    ASSIGN ir_line->* TO <fs_line>.


    DATA(lv_id) = |{ <fs_line>-id_simulacao ALPHA = OUT }|.
    CONDENSE lv_id.

*    DATA(lv_status) = <fs_line>-soma.

    IF NOT <fs_line>-codigo IS INITIAL.

      IF <fs_line>-red = 'X'.
        lv_icon = icon_closed_folder_orphaned.
      ELSEIF <fs_line>-green = 'X'.
        lv_icon = icon_closed_folder_uptodate.
      ELSE.
        lv_icon = icon_closed_folder_outdate.
      ENDIF.

*      lv_icon_expanded = icon_stock.
      lv_nodekey = |{ lv_id }#TP#{ <fs_line>-codigo }|.
      DATA(lv_node_text) = | { <fs_line>-codigo }|.
      lv_trigger_event = abap_true.
    ELSE.
      lv_icon = icon_plant.
      lv_icon_expanded = icon_plant.
      lv_nodekey = |{ lv_id }#TP#9999|.
      lv_node_text = 'Centros'.
      lv_trigger_event = abap_false.
    ENDIF.

    mo_tree->get_all_node_keys( IMPORTING node_key_table = DATA(lt_keys) ).

*    BREAK-POINT.

    IF NOT line_exists( lt_keys[ table_line = lv_nodekey ] ).

      mo_tree->add_node( EXPORTING node_key          = lv_nodekey
                                   relative_node_key = iv_parent
                                   isfolder          = ''
                                   text              = CONV #( lv_node_text )
                                   image             = lv_icon
                                   expanded_image    = lv_icon ).



      DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).

      APPEND VALUE ts_line_key( key     = lv_nodekey
                                node_key = lv_nodekey
                                text    = lv_node_text "<fs_line>-descricao
                                is_root = abap_false
                                content    = lv_xml
                                trigger_event = lv_trigger_event
                                guid = COND #( WHEN NOT <fs_line>-codigo IS INITIAL
                                                THEN <fs_line>-id_matriz_abast
                                                ELSE '' ) ) TO mt_nodes.

      mo_tree->expand_node( EXPORTING node_key = 'Root' ).
      return = lv_nodekey.
    ENDIF.

    return = lv_nodekey.


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


  METHOD node_double_click.

*    CHECK node_key <> 'Root'.

    DATA(ls_nodes) = mt_nodes[ key = node_key ].

    CHECK ls_nodes-trigger_event = abap_true.

    mo_tree->node_get_text( EXPORTING node_key       =  node_key   " Node key
                            IMPORTING text           =  DATA(lv_texto) ).

    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key = ls_nodes-node_key
        iv_guid     = ls_nodes-guid
        iv_xml_data = ls_nodes-content
        iv_texto    = lv_texto
        iv_additional_data = CONV #( ls_nodes-tipo ).

  ENDMETHOD.


  METHOD reset.
    REFRESH mt_nodes.
    mo_tree->delete_all_nodes( ).
  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS <ft> TYPE /qaps/t_matriz_header.

    mo_tree->delete_all_nodes( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    SORT <ft> BY id_simulacao DESCENDING werks ASCENDING.

    LOOP AT <ft> REFERENCE INTO DATA(lr_line).
      add_child_node( lr_line ).
    ENDLOOP.

  ENDMETHOD.


  METHOD update_node.

    DATA: lv_icon          TYPE tv_image,
          lv_icon_expanded TYPE tv_image.

*    BREAK-POINT.

    LOOP AT it_data INTO DATA(ls_data).

      DATA(ls_node) = mt_nodes[ guid = ls_data-id_matriz_abast ].

      CHECK ls_node-trigger_event = 'X'.

      IF ls_data-id_centro <> '00000000000000000000000000000000'.

        IF ls_data-red = 'X'.
          lv_icon = icon_red_light.
        ELSEIF ls_data-yellow = 'X'.
          lv_icon = icon_yellow_light.
        ELSEIF ls_data-green = 'X'.
          lv_icon = icon_green_light.
        ENDIF.

      ELSE.

        IF ls_data-red = 'X'.
          lv_icon = icon_closed_folder_orphaned.
        ELSEIF ls_data-green = 'X'.
          lv_icon = icon_closed_folder_uptodate.
        ELSE.
          lv_icon = icon_closed_folder_outdate.
        ENDIF.

      ENDIF.

      mo_tree->node_set_image(  node_key       = ls_node-node_key
                                image          = lv_icon ).

      mo_tree->node_set_expanded_image( node_key  = ls_node-node_key
                                        exp_image = lv_icon ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
