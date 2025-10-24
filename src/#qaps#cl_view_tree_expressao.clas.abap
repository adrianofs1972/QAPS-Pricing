class /QAPS/CL_VIEW_TREE_EXPRESSAO definition
  public
  inheriting from /QAPS/CL_VIEW_SIMPLE_TREE_BASE
  final
  create public .

public section.

  events ON_CHANGE_PARENT
    exporting
      value(IV_ID_NEW_PARENT) type /QAPS/ED_ID_CALC_NODE
      value(IV_XML_DATA) type STRING .
  events ON_NODE_REMOVE
    exporting
      value(IV_XML_DATA) type STRING
      value(IV_ACTION) type CHAR1
      value(IV_TIPO) type STRING
      value(IV_FCODE) type SY-UCOMM .
  events ON_NODE_ADDED
    exporting
      value(IV_XML_DATA) type STRING
      value(IV_ACTION) type CHAR1
      value(IV_TIPO) type STRING .

  methods UPDATE_NODE_CONTENT
    importing
      !IS_DATA type /QAPS/S_CALCULATION_NODE .

  methods ADD_NEW_CHILD_NODE
    redefinition .
  methods REMOVE_NODE
    redefinition .
  methods UPDATE
    redefinition .
protected section.

  methods ADD_CHILD_NODE
    redefinition .
  methods CUSTOMIZE_TOOLBAR
    redefinition .
  methods DRAG
    redefinition .
  methods DROP
    redefinition .
  methods FUNCTION_SELECTED
    redefinition .
  methods NODE_CONTEXT_MENU_REQUEST
    redefinition .
  methods NODE_CONTEXT_MENU_SELECT
    redefinition .
  methods NODE_DOUBLE_CLICK
    redefinition .
  methods DROP_COMPLETE
    redefinition .
private section.

  data MV_HANDLE_DRAG_DROP type I .
  data MO_BEHAVIOUR_SRC type ref to CL_DRAGDROP .
  data MO_BEHAVIOUR_TRG type ref to CL_DRAGDROP .
  data MV_HANDLE_DROP type I .

  methods INITIALIZE_DRAG_AN_DROP .
  methods SORT_TABLE
    changing
      !CT_DATA type /QAPS/T_CALCULATION_NODE .
ENDCLASS.



CLASS /QAPS/CL_VIEW_TREE_EXPRESSAO IMPLEMENTATION.


  METHOD add_child_node.

    FIELD-SYMBOLS <fs_line> TYPE /qaps/s_calculation_node.
    DATA: lv_icon      TYPE tv_image,
          lv_parent    TYPE tm_nodekey,
          lv_next_node TYPE tm_nodekey,
          lv_handle    TYPE i.


    ASSIGN ir_line->* TO <fs_line>.

    CASE <fs_line>-tipo_node.
      WHEN 'R'.
        lv_icon = icon_simulate.
        DATA(lv_node_text) = <fs_line>-descricao.
      WHEN 'C'.
        IF <fs_line>-price_field = 'X'.
          lv_icon = icon_graphics.
          lv_handle = mv_handle_drop.
        ELSE.
          lv_icon = icon_calculation.
          lv_handle = mv_handle_drag_drop.
        ENDIF.
        lv_node_text = <fs_line>-descricao.
      WHEN 'E'.
        lv_icon = icon_sym_real_server.
        lv_node_text = <fs_line>-dsc_custo_elementar.
    ENDCASE.

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_line ).

    DATA(lv_lines) = lines( mt_nodes ).
    lv_next_node = lv_lines + 1.

    APPEND VALUE ts_line_key( key     = lv_next_node
                              guid    = <fs_line>-id_calc_node
                              text    = lv_node_text
                              is_root = abap_false
                              content = lv_xml
                              tipo    = <fs_line>-tipo_node
                              node_key = lv_next_node ) TO mt_nodes.

    IF NOT <fs_line>-id_parent_node IS INITIAL.
      DATA(ls_node) = mt_nodes[ guid = <fs_line>-id_parent_node ].
      lv_parent = ls_node-node_key.
    ELSE.
      lv_parent = 'Root'.
    ENDIF.

    mo_tree->add_node( EXPORTING node_key          = CONV #( lv_next_node )
                                 relative_node_key = CONV #( lv_parent )
                                 isfolder          = ''
                                 text              = CONV #( lv_node_text )
                                 image             = lv_icon
                                 drag_drop_id      =  lv_handle
                                 expanded_image    = lv_icon ).





    mo_tree->expand_node(
     EXPORTING
       node_key            = lv_parent "'Root'    " Node key
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


  METHOD add_new_child_node.

    add_child_node( ir_line ).

  ENDMETHOD.


  METHOD customize_toolbar.

    mo_toolbar->add_button(
      EXPORTING
        icon =  ''
        fcode            = '&SEP001'
        butn_type        =  cntb_btype_sep   " Button Types Defined in CNTB
    ).

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'CALCULADA'
        icon      = icon_calculation
        butn_type = cntb_btype_button
*        text      = 'Var. Calculada'
        quickinfo = 'Criar Nova Variável Calculada' ).      "#EC NOTEXT

    mo_toolbar->add_button(
      EXPORTING
        icon =  ''
        fcode            = '&SEP002'
        butn_type        =  cntb_btype_sep   " Button Types Defined in CNTB
    ).

    mo_toolbar->add_button(
      EXPORTING
        fcode     = 'ELEMENTAR'
        icon      = icon_sym_real_server
        butn_type = cntb_btype_button
*        text      = 'Var. Elementar'
        quickinfo = 'Vincular Variável Elementar' ).        "#EC NOTEXT

  ENDMETHOD.


  METHOD drag.

    DATA: left_node_text TYPE tm_nodetxt,
          lo_dataobj     TYPE REF TO /qaps/cl_drag_drop_dataobject.

    DATA(ls_node) = mt_nodes[ node_key = node_key ].

    lo_dataobj = new /qaps/cl_drag_drop_dataobject( ).
    lo_dataobj->node_key = node_key.
    lo_dataobj->xml_content = ls_node-content.

    drag_drop_object->object = lo_dataobj.

  ENDMETHOD.


  METHOD drop.

    DATA: drag_object TYPE REF TO /qaps/cl_drag_drop_dataobject,
          lr_data     TYPE REF TO data,
          ls_node     TYPE /qaps/s_calculation_node.

    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.
      drag_object ?= drag_drop_object->object.
    ENDCATCH.
    IF sy-subrc = 1.
      drag_drop_object->abort( ).
      EXIT.
    ENDIF.

    DATA(ls_content) = mt_nodes[ node_key = node_key ].

    lr_data = REF #( ls_node ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml = ls_content-content
                                         CHANGING cr_data = lr_data ).


    RAISE EVENT on_change_parent
      EXPORTING
        iv_id_new_parent = ls_node-id_calc_node
        iv_xml_data      = drag_object->xml_content.

*    BREAK-POINT.

  ENDMETHOD.


  method DROP_COMPLETE.
*    BREAK-POINT.
*CALL METHOD SUPER->DROP_COMPLETE
*  EXPORTING
*    NODE_KEY         =
*    DRAG_DROP_OBJECT =
*    .
  endmethod.


  METHOD function_selected.

*    DATA lv_code TYPE sy-ucomm.

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

*    BREAK-POINT.

    DATA(ls_data) = VALUE #( mt_nodes[ key = lv_key ] OPTIONAL ).

    IF  ls_data-tipo = 'C'.

*      case fcode.
*        when ''. lv_code = ''.
*        when ''. lv_code = ''.
*      endcase.

      RAISE EVENT on_node_added
        EXPORTING
          iv_xml_data = ls_data-content
          iv_action   = 'C'
          iv_tipo     = CONV #( fcode ).

    ELSE.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_drag_an_drop.

    CHECK NOT mo_behaviour_src IS BOUND.
    CREATE OBJECT: mo_behaviour_src,
                   mo_behaviour_trg.

    mo_behaviour_src->add(
      EXPORTING
        flavor          = 'SOURCE'
        dragsrc         = 'X'
        droptarget      = 'X'
        effect          = cl_dragdrop=>move ).

    mo_behaviour_src->get_handle( IMPORTING handle = mv_handle_drag_drop ).

    mo_behaviour_trg->add(
     EXPORTING
       flavor          = 'SOURCE'
       dragsrc         = ''
       droptarget      = 'X'
       effect          = cl_dragdrop=>move ).

    mo_behaviour_trg->get_handle( IMPORTING handle = mv_handle_drop ).


  ENDMETHOD.


  METHOD node_context_menu_request.

    DATA: ls_data TYPE /qaps/calc_node,
          lr_data TYPE REF TO data.

    DATA(ls_node) = mt_nodes[ key = node_key ].

    lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ls_node-content
                                         CHANGING  cr_data = lr_data ).

    CASE ls_node-tipo.
      WHEN 'C'.
        CALL METHOD menu->add_function
          EXPORTING
            fcode = 'CALCULADA'
            text  = 'Criar Nova Variável Calculada'
            icon  = icon_calculation.

        CALL METHOD menu->add_function
          EXPORTING
            fcode = 'ELEMENTAR'
            text  = 'Vincular Variável Elementar'
            icon  = icon_element.

        IF ls_data-price_field IS INITIAL.

          CALL METHOD menu->add_function
            EXPORTING
              fcode = 'EXCLUIR'
              text  = 'Excluir Variável Calculada'
              icon  = icon_calculation.

        ENDIF.

      WHEN 'E'.

        CALL METHOD menu->add_function
          EXPORTING
            fcode = 'DESVINCULAR'
            text  = 'Desvincular Variável Elementar'
            icon  = icon_element.
    ENDCASE.

  ENDMETHOD.


  METHOD node_context_menu_select.

    DATA(ls_node) = mt_nodes[ node_key = node_key ].

    CASE fcode.
      WHEN 'CALCULADA' OR 'ELEMENTAR'.
        RAISE EVENT on_node_added
          EXPORTING
            iv_xml_data = ls_node-content
            iv_action   = 'C'
            iv_tipo     = CONV #( fcode ).
      WHEN 'EXCLUIR' OR 'DESVINCULAR'.
        RAISE EVENT on_node_remove
         EXPORTING
           iv_xml_data = ls_node-content
           iv_action   = 'C'
           iv_tipo     = CONV #( fcode )
           iv_fcode       = fcode.
    ENDCASE.

  ENDMETHOD.


  METHOD node_double_click.

    DATA(ls_node) = VALUE #( mt_nodes[ key = node_key ] OPTIONAL ).

    CHECK ls_node-tipo = 'C' OR ls_node-tipo = 'E'.

    RAISE EVENT on_node_double_click
      EXPORTING
        iv_node_key = node_key
        iv_xml_data = mt_nodes[ key = node_key ]-content.

  ENDMETHOD.


  METHOD remove_node.

    FIELD-SYMBOLS <fs_data> TYPE /qaps/calc_node.

    ASSIGN ir_line->* TO <fs_data>.

    DATA(ls_node) = mt_nodes[ guid = <fs_data>-id_calc_node ].
    mo_tree->delete_node( EXPORTING node_key = ls_node-node_key    ).

  ENDMETHOD.


  METHOD sort_table.

    DATA: lt_src TYPE /qaps/t_calculation_node,
          lt_trg TYPE /qaps/t_calculation_node,
          lv_qty type i.

    lt_src = ct_data.
    REFRESH ct_data.
    DATA(ls_root) = lt_src[ tipo_node = 'R' ].
    APPEND ls_root TO lt_trg.

    DELETE lt_src WHERE id_calc_node = ls_root-id_calc_node.

    WHILE lines( lt_src ) > 0.

      clear lv_qty.

      LOOP AT lt_trg INTO DATA(ls_trg).

        IF line_exists( lt_src[ id_parent_node = ls_trg-id_calc_node ] ).

          lv_qty = lv_qty + 1.

          DATA(lt_aux) = lt_src.
          DELETE lt_aux WHERE id_parent_node <> ls_trg-id_calc_node.

          SORT lt_aux BY posicao DESCENDING.
          APPEND LINES OF lt_aux TO lt_trg.

          DELETE lt_src WHERE id_parent_node = ls_trg-id_calc_node.

        ENDIF.

      ENDLOOP.

      check lv_qty = 0.
      refresh lt_src.

    ENDWHILE.

    ct_data = lt_trg.

  ENDMETHOD.


  METHOD update.

    DATA lt_sorted TYPE /qaps/t_calculation_node.
    FIELD-SYMBOLS <ft> TYPE /qaps/t_calculation_node.

    mo_tree->delete_all_nodes( ).
    REFRESH mt_nodes.

    initialize_drag_an_drop( ).

    add_root_node( iv_root_text = mv_root_text ).

    ASSIGN ir_data->* TO <ft>.

    sort_table( CHANGING ct_data = <ft> ).

    "Root
    LOOP AT <ft> REFERENCE INTO DATA(lr_line).
*      CHECK lr_line->tipo_node = 'R'.
      add_child_node( lr_line ).
    ENDLOOP.

*    "Variáveis de Pricing(Tabela)
*    LOOP AT <ft> REFERENCE INTO lr_line.
*      CHECK lr_line->tipo_node <> 'R'.
*
*      add_child_node( lr_line ).
*    ENDLOOP.

  ENDMETHOD.


  METHOD update_node_content.

    DATA lr_data TYPE REF TO data.
    DATA ls_data TYPE /qaps/s_calculation_node.

    ASSIGN mt_nodes[ guid = is_data-id_calc_node ] TO FIELD-SYMBOL(<fs_source>).

    lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = <fs_source>-content
                                         CHANGING  cr_data = lr_data ).

    ls_data-expressao               = is_data-expressao.
    ls_data-importacao_calculada    = is_data-importacao_calculada.
    ls_data-nacional_calculada      = is_data-nacional_calculada.
    ls_data-producao_calculada      = is_data-producao_calculada.
    ls_data-transferencia_calculada = is_data-transferencia_calculada.

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( lr_data ).
    <fs_source>-content = lv_xml.

  ENDMETHOD.
ENDCLASS.
