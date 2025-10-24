class /QAPS/CL_VIEW_EXPRESSAO_VALUES definition
  public
  inheriting from /QAPS/CL_VIEW_TREE_BASE
  final
  create public .

public section.

  data MT_EXPRESSAO type /QAPS/T_TREE_EXPRESSAO .

  methods SET_SESSION_DATA
    importing
      !IS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .

  methods UPDATE
    redefinition .
protected section.

  methods ADD_CHILD_NODE
    redefinition .
  methods ADD_ROOT_NODE
    redefinition .
  methods CUSTOMIZE_CATALOG
    redefinition .
  methods DISPLAY_TREE
    redefinition .
  methods GET_HIERARCHY_HEADER
    redefinition .
  methods ITEM_DOUBLE_CLICK
    redefinition .
  methods SET_EVENTS
    redefinition .
private section.

  types:
    BEGIN OF ts_node_expressao,
      id_calc_node TYPE /qaps/ed_id_calc_node,
      node_key     TYPE lvc_nkey,
      root         TYPE abap_bool,
    END OF ts_node_expressao .
  types:
    tt_node_expressao TYPE TABLE OF ts_node_expressao .

  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MT_CATALOG type LVC_T_FCAT .
  data MT_EXPRESSAO_NODES type TT_NODE_EXPRESSAO .

  methods UPDATE_CATALOG .
  methods CRIAR_HIERARQUIA_CHILDREN
    importing
      !IS_PARENT_NODE type TS_NODE_EXPRESSAO
      !IS_DATA type /QAPS/S_RETORNO_CALCULO .
  methods CRIAR_HIERARQUIA_PRICE_VALUE
    importing
      !IS_DATA type /QAPS/S_RETORNO_CALCULO .
  methods CRIAR_HIERARQUIA
    importing
      !IS_DATA type /QAPS/S_RETORNO_CALCULO .
ENDCLASS.



CLASS /QAPS/CL_VIEW_EXPRESSAO_VALUES IMPLEMENTATION.


  METHOD ADD_CHILD_NODE.

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


  method ADD_ROOT_NODE.

  endmethod.


  METHOD criar_hierarquia.

    REFRESH: mt_expressao,
             mt_expressao_nodes.

    mo_tree->delete_all_nodes( ).
    criar_hierarquia_price_value( is_data ).

  ENDMETHOD.


  METHOD criar_hierarquia_children.

    DATA lv_node_key TYPE lvc_nkey.

    DATA ls_node_layout	TYPE lvc_s_layn.
    DATA lv_node_text TYPE lvc_value.


    "Nós Root
*    BREAK c060863.
    DATA(lt_children) = is_data-t_expressao.
    DELETE lt_children WHERE id_parent_node <> is_parent_node-id_calc_node.

    LOOP AT lt_children INTO DATA(ls_expressao).
      CHECK ls_expressao-price_field = ''.

      DATA(ls_value) = ls_expressao-t_valores[ 1 ].

      DATA(ls_entry) = VALUE /qaps/s_tree_expressao(
          moeda             = ls_value-moeda
          valor             = ls_value-valor
          moeda_final       = ls_value-moeda_final
          valor_moeda_final = ls_value-valor_moeda_final
          fieldname         = ls_expressao-fieldname
          expressao         = ls_expressao-expressao
      ).

      CASE ls_expressao-tipo_node.
        WHEN 'E'.
          ls_node_layout = VALUE lvc_s_layn( isfolder   = ' '
                                             n_image    = icon_sym_real_server
                                             exp_image  = icon_sym_real_server ).
          lv_node_text = ls_expressao-dsc_custo_elementar.
        WHEN 'C'.
          ls_node_layout = VALUE lvc_s_layn( isfolder   = abap_true
                                             n_image    = icon_calculation
                                             exp_image  = icon_calculation ).
          lv_node_text = ls_expressao-descricao.
      ENDCASE.

      mo_tree->add_node(
        EXPORTING
          i_relat_node_key     = is_parent_node-node_key    " Node Already in Tree Hierarchy
          i_relationship       = cl_gui_column_tree=>relat_last_child    " How to Insert Node
          is_outtab_line       = ls_entry    " Attributes of Inserted Node
          is_node_layout       = ls_node_layout    " Node Layout
*          it_item_layout       =     " Item Layout
          i_node_text          = lv_node_text    " Hierarchy Node Text
        IMPORTING
          e_new_node_key       = lv_node_key
        EXCEPTIONS
          relat_node_not_found = 1
          node_not_found       = 2
          OTHERS               = 3
      ).

      DATA(ls_expressao_nodes) = VALUE ts_node_expressao(
          id_calc_node = ls_expressao-id_calc_node
          node_key     = lv_node_key ).
      APPEND ls_expressao_nodes TO mt_expressao_nodes.

      criar_hierarquia_children( is_data = is_data
                                 is_parent_node = ls_expressao_nodes ).

    ENDLOOP.


  ENDMETHOD.


  METHOD criar_hierarquia_price_value.

    DATA lv_node_key TYPE lvc_nkey.

    REFRESH mt_expressao.

    DATA ls_node_layout	TYPE lvc_s_layn.

    ls_node_layout = VALUE lvc_s_layn(
        isfolder   = abap_true
        n_image    = icon_graphics
        exp_image  = icon_graphics
    ).

    "Nós Root
*    break c060863.
    LOOP AT is_data-t_expressao INTO DATA(ls_expressao).
      CHECK ls_expressao-price_field = 'X'.

      DATA(ls_value) = ls_expressao-t_valores[ 1 ].

      DATA(ls_entry) = VALUE /qaps/s_tree_expressao(
          moeda             = ls_value-moeda
          valor             = ls_value-valor
          moeda_final       = ls_value-moeda_final
          valor_moeda_final = ls_value-valor_moeda_final
          fieldname         = ls_expressao-fieldname
          expressao         = ls_expressao-expressao
      ).

      mo_tree->add_node(
        EXPORTING
          i_relat_node_key     = ''    " Node Already in Tree Hierarchy
          i_relationship       = cl_gui_column_tree=>relat_last_child    " How to Insert Node
          is_outtab_line       = ls_entry    " Attributes of Inserted Node
          is_node_layout       = ls_node_layout    " Node Layout
*          it_item_layout       =     " Item Layout
          i_node_text          = CONV #( ls_expressao-descricao )    " Hierarchy Node Text
        IMPORTING
          e_new_node_key       = lv_node_key
        EXCEPTIONS
          relat_node_not_found = 1
          node_not_found       = 2
          OTHERS               = 3
      ).

      data(ls_expressao_nodes) = value ts_node_expressao(
                                        id_calc_node = ls_expressao-id_calc_node
                                        node_key     = lv_node_key
                                        root         = abap_true ).

      append ls_expressao_nodes to mt_expressao_nodes.

      criar_hierarquia_children( is_data = is_data
                                 is_parent_node = ls_expressao_nodes ).

    ENDLOOP.


  ENDMETHOD.


  METHOD customize_catalog.

*    DELETE ct_catalog WHERE fieldname = 'DESCRICAO'.
*    BREAK c060863.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'FIELDNAME'.
          <fs>-no_out = 'X'.
        WHEN 'EXPRESSAO'.
          <fs>-no_out = 'X'.
        WHEN 'MOEDA'.
          <fs>-no_out = 'X'.
        WHEN 'MOEDA_FINAL'.
          <fs>-no_out = 'X'.
      ENDCASE.
    ENDLOOP.

    mt_catalog = ct_catalog.

  ENDMETHOD.


  METHOD display_tree.

    mo_tree->set_table_for_first_display(
            EXPORTING
*              i_structure_name    = '/QAPS/S_TREE_EXPRESSAO'
              is_hierarchy_header = is_header
*              it_list_commentary  = lt_list_commentary
*              i_logo              = l_logo
*              i_background_id     = 'ALV_BACKGROUND'
*              i_save              = 'A'
*              is_variant          = ls_variant
            CHANGING
              it_outtab           = mt_expressao
              it_fieldcatalog     = it_catalog
              ).

  ENDMETHOD.


  METHOD get_hierarchy_header.

    return-heading = 'Expressão'.
    return-tooltip = 'Expressão'.
    return-width = 50.
    return-width_pix = ''.

  ENDMETHOD.


  method ITEM_DOUBLE_CLICK.
    BREAK-POINT.
  endmethod.


  method SET_EVENTS.
*CALL METHOD SUPER->SET_EVENTS
*    .
  endmethod.


  METHOD set_session_data.
    ms_lista_custo = is_lista_custo.
  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_retorno_calculo.
    DATA ls_data TYPE /qaps/s_tree_expressao.

    ASSIGN ir_data->* TO <fs>.

    update_catalog( ).

    criar_hierarquia( <fs> ).

*    break c060863.
    LOOP AT mt_expressao_nodes INTO DATA(ls_expressao_nodes).
      CHECK ls_expressao_nodes-root = 'X'.
      mo_tree->expand_node(
        EXPORTING
          i_node_key          = ls_expressao_nodes-node_key    " Node Key
*          i_level_count       = 1    " Number of Levels to be Expanded
*          i_expand_subtree    =     " 'X': Expand all Subsequent Nodes
*        EXCEPTIONS
*          failed              = 1
*          illegal_level_count = 2
*          cntl_system_error   = 3
*          node_not_found      = 4
*          cannot_expand_leaf  = 5
*          others              = 6
      ).
      IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.

    mo_tree->frontend_update( ).



  ENDMETHOD.


  METHOD update_catalog.

    mo_tree->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog   = DATA(lt_fcat) ).

*    break c060863.
    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'VALOR'.
          IF NOT ms_lista_custo-moeda_calculo IS INITIAL.
            <fs>-coltext = `Valor (` && ms_lista_custo-moeda_calculo && `)`.
          ELSEIF NOT ms_lista_custo-moeda_lista IS INITIAL.
            <fs>-coltext = `Valor (` && ms_lista_custo-moeda_lista && `)`.
          else.
            <fs>-coltext = `Valor`.
          ENDIF.
        WHEN 'VALOR_MOEDA_FINAL'.
          <fs>-coltext = `Valor (` && ms_lista_custo-moeda_lista && `)`.
      ENDCASE.
    ENDLOOP.

    mo_tree->set_frontend_fieldcatalog( it_fieldcatalog = lt_fcat ).

  ENDMETHOD.
ENDCLASS.
