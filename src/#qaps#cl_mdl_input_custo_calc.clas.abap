class /QAPS/CL_MDL_INPUT_CUSTO_CALC definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods GET_ALL_NODES
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/T_CALCULATION_NODE .
  methods UPDATE_NODE
    importing
      !IS_DATA type /QAPS/S_CALCULATION_NODE
      !IV_ACTION type CHAR10 .
  methods CHANGE_PARENT
    importing
      !IS_DATA type /QAPS/CALC_NODE
      !IV_ID_NEW_PARENT type /QAPS/ED_ID_CALC_NODE
    returning
      value(RETURN) type ABAP_BOOL .
  methods DESVICULAR_NODE_ELEMENTAR
    importing
      !IS_DATA type /QAPS/CALC_NODE
    returning
      value(RETURN) type ABAP_BOOL .
  methods EXCLUIR_NODE_CALCULADO
    importing
      !IS_DATA type /QAPS/CALC_NODE
    returning
      value(RETURN) type ABAP_BOOL .
  methods SAVE_EXPRESSAO
    importing
      !IS_DATA type /QAPS/S_CALCULATION_NODE
      !IT_PARAMETERS type /QAPS/T_CALCULATION_NODE .
  methods CHECK_EXPRESSAO
    importing
      !IS_DATA type /QAPS/S_CALCULATION_NODE
      !IT_PARAMETERS type /QAPS/T_CALCULATION_NODE .
  methods ASSIGN_VARIAVEL_ELEMENTAR
    importing
      !IS_PARENT type /QAPS/S_CALCULATION_NODE
      !IV_AUTOMATIC type ABAP_BOOL default ABAP_FALSE
      !IS_DATA type /QAPS/S_CUSTO_ELEMENTAR optional
    returning
      value(RETURN) type /QAPS/S_CALCULATION_NODE .
  methods CREATE_VARIAVEL_CALCULADA
    importing
      !IS_PARENT type /QAPS/S_CALCULATION_NODE
    returning
      value(RETURN) type /QAPS/S_CALCULATION_NODE .
  methods GET_CHILD_NODES
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_PARENT_NODE type /QAPS/ED_ID_CALC_NODE
    returning
      value(RETURN) type /QAPS/T_CALCULATION_NODE .
  methods GET_NODES
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/T_CALCULATION_NODE .
  methods GET_VARIAVEIS_ALL
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_UNAME type UNAME default SY-UNAME
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_VARIAVEIS
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_UNAME type UNAME default SY-UNAME
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_INPUT
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/T_VAR_INPUT .
  methods SET_PERIODO
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  methods GET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods GET_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
    returning
      value(RETURN) type /QAPS/T_TP_LISTA .
protected section.
private section.

  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .

  methods GET_PARENT_NODE
    importing
      !IV_ID_CALC_NODE type /QAPS/ED_ID_CALC_NODE
    returning
      value(RETURN) type /QAPS/CALC_NODE .
  methods GET_NODE
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_CALC_NODE type /QAPS/ED_ID_CALC_NODE
    returning
      value(RETURN) type /QAPS/S_CALCULATION_NODE .
  methods GET_PROGRAM_CODE
    importing
      !IS_DATA type /QAPS/S_CALCULATION_NODE
      !IT_PARAMETERS type /QAPS/T_CALCULATION_NODE
    returning
      value(RETURN) type /QAPS/T_PROGRAM_CODE .
  methods GET_NEXT_INTERNAL_NAME
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_TIPO_NODE type /QAPS/ED_TIPO_NODE
    returning
      value(RETURN) type LVC_FNAME .
  methods CREATE_VARIABLES
    importing
      !IS_ROOT type /QAPS/CALC_NODE .
  methods GET_VARIABLE_VALUES
    returning
      value(RETURN) type LVC_T_FCAT .
  methods INITIALIZE_NODES
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/T_CALCULATION_NODE .
  methods CREATE_ROOT_NODE
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/CALC_NODE .
  methods GET_REFERENCIAS
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
ENDCLASS.



CLASS /QAPS/CL_MDL_INPUT_CUSTO_CALC IMPLEMENTATION.


  METHOD assign_variavel_elementar.

    DATA lr_data TYPE REF TO data.
    DATA:
      ls_data    TYPE /qaps/s_custo_elementar,
      ls_entry   TYPE /qaps/calc_node,
      ls_message TYPE bapiret2.

    IF iv_automatic = abap_false.

      CALL FUNCTION '/QAPS/FM_VAR_ELEMENTAR_SELECT'
        DESTINATION 'NONE'
        EXPORTING
*         IV_ACTION  = 'C'
          is_parent  = is_parent
*         iv_id_tp_lista = is_parent-id_tp_lista
        IMPORTING
          es_data    = ls_data
          es_message = ls_message.

      IF ls_message-type = 'E'.
        MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
        RETURN.
      ENDIF.

    ELSE.
      ls_data = is_data.
    ENDIF.

    ls_entry = VALUE /qaps/calc_node(
        id_calc_node   = cl_system_uuid=>create_uuid_x16_static( )
        id_tp_lista   = is_parent-id_tp_lista
        id_parent_node = is_parent-id_calc_node
        tipo_node      = 'E'
        id_custo_elementar = ls_data-id_custo_elementar
*        descricao      = ls_data-descricao
        fieldname      = get_next_internal_name( iv_id_tp_lista = is_parent-id_tp_lista
                                                 iv_tipo_node    = 'E'
                         ) ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    INSERT /qaps/calc_node FROM ls_entry.

    return = CORRESPONDING #( ls_entry ).
    return-dsc_custo_elementar = ls_data-descricao.


  ENDMETHOD.


  METHOD change_parent.

    data: ls_data type /qaps/calc_node,
          lr_data type ref to data.

    DATA lv_message TYPE string.
    DATA(ls_parent) = get_parent_node( is_data-id_parent_node ).

    IF ls_parent-expressao CS is_data-fieldname.
      MESSAGE 'A variável não pode ser movida, pois está sendo utilizada na expressão superior'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    ls_data = CORRESPONDING #( is_data ).
    ls_data-id_parent_node = iv_id_new_parent.
    lr_data = ref #( ls_data ).

    preencher_dados_controle( CHANGING cr_data = lr_data ).
    modify /qaps/calc_node from ls_data.

    return = abap_true.

  ENDMETHOD.


  METHOD check_expressao.

    DATA: mess TYPE string,
          lin  TYPE i,
          wrd  TYPE string,
          dir  TYPE trdir.

    DATA(lt_program) = get_program_code( is_data       =  is_data
                                         it_parameters =  it_parameters  ).

    SYNTAX-CHECK FOR lt_program MESSAGE mess LINE lin WORD wrd.

    IF sy-subrc = 0.
      MESSAGE 'Expressão é consistente' TYPE 'S'.
    ELSEIF sy-subrc = 4.
      MESSAGE mess TYPE 'I'.
      EXIT.
    ELSEIF sy-subrc = 8.
      MESSAGE mess TYPE 'I'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD create_root_node.

    DATA lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/tp_lista
      WHERE id_tp_lista = @iv_id_tp_lista
      INTO @DATA(ls_lista).

    DATA(ls_entry) = VALUE /qaps/calc_node(
        id_calc_node   = cl_system_uuid=>create_uuid_x16_static( )
        id_tp_lista   = iv_id_tp_lista
        tipo_node      = 'R'
        descricao      = ls_lista-descricao ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    INSERT /qaps/calc_node FROM ls_entry.
    COMMIT WORK AND WAIT.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_tp_lista   = @iv_id_tp_lista
      AND tipo_node = 'R'
      INTO @return.

  ENDMETHOD.


  METHOD create_variables.

    DATA lr_data TYPE REF TO data.
    DATA lt_entries TYPE TABLE OF /qaps/calc_node.

    "Variables Price List - /QAPS/PRICE_ITM
    DATA(lt_variables) = get_variable_values( ).

    IF lines( lt_variables ) > 0.

      SELECT *
        FROM /qaps/calc_node
        FOR ALL ENTRIES IN @lt_variables
        WHERE fieldname = @lt_variables-fieldname
        AND id_tp_lista = @is_root-id_tp_lista
        AND tipo_node = 'C'
        AND price_field = 'X'
        INTO TABLE @DATA(lt_calc_node).

      DATA(lv_lines) = lines( lt_calc_node ).

      LOOP AT lt_variables INTO DATA(ls_variable).

        CHECK NOT line_exists( lt_calc_node[ fieldname = ls_variable-fieldname ] ).

        data(lv_posicao) = lv_lines + lines( lt_entries ) + 1.

        DATA(ls_entry) = VALUE /qaps/calc_node(
                  id_calc_node   = cl_system_uuid=>create_uuid_x16_static( )
                  id_tp_lista   = is_root-id_tp_lista
                  id_parent_node = is_root-id_calc_node
                  tipo_node      = 'C'
                  posicao        = lv_posicao
                  price_field    = abap_true
                  descricao      = ls_variable-scrtext_l
                  fieldname      = ls_variable-fieldname ).

        lr_data = REF #( ls_entry ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_entry TO lt_entries.

      ENDLOOP.

      IF lines( lt_entries ) > 0.
        INSERT /qaps/calc_node FROM TABLE lt_entries.
        COMMIT WORK AND WAIT.
      ENDIF.

*      BREAK-POINT.

    ENDIF.

  ENDMETHOD.


  METHOD create_variavel_calculada.

    DATA lr_data TYPE REF TO data.
    DATA:
*          ls_data     TYPE /qaps/s_calculation_node,
      ls_entry   TYPE /qaps/calc_node,
      ls_message TYPE bapiret2,
      lt_sval    TYPE TABLE OF sval,
      lv_return  TYPE char1.

    APPEND VALUE sval( tabname = '/QAPS/CALC_NODE'
                      fieldname = 'DESCRICAO'
                      fieldtext = 'Variável Calculada' ) TO lt_sval.

    APPEND VALUE sval( tabname = '/QAPS/CALC_NODE'
                        fieldname = 'IMPORTACAO_CALCULADA'
                        fieldtext = 'Importação'
                        value = 'X'
                        novaluehlp = 'X' ) TO lt_sval.

    APPEND VALUE sval( tabname = '/QAPS/CALC_NODE'
                        fieldname = 'NACIONAL_CALCULADA'
                        fieldtext = 'Nacional'
                        value = 'X'
                        novaluehlp = 'X' ) TO lt_sval.

    APPEND VALUE sval( tabname = '/QAPS/CALC_NODE'
                        fieldname = 'PRODUCAO_CALCULADA'
                        fieldtext = 'Produção'
                        value = 'X'
                        novaluehlp = 'X' ) TO lt_sval.

    APPEND VALUE sval( tabname = '/QAPS/CALC_NODE'
                        fieldname = 'TRANSFERENCIA_CALCULADA'
                        fieldtext = 'Transferência'
                        value = 'X'
                        novaluehlp = 'X' ) TO lt_sval.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Variável Calculada'
        start_column    = '15'
*       START_ROW       = '5'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF ls_message-type = 'A' OR sy-ucomm = 'CANC'.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = VALUE /qaps/calc_node(
        id_calc_node   = cl_system_uuid=>create_uuid_x16_static( )
        id_tp_lista   = is_parent-id_tp_lista
        id_parent_node = is_parent-id_calc_node
        tipo_node      = 'C'
        descricao      = lt_sval[ 1 ]-value
        importacao_calculada = lt_sval[ 2 ]-value
        nacional_calculada = lt_sval[ 3 ]-value
        producao_calculada = lt_sval[ 4 ]-value
        transferencia_calculada = lt_sval[ 5 ]-value
        fieldname      = get_next_internal_name( iv_id_tp_lista = is_parent-id_tp_lista
                                                 iv_tipo_node    = 'C'
                         ) ).

    INSERT /qaps/calc_node FROM ls_entry.

    return = CORRESPONDING #( ls_entry ).

  ENDMETHOD.


  METHOD desvicular_node_elementar.

    DATA lv_message TYPE string.
    DATA(ls_parent) = get_parent_node( is_data-id_parent_node ).

    IF ls_parent-expressao CS is_data-fieldname.
      MESSAGE 'A variável não pode ser desvinculada, pois está sendo utilizada na expressão superior'
        TYPE 'S' DISPLAY LIKE 'E'.
        return.
    ENDIF.

    lv_message = 'Deseja excluir a variável selecionada?'.

    IF question( lv_message ) = abap_true.
      DELETE FROM /qaps/calc_node WHERE id_calc_node = is_data-id_calc_node.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD excluir_node_calculado.

    DATA lv_message TYPE string.
    DATA(ls_parent) = get_parent_node( is_data-id_parent_node ).

    IF ls_parent-expressao CS is_data-fieldname.
      MESSAGE 'A variável não pode ser excluída, pois está sendo utilizada na expressão superior'
        TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lv_message = 'Deseja excluir a variável selecionada?'.

    IF question( lv_message ) = abap_true.
      DELETE FROM /qaps/calc_node WHERE id_calc_node = is_data-id_calc_node.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD get_all_nodes.

    SELECT *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_custo) = lo_custo_elementar->get_variaveis_by_tipo_lista( iv_id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      IF NOT <fs>-id_custo_elementar IS INITIAL.
        DATA(ls_custo) = VALUE #( lt_custo[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).
        <fs>-dsc_custo_elementar = ls_custo-descricao.
        <fs>-tipo_dado = ls_custo-tipo_dado.
      ELSE.
        <fs>-tipo_dado = 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_child_nodes.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      AND   id_calc_node = @iv_id_parent_node
      INTO @DATA(ls_node).

    SELECT *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      AND   id_parent_node = @iv_id_parent_node
      INTO CORRESPONDING FIELDS OF TABLE @return.

    IF ls_node-price_field = 'X'.

      SELECT *
        FROM /qaps/calc_node
        WHERE id_tp_lista = @iv_id_tp_lista
        AND   id_calc_node <> @iv_id_parent_node
        and   price_field = 'X'
        APPENDING CORRESPONDING FIELDS OF table @return.

    ENDIF.

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_custo) = lo_custo_elementar->get_variaveis_by_tipo_lista( iv_id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      IF NOT <fs>-id_custo_elementar IS INITIAL.
        DATA(ls_custo) = VALUE #( lt_custo[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).
        <fs>-dsc_custo_elementar = ls_custo-descricao.
        <fs>-tipo_dado = ls_custo-tipo_dado.
      ELSE.
        <fs>-tipo_dado = 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_INPUT.
*
*    SELECT *
*      FROM /qaps/var_input
*      WHERE id_simulacao = @iv_id_simulacao
*      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
*      INTO CORRESPONDING FIELDS OF TABLE @return.
*
*    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( iv_action = 'C' ).
*    DATA(lo_ponto) = NEW /qaps/cl_mdl_logistica( iv_action = 'C' ).
*
*    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
*    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).
*
*    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
*    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
*    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
*    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
*    DATA(lt_ponto) = lo_ponto->get_pontos( ).
*    DATA(lt_regiao) = lo_ponto->get_regioes( ).
*
**    BREAK-POINT.
*    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
*
*      CASE <fs>-tipo_regra.
*        WHEN 'GP'.
*          <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
*        WHEN 'MP'.
*          <fs>-dsc_input = <fs>-mat_planejado.
*        WHEN 'AG'.
*          <fs>-dsc_input = <fs>-agregador.
*        WHEN 'MA'.
*          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
*          CONDENSE lv_matnr.
*          <fs>-key_input = lv_matnr.
*          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
*      ENDCASE.
*
*      <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).
*
*      CASE <fs>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = lt_ponto[ id_ponto = <fs>-id_origem ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_origem = ls_ponto-lifnr.
*              <fs>-dsc_tipo_origem = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_origem = ls_ponto-kunnr.
*              <fs>-dsc_tipo_origem = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_origem = ls_ponto-werks.
*              <fs>-dsc_tipo_origem = 'Centro'.
*          ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*        WHEN 'G'.
**          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_regiao ]-codigo OPTIONAL ).
*      ENDCASE.
*
*      <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).
*
*      CASE <fs>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = lt_ponto[ id_ponto = <fs>-id_destino ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_destino = ls_ponto-lifnr.
*              <fs>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_destino = ls_ponto-kunnr.
*              <fs>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_destino = ls_ponto-werks.
*              <fs>-dsc_tipo_destino = 'Centro'.
*          ENDCASE.
*        WHEN 'R'.
*          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*        WHEN 'G'.
**          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_regiao ]-codigo OPTIONAL ).
*      ENDCASE.
*
*      "Categoria
*      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).
*
*      "Modal
*      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).
*
*      "Processo
*      <fs>-dsc_processo = value #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
*
*      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_internal_name.

    DATA lv_sufixo TYPE numc3.

    SELECT SINGLE COUNT( * ) AS qty
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      AND price_field = ''
      AND tipo_node = @iv_tipo_node
      INTO @DATA(lv_qty).

    IF lv_qty = 0.

      lv_qty = lv_qty + 1.
      lv_sufixo = lv_qty.

      CASE iv_tipo_node.
        WHEN 'C'.
          return = `CALC_` && lv_sufixo.
        WHEN 'E'.
          return = `ELEM_` && lv_sufixo.
      ENDCASE.

    ELSE.

      SELECT MAX( fieldname ) AS fieldname
        FROM /qaps/calc_node
        WHERE id_tp_lista = @iv_id_tp_lista
        AND price_field = ''
        AND tipo_node = @iv_tipo_node
        INTO @DATA(lv_fieldname).

      split lv_fieldname at '_' into data(lv_prefixo) data(lv_item_max)."lv_qty.

      lv_qty = lv_item_max + 1.
      lv_sufixo = lv_qty.

      CASE iv_tipo_node.
        WHEN 'C'.
          return = `CALC_` && lv_sufixo.
        WHEN 'E'.
          return = `ELEM_` && lv_sufixo.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD get_node.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      AND   id_calc_node = @iv_id_calc_node
      INTO CORRESPONDING FIELDS OF @return.

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_custo) = lo_custo_elementar->get_variaveis_by_tipo_lista( iv_id_tp_lista ).

    IF NOT return-id_custo_elementar IS INITIAL.
      return-dsc_custo_elementar = VALUE #( lt_custo[ id_custo_elementar = return-id_custo_elementar ]-descricao OPTIONAL ).
    ENDIF.

  ENDMETHOD.


  METHOD get_nodes.

    initialize_nodes( iv_id_tp_lista ).

    SELECT *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_custo) = lo_custo_elementar->get_variaveis_by_tipo_lista( iv_id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      IF NOT <fs>-id_custo_elementar IS INITIAL.
        DATA(ls_custo) = VALUE #( lt_custo[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).
        <fs>-dsc_custo_elementar      = ls_custo-descricao.
        <fs>-tipo_dado                = ls_custo-tipo_dado.
        <fs>-importacao_elementar     = ls_custo-importacao.
        <fs>-nacional_elementar       = ls_custo-nacional.
        <fs>-producao_elementar       = ls_custo-producao.
        <fs>-transferencia_elementar  = ls_custo-transferencia.
      ELSE.
        <fs>-tipo_dado = 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_parent_node.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_calc_node = @iv_id_calc_node
      INTO @return.

  ENDMETHOD.


  METHOD get_program_code.

    DATA: prog TYPE string,
*          return  TYPE STANDARD returnLE OF string,
          mess TYPE string.

    APPEND 'PROGRAM subpool.'                        TO return.

*    CASE is_data-tipo_dado.
*      WHEN '1'.
        APPEND `DATA ` && is_data-fieldname && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO return.
*      WHEN '2'.
*        APPEND `DATA ` && is_data-fieldname && ` TYPE /QAPS/PERCENTUAL.` TO return.
*    ENDCASE.

    "Variáveis
    LOOP AT it_parameters INTO DATA(ls_parameters).
      CASE ls_parameters-tipo_dado.
        WHEN '1'.
          APPEND `DATA ` && ls_parameters-fieldname && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO return.
        WHEN '2'.
          APPEND `DATA ` && ls_parameters-fieldname && ` TYPE /QAPS/PERCENTUAL.` TO return.
      ENDCASE.
    ENDLOOP.

    APPEND is_data-fieldname && ` = ` && is_data-expressao && `.` TO return.

  ENDMETHOD.


  METHOD GET_REFERENCIAS.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @it_data
      WHERE id_simulacao = @it_data-id_original
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD GET_SIMULACAO.

    IF iv_id_simulacao IS INITIAL.

      SELECT *
        FROM /qaps/simulacao
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/simulacao
        WHERE id_simulacao = @iv_id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    IF lines( return ) > 0.

      DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
      DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista(  ).

      DATA(lt_ref) = get_referencias( return ).

      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

        CASE <fs>-status.
          WHEN 'A'. <fs>-icon = icon_green_light.
          WHEN 'F'. <fs>-icon = icon_red_light.
        ENDCASE.

        <fs>-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_SIMUL'
                                                                iv_value  = CONV #( <fs>-status ) ).

        <fs>-dsc_referencia = VALUE #( lt_ref[ id_simulacao = <fs>-id_original ]-descricao OPTIONAL ).

        <fs>-dsc_tp_lista = VALUE #( lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao ).

      ENDLOOP.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD GET_TIPO_LISTA.

    IF iv_id_tipo_lista IS INITIAL.

      SELECT *
        FROM /qaps/tp_lista
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/tp_lista
        WHERE id_tp_lista = @iv_id_tipo_lista
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD get_variable_values.

    DATA lt_fcat TYPE lvc_t_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/PRICE_ITM'
      CHANGING
        ct_fieldcat      = return
*     EXCEPTIONS
*       INCONSISTENT_INTERFACE       = 1
*       PROGRAM_ERROR    = 2
*       OTHERS           = 3
      .

    DELETE return WHERE datatype <> 'CURR'.
*    return = VALUE #( FOR wa IN lt_fcat
*                      ( sign = 'I' option = 'EQ' low = wa-fieldname ) ).


  ENDMETHOD.


  METHOD GET_VARIAVEIS.

    DATA lr_area TYPE RANGE OF /qaps/area_user-id_area.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    CHECK NOT ls_simulacao IS INITIAL.

    SELECT /qaps/lista_area~id_area
      FROM /qaps/lista_area
      INNER JOIN /qaps/area_user
      ON  /qaps/area_user~id_tp_lista = /qaps/lista_area~id_tp_lista
      AND /qaps/area_user~id_area = /qaps/lista_area~id_area
      WHERE /qaps/lista_area~id_tp_lista = @ls_simulacao-id_tp_lista
      AND /qaps/lista_area~ativo = 'X'
      AND /qaps/area_user~ativo = 'X'
      AND uname = @iv_uname
      INTO TABLE @DATA(lt_area_user).

    CHECK lines( lt_area_user ) > 0.

    lr_area = VALUE #( FOR wa IN lt_area_user
                       ( sign = 'I' option = 'EQ' low = wa-id_area ) ).

    SELECT *
      FROM /qaps/custo_elm
      WHERE id_area IN @lr_area
      and   origem_dado  = '1'
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).

    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).
    DATA(lt_areas) = lo_model_tp_lista->get_areas( iv_id_tp_lista = ls_simulacao-id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      <fs>-icon = icon_green_light.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao.
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_areas[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

    sort return by descricao ASCENDING.

  ENDMETHOD.


  METHOD GET_VARIAVEIS_ALL.

    DATA lr_area TYPE RANGE OF /qaps/area_user-id_area.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    CHECK NOT ls_simulacao IS INITIAL.

    SELECT /qaps/lista_area~id_area
      FROM /qaps/lista_area
*      INNER JOIN /qaps/area_user
*      ON  /qaps/area_user~id_tp_lista = /qaps/lista_area~id_tp_lista
*      AND /qaps/area_user~id_area = /qaps/lista_area~id_area
      WHERE /qaps/lista_area~id_tp_lista = @ls_simulacao-id_tp_lista
*      AND /qaps/lista_area~ativo = 'X'
*      AND /qaps/area_user~ativo = 'X'
*      AND uname = @iv_uname
      INTO TABLE @DATA(lt_area_user).

    CHECK lines( lt_area_user ) > 0.

    lr_area = VALUE #( FOR wa IN lt_area_user
                       ( sign = 'I' option = 'EQ' low = wa-id_area ) ).

    SELECT *
      FROM /qaps/custo_elm
      WHERE id_area IN @lr_area
      and   origem_dado  = '1'
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).

    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).
    DATA(lt_areas) = lo_model_tp_lista->get_areas( iv_id_tp_lista = ls_simulacao-id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      <fs>-icon = icon_green_light.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao.
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_areas[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

    sort return by descricao ASCENDING.

  ENDMETHOD.


  METHOD initialize_nodes.

    SELECT SINGLE *
      FROM /qaps/calc_node
      WHERE id_tp_lista = @iv_id_tp_lista
      AND tipo_node = 'R'
      INTO @DATA(ls_root).

    IF sy-subrc NE 0.
      ls_root = create_root_node( iv_id_tp_lista ).
    ENDIF.

    "Config Controladoria/Variáveis Iniciais
    /qaps/cl_pricing_initial_cfg=>create_initial_config( ).

    create_variables( ls_root ).

  ENDMETHOD.


  METHOD QUESTION.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR      = ' '
*       DIAGNOSE_OBJECT             = ' '
        text_question = iv_message
*       TEXT_BUTTON_1 = 'Ja'(001)
*       ICON_BUTTON_1 = ' '
*       TEXT_BUTTON_2 = 'Nein'(002)
*       ICON_BUTTON_2 = ' '
*       DEFAULT_BUTTON              = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN  = 25
*       START_ROW     = 6
*       POPUP_TYPE    =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer        = lv_answer
*     TABLES
*       PARAMETER     =
*     EXCEPTIONS
*       TEXT_NOT_FOUND              = 1
*       OTHERS        = 2
      .

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD save_expressao.

    DATA: mess TYPE string,
          lin  TYPE i,
          wrd  TYPE string,
          dir  TYPE trdir.

    DATA: ls_entry TYPE /qaps/calc_node,
          lr_data  TYPE REF TO data.

    DATA(lt_program) = get_program_code( is_data       =  is_data
                                         it_parameters =  it_parameters  ).

    SYNTAX-CHECK FOR lt_program MESSAGE mess LINE lin WORD wrd.

    IF sy-subrc = 0.
      ls_entry = CORRESPONDING #( is_data ).
      CONDENSE ls_entry-expressao.
      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      UPDATE /qaps/calc_node
      SET expressao = ls_entry-expressao
          modified_by = ls_entry-modified_by
          modified_in = ls_entry-modified_in
          modified_on = ls_entry-modified_on
       WHERE id_calc_node = ls_entry-id_calc_node.

      MESSAGE 'Expressão salva com sucesso' TYPE 'S'.
    ELSEIF sy-subrc = 4.
      MESSAGE mess TYPE 'I'.
      EXIT.
    ELSEIF sy-subrc = 8.
      MESSAGE mess TYPE 'I'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD SET_PERIODO.
    ms_periodo = is_periodo.
  ENDMETHOD.


  METHOD update_node.

    DATA: ls_entry TYPE /qaps/calc_node,
          lr_data  TYPE REF TO data.

    ls_entry = CORRESPONDING #( is_data ).
    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    CASE iv_action.
      WHEN 'IMP'.
        UPDATE /qaps/calc_node
        SET importacao_calculada  = ls_entry-importacao_calculada
            modified_by           = ls_entry-modified_by
            modified_in           = ls_entry-modified_in
            modified_on           = ls_entry-modified_on
         WHERE id_calc_node       = ls_entry-id_calc_node.
      WHEN 'NAC'.
        UPDATE /qaps/calc_node
        SET nacional_calculada   = ls_entry-nacional_calculada
            modified_by          = ls_entry-modified_by
            modified_in          = ls_entry-modified_in
            modified_on          = ls_entry-modified_on
         WHERE id_calc_node      = ls_entry-id_calc_node.
      WHEN 'PROD'.
        UPDATE /qaps/calc_node
        SET producao_calculada  = ls_entry-producao_calculada
            modified_by         = ls_entry-modified_by
            modified_in         = ls_entry-modified_in
            modified_on         = ls_entry-modified_on
         WHERE id_calc_node     = ls_entry-id_calc_node.
      WHEN 'TRAN'.
        UPDATE /qaps/calc_node
        SET transferencia_calculada = ls_entry-transferencia_calculada
            modified_by             = ls_entry-modified_by
            modified_in             = ls_entry-modified_in
            modified_on             = ls_entry-modified_on
         WHERE id_calc_node         = ls_entry-id_calc_node.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
