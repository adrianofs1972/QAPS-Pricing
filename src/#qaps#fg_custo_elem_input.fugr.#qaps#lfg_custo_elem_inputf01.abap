*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  FILL_SOURCE_TARGET
*&---------------------------------------------------------------------*
*&      Form  FILL_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes.

  IF gv_loaded IS INITIAL.
    PERFORM fill_list_tp_lista.
    PERFORM fill_list_area.
    gv_loaded = abap_true.
  ENDIF.

  IF gv_action = 'E' AND gv_loaded_edit = abap_false.
    PERFORM pre_fill_edicao.
    gv_loaded_edit = abap_true.
  ENDIF.

ENDFORM.
FORM fill_list_tp_lista.

  SELECT *
    FROM /qaps/tp_lista
    INTO CORRESPONDING FIELDS OF TABLE  @gt_tp_lista.

  SORT gt_tp_lista BY descricao ASCENDING.

  APPEND INITIAL LINE TO gt_list_tp_lista ASSIGNING FIELD-SYMBOL(<fs_value>).
  <fs_value>-key  = 0.
  <fs_value>-text = '(Selecionar)'.

  LOOP AT gt_tp_lista ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.
    APPEND INITIAL LINE TO gt_list_tp_lista ASSIGNING <fs_value>.
    <fs_value>-key  = <fs>-index.
    <fs_value>-text = <fs>-descricao.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TP_LISTA-INDEX'
      values = gt_list_tp_lista.

ENDFORM.
FORM fill_list_area.

  DATA: ls_value LIKE LINE OF gt_list_area.

  SELECT id_area, descricao
              FROM /qaps/area
              INTO CORRESPONDING FIELDS OF TABLE @gt_area.

  SORT gt_area BY descricao ASCENDING.

  LOOP AT gt_area ASSIGNING FIELD-SYMBOL(<fs_list_area>).
    <fs_list_area>-index = sy-tabix.
    ls_value-key  = <fs_list_area>-index.
    ls_value-text = <fs_list_area>-descricao.
    APPEND ls_value TO gt_list_area.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_AREA-INDEX'
      values = gt_list_area.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRE_FILL_EDICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_fill_edicao .

  CLEAR: gv_escopo_global,gv_escopo_tp_lista.

  CASE gs_data-escopo.
    WHEN 'G'.
      gv_escopo_global = abap_true.
      gv_required = abap_false.
    WHEN 'T'.
      gv_escopo_tp_lista = abap_true.
      gs_tp_lista-index = VALUE #( gt_tp_lista[ id_tp_lista = gs_data-id_tp_lista ]-index OPTIONAL ).

      IF gs_tp_lista-index IS INITIAL.
        gv_escopo_tp_lista = abap_false.
        gv_escopo_global = abap_true.
        gv_required = abap_false.
      ELSE.
        gv_required = abap_true.
      ENDIF.
  ENDCASE.

  gs_area-index = VALUE #( gt_area[ id_area = gs_data-id_area ]-index OPTIONAL ).

  CLEAR: gv_tipo_dado_valor,gv_tipo_dado_percentual.
  CASE gs_data-tipo_dado.
    WHEN '1'.
      gv_tipo_dado_valor = abap_true.
    WHEN '2'.
      gv_tipo_dado_percentual = abap_true.
  ENDCASE.

  CLEAR: gv_origem_dado_input, gv_origem_dado_custom, gv_origem_dado_tabela.
  CASE gs_data-origem_dado.
    WHEN '1'. gv_origem_dado_input = abap_true.
    WHEN '2'. gv_origem_dado_custom = abap_true.
    WHEN '3'. gv_origem_dado_tabela = abap_true.
  ENDCASE.

  CLEAR:  gv_tipo_variavel_geral,
          gv_tipo_variavel_compra,
          gv_tipo_variavel_logistica,
          gv_tipo_variavel_produtiva,
          gv_tipo_variavel_frete.
  CASE gs_data-tipo_variavel.
    WHEN 'G'. gv_tipo_variavel_geral = abap_true.
    WHEN 'C'. gv_tipo_variavel_compra = abap_true.
    WHEN 'L'. gv_tipo_variavel_logistica = abap_true.
    WHEN 'P'. gv_tipo_variavel_produtiva = abap_true.
    WHEN 'F'. gv_tipo_variavel_frete = abap_true.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  IF gv_tipo_dado_valor = 'X' AND gs_data-moeda IS INITIAL.
    MESSAGE 'Moeda é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_required = abap_true AND gs_tp_lista-index = 0.
    MESSAGE 'Tipo de Lista é campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
