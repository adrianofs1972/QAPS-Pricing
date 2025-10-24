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
*FORM fill_list_regra.
*
*  DATA: lt_list  TYPE vrm_values,
*        ls_value LIKE LINE OF lt_list.
*
*  DATA(lt_data) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
*
**  SORT lt_data BY descricao ASCENDING.
*
*  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs>).
*    APPEND INITIAL LINE TO gt_lis_regra ASSIGNING FIELD-SYMBOL(<fs_regra>).
*    <fs_regra>-index = sy-tabix.
*    <fs_regra>-tipo_regra = <fs>-valpos.
*    <fs_regra>-descricao = <fs>-ddtext.
*
*    ls_value-key  = <fs>-valpos.
*    ls_value-text = <fs>-ddtext.
*    APPEND ls_value TO lt_list.
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LIST_REGRA-INDEX'
*      values = lt_list.
*
*ENDFORM.
*FORM fill_listarea.
*
*  DATA: lt_list  TYPE vrm_values,
*        ls_value LIKE LINE OF lt_list.
*
*  SELECT id_area, descricao
*              FROM /qaps/area
*              INTO CORRESPONDING FIELDS OF TABLE @gt_listarea.
*
*  SORT gt_listarea BY descricao ASCENDING.
*
*  LOOP AT gt_listarea ASSIGNING FIELD-SYMBOL(<fs_listarea>).
*    <fs_listarea>-index = sy-tabix.
*    ls_value-key  = <fs_listarea>-index.
*    ls_value-text = <fs_listarea>-descricao.
*    APPEND ls_value TO lt_list.
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_LISTAREA-INDEX'
*      values = lt_list.
*
*ENDFORM.
*
**{   INSERT         &$&$&$&$                                          1
**&---------------------------------------------------------------------*
**&      Form  FILL_LIST_BOXES
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM fill_list_boxes .
*
*  CHECK gv_loaded IS INITIAL.
*
*  PERFORM fill_list_regra.
*  PERFORM fill_listarea.
*
*  gv_loaded = abap_true.
*
*ENDFORM.
**}   INSERT
*&---------------------------------------------------------------------*
*&      Form  F4_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_werks .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT /qaps/centro~werks, name1, /qaps/grp_planta~descricao
  FROM /qaps/centro
  INNER JOIN t001w
  ON /qaps/centro~werks = t001w~werks
  LEFT JOIN /qaps/grp_planta
  ON /qaps/centro~id_grp_planta = /qaps/grp_planta~id_grp_planta
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WERKS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-WERKS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_grp_planta .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo,descricao
  FROM /qaps/grp_planta
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-COD_GRP_PLANTA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_kunnr .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT kunnr,name1
  FROM kna1
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'KUNNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-KUNNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_LIFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_lifnr .
  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT lifnr,name1
  FROM lfa1
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'LIFNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-LIFNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.
ENDFORM.
FORM f4_cais.
  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT cod_cais,cais,porto
  FROM /qaps/cais
  INNER JOIN /qaps/porto
  ON   /qaps/cais~id_porto = /qaps/porto~id_porto
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'COD_CAIS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-COD_CAIS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LIST_BOXES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes.

  CHECK gv_loaded = abap_false.

*  PERFORM fill_list_cidades.
  gv_loaded = abap_true.

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

  DEFINE show_message.
    MESSAGE &1 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  END-OF-DEFINITION.

*  IF gs_cidade-index IS INITIAL.
*    show_message 'Cidade é um campo obrigatório'.
*  ENDIF.

  CASE gv_option.
    WHEN 'W'.
      IF gs_data-werks IS INITIAL.
        show_message 'Centro é campo obrigatório'.
      ENDIF.
      SELECT SINGLE * FROM /qaps/v_ponto
        WHERE codigo = @gs_data-werks
        INTO CORRESPONDING FIELDS OF @gs_data.
    WHEN 'G'.
      IF gs_data-cod_grp_planta IS INITIAL.
        show_message 'Grp Planta é campo obrigatório'.
      ENDIF.

      SELECT SINGLE *
        FROM /qaps/grp_planta
        WHERE codigo = @gs_data-cod_grp_planta
        INTO @data(ls_grp_planta).
  ENDCASE.

  IF sy-subrc NE 0.
    show_message 'Entrada inexistente'.
  ENDIF.

  LEAVE TO SCREEN 0.

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

*  BREAK-POINT.

  CHECK gv_loaded_edit = abap_false.

  CLEAR: gv_tipo_ponto_planta,
         gv_tipo_ponto_grp_planta.

  IF NOT gs_data-werks IS INITIAL.
    gs_data-dsc_werks = gs_data-dsc_werks.
    gv_tipo_ponto_planta = 'X'.
    gv_option = 'W'.
  ELSEIF NOT gs_data-cod_grp_planta IS INITIAL.
*    gs_data-dsc_ = gs_data-dsc_grp_planta.
    gv_tipo_ponto_grp_planta = 'X'.
    gv_option = 'F'.
  ENDIF.

  gv_loaded_edit = abap_true.

ENDFORM.
