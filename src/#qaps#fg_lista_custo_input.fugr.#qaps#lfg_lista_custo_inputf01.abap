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
FORM f4_dsc_fonte.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT descricao
  FROM /qaps/fonte_cmb
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ID_SIMULACAO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-DSC_FONTE'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_id_simulacao.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT id_simulacao,descricao
  FROM /qaps/simulacao
  WHERE status = 'A'
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ID_SIMULACAO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-ID_SIMULACAO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_moeda_calculo.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT waers,ktext
  FROM tcurt
  WHERE spras = @sy-langu
  AND waers IN @gr_waers
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WAERS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MOEDA_CALCULO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_moeda_lista.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT waers,ktext
  FROM tcurt
  WHERE spras = @sy-langu
  AND waers IN @gr_waers
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WAERS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MOEDA_LISTA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

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

  IF gs_data-descricao IS INITIAL.
    show_message 'Descrição é um campo obrigatório'.
  ENDIF.

  IF gs_data-id_simulacao IS INITIAL.
    show_message 'Simulação é um campo obrigatório'.
  ENDIF.

  IF NOT gs_data-id_simulacao IS INITIAL.
    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( gs_data-id_simulacao ).
    IF ls_simulacao-status <> 'A'.
      show_message 'Status da simulação não permite gerar lista de custos'.
    ENDIF.
  ENDIF.

  SELECT *
    FROM /qaps/fonte_cmb
    INTO TABLE @DATA(lt_fonte_cmb).

  IF NOT line_exists( lt_fonte_cmb[ descricao = gs_data-dsc_fonte ] ).
    show_message 'Fonte de Câmbio inexistente'.
  ENDIF.

  IF gs_data-moeda_calculo IS INITIAL.
    show_message 'Moeda Cálculo é um campo obrigatório'.
  ENDIF.

  IF gs_data-moeda_lista IS INITIAL.
    show_message 'Moeda Lista é um campo obrigatório'.
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

  CHECK gv_loaded_edit = abap_false.

  gv_loaded_edit = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE_MOEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_range_moeda .

  REFRESH gr_waers.

  SELECT DISTINCT moeda_local AS moeda
    FROM /qaps/tx_cambio
    INTO TABLE @DATA(lt_moeda).

  SELECT DISTINCT moeda_final AS moeda
    FROM /qaps/tx_cambio
    APPENDING TABLE @lt_moeda.

  SORT lt_moeda BY moeda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_moeda COMPARING moeda.

  gr_waers = VALUE #( FOR wa IN lt_moeda
                      ( sign = 'I' option = 'EQ' low = wa-moeda ) ).

ENDFORM.
