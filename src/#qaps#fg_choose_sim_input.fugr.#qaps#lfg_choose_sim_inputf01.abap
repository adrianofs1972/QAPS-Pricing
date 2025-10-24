*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_CHOOSE_SIM_INPUTF01.
*----------------------------------------------------------------------*
FORM fill_simulacao.

  DATA lv_index TYPE sy-tabix.

  refresh: gt_list_simulacao.

  SELECT id_simulacao, descricao
    FROM /qaps/simulacao
    where status = 'A'
    INTO CORRESPONDING FIELDS OF TABLE @gt_simulacao.

  SORT gt_simulacao BY id_simulacao.

  APPEND INITIAL LINE TO gt_list_simulacao ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_simulacao ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    lv_index = lv_index + 1.
    APPEND INITIAL LINE TO gt_list_simulacao ASSIGNING <fs_vrm>.
    <fs_listbox>-index = lv_index.
    <fs_vrm>-key  = lv_index.
    data(lv_id_simulacao) = |{ <fs_listbox>-id_simulacao ALPHA = out }|.
    CONDENSE lv_id_simulacao NO-GAPS.
    <fs_vrm>-text = lv_id_simulacao && ` - ` && <fs_listbox>-descricao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_SIMULACAO-INDEX'
      values = gt_list_simulacao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LIST_BOXES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes .

  CHECK gv_loaded IS INITIAL.

  REFRESH: gt_simulacao.

  PERFORM fill_simulacao.

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
  IF gs_simulacao-index = 0.
    MESSAGE 'Simulação é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
