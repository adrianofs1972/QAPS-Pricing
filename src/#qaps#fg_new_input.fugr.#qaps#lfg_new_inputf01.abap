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
FORM fill_list_regra.

  DATA: lt_list  TYPE vrm_values,
        ls_value LIKE LINE OF lt_list.

  CHECK gs_loaded-tipo_regra IS INITIAL.

  DATA(lt_data) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).

*  SORT lt_data BY descricao ASCENDING.

  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs>).
    APPEND INITIAL LINE TO gt_lis_regra ASSIGNING FIELD-SYMBOL(<fs_regra>).
    <fs_regra>-index = sy-tabix.
    <fs_regra>-tipo_regra = <fs>-valpos.
    <fs_regra>-descricao = <fs>-ddtext.

    ls_value-key  = <fs>-valpos.
    ls_value-text = <fs>-ddtext.
    APPEND ls_value TO lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LIST_REGRA-INDEX'
      values = lt_list.

  gs_loaded-tipo_regra = abap_true.

ENDFORM.
FORM fill_listarea.

  DATA: lt_list  TYPE vrm_values,
        ls_value LIKE LINE OF lt_list.

  CHECK gs_loaded-area IS INITIAL.
  SELECT id_area, descricao
              FROM /qaps/area
              INTO CORRESPONDING FIELDS OF TABLE @gt_listarea.

  SORT gt_listarea BY descricao ASCENDING.

  LOOP AT gt_listarea ASSIGNING FIELD-SYMBOL(<fs_listarea>).
    <fs_listarea>-index = sy-tabix.
    ls_value-key  = <fs_listarea>-index.
    ls_value-text = <fs_listarea>-descricao.
    APPEND ls_value TO lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LISTAREA-INDEX'
      values = lt_list.

  gs_loaded-area = abap_true.

ENDFORM.
