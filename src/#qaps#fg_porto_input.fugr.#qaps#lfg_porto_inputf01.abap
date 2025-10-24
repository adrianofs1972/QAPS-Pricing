*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
FORM fill_list_boxes .

  CHECK gv_loaded IS INITIAL.

  REFRESH: gt_list_cidade.

  PERFORM fill_list_cidade.

  gv_loaded = abap_true.

ENDFORM.
FORM fill_list_cidade.


  SELECT id_cidade,cidade,uf
    FROM /qaps/cidade
    INTO CORRESPONDING FIELDS OF TABLE @gt_cidade.

  SORT gt_cidade BY uf ASCENDING cidade ASCENDING.

  APPEND INITIAL LINE TO gt_list_cidade ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_cidade ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    APPEND INITIAL LINE TO gt_list_cidade ASSIGNING <fs_vrm>.
    <fs_listbox>-index = sy-tabix.
    <fs_vrm>-key  = <fs_listbox>-index.
    <fs_vrm>-text = <fs_listbox>-uf && ` - ` && <fs_listbox>-cidade.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_CIDADE-INDEX'
      values = gt_list_cidade.

ENDFORM.
FORM pre_fill_edicao .

  CHECK gv_loaded_edit IS INITIAL.

  gs_cidade-index = VALUE #( gt_cidade[ id_cidade = gs_data-id_cidade ]-index OPTIONAL ).

  gv_loaded_edit = abap_true.

ENDFORM.
