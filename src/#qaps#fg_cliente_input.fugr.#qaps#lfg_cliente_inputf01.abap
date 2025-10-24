*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*

FORM fill_list_boxes .

  CHECK gv_loaded IS INITIAL.

  REFRESH: gt_list_cidade, gt_cidade,
           gt_list_grp_cli, gt_grp_cliente.

  PERFORM fill_list_cidade.
  PERFORM fill_list_grp_cliente.

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
FORM fill_list_grp_cliente.


  SELECT id_grp_cliente, codigo, descricao
    FROM /qaps/grp_cli
    INTO CORRESPONDING FIELDS OF TABLE @gt_grp_cliente.

  SORT gt_grp_cliente BY codigo ASCENDING.

  APPEND INITIAL LINE TO gt_list_grp_cli ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_grp_cliente ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    APPEND INITIAL LINE TO gt_list_grp_cli ASSIGNING <fs_vrm>.
    <fs_listbox>-index = sy-tabix.
    <fs_vrm>-key  = <fs_listbox>-index.
    <fs_vrm>-text = <fs_listbox>-codigo && ` - ` && <fs_listbox>-descricao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_GRP_CLIENTE-INDEX'
      values = gt_list_grp_cli.

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

  CHECK gv_loaded_edit IS INITIAL.

  gs_cidade-index = VALUE #( gt_cidade[ id_cidade = gs_data-id_cidade ]-index OPTIONAL ).
  gs_grp_cliente-index = VALUE #( gt_grp_cliente[ id_grp_cliente = gs_data-id_grp_cliente ]-index OPTIONAL ).

  gv_loaded_edit = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_kunnr .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT kunnr, name1
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
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  IF gs_grp_cliente-index IS INITIAL.
    MESSAGE 'Grupo de Cliente é obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
