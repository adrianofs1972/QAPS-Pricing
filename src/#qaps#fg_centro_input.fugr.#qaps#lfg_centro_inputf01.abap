*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  FILL_SOURCE_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_ID_TP_LISTA  text
*----------------------------------------------------------------------*
FORM fill_source_target  USING uv_id TYPE /qaps/ed_tp_lista.

*  DATA: lr_area_exclude TYPE RANGE OF /qaps/ed_id_area,
*        lr_area_include TYPE RANGE OF /qaps/ed_id_area.
*
*  REFRESH: gt_source,
*           gt_target.
*
*  SELECT *
*    FROM /qaps/lista_area
*    WHERE id_tp_lista = @uv_id
*    INTO TABLE @DATA(lt_lista).
*
*  IF lines( lt_lista ) > 0.
*    lr_area_exclude = VALUE #( FOR wa IN lt_lista
*                       ( sign  = 'E' option = 'EQ' low = wa-id_area ) ).
*
*    lr_area_include = VALUE #( FOR wa IN lt_lista
*                                ( sign  = 'I' option = 'EQ' low = wa-id_area ) ).
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_exclude
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_include
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_target.
*
*  ELSE.
*    SELECT *
*      FROM /qaps/area
*      INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*  ENDIF.


ENDFORM.
FORM fill_list_grp_planta.


  SELECT id_grp_planta, codigo, descricao
              FROM /qaps/grp_planta
              INTO CORRESPONDING FIELDS OF TABLE @gt_grp_planta.

  SORT gt_grp_planta BY codigo ASCENDING.

  APPEND INITIAL LINE TO gt_list_grp_planta ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_grp_planta ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    APPEND INITIAL LINE TO gt_list_grp_planta ASSIGNING <fs_vrm>.
    <fs_listbox>-index = sy-tabix.
    <fs_vrm>-key  = <fs_listbox>-index.
    <fs_vrm>-text = <fs_listbox>-codigo && ` - ` && <fs_listbox>-descricao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_GRP_PLANTA-INDEX'
      values = gt_list_grp_planta.

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

  REFRESH: gt_list_grp_planta,
           gt_list_cidade.

  PERFORM fill_list_grp_planta.
  PERFORM fill_list_cidade.

  gv_loaded = abap_true.

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

  gs_grp_planta-index = VALUE #( gt_grp_planta[ id_grp_planta = gs_data-id_grp_planta ]-index OPTIONAL ).

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
FORM f4_werks .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT werks, name1
  FROM t001w
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
