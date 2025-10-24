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
FORM fill_list_regiao.


  SELECT id_regiao, codigo, descricao
              FROM /qaps/regiao_prc
              INTO CORRESPONDING FIELDS OF TABLE @gt_regiao.

  SORT gt_regiao BY codigo ASCENDING.

  APPEND INITIAL LINE TO gt_list_regiao ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_regiao ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    APPEND INITIAL LINE TO gt_list_regiao ASSIGNING <fs_vrm>.
    <fs_listbox>-index = sy-tabix.
    <fs_vrm>-key  = <fs_listbox>-index.
    <fs_vrm>-text = <fs_listbox>-codigo && ` - ` && <fs_listbox>-descricao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_REGIAO-INDEX'
      values = gt_list_regiao.

ENDFORM.
FORM fill_list_uf.

  DATA(lt_uf) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_UF' ).

  LOOP AT lt_uf ASSIGNING FIELD-SYMBOL(<fs_uf>).

    APPEND INITIAL LINE TO gt_uf ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    APPEND INITIAL LINE TO gt_list_uf ASSIGNING FIELD-SYMBOL(<fs_vrm>).
    <fs_listbox>-index = sy-tabix.
    <fs_listbox>-uf = <fs_uf>-domvalue_l.
    <fs_listbox>-descricao = <fs_uf>-domvalue_l && ` - ` && <fs_uf>-ddtext.

    <fs_vrm>-key  = <fs_listbox>-index.
    <fs_vrm>-text = <fs_uf>-domvalue_l && ` - ` && <fs_uf>-ddtext.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_UF-INDEX'
      values = gt_list_uf.

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

  PERFORM fill_list_regiao.
  PERFORM fill_list_uf.

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

  gs_uf-index = VALUE #( gt_uf[ uf = gs_cidade-uf ]-index OPTIONAL ).
  gs_regiao-index = VALUE #( gt_regiao[ id_regiao = gs_cidade-id_regiao ]-index OPTIONAL ).

  gv_loaded_edit = abap_true.

ENDFORM.
