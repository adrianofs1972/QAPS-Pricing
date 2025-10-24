*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
FORM f4_matnr.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT /qaps/material~matnr, maktx
  FROM /qaps/material
    INNER JOIN makt
    ON /qaps/material~matnr = makt~matnr
    WHERE makt~spras = @sy-langu
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MATNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MATNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_CATEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_categ .

  TYPES: BEGIN OF ts_categoria,
           categoria TYPE /qaps/ed_categoria,
           texto     TYPE ddtext,
         END OF ts_categoria.

  DATA lt_data TYPE TABLE OF ts_categoria.
  DATA lt_return_tab TYPE TABLE OF ddshretval.

  DATA(lt_categoria) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_CATEGORIA' ).

  lt_data = VALUE #( FOR wa IN lt_categoria
                      ( categoria = wa-domvalue_l texto = wa-ddtext ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CATEGORIA'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-CATEGORIA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  F4_MOEDA_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_moeda_final .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT waers,ltext,ktext
    FROM tcurt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WAERS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MOEDA_FINAL'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*}   INSERT
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

  PERFORM fill_fontes.

  gv_loaded = abap_true.

ENDFORM.
FORM fill_fontes.

  DATA: ls_value LIKE LINE OF gt_list_fonte.

  SELECT id_fonte, descricao
              FROM /qaps/fonte_cmb
              INTO CORRESPONDING FIELDS OF TABLE @gt_fonte.

  SORT gt_fonte BY descricao ASCENDING.

  LOOP AT gt_fonte ASSIGNING FIELD-SYMBOL(<fs_list_fonte>).
    <fs_list_fonte>-index = sy-tabix.
    ls_value-key  = <fs_list_fonte>-index.
    ls_value-text = <fs_list_fonte>-descricao.
    APPEND ls_value TO gt_list_fonte.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_FONTE-INDEX'
      values = gt_list_fonte.

ENDFORM.
