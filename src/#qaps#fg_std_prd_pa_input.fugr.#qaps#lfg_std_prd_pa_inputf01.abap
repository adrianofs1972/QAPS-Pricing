*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
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
FORM f4_grp_planta.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo, descricao
  FROM /qaps/grp_planta
  INTO TABLE @DATA(lt_data).

  SORT lt_data BY codigo.

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
*&      Form  F4_REGIAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_regiao .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo,descricao
  FROM /qaps/regiao_prc
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-COD_REGIAO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  DEFINE error_message.
    MESSAGE &1 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  END-OF-DEFINITION.

  IF gs_data-matnr IS INITIAL.
    error_message 'Material é um campo obrigatório'.
  ENDIF.

  IF gs_data-categoria IS INITIAL.
    error_message 'Categoria é um campo obrigatório'.
  ENDIF.

  IF gs_data-meins IS INITIAL.
    error_message 'Unidade de Medida é um campo obrigatório'.
  ENDIF.

  IF gv_destino_centro = 'X' AND gs_data-werks IS INITIAL.
    error_message 'Centro é um campo obrigatório'.
  ELSEIF gv_destino_regiao = 'X' AND gs_data-cod_regiao IS INITIAL.
    error_message 'Região é um campo obrigatório'.
  ELSEIF gv_destino_grp_planta = 'X' AND gs_data-cod_grp_planta IS INITIAL.
    error_message 'Grupo de Plantas é um campo obrigatório'.
  ENDIF.

  IF gv_destino_centro = 'X' AND NOT line_exists( mt_centro[ werks = gs_data-werks ] ).
    error_message 'Centro inválido'.
  ELSEIF gv_destino_regiao = 'X' AND NOT line_exists( mt_regiao[ codigo = gs_data-cod_regiao ] ).
    error_message 'Região é um campo obrigatório'.
  ELSEIF gv_destino_grp_planta = 'X' AND NOT line_exists( mt_grp_planta[ codigo = gs_data-cod_grp_planta ] ).
    error_message 'Grupo de Plantas é um campo obrigatório'.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_MEINS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_meins .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT t006~msehi,mseht
  FROM t006
  INNER JOIN t006a
  ON t006~msehi = t006a~msehi
  WHERE spras = @sy-langu
  AND kzkeh   = 'X'
  AND dimid   = 'MASS'
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MSEHI'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MEINS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_tables .

  IF NOT go_logistica IS BOUND.
    go_logistica = NEW /qaps/cl_mdl_logistica( ).
  ENDIF.

  IF lines( mt_grp_planta ) = 0.
    mt_grp_planta = go_logistica->get_grp_planta( ).
  ENDIF.

  IF lines( mt_regiao ) = 0.
    mt_regiao = go_logistica->get_regioes( ).
  ENDIF.

  IF lines( mt_centro ) = 0.
    mt_centro = go_logistica->get_centros( ).
  ENDIF.


ENDFORM.
