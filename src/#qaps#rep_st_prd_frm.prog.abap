*----------------------------------------------------------------------*
***INCLUDE /QAPS/REP_TIPO_LISTA_FRM.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_0500 .

  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

  APPEND 'DELETE' TO lt_ucomm.
  APPEND 'EXPORT' TO lt_ucomm.
  APPEND 'IMPORT' TO lt_ucomm.

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING lt_ucomm.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_std_producao( ).
  ENDIF.


ENDFORM.
FORM initialize_1000 .

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING 'NOVO'.


  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_std_producao( ).
  ENDIF.

  go_controller->set_header( is_header = gs_header ).
  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

*  LOOP AT SCREEN.
*
*    IF NOT gs_header-werks IS INITIAL.
*      IF screen-group1 = 'REG'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF screen-group1 = 'WRK'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.


ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  PBO_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pbo_0500 .
  PERFORM initialize_0500.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PAI_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pai_0500 .

  CASE sy-ucomm.
    WHEN 'NOVO'.
      PERFORM f_novo.
    WHEN 'ENTER'.
      PERFORM submit_data.
  ENDCASE.

ENDFORM.
FORM f_novo.

  TRY.

      gs_header = go_controller->create_std_producao( ).
      CALL SCREEN 1000.

    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
FORM submit_data.

  TRY.

      IF NOT gv_codigo IS INITIAL.
        gs_header = go_controller->get_header( iv_codigo = gv_codigo ).
        CALL SCREEN 1000.
      ENDIF.

    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

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
      dynprofield = 'GV_WERKS'
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
      dynprofield = 'GV_MATNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_cod_regiao.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo,descricao
  FROM /qaps/regiao_prc
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_COD_REGIAO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_categoria.

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
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_CATEGORIA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_CODIGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_codigo .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo,descricao
    FROM /qaps/std_prd_h
      INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_CODIGO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXPORTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exportar .

  TRY.
      go_controller->export_file( ).
    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
FORM f_importar .

  TRY.
      go_controller->import_file( ).
    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_delete .
  TRY.
      go_controller->delete_std_producao( is_header = gs_header ).
      LEAVE PROGRAM.
    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).    "
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.
ENDFORM.
