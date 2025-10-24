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

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING 'DELETE'.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_taxa_cambio( ).
  ENDIF.

  DATA(lv_actual_month) = sy-datum(6).

  PERFORM fill_p100_fields USING ''
                                 ''
                                 lv_actual_month
                                 sy-langu.



*  gs_data-id_original = is_data-id_original.
  gs_periodo_inicial-year = sy-datum(4).
  gs_periodo_inicial-month = sy-datum+4(2).

ENDFORM.
FORM initialize_1000 .

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING 'NOVO'.


  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_taxa_cambio( ).
  ENDIF.

  go_controller->set_header( gs_header ).
  go_controller->set_periodo( gs_periodo ).
  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

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
FORM pbo_0500.
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
    WHEN 'ENTER'.
      PERFORM submit_data.
  ENDCASE.

ENDFORM.
FORM submit_data.

  TRY.

      IF NOT gv_waers IS INITIAL
        AND NOT gs_periodo_inicial-year IS INITIAL
        AND NOT gs_periodo_inicial-month IS INITIAL
        AND NOT gs_periodo_final-year    IS INITIAL
        AND NOT gs_periodo_final-month   IS INITIAL.

        gs_header = go_controller->get_header( gv_waers ).
        gs_periodo = VALUE /qaps/s_periodo_interval(
            inicial = gs_periodo_inicial-year && gs_periodo_inicial-month
            final   = gs_periodo_final-year && gs_periodo_final-month ).
        CLEAR gv_waers.
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
FORM f4_waers.

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
      dynprofield = 'GV_WAERS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
