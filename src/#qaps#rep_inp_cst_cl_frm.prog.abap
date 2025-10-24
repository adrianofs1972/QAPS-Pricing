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
FORM initialize .

  SET TITLEBAR '1000'.
  IF gv_tree_visible = abap_true.
    SET PF-STATUS '1000' EXCLUDING 'SHOW'.
  ELSE.
    SET PF-STATUS '1000' EXCLUDING 'HIDE'.
  ENDIF.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_input_custo_calc( ).
  ENDIF.

  go_controller->set_tipo_lista( gs_tp_lista ).
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
FORM pbo_0500 .
  SET TITLEBAR '1000'.
  SET PF-STATUS '1000'.
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

  DATA lv_id_tp_lista TYPE /qaps/ed_tp_lista.

*  lv_id_simulacao = |{ gv_id_tp_lista ALPHA = IN WIDTH  = 10 }|.

  SELECT SINGLE *
    FROM /qaps/tp_lista
    WHERE cod_tp_lista = @gv_cod_tp_lista
    INTO CORRESPONDING FIELDS OF @gs_tp_lista.

  IF sy-subrc IS INITIAL.
    CLEAR gv_cod_tp_lista.
    CALL SCREEN 1000.
  ELSE.
    CLEAR gv_cod_tp_lista.
    MESSAGE 'Lista não existe' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_TIPO_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_tipo_lista .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT cod_tp_lista,descricao
  FROM /qaps/tp_lista
  INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'COD_TP_LISTA'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_COD_TP_LISTA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
