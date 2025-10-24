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
    go_controller = NEW /qaps/cl_ctrl_input_custo_elem( ).
  ENDIF.

  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  F_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_export .

  TRY.
      go_controller->export_file( ).
    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_import .

  TRY.
      go_controller->import_file( ).
    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
