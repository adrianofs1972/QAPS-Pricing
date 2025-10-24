*----------------------------------------------------------------------*
***INCLUDE /QAPS/REP_TIPO_LISTA_FRM.
*----------------------------------------------------------------------*

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

  IF gv_detail = abap_true.
    SET PF-STATUS 'DETAIL'.
  ELSEIF gv_tree_visible = abap_true.
    SET PF-STATUS '1000' EXCLUDING 'SHOW'.
  ELSE.
    SET PF-STATUS '1000' EXCLUDING 'HIDE'.
  ENDIF.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_lista_custo( ).
    go_events = NEW lcl_events( ).
    SET HANDLER go_events->on_change_mode FOR go_controller.
  ENDIF.

  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

ENDFORM.
