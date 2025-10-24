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
  SET PF-STATUS '1000'.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_tipo_lista( ).
  ENDIF.

  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

ENDFORM.
*}   INSERT
