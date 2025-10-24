*----------------------------------------------------------------------*
***INCLUDE /QAPS/REP_TIPO_LISTA_PAI.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  SCREEN_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_command INPUT.
  CASE sy-ucomm.
    WHEN 'SHOW'.
      go_controller->show_treeview( ).
      gv_tree_visible = abap_true.
    WHEN 'HIDE'.
      go_controller->hide_treeview( ).
      gv_tree_visible = abap_false.
    WHEN 'UPDATE'.
      go_controller->update_matriz_premissa( ).
  ENDCASE.
ENDMODULE.
