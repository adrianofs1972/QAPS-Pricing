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
  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  PAI_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0500 INPUT.
  perform pai_0500.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  F4_TIPO_LISTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tipo_lista INPUT.
  PERFORM f4_tipo_lista.
ENDMODULE.
