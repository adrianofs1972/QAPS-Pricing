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
    WHEN 'BACK'.
      IF gv_detail = abap_false.
        LEAVE PROGRAM.
      ENDIF.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  PAI_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'EXPORTAR'.
      go_controller->exportar( ).
    WHEN 'SHOW'.
      go_controller->show_treeview( ).
      gv_tree_visible = abap_true.
    WHEN 'HIDE'.
      go_controller->hide_treeview( ).
      gv_tree_visible = abap_false.
  ENDCASE.
ENDMODULE.
*}   INSERT
