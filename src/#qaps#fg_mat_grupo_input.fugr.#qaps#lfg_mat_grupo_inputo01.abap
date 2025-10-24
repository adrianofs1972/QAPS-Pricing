*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTO01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000' WITH gv_tiltle.
  PERFORM create_objects.
  PERFORM fill_listbox.
ENDMODULE.
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000' WITH gv_tiltle.
  PERFORM fill_listbox.
ENDMODULE.
