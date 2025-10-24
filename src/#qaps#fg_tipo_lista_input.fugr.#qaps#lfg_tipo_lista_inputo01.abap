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
  SET TITLEBAR '1000'.
ENDMODULE.
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000'.
  PERFORM create_objects.
ENDMODULE.
MODULE status_3000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000'.
  PERFORM create_objects.
ENDMODULE.
*}   INSERT
