*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTO01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000'.
*  PERFORM create_objects.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000'.

  CHECK gv_action = 'E'.

  LOOP AT SCREEN.
    CHECK screen-name = 'GS_REGIAO-CODIGO'.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
