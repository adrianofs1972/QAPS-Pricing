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

  PERFORM fill_list_boxes.
  PERFORM pre_fill_edicao.

  CHECK gv_action = 'E'.

  LOOP AT SCREEN.
    CHECK screen-name = 'GS_DATA-COD_CAIS'.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
