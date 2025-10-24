*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTI01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'RIGHT' OR 'RIGHT_ALL' OR 'LEFT' OR 'LEFT_ALL'.
      PERFORM move USING sy-ucomm.
    WHEN 'OK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE user_command_2000 INPUT.

  CASE sy-ucomm.
    WHEN 'OK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  CREATE_DROPDOWN_BOX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
