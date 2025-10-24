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
    WHEN 'OK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE f4_matnr INPUT.
  PERFORM f4_matnr.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_MOEDA_FINAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_moeda_final INPUT.
  PERFORM f4_moeda_final.
ENDMODULE.
