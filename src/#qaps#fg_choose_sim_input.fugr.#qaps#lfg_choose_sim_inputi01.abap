*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_CHOOSE_SIM_INPUTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.
      perform f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
