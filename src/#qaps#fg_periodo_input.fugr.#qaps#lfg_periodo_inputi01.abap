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
MODULE listbox_month INPUT.
  PERFORM listbox_month CHANGING gs_periodo-month.
ENDMODULE.                 " LISTBOX_MONTH  INPUT

MODULE listbox_year INPUT.
  PERFORM listbox_year CHANGING gs_periodo-year.
ENDMODULE.                 " LSITBOX_YEAR  INPUT
