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
    WHEN 'OK'.

      gs_data-periodo_inicial = gs_periodo_inicial-year && gs_periodo_inicial-month.
      gs_data-periodo_final = gs_periodo_final-year && gs_periodo_final-month.

      IF gs_data-periodo_inicial > gs_data-periodo_final.
        MESSAGE 'Período final deve ser superior ao período inicial' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE periodo_inicial_month INPUT.
  PERFORM listbox_month CHANGING gs_periodo_inicial-month.
ENDMODULE.                 " LISTBOX_MONTH  INPUT

MODULE periodo_inicial_year INPUT.
  PERFORM listbox_year CHANGING gs_periodo_inicial-year.
ENDMODULE.                 " LSITBOX_YEAR  INPUT
MODULE periodo_final_month INPUT.
  PERFORM listbox_month CHANGING gs_periodo_final-month.
ENDMODULE.                 " LISTBOX_MONTH  INPUT

MODULE periodo_final_year INPUT.
  PERFORM listbox_year CHANGING gs_periodo_final-year.
ENDMODULE.                 " LSITBOX_YEAR  INPUT
