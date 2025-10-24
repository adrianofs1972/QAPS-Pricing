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
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  SCREEN_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_command INPUT.
*  CASE sy-ucomm.
*    WHEN 'DELETE'.
*      TRY.
*          go_controller->delete_std_producao( is_header = gs_header ).
*          LEAVE PROGRAM.
*        CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).    "
*          DATA(ls_message) = lo_excep->get_message( ).
*          MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
*      ENDTRY.
*  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  PAI_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0500 INPUT.
  PERFORM pai_0500.
ENDMODULE.
MODULE f4_waers INPUT.
  PERFORM f4_waers.
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
