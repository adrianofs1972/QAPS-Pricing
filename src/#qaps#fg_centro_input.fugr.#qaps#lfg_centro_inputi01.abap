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

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  F4_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_werks INPUT.
  PERFORM f4_werks.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  FILL_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fill_werks INPUT.
  TRY.
      gs_data-dsc_werks = /qaps/cl_helper_text=>get_werks_text( gs_data-werks ).
    CATCH /qaps/cx_general.  "
      CLEAR gs_data-werks.
  ENDTRY.
ENDMODULE.
