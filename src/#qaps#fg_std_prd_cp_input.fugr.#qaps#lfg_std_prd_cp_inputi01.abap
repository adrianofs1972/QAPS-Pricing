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
*      LEAVE TO SCREEN 0.
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE f4_matnr INPUT.
  PERFORM f4_matnr.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  COMP_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE comp_description INPUT.
  TRY.
      gs_data-dsc_maktx = /qaps/cl_helper_text=>get_material_text( gs_data-componente ).
    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-componente,
             gs_data-dsc_maktx.
  ENDTRY.
ENDMODULE.
