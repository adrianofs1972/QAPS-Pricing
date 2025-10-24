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
  CASE sy-ucomm.
    WHEN 'TIPO_PONTO'.
      IF gv_tipo_ponto_planta = abap_true.
        gv_option = 'W'.
      ELSEIF gv_tipo_ponto_grp_planta = abap_true.
        gv_option = 'G'.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'TIPO_PONTO'.
      IF gv_tipo_ponto_planta = abap_true.
        gv_option = 'W'.
      ELSEIF gv_tipo_ponto_grp_planta = abap_true.
        gv_option = 'G'.
      ENDIF.
    WHEN 'OK' .
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE f4_werks INPUT.
  PERFORM f4_werks.
ENDMODULE.
MODULE f4_grp_planta INPUT.
  PERFORM f4_grp_planta.
ENDMODULE.
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
MODULE fill_cod_grp_planta INPUT.

  select SINGLE descricao
    from /qaps/grp_planta
    where codigo = @gs_data-cod_grp_planta
    into @gs_data-dsc_grp_planta.

  if sy-subrc ne 0.
      CLEAR: gs_data-id_grp_planta,
             gs_data-cod_grp_planta,
             gs_data-dsc_grp_planta.
  endif.
ENDMODULE.
