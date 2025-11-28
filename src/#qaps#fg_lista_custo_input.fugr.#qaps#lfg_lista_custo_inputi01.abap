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
*      IF gv_tipo_ponto_planta = abap_true.
*        gv_option = 'W'.
*      ELSEIF gv_tipo_ponto_cliente = abap_true.
*        gv_option = 'C'.
*      ELSEIF gv_tipo_ponto_fornecedor = abap_true.
*        gv_option = 'F'.
*      ELSEIF gv_tipo_ponto_cais = abap_true.
*        gv_option = 'I'.
*      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'TIPO_PONTO'.
*      IF gv_tipo_ponto_planta = abap_true.
*        gv_option = 'W'.
*      ELSEIF gv_tipo_ponto_cliente = abap_true.
*        gv_option = 'C'.
*      ELSEIF gv_tipo_ponto_fornecedor = abap_true.
*        gv_option = 'F'.
*      ELSEIF gv_tipo_ponto_cais = abap_true.
*        gv_option = 'I'.
*      ENDIF.
    WHEN 'OK' .
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE f4_dsc_fonte INPUT.
  PERFORM f4_dsc_fonte.
ENDMODULE.
MODULE f4_id_simulacao INPUT.
  PERFORM f4_id_simulacao.
ENDMODULE.
MODULE f4_moeda_calculo INPUT.
  PERFORM f4_moeda_calculo.
ENDMODULE.
MODULE f4_moeda_lista INPUT.
  PERFORM f4_moeda_lista.
ENDMODULE.
MODULE f4_metodo_custeio INPUT.
  PERFORM f4_metodo_custeio.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILL_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fill_moeda_calculo INPUT.
  TRY.
      gs_data-dsc_moeda_calculo = /qaps/cl_helper_text=>get_waers_text( gs_data-moeda_calculo ).
    CATCH /qaps/cx_general.  "
      CLEAR gs_data-moeda_calculo.
  ENDTRY.
ENDMODULE.
MODULE fill_moeda_lista INPUT.
  TRY.
      gs_data-dsc_moeda_lista = /qaps/cl_helper_text=>get_waers_text( gs_data-moeda_lista ).
    CATCH /qaps/cx_general.  "
      CLEAR gs_data-moeda_lista.
  ENDTRY.
ENDMODULE.
MODULE fill_fonte_cambio INPUT.
  PERFORM fill_fonte_cambio.

*  TRY.
*      gs_data-dsc_moeda_lista = /qaps/cl_helper_text=>get_waers_text( gs_data-moeda_lista ).
*    CATCH /qaps/cx_general.  "
*      CLEAR gs_data-moeda_lista.
*  ENDTRY.
ENDMODULE.
MODULE fill_metodo_custeio INPUT.
  PERFORM fill_metodo_custeio.
ENDMODULE.

MODULE fill_id_simulacao INPUT.

  SELECT SINGLE descricao
    FROM /qaps/simulacao
    WHERE id_simulacao = @gs_data-id_simulacao
    INTO @gs_data-dsc_simulacao.

  IF sy-subrc NE 0.
    CLEAR: gs_data-id_simulacao,
           gs_data-dsc_simulacao.
  ENDIF.

ENDMODULE.
