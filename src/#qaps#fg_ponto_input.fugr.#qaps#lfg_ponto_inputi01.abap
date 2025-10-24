**----------------------------------------------------------------------*
****INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTI01.
**----------------------------------------------------------------------*
*
**{   INSERT         ECDK9A0F42                                        1
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_1000  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE exit_command INPUT.
*  CASE sy-ucomm.
*    WHEN 'TIPO_PONTO'.
*      IF gv_tipo_ponto_planta = abap_true.
*        gv_option = 'W'.
*      ELSEIF gv_tipo_ponto_cliente = abap_true.
*        gv_option = 'C'.
*      ELSEIF gv_tipo_ponto_fornecedor = abap_true.
*        gv_option = 'F'.
*      ELSEIF gv_tipo_ponto_cais = abap_true.
*        gv_option = 'I'.
*      ENDIF.
*    WHEN 'CANCEL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
*ENDMODULE.
*MODULE user_command_1000 INPUT.
*  CASE sy-ucomm.
*    WHEN 'TIPO_PONTO'.
*      IF gv_tipo_ponto_planta = abap_true.
*        gv_option = 'W'.
*      ELSEIF gv_tipo_ponto_cliente = abap_true.
*        gv_option = 'C'.
*      ELSEIF gv_tipo_ponto_fornecedor = abap_true.
*        gv_option = 'F'.
*      ELSEIF gv_tipo_ponto_cais = abap_true.
*        gv_option = 'I'.
*      ENDIF.
*    WHEN 'OK' .
*      PERFORM f_ok.
*    WHEN 'CANCEL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
*ENDMODULE.
*MODULE f4_werks INPUT.
*  PERFORM f4_werks.
*ENDMODULE.
*MODULE f4_kunnr INPUT.
*  PERFORM f4_kunnr.
*ENDMODULE.
*MODULE f4_lifnr INPUT.
*  PERFORM f4_lifnr.
*ENDMODULE.
*MODULE f4_cais INPUT.
*  PERFORM f4_cais.
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  FILL_WERKS  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE fill_werks INPUT.
*  TRY.
*      gs_data-dsc_werks = /qaps/cl_helper_text=>get_werks_text( gs_data-werks ).
*    CATCH /qaps/cx_general.  "
*      CLEAR gs_data-werks.
*  ENDTRY.
*
*ENDMODULE.
*MODULE fill_kunnr INPUT.
*  TRY.
*      gs_data-dsc_kunnr = /qaps/cl_helper_text=>get_kunnr_text( gs_data-kunnr ).
*    CATCH /qaps/cx_general.  "
*      CLEAR gs_data-kunnr.
*  ENDTRY.
*ENDMODULE.
*MODULE fill_lifnr INPUT.
*  TRY.
*      gs_data-dsc_lifnr = /qaps/cl_helper_text=>get_lifnr_text( gs_data-lifnr ).
*    CATCH /qaps/cx_general.  "
*      CLEAR gs_data-lifnr.
*  ENDTRY.
*ENDMODULE.
*MODULE fill_cais INPUT.
*
*  SELECT SINGLE cais
*    FROM /qaps/cais
*    WHERE cod_cais = @gs_data-cod_cais
*    INTO @gs_data-dsc_cais.
*
*  IF sy-subrc NE 0.
*    CLEAR: gs_data-cod_cais,
*           gs_data-dsc_cais.
*  ENDIF.
*
*ENDMODULE.
