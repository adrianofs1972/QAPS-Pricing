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
    WHEN 'ESCOPO'.
      IF gv_escopo_global = abap_true.
        gv_required = abap_false.
      ELSEIF gv_escopo_tp_lista = abap_true.
        gv_required = abap_true.
      ENDIF.
    WHEN 'TIPO_VALOR'.
      IF gv_tipo_dado_valor = abap_true.
        gv_required_moeda = abap_true.
      ELSEIF gv_tipo_dado_percentual = abap_true.
        gv_required_moeda = abap_false.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE user_command_1000 INPUT.
  CASE sy-ucomm.
    WHEN 'ESCOPO'.
      IF gv_escopo_global = abap_true.
        gv_required = abap_false.
      ELSEIF gv_escopo_tp_lista = abap_true.
        gv_required = abap_true.
      ENDIF.
    WHEN 'TIPO_VALOR'.
      IF gv_tipo_dado_valor = abap_true.
        gv_required_moeda = abap_true.
      ELSEIF gv_tipo_dado_percentual = abap_true.
        gv_required_moeda = abap_false.
      ENDIF.
    WHEN 'OK'.
      PERFORM F_OK.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
