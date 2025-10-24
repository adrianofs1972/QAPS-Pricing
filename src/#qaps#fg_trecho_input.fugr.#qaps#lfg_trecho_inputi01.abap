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
*    WHEN 'TIPO_REGRA'.
*      PERFORM set_tipo_regra.
    WHEN 'ORIGEM'.
      PERFORM set_origem.
    WHEN 'DESTINO'.
      PERFORM set_destino.
    WHEN 'OK'.
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modal INPUT.

  gs_data-dsc_modal = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_MODAL'
                                                             iv_value  = conv #( gs_data-id_modal ) ).

  IF gs_data-dsc_modal IS INITIAL.
    CLEAR gs_data-id_modal.
  ENDIF.

ENDMODULE.
