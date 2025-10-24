*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTO01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  PERFORM fill_list_boxes.

  PERFORM screen_general.
  PERFORM screen_tipo_regra.
*  PERFORM screen_origem.
*  PERFORM screen_destino.

  if gv_first_time = abap_true.
    PERFORM fill_origem.
    PERFORM fill_destino.
    gv_first_time = abap_false.
  endif.

ENDMODULE.
