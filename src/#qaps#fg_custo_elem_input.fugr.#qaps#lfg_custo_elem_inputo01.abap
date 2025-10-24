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
  SET PF-STATUS 'GENERAL'.
  SET TITLEBAR '1000' WITH gv_tiltle.

  PERFORM fill_list_boxes.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GS_TP_LISTA-INDEX'.

        IF gv_required = abap_false.
*          screen-required = 0.
          screen-input = 0.
        ELSE.
          screen-input = 1.
*          screen-required = 1.
        ENDIF.
        MODIFY SCREEN.

      WHEN 'GS_DATA-MOEDA'.

        IF gv_required_moeda = abap_false.
*          screen-required = 0.
          screen-input = 0.
          CLEAR gs_data-moeda.
        ELSE.
          screen-input = 1.
*          screen-required = 1.
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.

  ENDLOOP.


ENDMODULE.
