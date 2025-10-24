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

  LOOP AT SCREEN.

    CASE gv_action.

      WHEN 'C'.
        CHECK screen-name = 'GS_LISTBOX-INDEX'.

        IF gv_required = abap_false.
          screen-required = 0.
          screen-input = 0.
        ELSE.
          screen-input = 1.
          screen-required = 1.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'U'.
        CHECK screen-name <> 'GS_DATA-DESCRICAO'.
        screen-required = 0.
        screen-input = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.


ENDMODULE.
