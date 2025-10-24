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
    IF screen-name = 'GS_DATA-WERKS'.
      CASE gv_option.
        WHEN 'P'.
          screen-input = '1'.
        WHEN OTHERS.
          screen-input = '0'.
          CLEAR: gs_data-werks,gs_data-dsc_werks.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'GS_DATA-COD_REGIAO'.

      CASE gv_option.
        WHEN 'R'.
          screen-input = '1'.
        WHEN OTHERS.
          screen-input = '0'.
          CLEAR: gs_data-cod_regiao,gs_data-dsc_regiao.
      ENDCASE.
      MODIFY SCREEN.

    ENDIF.

    IF screen-name = 'GS_DATA-COD_GRP_PLANTA'.

      CASE gv_option.
        WHEN 'G'.
          screen-input = '1'.
        WHEN OTHERS.
          screen-input = '0'.
          CLEAR: gs_data-cod_grp_planta,gs_data-dsc_grp_planta.
      ENDCASE.
      MODIFY SCREEN.

    ENDIF.


    IF screen-name = 'GS_DATA-MATNR' AND gv_action = 'E'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDMODULE.
