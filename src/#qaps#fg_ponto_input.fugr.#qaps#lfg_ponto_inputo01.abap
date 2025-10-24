**----------------------------------------------------------------------*
****INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTO01.
**----------------------------------------------------------------------*
*
**{   INSERT         ECDK9A0F42                                        1
**&---------------------------------------------------------------------*
**&      Module  STATUS_1000  OUTPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE status_1000 OUTPUT.
*  SET PF-STATUS 'GENERAL'.
*  SET TITLEBAR '1000' WITH gv_tiltle.
*
*  PERFORM fill_list_boxes.
*
*  IF gv_action = 'E'.
*    PERFORM pre_fill_edicao.
*  ENDIF.
*
*  LOOP AT SCREEN.
*
*    IF screen-name = 'GS_DATA-WERKS'.
*      IF gv_option = 'W'.
*        screen-input = 1.
**        screen-required = 1.
*      ELSE.
*        CLEAR: gs_data-werks,
*               gs_data-dsc_werks.
*        screen-input = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_DATA-KUNNR'.
*      IF gv_option = 'C'.
*        screen-input = 1.
**        screen-required = 1.
*      ELSE.
*        CLEAR: gs_data-kunnr,
*               gs_data-dsc_kunnr.
*        screen-input = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_DATA-LIFNR'.
*      IF gv_option = 'F'.
*        screen-input = 1.
**        screen-required = 1.
*      ELSE.
*        CLEAR: gs_data-lifnr,
*               gs_data-dsc_lifnr.
*        screen-input = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'GS_DATA-COD_CAIS'.
*      IF gv_option = 'I'.
*        screen-input = 1.
**        screen-required = 1.
*      ELSE.
*        CLEAR: gs_data-cod_cais,
*               gs_data-dsc_cais.
*        screen-input = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*
*  ENDLOOP.
*
*
*ENDMODULE.
