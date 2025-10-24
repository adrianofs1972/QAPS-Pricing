*----------------------------------------------------------------------*
***INCLUDE /QAPS/REP_TIPO_LISTA_PAI.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CASE sy-dynnr.
        WHEN '0500'.
          LEAVE PROGRAM.
        WHEN '1000'.
*          SET SCREEN 0.
*          LEAVE SCREEN.
          LEAVE TO TRANSACTION '/QAPS/REL_CUSTO_COMP Relatório'.
      ENDCASE.
  ENDCASE.
ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  SCREEN_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_command INPUT.

ENDMODULE.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  PAI_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0500 INPUT.
  PERFORM pai_0500.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_CODIGO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_codigo_ref INPUT.
  PERFORM f4_codigo USING 'R'.
ENDMODULE.
MODULE f4_codigo_comp INPUT.
  PERFORM f4_codigo USING 'C'.
ENDMODULE.
MODULE f4_per_ref INPUT.
  PERFORM f4_periodo USING 'R'.
ENDMODULE.
MODULE f4_per_comp INPUT.
  PERFORM f4_periodo USING 'C'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_DATE_COMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_date INPUT.

  IF gs_header-cod_lista_custo_ref <> gv_cod_lista_custo_ref_prev.

    PERFORM fill_periodo USING gs_header-cod_lista_custo_ref
                         CHANGING gt_periodo_ref gt_list_periodo_ref.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'GS_PERIODO_REF-INDEX'
        values = gt_list_periodo_ref.
    gv_cod_lista_custo_ref_prev = gs_header-cod_lista_custo_ref.
    IF gs_header-cod_lista_custo_ref IS INITIAL.
      CLEAR gs_header-periodo_ref.
    ENDIF.
  ENDIF.

  IF gs_header-cod_lista_custo_comp <> gv_cod_lista_custo_comp_prev.

    PERFORM fill_periodo USING gs_header-cod_lista_custo_comp
                         CHANGING gt_periodo_comp gt_list_periodo_comp.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'GS_PERIODO_COMP-INDEX'
        values = gt_list_periodo_comp.
    gv_cod_lista_custo_comp_prev = gs_header-cod_lista_custo_comp.
    IF gs_header-cod_lista_custo_comp IS INITIAL.
      CLEAR gs_header-periodo_comp.
    ENDIF.
  ENDIF.
ENDMODULE.
