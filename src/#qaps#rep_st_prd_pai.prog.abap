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
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
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
  CASE sy-ucomm.
    WHEN 'EXPORT'.
      PERFORM f_exportar.
    WHEN 'IMPORT'.
      PERFORM f_importar.
    WHEN 'DELETE'.
      PERFORM f_delete.
  ENDCASE.
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
*}   INSERT

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  F4_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_werks INPUT.
  PERFORM f4_werks.
ENDMODULE.
MODULE f4_matnr INPUT.
  PERFORM f4_matnr.
ENDMODULE.
MODULE f4_cod_regiao INPUT.
  PERFORM f4_cod_regiao.
ENDMODULE.
MODULE f4_categoria INPUT.
  PERFORM f4_categoria.
ENDMODULE.
*}   INSERT

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  DSC_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dsc_werks INPUT.
*  TRY.
*      gs_header-dsc_werks = /qaps/cl_helper_text=>get_werks_text( gv_werks ).
*    CATCH /qaps/cx_general.    "
*      CLEAR: gv_werks,
*             gs_header-dsc_werks.
*  ENDTRY.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  DSC_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dsc_matnr INPUT.
*  TRY.
*      gs_header-dsc_maktx = /qaps/cl_helper_text=>get_material_text( gv_matnr ).
*    CATCH /qaps/cx_general.    "
*      CLEAR: gv_matnr,
*             gs_header-dsc_maktx.
*  ENDTRY.
ENDMODULE.
MODULE dsc_categ INPUT.
*  TRY.
*      gs_header-dsc_categoria = /qaps/cl_helper_text=>get_domain_text(
*                          iv_domain =  '/QAPS/D_CATEGORIA'
*                          iv_value  = CONV #( gv_categoria ) ).
*    CATCH /qaps/cx_general.    "
*      CLEAR: gv_cod_regiao,
*             gs_header-categoria.
*  ENDTRY.
ENDMODULE.
MODULE dsc_regiao INPUT.

*  DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
*  DATA(ls_regiao) = lo_logistica->get_regiao_by_code( iv_codigo = gv_cod_regiao  ).
*
*  IF NOT ls_regiao-descricao IS INITIAL.
*    gs_header-dsc_regiao = ls_regiao-descricao.
*  ELSE.
*    CLEAR: gv_cod_regiao,
*           gs_header-dsc_regiao.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_CODIGO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_codigo INPUT.
  PERFORM f4_codigo.
ENDMODULE.
