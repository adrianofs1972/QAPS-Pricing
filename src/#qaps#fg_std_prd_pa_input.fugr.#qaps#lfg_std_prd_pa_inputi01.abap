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
    WHEN 'DESTINO'.
      IF gv_destino_centro = 'X'.
        gv_option = 'P'.
      ELSEIF gv_destino_grp_planta = 'X'.
        gv_option = 'G'.
      ELSEIF gv_destino_regiao = 'X'.
        gv_option = 'R'.
      ENDIF.
    WHEN 'OK'.
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
MODULE f4_matnr INPUT.
  PERFORM f4_matnr.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  COMP_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE comp_description INPUT.
  TRY.
      gs_data-dsc_maktx = /qaps/cl_helper_text=>get_material_text( gs_data-matnr ).
    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-matnr,
             gs_data-dsc_maktx.
  ENDTRY.
ENDMODULE.
MODULE categ_description INPUT.
  TRY.
      gs_data-dsc_categoria = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_CATEGORIA'
                                                                     iv_value  = CONV #( gs_data-categoria ) ).
    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-categoria,
             gs_data-dsc_categoria.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_CATEG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_categ INPUT.
  PERFORM f4_categ.
ENDMODULE.

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  REGIAO_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE regiao_description INPUT.
  TRY.
      IF gs_data-cod_regiao <> ''.
        SELECT SINGLE descricao
          FROM /qaps/regiao_prc
          WHERE codigo = @gs_data-cod_regiao
          INTO @gs_data-dsc_regiao.

        IF sy-subrc NE 0.
          CLEAR: gs_data-cod_regiao,
               gs_data-dsc_regiao.
          MESSAGE 'Região inválida' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.

    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-cod_regiao,
             gs_data-dsc_regiao.
  ENDTRY.
ENDMODULE.
MODULE grp_planta_description INPUT.
  TRY.
      IF gs_data-cod_grp_planta <> ''.
        SELECT SINGLE descricao
          FROM /qaps/grp_planta
          WHERE codigo = @gs_data-cod_grp_planta
          INTO @gs_data-dsc_grp_planta.

        IF sy-subrc NE 0.
          CLEAR: gs_data-cod_grp_planta,
               gs_data-dsc_grp_planta.
          MESSAGE 'Grp Planta inválido' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.

    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-cod_grp_planta,
             gs_data-dsc_grp_planta.
  ENDTRY.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  F4_REGIAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_regiao INPUT.
  PERFORM f4_regiao.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_werks INPUT.
  PERFORM f4_werks.
ENDMODULE.
MODULE f4_grp_planta INPUT.
  PERFORM f4_grp_planta.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  WERKS_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE werks_description INPUT.
  TRY.
      IF gs_data-werks <> ''.
        gs_data-dsc_werks = /qaps/cl_helper_text=>get_werks_text( gs_data-werks ).
      ENDIF.

    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-werks,
             gs_data-dsc_werks.
      MESSAGE 'Centro inválido' TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_MEINS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_meins INPUT.
  PERFORM f4_meins.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MEINS_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE meins_description INPUT.

  TRY.
      gs_data-dsc_meins = /qaps/cl_helper_text=>get_meins_text( gs_data-meins ).
    CATCH /qaps/cx_general.    "
      CLEAR: gs_data-meins,
             gs_data-dsc_meins.
  ENDTRY.

ENDMODULE.
