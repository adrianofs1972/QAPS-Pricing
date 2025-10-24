PROCESS BEFORE OUTPUT.
  MODULE pbo_0500.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
*  CHAIN.
*    FIELD gv_werks      MODULE dsc_werks.
*    FIELD gv_matnr      MODULE dsc_matnr.
*    FIELD gv_categoria  MODULE dsc_categ.
*    FIELD gv_cod_regiao MODULE dsc_regiao.
*
*  ENDCHAIN.
  MODULE pai_0500.

PROCESS ON VALUE-REQUEST.
  FIELD: gv_codigo      MODULE f4_codigo.
*         gv_matnr      MODULE f4_matnr,
*         gv_categoria  MODULE f4_categoria,
*         gv_cod_regiao MODULE f4_cod_regiao.
