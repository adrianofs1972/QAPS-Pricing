*&---------------------------------------------------------------------*
*& Include /QAPS/REP_TIPO_LISTA_TOP                          PoolMóds.        /QAPS/REP_TIPO_LISTA
*&
*&---------------------------------------------------------------------*
PROGRAM /qaps/rep_st_prd_top.

*{   INSERT         ECDK9A0F42                                        1
DATA: go_controller TYPE REF TO /qaps/cl_ctrl_std_producao.
DATA gv_tree_visible TYPE abap_bool VALUE abap_true.

DATA: gv_codigo        TYPE /qaps/s_std_producao_header-codigo,
*      gv_matnr         TYPE matnr,
*      gv_categoria     TYPE /qaps/ed_categoria,
*      gv_option_regiao TYPE abap_bool VALUE 'X',
*      gv_option_werks  TYPE abap_bool,
*      gv_option        TYPE c VALUE 'R', "R-Região, C-Centro
*      gv_werks         TYPE werks_d,
*      gv_cod_regiao    TYPE /qaps/ed_cod_regiao,
      gs_header        TYPE /qaps/s_std_producao_header.

*}   INSERT
