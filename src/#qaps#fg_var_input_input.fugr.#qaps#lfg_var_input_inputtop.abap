FUNCTION-POOL /QAPS/FG_VAR_INPUT_INPUT.    "MESSAGE-ID ..

TYPE-POOLS vrm.

*TYPES: BEGIN OF ts_line,
*         id_categoria TYPE /qaps/grp_prod-id_grupo_produto,
*         descricao    TYPE /qaps/grp_prod-descricao,
*         index(40)    TYPE c,
*       END OF ts_line.
*
*
*TYPES: BEGIN OF ts_grupo_produto,
*         id_grupo_produto TYPE /qaps/grp_prod-id_grupo_produto,
*         descricao        TYPE /qaps/grp_prod-descricao,
*         index            TYPE i,
*       END OF ts_grupo_produto.
*
*TYPES: BEGIN OF ts_mat_planejado,
*         mat_planejado TYPE /qaps/material-mat_planejado,
*         index         TYPE i,
*       END OF ts_mat_planejado.
*
*TYPES: BEGIN OF ts_agregador,
*         agregador TYPE /qaps/material-agregador,
*         index     TYPE i,
*       END OF ts_agregador.
*
*TYPES: BEGIN OF ts_ponto,
*         id_ponto   TYPE /qaps/v_ponto-id_ponto,
*         tipo_ponto TYPE /qaps/v_ponto-tipo_ponto,
**         werks      TYPE /qaps/v_ponto-werks,
**         kunnr      TYPE /qaps/v_ponto-kunnr,
**         lifnr      TYPE /qaps/v_ponto-lifnr,
*         cod_porto  TYPE /qaps/porto-cod_porto,
*         porto      TYPE /qaps/porto-porto,
*         index      TYPE i,
*       END OF ts_ponto.
*
*TYPES: BEGIN OF ts_regiao,
*         id_regiao TYPE /qaps/regiao_prc-id_regiao,
*         descricao TYPE /qaps/regiao_prc-descricao,
*         index     TYPE i,
*       END OF ts_regiao.
*
*TYPES: BEGIN OF ts_grp_planta,
*         id_grp_planta TYPE /qaps/grp_planta-id_grp_planta,
*         descricao     TYPE /qaps/grp_planta-descricao,
*         index         TYPE i,
*       END OF ts_grp_planta.
*
*TYPES: BEGIN OF ts_porto,
*         id_ponto   TYPE /qaps/v_ponto-id_ponto,
*         tipo_ponto TYPE /qaps/v_ponto-tipo_ponto,
*         werks      TYPE /qaps/v_ponto-werks,
*         kunnr      TYPE /qaps/v_ponto-kunnr,
*         lifnr      TYPE /qaps/v_ponto-lifnr,
*         cod_porto  TYPE /qaps/porto-cod_porto,
*         porto      TYPE /qaps/porto-porto,
*         index      TYPE i,
*       END OF ts_porto.
*
*TYPES: BEGIN OF ts_modal,
*         id_modal TYPE domvalue_l,
*         modal    TYPE ddtext,
*         index    TYPE i,
*       END OF ts_modal.
*
*TYPES: BEGIN OF ts_categ,
*         id_categoria TYPE /qaps/categ_trns-id_categoria,
*         descricao    TYPE /qaps/categ_trns-descricao,
*         index        TYPE i,
*       END OF ts_categ.
*
*TYPES: BEGIN OF ts_processo,
*         id_processo  TYPE /qaps/custo_prc-id_processo,
*         descricao    TYPE /qaps/custo_prc-descricao,
*         index        TYPE i,
*       END OF ts_processo.
*
*"Área
*DATA:
*  gv_loaded TYPE abap_bool, "ts_loaded,
*  gv_search TYPE char30,
*  gv_tiltle TYPE char30.
*
*DATA: gt_list_grupo_produtos   TYPE vrm_values,
*      gt_list_mat_planejado    TYPE vrm_values,
*      gt_list_agregador        TYPE vrm_values,
*      gt_list_origem_ponto     TYPE vrm_values,
*      gt_list_destino_ponto    TYPE vrm_values,
*      gt_list_origem_regiao    TYPE vrm_values,
*      gt_list_destino_regiao   TYPE vrm_values,
*      gt_list_origem_grp_plan  TYPE vrm_values,
*      gt_list_destino_grp_plan TYPE vrm_values,
*      gt_list_origem_porto     TYPE vrm_values,
*      gt_list_destino_porto    TYPE vrm_values,
*      gt_list_modal            TYPE vrm_values,
*      gt_list_categ            TYPE vrm_values,
*      gt_list_processo         TYPE vrm_values.
*
*DATA: gt_list_gp    TYPE TABLE OF ts_grupo_produto,
*      gt_list_mp    TYPE TABLE OF ts_mat_planejado,
*      gt_list_ag    TYPE TABLE OF ts_agregador,
*      gt_list_orp   TYPE TABLE OF ts_ponto,
*      gt_list_dsp   TYPE TABLE OF ts_ponto,
*      gt_list_orr   TYPE TABLE OF ts_regiao,
*      gt_list_dsr   TYPE TABLE OF ts_regiao,
*      gt_list_org   TYPE TABLE OF ts_grp_planta,
*      gt_list_dsg   TYPE TABLE OF ts_grp_planta,
*      gt_list_orpp  TYPE TABLE OF ts_porto,
*      gt_list_dspp  TYPE TABLE OF ts_porto,
*      gt_list_mod   TYPE TABLE OF ts_modal,
*      gt_list_cattr TYPE TABLE OF ts_categ,
*      gt_list_proc  TYPE TABLE OF ts_processo.
*
*DATA: gs_list_gp    TYPE ts_grupo_produto,
*      gs_list_mp    TYPE ts_mat_planejado,
*      gs_list_ag    TYPE ts_agregador,
*      gs_list_mt    TYPE ts_line,
*      "Origem
*      gs_list_orp   TYPE ts_ponto,
*      gs_list_orr   TYPE ts_regiao,
*      gs_list_org   TYPE ts_grp_planta,
*      gs_list_orpp  TYPE ts_line,
*      "Destino
*      gs_list_dsp   TYPE ts_ponto,
*      gs_list_dsr   TYPE ts_regiao,
*      gs_list_dsg   TYPE ts_grp_planta,
*      gs_list_dspp  TYPE ts_porto,
*      "Transporte
*      gs_list_mod   TYPE ts_modal,
*      gs_list_cattr TYPE ts_categ,
*      "Processo
*      gs_list_proc  TYPE ts_processo.
*
*DATA: gs_data            TYPE /qaps/s_var_input,
*      gs_custo_elementar TYPE  /qaps/s_custo_elementar.
*
*
*DATA: gv_tr_option        TYPE i VALUE 1,
*      gv_tr_geral         TYPE abap_bool VALUE abap_true,
*      gv_tr_grupo_produto TYPE abap_bool,
*      gv_tr_mat_planejado TYPE abap_bool,
*      gv_tr_agregador     TYPE abap_bool,
*      gv_tr_material      TYPE abap_bool.
**
*DATA: gv_origem_option     TYPE i VALUE 1,
*      gv_origem_ponto      TYPE abap_bool VALUE abap_true,
*      gv_origem_regiao     TYPE abap_bool,
*      gv_origem_grp_planta TYPE abap_bool,
*      gv_origem_porto      TYPE abap_bool.
*
*DATA: gv_destino_option     TYPE i VALUE 1,
*      gv_destino_ponto      TYPE abap_bool VALUE abap_true,
*      gv_destino_regiao     TYPE abap_bool,
*      gv_destino_grp_planta TYPE abap_bool,
*      gv_destino_porto      TYPE abap_bool.
*
*DATA gv_required TYPE abap_bool.
