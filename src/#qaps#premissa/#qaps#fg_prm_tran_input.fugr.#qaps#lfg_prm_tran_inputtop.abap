FUNCTION-POOL /qaps/fg_prm_tran_input.    "MESSAGE-ID ..

TYPE-POOLS vrm.

CONSTANTS gc_guid_null TYPE guid16 VALUE '00000000000000000000000000000000'.

*TYPES: BEGIN OF ts_line,
*         id_categoria TYPE /qaps/grp_prod-id_grupo_produto,
*         descricao    TYPE /qaps/grp_prod-descricao,
*         index(40)    TYPE c,
*       END OF ts_line.

TYPES: BEGIN OF ts_grupo_produto,
         id_grupo_produto TYPE /qaps/grp_prod-id_grupo_produto,
         descricao        TYPE /qaps/grp_prod-descricao,
         index            TYPE i,
       END OF ts_grupo_produto.

TYPES: BEGIN OF ts_mat_planejado,
         mat_planejado TYPE /qaps/material-mat_planejado,
         index         TYPE i,
       END OF ts_mat_planejado.

TYPES: BEGIN OF ts_agregador,
*         id_premissa   TYPE /qaps/prem_hdr-id_premissa,
*         id_grp_planta TYPE /qaps/prem_hdr-id_grp_planta,
*         id_centro     TYPE /qaps/prem_hdr-id_centro,
         agregador TYPE /qaps/material-agregador,
         index     TYPE i,
       END OF ts_agregador.

TYPES: BEGIN OF ts_tipo_origem_destino,
         id_ponto   TYPE /qaps/v_ponto-id_ponto,
         tipo_ponto TYPE /qaps/v_ponto-tipo_ponto,
         id_externo TYPE /qaps/v_ponto-id_externo,
         codigo     TYPE /qaps/v_ponto-codigo,
         descricao  TYPE /qaps/v_ponto-descricao,
         index      TYPE i,
       END OF ts_tipo_origem_destino.

TYPES: BEGIN OF ts_modal,
         id_modal TYPE domvalue_l,
         modal    TYPE ddtext,
         index    TYPE i,
       END OF ts_modal.

TYPES: BEGIN OF ts_categ,
         id_categoria TYPE /qaps/categ_trns-id_categoria,
         descricao    TYPE /qaps/categ_trns-descricao,
         index        TYPE i,
       END OF ts_categ.

TYPES: BEGIN OF ts_processo,
         id_processo TYPE /qaps/custo_prc-id_processo,
         descricao   TYPE /qaps/custo_prc-descricao,
         index       TYPE i,
       END OF ts_processo.

TYPES: BEGIN OF ts_trecho,
         id_trecho TYPE /qaps/s_trecho-id_trecho,
         descricao TYPE string,
         index     TYPE i,
       END OF ts_trecho.

"Área
DATA:
  gv_loaded TYPE abap_bool, "ts_loaded,
  gv_search TYPE char30,
  gv_tiltle TYPE char30.

DATA: gt_list_grupo_produtos TYPE vrm_values,
      gt_list_mat_planejado  TYPE vrm_values,
      gt_list_agregador      TYPE vrm_values,
      gt_list_tipo_origem    TYPE vrm_values,
      gt_list_tipo_destino   TYPE vrm_values,
      gt_list_modal          TYPE vrm_values,
      gt_list_categ          TYPE vrm_values,
      gt_list_processo       TYPE vrm_values,
      gt_list_trecho         TYPE vrm_values.

DATA: gt_list_gp      TYPE TABLE OF ts_grupo_produto,
      gt_list_mp      TYPE TABLE OF ts_mat_planejado,
      gt_list_ag      TYPE TABLE OF ts_agregador,
      gt_tipo_origem  TYPE TABLE OF ts_tipo_origem_destino,
      gt_tipo_destino TYPE TABLE OF ts_tipo_origem_destino,
      gt_list_mod     TYPE TABLE OF ts_modal,
      gt_list_cattr   TYPE TABLE OF ts_categ,
      gt_list_proc    TYPE TABLE OF ts_processo,
      gt_trecho       TYPE TABLE OF ts_trecho.

DATA: gs_list_gp      TYPE ts_grupo_produto,
      gs_list_mp      TYPE ts_mat_planejado,
      gs_list_ag      TYPE ts_agregador,
*      gs_list_mt      TYPE ts_line,
      gs_tipo_origem  TYPE ts_tipo_origem_destino,
      gs_tipo_destino TYPE ts_tipo_origem_destino,
      "Transporte
      gs_list_mod     TYPE ts_modal,
      gs_list_cattr   TYPE ts_categ,
      "Processo
      gs_list_proc    TYPE ts_processo,
      gs_trecho       TYPE ts_trecho.

DATA: gs_data            TYPE /qaps/s_var_input.
*      gs_custo_elementar TYPE  /qaps/s_custo_elementar.


DATA: gv_tr_option        TYPE i VALUE 4,
      gv_tr_geral         TYPE abap_bool,
      gv_tr_grupo_produto TYPE abap_bool,
      gv_tr_mat_planejado TYPE abap_bool,
      gv_tr_agregador     TYPE abap_bool VALUE abap_true,
      gv_tr_material      TYPE abap_bool.

DATA:
*      gv_origem_geral      TYPE abap_bool VALUE abap_true,
*      gv_origem_regiao     TYPE abap_bool,
*      gv_origem_cidade     TYPE abap_bool,
  gv_origem_grp_planta TYPE abap_bool VALUE abap_true,
  gv_origem_centro     TYPE abap_bool.
*      gv_origem_cliente    TYPE abap_bool,
*      gv_origem_fornec     TYPE abap_bool,
*      gv_origem_porto      TYPE abap_bool,
*      gv_origem_term_port  TYPE abap_bool.

DATA: gv_destino_geral      TYPE abap_bool VALUE abap_true,
      gv_destino_regiao     TYPE abap_bool,
      gv_destino_cidade     TYPE abap_bool,
      gv_destino_grp_planta TYPE abap_bool  VALUE abap_true,
      gv_destino_centro     TYPE abap_bool,
      gv_destino_cliente    TYPE abap_bool,
      gv_destino_fornec     TYPE abap_bool,
      gv_destino_porto      TYPE abap_bool,
      gv_destino_term_port  TYPE abap_bool.

DATA: gv_first_time TYPE abap_bool,
      gs_header     TYPE /qaps/s_premissa_header,
      gs_simulacao  TYPE /qaps/s_simulacao.
*
*DATA gv_required TYPE abap_bool.
