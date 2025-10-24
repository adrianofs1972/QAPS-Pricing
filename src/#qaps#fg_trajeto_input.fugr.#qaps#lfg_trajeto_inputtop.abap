FUNCTION-POOL /QAPS/FG_TRAJETO_INPUT.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

TYPES: BEGIN OF ts_tipo_origem_destino,
         id_ponto   TYPE /qaps/v_ponto-id_ponto,
         tipo_ponto TYPE /qaps/v_ponto-tipo_ponto,
         id_externo TYPE /qaps/v_ponto-id_externo,
         codigo     TYPE /qaps/v_ponto-codigo,
         descricao  TYPE /qaps/v_ponto-descricao,
         index      TYPE i,
       END OF ts_tipo_origem_destino.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.
DATA gs_data         TYPE /qaps/s_trajeto.

DATA: gt_list_tipo_origem    TYPE vrm_values,
      gt_list_tipo_destino   TYPE vrm_values,
      gs_tipo_origem  TYPE ts_tipo_origem_destino,
      gs_tipo_destino TYPE ts_tipo_origem_destino.

DATA: gt_tipo_origem  TYPE TABLE OF ts_tipo_origem_destino,
      gt_tipo_destino TYPE TABLE OF ts_tipo_origem_destino.

DATA: gv_origem_geral      TYPE abap_bool VALUE abap_true,
      gv_origem_regiao     TYPE abap_bool,
      gv_origem_cidade     TYPE abap_bool,
      gv_origem_grp_planta TYPE abap_bool,
      gv_origem_centro     TYPE abap_bool,
      gv_origem_cliente    TYPE abap_bool,
      gv_origem_fornec     TYPE abap_bool,
      gv_origem_porto      TYPE abap_bool,
      gv_origem_term_port  TYPE abap_bool.

DATA: gv_destino_geral      TYPE abap_bool VALUE abap_true,
      gv_destino_regiao     TYPE abap_bool,
      gv_destino_cidade     TYPE abap_bool,
      gv_destino_grp_planta TYPE abap_bool,
      gv_destino_centro     TYPE abap_bool,
      gv_destino_cliente    TYPE abap_bool,
      gv_destino_fornec     TYPE abap_bool,
      gv_destino_porto      TYPE abap_bool,
      gv_destino_term_port  TYPE abap_bool.
