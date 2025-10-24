FUNCTION-POOL /qaps/fg_custo_elem_input.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_tipo_lista,
         id_tp_lista TYPE /qaps/tp_lista-id_tp_lista,
         descricao   TYPE /qaps/tp_lista-descricao,
         index       TYPE i,
       END OF ts_tipo_lista.

TYPES: BEGIN OF ts_area,
         id_area   TYPE /qaps/area-id_area,
         descricao TYPE /qaps/area-descricao,
         index     TYPE i,
       END OF ts_area.

DATA gv_action TYPE char1.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_search      TYPE char30,
      gv_tiltle      TYPE char30.

DATA: gt_list_tp_lista TYPE vrm_values,
      gt_list_area     TYPE vrm_values.

DATA: gt_tp_lista TYPE TABLE OF ts_tipo_lista,
      gs_tp_lista TYPE ts_tipo_lista,
      gt_area     TYPE TABLE OF ts_area,
      gs_area     TYPE ts_area.

DATA gs_data TYPE /qaps/s_custo_elementar.

"Escopo variável
DATA: gv_escopo_global   TYPE abap_bool,
      gv_escopo_tp_lista TYPE abap_bool.

"tipo dado
DATA: gv_tipo_dado_valor      TYPE abap_bool,
      gv_tipo_dado_percentual TYPE abap_bool.

DATA: gv_origem_dado_input  TYPE abap_bool,
      gv_origem_dado_tabela TYPE abap_bool,
      gv_origem_dado_custom TYPE abap_bool.

DATA: gv_tipo_variavel_geral     TYPE abap_bool,
      gv_tipo_variavel_compra    TYPE abap_bool,
      gv_tipo_variavel_logistica TYPE abap_bool,
      gv_tipo_variavel_produtiva TYPE abap_bool,
      gv_tipo_variavel_frete     TYPE abap_bool.

DATA: gv_required       TYPE abap_bool,
      gv_required_moeda TYPE abap_bool VALUE abap_true.
