FUNCTION-POOL /QAPS/FG_NEW_INPUT.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_tipo_regra,
         tipo_regra TYPE /qaps/tipo_regra,
         descricao  TYPE ddtext,
         index      TYPE i,
       END OF ts_tipo_regra.

TYPES: BEGIN OF ts_area,
         id_area   TYPE /qaps/area-id_area,
         descricao TYPE /qaps/area-descricao,
         index     TYPE i,
       END OF ts_area.

TYPES: BEGIN OF ts_loaded,
         tipo_regra TYPE abap_bool,
         area     TYPE abap_bool,
       END OF ts_loaded.

"Área
DATA: gs_loaded TYPE ts_loaded,
      gv_search TYPE char30,
      gv_tiltle TYPE char30.

DATA: gt_lis_regra TYPE TABLE OF ts_tipo_regra,
      gs_lis_regra TYPE ts_tipo_regra,
      gt_listarea  TYPE TABLE OF ts_area,
      gs_listarea  TYPE ts_area.

DATA gs_data TYPE /qaps/s_var_input.

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
      gv_tipo_variavel_logistica TYPE abap_bool,
      gv_tipo_variavel_produtiva TYPE abap_bool.

DATA gv_required TYPE abap_bool.
