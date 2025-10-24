FUNCTION-POOL /qaps/fg_ponto_input.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_cidade,
         id_cidade TYPE /qaps/cidade-id_cidade,
         cidade    TYPE /qaps/cidade-cidade,
         index     TYPE i,
       END OF ts_cidade.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c,
      gv_search      TYPE char30,
      gv_tiltle      TYPE char30.

DATA gs_data TYPE /qaps/s_ponto.

"Tipo de Ponto
DATA: gv_tipo_ponto_planta     TYPE abap_bool,
      gv_tipo_ponto_cliente    TYPE abap_bool,
      gv_tipo_ponto_fornecedor TYPE abap_bool,
*      gv_tipo_ponto_porto      TYPE abap_bool,
      gv_tipo_ponto_cais       TYPE abap_bool.

DATA gv_option TYPE c VALUE 'W'.

DATA gt_list_cidade         TYPE vrm_values.

DATA: gt_cidade TYPE TABLE OF ts_cidade,
      gs_cidade TYPE ts_cidade.
