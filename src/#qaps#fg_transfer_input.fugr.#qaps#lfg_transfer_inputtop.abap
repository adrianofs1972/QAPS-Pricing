FUNCTION-POOL /qaps/fg_transfer_input.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_data,
         id_ponto       TYPE /qaps/id_ponto,
         werks          TYPE werks_d,
         dsc_werks      TYPE t001w-name1,
         id_grp_planta  TYPE /qaps/grp_planta-id_grp_planta,
         cod_grp_planta TYPE /qaps/grp_planta-codigo,
         dsc_grp_planta TYPE /qaps/grp_planta-descricao,
       END OF ts_data.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c,
      gv_search      TYPE char30,
      gv_tiltle      TYPE char30.

DATA: gs_data   TYPE ts_data,
      gs_return TYPE /qaps/s_ponto.

"Tipo de Ponto
DATA: gv_tipo_ponto_planta     TYPE abap_bool,
      gv_tipo_ponto_grp_planta TYPE abap_bool.


DATA gv_option TYPE c VALUE 'W'.
