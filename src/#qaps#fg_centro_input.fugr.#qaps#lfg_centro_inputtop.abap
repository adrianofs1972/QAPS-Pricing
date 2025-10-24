FUNCTION-POOL /qaps/fg_centro_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

"Região
TYPES: BEGIN OF ts_line_grp_planta,
         id_grp_planta TYPE /qaps/grp_planta-id_grp_planta,
         codigo        TYPE /qaps/grp_planta-codigo,
         descricao     TYPE /qaps/grp_planta-descricao,
         index         TYPE i,
       END OF ts_line_grp_planta.

TYPES: BEGIN OF ts_line_cidade,
         id_cidade TYPE /qaps/cidade-id_cidade,
         cidade    TYPE /qaps/cidade-cidade,
         uf        TYPE /qaps/cidade-uf,
         index     TYPE i,
       END OF ts_line_cidade.

DATA: gt_list_cidade TYPE vrm_values,
      gt_cidade      TYPE TABLE OF ts_line_cidade,
      gs_cidade      TYPE ts_line_cidade.

DATA: gt_list_grp_planta TYPE vrm_values,
      gt_grp_planta      TYPE TABLE OF ts_line_grp_planta,
      gs_grp_planta      TYPE ts_line_grp_planta.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.

DATA gs_data TYPE /qaps/s_centro.
