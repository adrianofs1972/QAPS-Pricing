FUNCTION-POOL /qaps/fg_grp_planta_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

"Região
TYPES: BEGIN OF ts_line_regiao,
         id_regiao TYPE /qaps/regiao_prc-id_regiao,
         codigo    TYPE /qaps/regiao_prc-codigo,
         descricao TYPE /qaps/regiao_prc-descricao,
         index     TYPE i,
       END OF ts_line_regiao.

DATA: gt_list_regiao TYPE vrm_values,
      gt_regiao      TYPE TABLE OF ts_line_regiao,
      gs_regiao      TYPE ts_line_regiao.

"UF
TYPES: BEGIN OF ts_line_uf,
         uf        TYPE /qaps/s_cidade-uf,
         descricao TYPE ddtext,
         index     TYPE i,
       END OF ts_line_uf.

DATA: gt_list_uf TYPE vrm_values,
      gt_uf      TYPE TABLE OF ts_line_uf,
      gs_uf      TYPE ts_line_uf.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.
DATA gs_grp_planta TYPE /qaps/s_grp_planta.
