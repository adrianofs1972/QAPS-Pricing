FUNCTION-POOL /QAPS/FG_PORTO_INPUT.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

TYPES: BEGIN OF ts_line_cidade,
         id_cidade TYPE /qaps/cidade-id_cidade,
         cidade    TYPE /qaps/cidade-cidade,
         uf        TYPE /qaps/cidade-uf,
         index     TYPE i,
       END OF ts_line_cidade.

DATA: gt_list_cidade TYPE vrm_values,
      gt_cidade      TYPE TABLE OF ts_line_cidade,
      gs_cidade      TYPE ts_line_cidade.

DATA: gv_loaded TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action type c.
DATA gs_data TYPE /qaps/s_porto.
