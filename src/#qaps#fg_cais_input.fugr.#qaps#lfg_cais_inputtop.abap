FUNCTION-POOL /qaps/fg_cais_input.    "MESSAGE-ID ..

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

"Região
TYPES: BEGIN OF ts_line_porto,
         id_porto  TYPE /qaps/porto-id_porto,
         codigo    TYPE /qaps/porto-cod_porto,
         descricao TYPE /qaps/porto-porto,
         index     TYPE i,
       END OF ts_line_porto.

DATA: gt_list_porto TYPE vrm_values,
      gt_porto      TYPE TABLE OF ts_line_porto,
      gs_porto      TYPE ts_line_porto.

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
DATA gs_data TYPE /qaps/s_cais.
