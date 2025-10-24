FUNCTION-POOL /qaps/fg_cliente_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

TYPES: BEGIN OF ts_line_cidade,
         id_cidade TYPE /qaps/cidade-id_cidade,
         cidade    TYPE /qaps/cidade-cidade,
         uf        TYPE /qaps/cidade-uf,
         index     TYPE i,
       END OF ts_line_cidade.

TYPES: BEGIN OF ts_line_grp_cliente,
         id_grp_cliente TYPE /qaps/grp_cli-id_grp_cliente,
         codigo         TYPE /qaps/grp_cli-codigo,
         descricao      TYPE /qaps/grp_cli-descricao,
         index          TYPE i,
       END OF ts_line_grp_cliente.

DATA: gt_list_cidade  TYPE vrm_values,
      gt_list_grp_cli TYPE vrm_values,
      gt_cidade       TYPE TABLE OF ts_line_cidade,
      gs_cidade       TYPE ts_line_cidade,
      gt_grp_cliente  TYPE TABLE OF ts_line_grp_cliente,
      gs_grp_cliente  TYPE ts_line_grp_cliente.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.

DATA gs_data TYPE /qaps/s_cliente.
