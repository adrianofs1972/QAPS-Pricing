FUNCTION-POOL /QAPS/FG_STD_PROD_INPUT.    "MESSAGE-ID ..

* Local class implementation
TABLES: /qaps/std_prd_h.

TYPES: BEGIN OF ts_line,
         id_std_producao TYPE /qaps/std_prd_h-id_std_producao,
         codigo          TYPE /qaps/std_prd_h-codigo,
         descricao       TYPE /qaps/std_prd_h-descricao,
         index           TYPE i,
       END OF ts_line.

"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30,
      gv_tiltle TYPE char30.

DATA:
*      gt_source TYPE /qaps/t_material,
*      gt_target TYPE /qaps/t_material,
      gt_std_prd  TYPE TABLE OF /qaps/std_prd_h,
      gs_std_prd  TYPE /qaps/std_prd_h.

DATA: gt_list    TYPE vrm_values,
      gt_listbox TYPE TABLE OF ts_line,
      gs_listbox TYPE ts_line,
      gs_value   LIKE LINE OF gt_list.

*CLASS dynpro_utilities DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS value_help.
*ENDCLASS.
