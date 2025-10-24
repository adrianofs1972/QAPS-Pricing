FUNCTION-POOL /qaps/fg_mat_grupo_input.    "MESSAGE-ID ..

* Local class implementation
TABLES: /qaps/categ_trns.

TYPES: BEGIN OF ts_line,
         id_categoria TYPE /qaps/categ_trns-id_categoria,
         descricao    TYPE /qaps/categ_trns-descricao,
         index        TYPE i,
       END OF ts_line.

"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30,
      gv_tiltle TYPE char30.
DATA: gt_source TYPE /qaps/t_material,
      gt_target TYPE /qaps/t_material,
      gt_categ  TYPE TABLE OF /qaps/categ_trns,
      gs_categ  TYPE /qaps/categ_trns.

DATA: go_cont_source TYPE REF TO cl_gui_custom_container,
      go_cont_target TYPE REF TO cl_gui_custom_container.

DATA: go_alv_source TYPE REF TO cl_gui_alv_grid,
      go_alv_target TYPE REF TO cl_gui_alv_grid.

DATA: gt_list    TYPE vrm_values,
      gt_listbox TYPE TABLE OF ts_line,
      gs_listbox TYPE ts_line,
      gs_value   LIKE LINE OF gt_list.

CLASS dynpro_utilities DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS value_help.
ENDCLASS.

CLASS dynpro_utilities IMPLEMENTATION.
  METHOD value_help.
*    TYPES: BEGIN OF line,
*             id_categoria TYPE /qaps/categ_trns-id_categoria,
*             descricao    TYPE /qaps/categ_trns-descricao,
*           END OF line.
*    DATA lt_list TYPE STANDARD TABLE OF line.
*    SELECT id_categoria, descricao
*                FROM /qaps/categ_trns
*                INTO CORRESPONDING FIELDS OF TABLE @lt_list.
*
*    SORT lt_list BY id_categoria ASCENDING.
*
*    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*      EXPORTING
*        retfield        = 'ID_CATEGORIA'
*        value_org       = 'S'
*      TABLES
*        value_tab       = lt_list
*      EXCEPTIONS
*        parameter_error = 1
*        no_values_found = 2
*        OTHERS          = 3.
*    IF sy-subrc <> 0.
*      ...
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
