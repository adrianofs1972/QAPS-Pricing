FUNCTION-POOL /QAPS/FG_AREA_USER_INPUT.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition
"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30.
DATA: gt_source TYPE /qaps/t_area_user,
      gt_target TYPE /qaps/t_area_user.

DATA: go_cont_source TYPE REF TO cl_gui_custom_container,
      go_cont_target TYPE REF TO cl_gui_custom_container.

DATA: go_alv_source TYPE REF TO cl_gui_alv_grid,
      go_alv_target TYPE REF TO cl_gui_alv_grid.
