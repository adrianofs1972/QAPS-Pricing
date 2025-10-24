FUNCTION-POOL /qaps/fg_aprovadores_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition
"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30.
DATA: gt_source TYPE /qaps/t_lista_aprov,
      gt_target TYPE /qaps/t_lista_aprov.

DATA: go_cont_source TYPE REF TO cl_gui_custom_container,
      go_cont_target TYPE REF TO cl_gui_custom_container.

DATA: go_alv_source TYPE REF TO cl_gui_alv_grid,
      go_alv_target TYPE REF TO cl_gui_alv_grid.
