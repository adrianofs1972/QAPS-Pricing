FUNCTION-POOL /QAPS/FG_TRAJ_TRECHO_INPUT.    "MESSAGE-ID ..

"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30,
      gv_tiltle TYPE char30.
DATA: gt_source TYPE /qaps/t_trecho,
      gt_target TYPE /qaps/t_trecho.

DATA: go_cont_source TYPE REF TO cl_gui_custom_container,
      go_cont_target TYPE REF TO cl_gui_custom_container.

DATA: go_alv_source TYPE REF TO cl_gui_alv_grid,
      go_alv_target TYPE REF TO cl_gui_alv_grid.
