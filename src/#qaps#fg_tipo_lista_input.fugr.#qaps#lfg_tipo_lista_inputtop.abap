FUNCTION-POOL /qaps/fg_tipo_lista_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition
DATA: gs_tipo_lista TYPE /qaps/s_tp_lista.

"Área
data gv_loaded type abap_bool.
DATA: gt_source TYPE /qaps/t_area,
      gt_target TYPE /qaps/t_area.

data: go_cont_source type ref to cl_gui_custom_container,
      go_cont_target type ref to cl_gui_custom_container.

data: go_alv_source type ref to cl_gui_alv_grid,
      go_alv_target type ref to cl_gui_alv_grid.
