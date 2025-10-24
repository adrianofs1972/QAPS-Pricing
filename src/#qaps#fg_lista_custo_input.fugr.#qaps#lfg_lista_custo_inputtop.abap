FUNCTION-POOL /qaps/fg_lista_custo_input.    "MESSAGE-ID ..

DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c,
      gv_search      TYPE char30,
      gv_tiltle      TYPE char30.

DATA gs_data TYPE /qaps/s_lista_header.

DATA gv_option TYPE c VALUE 'W'.

DATA gr_waers TYPE RANGE OF waers.
