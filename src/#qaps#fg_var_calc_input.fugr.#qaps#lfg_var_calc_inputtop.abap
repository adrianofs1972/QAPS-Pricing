FUNCTION-POOL /qaps/fg_var_calc_input.    "MESSAGE-ID ..

CLASS lcl_event DEFINITION.
  PUBLIC SECTION.
    METHODS: on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id
                                                                                     e_row_id
                                                                                     es_row_no
                                                                                     sender.
ENDCLASS.


DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30,
      gv_tiltle TYPE char30.

DATA gs_data TYPE /qaps/s_custo_elementar.

DATA: go_alv TYPE REF TO cl_gui_alv_grid,
      go_event type REF TO lcl_event.
DATA: gt_data TYPE /qaps/t_custo_elementar.

CLASS lcl_event IMPLEMENTATION.
  METHOD on_hotspot_click.
    gs_data = gt_data[ e_row_id-index ].
    sy-ucomm = 'OK'.
    LEAVE TO SCREEN 0.
  ENDMETHOD.
ENDCLASS.
