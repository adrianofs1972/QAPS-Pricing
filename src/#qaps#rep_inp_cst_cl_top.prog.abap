*&---------------------------------------------------------------------*
*& Include /QAPS/REP_TIPO_LISTA_TOP                          PoolMóds.        /QAPS/REP_TIPO_LISTA
*&
*&---------------------------------------------------------------------*
PROGRAM /qaps/rep_material.

*{   INSERT         ECDK9A0F42                                        1
DATA: go_controller TYPE REF TO /qaps/cl_ctrl_input_custo_calc.
DATA gv_tree_visible TYPE abap_bool VALUE abap_true.

DATA: gv_id_tp_lista  TYPE /qaps/s_tp_lista-id_tp_lista,
      gv_cod_tp_lista TYPE /qaps/s_tp_lista-cod_tp_lista,
      gs_tp_lista     TYPE /qaps/s_tp_lista.

*}   INSERT
