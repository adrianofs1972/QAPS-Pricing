*&---------------------------------------------------------------------*
*& Include /QAPS/REP_TIPO_LISTA_TOP                          PoolMóds.        /QAPS/REP_TIPO_LISTA
*&
*&---------------------------------------------------------------------*
PROGRAM /qaps/rep_material.

DATA: go_controller TYPE REF TO /qaps/cl_ctrl_rel_0002.
DATA gv_tree_visible TYPE abap_bool VALUE abap_true.
DATA gv_detail TYPE abap_bool VALUE abap_false.
