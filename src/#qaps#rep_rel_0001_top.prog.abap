*&---------------------------------------------------------------------*
*& Include /QAPS/REP_TIPO_LISTA_TOP                          PoolMóds.        /QAPS/REP_TIPO_LISTA
*&
*&---------------------------------------------------------------------*
PROGRAM /qaps/rep_tipo_lista_top.

DATA: go_controller TYPE REF TO /qaps/cl_ctrl_rel_0001.
DATA gv_tree_visible TYPE abap_bool VALUE abap_true.

DATA: gs_header TYPE /qaps/s_rel_0001_header.

DATA: gv_cod_lista_custo_ref_prev	 TYPE /qaps/ed_cod_lista_custo,
      gv_cod_lista_custo_comp_prev TYPE /qaps/ed_cod_lista_custo.

TYPES: BEGIN OF ts_periodo,
         periodo TYPE /qaps/simulacao-periodo_inicial,
         index   TYPE i,
       END OF ts_periodo.

TYPES tt_periodo TYPE TABLE OF ts_periodo.

DATA: gt_list_periodo_ref TYPE vrm_values,
      gt_periodo_ref      TYPE TABLE OF ts_periodo,
      gs_periodo_ref      TYPE ts_periodo.

DATA: gt_list_periodo_comp TYPE vrm_values,
      gt_periodo_comp      TYPE TABLE OF ts_periodo,
      gs_periodo_comp      TYPE ts_periodo.
