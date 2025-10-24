FUNCTION-POOL /qaps/fg_std_prd_pa_input.    "MESSAGE-ID ..


"Área
DATA: gv_loaded TYPE abap_bool,
      gv_action TYPE c,
      gv_tiltle TYPE char30.

DATA gs_data TYPE /qaps/s_std_producao_pa.

DATA: gv_destino_centro     TYPE abap_bool VALUE abap_true,
      gv_destino_regiao     TYPE abap_bool,
      gv_destino_grp_planta TYPE abap_bool,
      gv_option             TYPE c VALUE 'P'. "P - Planta, R - Região, G - Grupo de Plantas

DATA: mt_grp_planta TYPE /qaps/t_grp_planta,
      mt_regiao     TYPE /qaps/t_regiao,
      mt_centro     TYPE /qaps/t_centro.

DATA go_logistica TYPE REF TO /qaps/cl_mdl_logistica.
