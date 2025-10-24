FUNCTION-POOL /qaps/fg_std_prd_cp_input.    "MESSAGE-ID ..


"Área
DATA: gv_loaded TYPE abap_bool,
      gv_action type c,
      gv_tiltle TYPE char30.

DATA: gs_data        TYPE /qaps/s_std_producao_cp,
      gs_pa          TYPE /qaps/s_std_producao_pa,
      gr_not_allowed TYPE RANGE OF matnr.
