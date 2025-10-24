FUNCTION-POOL /QAPS/FG_CHOOSE_SIM_INPUT.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_CHOOSE_SIM_INPUTD...     " Local class definition
types: begin of ts_simulacao,
          id_simulacao TYPE /qaps/ed_id_simulacao,
          descricao    TYPE /qaps/simulacao-descricao,
          index        TYPE i,
       end of ts_simulacao.

DATA: gt_list_simulacao TYPE vrm_values,
      gt_simulacao      TYPE TABLE OF ts_simulacao,
      gs_simulacao      TYPE ts_simulacao.

DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.
