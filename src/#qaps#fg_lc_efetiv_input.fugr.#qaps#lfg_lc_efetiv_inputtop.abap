FUNCTION-POOL /qaps/fg_lc_efetiv_input.    "MESSAGE-ID ..

* INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTD...     " Local class definition

TYPES: BEGIN OF ts_grupo_aps,
         /qaps/grkey TYPE /qaps/zmigrkey,
         /qaps/grdes TYPE /qaps/zmigrdes,
         index       TYPE i,
       END OF ts_grupo_aps.

DATA: gt_list_grupo_aps TYPE vrm_values,
      gt_grupo_aps      TYPE TABLE OF ts_grupo_aps,
      gs_grupo_aps      TYPE ts_grupo_aps,
      gt_fcat           TYPE lvc_t_fcat.

"Área
DATA: gv_loaded      TYPE abap_bool,
      gv_loaded_edit TYPE abap_bool,
      gv_action      TYPE c.

DATA: gs_data      TYPE /qaps/s_efetivacao,
      gs_header    TYPE /qaps/s_lista_header,
      gs_simulacao TYPE /qaps/s_simulacao.

DATA: go_container      TYPE REF TO cl_gui_custom_container,
      go_alv            TYPE REF TO cl_gui_alv_grid,
      gt_periodo_versao TYPE /qaps/t_efet_periodo_versao.
