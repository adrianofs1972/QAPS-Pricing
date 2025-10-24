FUNCTION-POOL /qaps/fg_taxa_cambio_input.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_fonte,
         id_fonte  TYPE /qaps/fonte_cmb-id_fonte,
         descricao TYPE /qaps/fonte_cmb-descricao,
         index     TYPE i,
       END OF ts_fonte.

DATA: gt_list_fonte TYPE vrm_values.

DATA: gt_fonte TYPE TABLE OF ts_fonte,
      gs_fonte TYPE ts_fonte.

DATA: gv_loaded TYPE abap_bool,
      gv_tiltle TYPE char30.

DATA gs_data TYPE /qaps/s_taxa_cambio.
