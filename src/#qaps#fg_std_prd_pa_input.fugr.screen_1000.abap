PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  FIELD: gs_data-matnr          MODULE comp_description,
         gs_data-categoria      MODULE categ_description,
         gs_data-cod_regiao     MODULE regiao_description,
         gs_data-cod_grp_planta MODULE grp_planta_description,
         gs_data-werks          MODULE werks_description,
         gs_data-meins          MODULE meins_description.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_data-matnr           MODULE f4_matnr,
         gs_data-werks           MODULE f4_werks,
         gs_data-cod_grp_planta  MODULE f4_grp_planta,
         gs_data-categoria       MODULE f4_categ,
         gs_data-cod_regiao      MODULE f4_regiao,
         gs_data-meins           MODULE f4_meins.
