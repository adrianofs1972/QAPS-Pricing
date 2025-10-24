PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  FIELD gs_data-matnr MODULE comp_description.
  FIELD gs_data-categoria MODULE categ_description.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_data-matnr      MODULE f4_matnr,
         gs_data-categoria  MODULE f4_categ.
