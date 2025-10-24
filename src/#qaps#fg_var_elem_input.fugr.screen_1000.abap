PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  CHAIN.
    FIELD gs_data-matnr MODULE fill_description.
  ENDCHAIN.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD gs_data-matnr MODULE f4_matnr.
