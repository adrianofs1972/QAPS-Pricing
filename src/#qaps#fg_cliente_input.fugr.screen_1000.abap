PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  CHAIN.
    FIELD: gs_data-kunnr    MODULE fill_kunnr.
  ENDCHAIN.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_data-kunnr MODULE f4_kunnr.
