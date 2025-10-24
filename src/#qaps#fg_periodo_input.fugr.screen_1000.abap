PROCESS BEFORE OUTPUT.

  MODULE status_1000.

PROCESS AFTER INPUT.

  MODULE exit_command AT EXIT-COMMAND.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.

  FIELD gs_periodo-year   MODULE listbox_year.
  FIELD gs_periodo-month  MODULE listbox_month.
