PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.

  FIELD gs_periodo_inicial-year   MODULE periodo_inicial_year.
  FIELD gs_periodo_inicial-month  MODULE periodo_inicial_month.

  FIELD gs_periodo_final-year   MODULE periodo_final_year.
  FIELD gs_periodo_final-month  MODULE periodo_final_month.
