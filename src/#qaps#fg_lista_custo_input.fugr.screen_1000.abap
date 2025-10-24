PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  CHAIN.
    FIELD: gs_data-id_simulacao   MODULE fill_id_simulacao,
           gs_data-moeda_calculo  MODULE fill_moeda_calculo,
           gs_data-moeda_lista    MODULE fill_moeda_lista.
  ENDCHAIN.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_data-id_simulacao   MODULE f4_id_simulacao,
         gs_data-dsc_fonte      MODULE f4_dsc_fonte,
         gs_data-moeda_calculo  MODULE f4_moeda_calculo,
         gs_data-moeda_lista    MODULE f4_moeda_lista.
