PROCESS BEFORE OUTPUT.
  MODULE status_1000.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  CHAIN.
    FIELD: gs_data-werks    MODULE fill_werks,
           gs_data-kunnr    MODULE fill_kunnr,
           gs_data-lifnr    MODULE fill_lifnr,
           gs_data-cod_cais MODULE fill_cais.
  ENDCHAIN.
  MODULE user_command_1000.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_data-werks MODULE f4_werks,
         gs_data-kunnr MODULE f4_kunnr,
         gs_data-lifnr MODULE f4_lifnr,
         gs_data-cod_cais MODULE f4_cais.
