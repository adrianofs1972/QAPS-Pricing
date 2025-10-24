PROCESS BEFORE OUTPUT.
  MODULE pbo_0500.

PROCESS AFTER INPUT.
  MODULE exit_command AT EXIT-COMMAND.
  CHAIN.
    FIELD: gs_header-cod_lista_custo_ref,
           gs_header-cod_lista_custo_comp.
    MODULE change_date ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE pai_0500.

PROCESS ON VALUE-REQUEST.
  FIELD: gs_header-cod_lista_custo_ref      MODULE f4_codigo_ref,
         gs_header-cod_lista_custo_comp     MODULE f4_codigo_comp.
