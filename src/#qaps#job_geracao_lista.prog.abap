*&---------------------------------------------------------------------*
*& Report /QAPS/JOB_GERACAO_LISTA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /qaps/job_geracao_lista.

PARAMETERS: p_codigo TYPE /qaps/ed_cod_lista_custo.

START-OF-SELECTION.

  PERFORM f_execute.

FORM f_execute.

  DATA(lo_gerador) = NEW /qaps/cl_lista_custo_generator( ).

  TRY.
      lo_gerador->job_execute( iv_cod_lista_custo = p_codigo ).


    CATCH /qaps/cx_lista_custo_generator INTO DATA(lo_excep).    "
      WRITE lo_excep->get_message( ).
  ENDTRY.

ENDFORM.
