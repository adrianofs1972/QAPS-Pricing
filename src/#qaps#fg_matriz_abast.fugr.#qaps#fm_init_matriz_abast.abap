FUNCTION /qaps/fm_init_matriz_abast.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ID_SIMULACAO) TYPE  /QAPS/ED_ID_SIMULACAO
*"----------------------------------------------------------------------

  "Update Matriz Abastecimento
*  DATA(lo_matriz) = NEW /qaps/cl_mdl_matriz_abast( ).
*  lo_matriz->initialize( iv_id_simulacao ).

ENDFUNCTION.
