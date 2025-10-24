*&---------------------------------------------------------------------*
*&  Include           /QAPS/REP_LISTA_CUSTO_CLS
*&---------------------------------------------------------------------*
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_change_mode FOR EVENT on_change_mode OF  /qaps/cl_ctrl_lista_custo
      IMPORTING iv_mode.
ENDCLASS.
CLASS lcl_events IMPLEMENTATION.
  METHOD on_change_mode.
    SET PF-STATUS 'DETAIL'.
    gv_detail = abap_true.
  ENDMETHOD.
ENDCLASS.
