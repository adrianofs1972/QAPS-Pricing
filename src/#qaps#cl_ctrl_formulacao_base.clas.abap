class /QAPS/CL_CTRL_FORMULACAO_BASE definition
  public
  abstract
  create public .

public section.

  interfaces /QAPS/IF_CTRL_FORMULACAO .

  aliases APROVE
    for /QAPS/IF_CTRL_FORMULACAO~APROVE .
  aliases FORMULATE
    for /QAPS/IF_CTRL_FORMULACAO~FORMULATE .
  aliases INITIALIZE
    for /QAPS/IF_CTRL_FORMULACAO~INITIALIZE .
  aliases REJECT
    for /QAPS/IF_CTRL_FORMULACAO~REJECT .
  aliases RESET
    for /QAPS/IF_CTRL_FORMULACAO~RESET .
  aliases SAVE
    for /QAPS/IF_CTRL_FORMULACAO~SAVE .
  aliases UPDATE
    for /QAPS/IF_CTRL_FORMULACAO~UPDATE .

  methods CONSTRUCTOR
    importing
      !IV_ACTION type CHAR1
      !IV_REPID type SY-REPID .
protected section.

  data MV_INITIALIZED type ABAP_BOOL .
  data MV_REPID type SY-REPID .
  data MV_ACTION type C .
private section.
ENDCLASS.



CLASS /QAPS/CL_CTRL_FORMULACAO_BASE IMPLEMENTATION.


  METHOD /qaps/if_ctrl_formulacao~reset.
  ENDMETHOD.


  METHOD /qaps/if_ctrl_formulacao~update.
  ENDMETHOD.


  METHOD constructor.
    mv_action = iv_action.
    mv_repid = iv_repid.
*    mo_model = NEW /qaps/cl_formulacao( iv_action ).
*    SET HANDLER disponibilidade FOR mo_model.
  ENDMETHOD.
ENDCLASS.
