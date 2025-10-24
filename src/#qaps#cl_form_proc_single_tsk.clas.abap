class /QAPS/CL_FORM_PROC_SINGLE_TSK definition
  public
  inheriting from CL_ABAP_PARALLEL
  final
  create public .

public section.

  methods RETURN_FM
    importing
      !P_TASK type CLIKE .

  methods DO
    redefinition .
protected section.
private section.

  data GV_CHKEY type /QAPS/ZMICHKEY .
  data GV_EXECUTED type ABAP_BOOL .
  data GV_REFORMULATE type FLAG .
  data MS_DATA type /QAPS/S_FORMULACAO_PROCESS .

  methods CALL_FM
    exporting
      !EV_REFORMULATE type FLAG
      !EV_CHKEY type /QAPS/ZMICHKEY
    changing
      !CS_DATA type /QAPS/S_FORMULACAO_PROCESS .
  methods EXECUTE
    changing
      !CS_DATA type /QAPS/S_FORMULACAO_PROCESS .
  methods INCREMENT_CARACTERISTICA
    importing
      !IV_CHKEY type /QAPS/ZMICHKEY
      !IV_TIMES type I
    changing
      !CT_DATA type /QAPS/T_RS .
ENDCLASS.



CLASS /QAPS/CL_FORM_PROC_SINGLE_TSK IMPLEMENTATION.


  METHOD call_fm.

    CLEAR: gv_executed,
           gv_reformulate,
           gv_chkey.

    ms_data = cs_data.

    CALL FUNCTION '/QAPS/FORMULACAO_V2'
      STARTING NEW TASK 'FORMULATION'
      CALLING return_fm ON END OF TASK
      EXPORTING
        is_header            = ms_data-is_header
        it_rs_param          = ms_data-it_rs_param
        it_pc_param          = ms_data-it_pc_param
        iv_prio_vs_gerencial = ms_data-iv_prio_vs_gerencial
        iv_gerencial         = ms_data-iv_gerencial
        iv_reposicao         = ms_data-iv_reposicao
        iv_contabil          = ms_data-iv_contabil
        iv_divisor           = ms_data-divisor.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_executed = abap_true.

    cs_data = ms_data.
    ev_reformulate = gv_reformulate.
    ev_chkey = gv_chkey.

  ENDMETHOD.


  METHOD DO.

    DATA:
*          shared_record TYPE main=>shared_record,
      input  TYPE /qaps/s_formulacao_process,
      output TYPE /qaps/s_formulacao_process.

**   Exported by MAIN->PROCCESS
**    IMPORT buffer_task_shared = shared_record FROM DATA BUFFER p_in_all.
*
**   Exported by MAIN->PROCCESS
    IMPORT buffer_task = input FROM DATA BUFFER p_in.

    execute( CHANGING cs_data = input ).

    output = input.

    EXPORT buffer_result = output TO DATA BUFFER p_out.


  ENDMETHOD.


  method EXECUTE.

    DATA: lv_valido      TYPE abap_bool,
          lv_reformulate TYPE flag,
          lv_times       TYPE i,
          lv_chkey       TYPE /qaps/zmichkey.

    WHILE lv_valido = ''.

      call_fm( IMPORTING ev_reformulate = lv_reformulate
                         ev_chkey       = lv_chkey
               CHANGING cs_data = cs_data ).

      IF lv_reformulate = 'X' AND lv_times < 20.

        lv_times = lv_times + 1.
        cs_data-it_rs_param = cs_data-et_rs_param.
        increment_caracteristica( EXPORTING iv_chkey = lv_chkey
                                            iv_times = lv_times
                                  CHANGING  ct_data  = cs_data-it_rs_param ).


      ELSE.
        lv_valido = 'X'.
        cs_data-reformulate = lv_times.
      ENDIF.

    ENDWHILE.

  endmethod.


  METHOD increment_caracteristica.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_rs>).

      CHECK <fs_rs>-chkey = iv_chkey.

      IF <fs_rs>-gmeng_bk IS INITIAL.
        <fs_rs>-gmeng_bk = <fs_rs>-gmeng.
      ENDIF.

*      IF iv_times <= 5.
      <fs_rs>-gmeng = <fs_rs>-gmeng + '0.005'."( '0.005' * iv_times ).
*      ELSE.
*        <fs_rs>-gmeng = <fs_rs>-gmeng_bk + ( '0.010' * iv_times ).
*      ENDIF.
      <fs_rs>-ymeng = <fs_rs>-gmeng.
    ENDLOOP.

  ENDMETHOD.


  METHOD return_fm.

    RECEIVE RESULTS FROM FUNCTION '/QAPS/FORMULACAO_V2'
      IMPORTING
        et_re                = ms_data-et_re
        et_ng                = ms_data-et_ng
        et_header            = ms_data-et_header
        et_items             = ms_data-et_items
        et_niveis_garantia   = ms_data-et_niveis_garantia
        ev_message           = ms_data-ev_message
        ev_type              = ms_data-ev_type
        ev_number            = ms_data-ev_number
        ev_reformulate       = gv_reformulate
        ev_chkey             = gv_chkey
        et_rs_param          = ms_data-et_rs_param.

    gv_executed = abap_true.

  ENDMETHOD.
ENDCLASS.
