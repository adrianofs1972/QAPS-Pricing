FUNCTION /qaps/formulacao_v2 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_HEADER) TYPE  /QAPS/S_FORMULACAO
*"     VALUE(IT_RS_PARAM) TYPE  /QAPS/T_RS OPTIONAL
*"     VALUE(IT_PC_PARAM) TYPE  /QAPS/T_PC OPTIONAL
*"     VALUE(IV_PRIO_VS_GERENCIAL) TYPE  FLAG DEFAULT 'X'
*"     VALUE(IV_GERENCIAL) TYPE  FLAG OPTIONAL
*"     VALUE(IV_REPOSICAO) TYPE  FLAG OPTIONAL
*"     VALUE(IV_CONTABIL) TYPE  FLAG OPTIONAL
*"     VALUE(IV_DIVISOR) TYPE  INT2
*"  EXPORTING
*"     VALUE(ET_RE) TYPE  /QAPS/T_RE
*"     VALUE(ET_NG) TYPE  /QAPS/T_NG
*"     VALUE(ET_HEADER) TYPE  /QAPS/T_ZMIFIC0
*"     VALUE(ET_ITEMS) TYPE  /QAPS/T_ZMIFIC1
*"     VALUE(ET_NIVEIS_GARANTIA) TYPE  /QAPS/T_ZMIFIC2
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"     VALUE(EV_TYPE) TYPE  BAPI_MTYPE
*"     VALUE(EV_NUMBER) TYPE  SYMSGNO
*"     VALUE(EV_REFORMULATE) TYPE  FLAG
*"     VALUE(EV_CHKEY) TYPE  /QAPS/ZMICHKEY
*"     VALUE(ET_RS_PARAM) TYPE  /QAPS/T_RS
*"----------------------------------------------------------------------

  DATA: lv_valid   TYPE c,
        lv_subrc   TYPE sy-subrc,
        lv_id      TYPE i,
        ls_message TYPE  /qaps/s_general_error.

  DATA lo_excep TYPE REF TO /qaps/cx_formulation_error.
  FIELD-SYMBOLS <fs_rs> TYPE /qaps/s_rs. "ty_rs.

  PERFORM clear_workarea.

  REFRESH: et_re,
           et_ng,
           et_header,
           et_items,
           et_niveis_garantia.
  clear:   ev_reformulate,
           ev_chkey.

  CLEAR: ev_message,
         ev_type,
         ev_number.

*  opert = 'I'.
  zdiv = iv_divisor.

  it_rs[] = CORRESPONDING #( it_rs_param ).
  it_pc[] = CORRESPONDING #( it_pc_param ).

  /qaps/zmidfcig-matnr = is_header-matnr.
  /qaps/zmidfcig-werks = is_header-werks.
  /qaps/zmidfcig-/qaps/grkey = is_header-grkey.
  /qaps/zmidfcig-/qaps/period = is_header-period.
  /qaps/zmidfcig-/qaps/versao = is_header-versao.
*  /qaps/zmidfcig-/qaps/dt_entrega = is_header-dt_entrega.
  /qaps/zmidfcig-/qaps/rmeng = '1.000'.

  IF iv_prio_vs_gerencial = abap_true.
    /qaps/zmidfcig-/qaps/flpxg = 'X'.
  ELSEIF iv_gerencial = abap_true.
    /qaps/zmidfcig-/qaps/flger = 'X'.
  ELSEIF iv_reposicao = abap_true.
    /qaps/zmidfcig-/qaps/flrep = 'X'.
  ELSEIF iv_contabil = abap_true.
    /qaps/zmidfcig-/qaps/flcont = 'X'.
  ELSE.
    ev_type = 'E'.
    ev_message = 'Selecionar o tipo de opção'.
    RETURN.
  ENDIF.

  SELECT * FROM /qaps/zmi01
   INTO TABLE it_zmi01
   WHERE matnr = is_header-matnr
     AND werks = is_header-werks
     AND /qaps/grkey = is_header-grkey.

  PERFORM fill_restricao_item.
  PERFORM fill_components.

  TRY.

      PERFORM validacoes_preliminares USING is_header.

      PERFORM exec_simulation.

      et_re     = CORRESPONDING #( it_re[] ).
      et_ng     = CORRESPONDING #( it_ng[] ).

      LOOP AT et_re ASSIGNING FIELD-SYMBOL(<fs_re>).
        <fs_re>-valor = <fs_re>-cmeng * <fs_re>-verpr.
        <fs_re>-cmuni = 'TO'.
      ENDLOOP.

      SORT et_ng BY chkey.

      lv_id = 4.

      LOOP AT et_ng ASSIGNING FIELD-SYMBOL(<fs_ng>).

        TRY.
            <fs_ng>-teor_nominal = it_rs_param[ chkey = <fs_ng>-chkey ]-ref.

            IF <fs_ng>-teor_nominal IS INITIAL.
              <fs_ng>-teor_nominal = floor( <fs_ng>-gmeng ).
            ENDIF.


          CATCH cx_sy_itab_line_not_found.

        ENDTRY.

        CASE <fs_ng>-chkey.
          WHEN 'N'.
            <fs_ng>-ord = '1'.
          WHEN 'P'.
            <fs_ng>-ord = '2'.
          WHEN 'K'.
            <fs_ng>-ord = '3'.
          WHEN OTHERS.
            <fs_ng>-ord = lv_id.
            lv_id = lv_id + 1.
        ENDCASE.
      ENDLOOP.

      SORT et_ng BY ord ASCENDING.

      et_header = CORRESPONDING #( it_zmicig0 ).
      et_items  = CORRESPONDING #( it_zmicig1 ).

      LOOP AT et_items ASSIGNING FIELD-SYMBOL(<fs_items>).
        <fs_items>-/qaps/cmuni = 'TO'.
      ENDLOOP.

      et_niveis_garantia = CORRESPONDING #( it_zmicig2 ).

      LOOP AT it_zmicig2 ASSIGNING FIELD-SYMBOL(<fs_cig2>).

        TRY.
            <fs_cig2>-teor_nominal = it_rs_param[ chkey = <fs_cig2>-/qaps/chkey ]-ref.
          CATCH cx_sy_itab_line_not_found.

        ENDTRY.

      ENDLOOP.

      IF lines( it_rs_param ) = 0.
        et_rs_param = it_rs.
      else.
        et_rs_param = it_rs.
      ENDIF.

      ls_message = VALUE /qaps/s_general_error(
          type    = 'S'
          number  = '000'
          message = 'Formulação executada com sucesso' ).

    CATCH /qaps/cx_formulation_error INTO DATA(lo_formulation_error).
      IF lines( it_rs_param ) = 0.
        et_rs_param = it_rs.
      else.
        et_rs_param = it_rs.
      ENDIF.
      ev_reformulate = abap_true.
      ev_chkey = lo_formulation_error->get_increment( ).
      ls_message-message = 'Reformulate'.
      ls_message-type = 'E'.
    CATCH /qaps/cx_div_no_result INTO DATA(lo_div_no_result).
      ls_message-message = lo_div_no_result->get_message( ).
      ls_message-type = 'E'.
    CATCH /qaps/cx_general INTO DATA(lo_general).
      DATA(ls_bapiret2) = lo_general->get_message( ).
      ls_message-message = ls_bapiret2-message.
      ls_message-type = ls_bapiret2-type.
  ENDTRY.

  IF ls_message-type <> 'S'.
*    REFRESH: et_re,
*             et_ng.
*    CLEAR es_resultado.
  ENDIF.

  ev_type = ls_message-type.
  ev_number = ls_message-number.
  ev_message = ls_message-message.


ENDFUNCTION.
