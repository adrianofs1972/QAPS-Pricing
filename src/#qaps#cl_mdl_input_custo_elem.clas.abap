class /QAPS/CL_MDL_INPUT_CUSTO_ELEM definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_ACTION type /QAPS/PROCESS_ACTION optional .
  methods DELETE_INPUT_BY_ID
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT_SELECTED
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_INPUT
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT_SELECTED
    returning
      value(RETURN) type ABAP_BOOL .
  methods IMPORT_FILE
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
    raising
      /QAPS/CX_PRICING_ERROR .
  methods EXPORT_FILE
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
    raising
      /QAPS/CX_PRICING_ERROR .
  methods UPDATE_INPUT
    importing
      !IT_CHANGED_DATA type /QAPS/T_CHANGED_DATA .
  methods GET_VARIAVEIS_BY_INPUT_ID
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT_SELECTED
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_VARIAVEIS
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IV_UNAME type UNAME default SY-UNAME
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_INPUT_BY_SIMUL_PREMISSA
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_VAR_INPUT .
  methods GET_INPUT_BY_SIMULACAO_MATRIZ
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_VAR_INPUT .
  methods GET_INPUT
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/T_VAR_INPUT .
  methods GET_INPUT_BY_ID
    importing
      !IR_ID_VAR_INPUT type /QAPS/R_ID_VAR_INPUT
    returning
      value(RETURN) type /QAPS/T_VAR_INPUT .
  methods SET_PERIODO
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  methods GET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods GET_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
    returning
      value(RETURN) type /QAPS/T_TP_LISTA .
  methods SYNC_MATRIZ_ABASTECIMENTO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_INPUT_BY_TABLE
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_INPUT
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_DATA type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_INPUT_BY_SIMULACAO_ALL
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_FILE_VAR_INPUT .
protected section.
private section.

  data MO_LOGISTICA type ref to /QAPS/CL_MDL_LOGISTICA .
  data MS_SIMULACAO type /QAPS/SIMULACAO .
  data MT_INPUT type /QAPS/T_VAR_INPUT .
  data MT_CATALOG type LVC_T_FCAT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MT_DATA type ref to DATA .
  data MS_DATA type ref to DATA .

  methods FILL_HELP_VALUES
    returning
      value(RETURN) type /QAPS/T_FILE_HELP_VALUES .
  methods GET_DISPLAY_NAME_MAPPING
    importing
      !IR_DATA type ref to DATA optional
    returning
      value(RETURN) type /QAPS/T_DISPLAY_NAME .
  methods NAME_TREATMENT
    importing
      !IV_NAME type STRING
    returning
      value(RETURN) type STRING .
  methods GET_INPUT_BY_SIMULACAO
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/T_FILE_VAR_INPUT .
  methods CREATE_DYNAMIC_CATALOG
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type LVC_T_FCAT .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT .
  methods MERGE_DATA
    importing
      !IT_DATA type /QAPS/T_FILE_VAR_INPUT
      !IT_FCAT type LVC_T_FCAT .
  methods GET_REFERENCIAS
    importing
      !IT_DATA type /QAPS/T_SIMULACAO
    returning
      value(RETURN) type /QAPS/T_SIMULACAO .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
ENDCLASS.



CLASS /QAPS/CL_MDL_INPUT_CUSTO_ELEM IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_logistica = NEW /qaps/cl_mdl_logistica( ).
  ENDMETHOD.


  METHOD create_dynamic_catalog.

    DATA: lt_fcat       TYPE lvc_t_fcat,
          lt_fcat_comp  TYPE lvc_t_fcat,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_VAR_INPUT'
      CHANGING
        ct_fieldcat      = lt_fcat.

    DATA(ls_valor_template)      = lt_fcat[ fieldname = 'VALOR' ].

    CASE is_custo_elementar-tipo_variavel.
      WHEN 'G'."Geral
        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
                        AND  fieldname <> 'TIPO_ORIGEM'
                        AND  fieldname <> 'DSC_ORIGEM'
                        AND  fieldname <> 'TIPO_DESTINO'
                        AND  fieldname <> 'DSC_DESTINO'.

        LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'TIPO_ORIGEM'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_ORIGEM'. <fs_fcat>-col_pos = 5.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 6.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 7.
          ENDCASE.
        ENDLOOP.

      WHEN 'L'."Logística
        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
                        AND  fieldname <> 'DSC_MODAL'
                        AND  fieldname <> 'DSC_CATEGORIA'
                        AND  fieldname <> 'TIPO_ORIGEM'
                        AND  fieldname <> 'DSC_ORIGEM'
                        AND  fieldname <> 'TIPO_DESTINO'
                        AND  fieldname <> 'DSC_DESTINO'.

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'MODAL'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_CATEGORIA'. <fs_fcat>-col_pos = 5.
            WHEN 'TIPO_ORIGEM'. <fs_fcat>-col_pos = 6.
            WHEN 'DSC_ORIGEM'. <fs_fcat>-col_pos = 7.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 8.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 9.
          ENDCASE.
        ENDLOOP.

      WHEN 'P'."Produtiva
        DELETE lt_fcat WHERE fieldname  <> 'TIPO_REGRA'
                       AND  fieldname  <> 'KEY_INPUT'
                       AND  fieldname  <> 'TIPO_DESTINO'
                       AND  fieldname  <> 'DSC_DESTINO'
                       AND  fieldname  <> 'DSC_PROCESSO'.

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 5.
            WHEN 'DSC_PROCESSO'. <fs_fcat>-col_pos = 6.
          ENDCASE.
        ENDLOOP.

      WHEN 'F'."Frete
        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
*                        AND  fieldname <> 'DSC_MODAL'
                        AND  fieldname <> 'COD_TRECHO'
                        .

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'COD_TRECHO'. <fs_fcat>-col_pos = 4.
          ENDCASE.
        ENDLOOP.

      WHEN 'C'."Compras

        DELETE lt_fcat WHERE fieldname <> 'TIPO_REGRA'
                        AND  fieldname <> 'KEY_INPUT'
                        AND  fieldname <> 'TIPO_ORIGEM'
                        AND  fieldname <> 'DSC_ORIGEM'
                        AND  fieldname <> 'TIPO_DESTINO'
                        AND  fieldname <> 'DSC_DESTINO'.

        LOOP AT lt_fcat ASSIGNING <fs_fcat>.
          <fs_fcat>-key = 'X'.
          CASE <fs_fcat>-fieldname.
            WHEN 'ID_SIMULACAO'. <fs_fcat>-col_pos = 1.
            WHEN 'TIPO_REGRA'. <fs_fcat>-col_pos = 2.
            WHEN 'KEY_INPUT'. <fs_fcat>-col_pos = 3.
            WHEN 'TIPO_ORIGEM'. <fs_fcat>-col_pos = 4.
            WHEN 'DSC_ORIGEM'. <fs_fcat>-col_pos = 5.
            WHEN 'TIPO_DESTINO'. <fs_fcat>-col_pos = 6.
            WHEN 'DSC_DESTINO'. <fs_fcat>-col_pos = 7.
          ENDCASE.
        ENDLOOP.

    ENDCASE.

    LOOP AT lt_fcat ASSIGNING <fs_fcat>.
      <fs_fcat>-col_pos = sy-tabix.
    ENDLOOP.

    lv_pos = lines( lt_fcat )." + 1.

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial <= is_simulacao-periodo_final.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      lv_periodo = lv_mes && `/` && lv_ano.

      check lv_periodo_inicial <= is_simulacao-periodo_final.

      DATA(ls_val_new)    = ls_valor_template.

      ls_val_new-reptext = ls_val_new-scrtext_s
        = ls_val_new-scrtext_m = ls_val_new-scrtext_l = lv_periodo.
      ls_val_new-parameter0 = 'VALOR'.
      ls_val_new-parameter1 = lv_ano && lv_mes.

      ls_val_new-fieldname   =  lv_mes && `_` && lv_ano.

      CLEAR: ls_val_new-ref_table.
      ls_val_new-col_pos = lv_pos.

      APPEND: ls_val_new TO lt_fcat.

    ENDWHILE.

    mt_catalog = lt_fcat.
    return = lt_fcat.


  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_comp>  TYPE abap_componentdescr.

    DATA: lr_table      TYPE REF TO data,
          lr_line       TYPE REF TO data,
          lr_sdescr     TYPE REF TO cl_abap_structdescr,
          lr_tdescr     TYPE REF TO cl_abap_tabledescr,
          lt_components TYPE abap_component_tab,
          lt_style      TYPE lvc_t_styl,
          lt_color      TYPE lvc_t_scol.

    CLEAR mt_data.

    cl_alv_table_create=>create_dynamic_table(
      EXPORTING
        it_fieldcatalog = it_fcat
      IMPORTING
        ep_table        = mt_data "lr_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2 ).

    IF sy-subrc IS INITIAL.

      lr_tdescr  ?= cl_abap_tabledescr=>describe_by_data_ref( mt_data ).
      lr_sdescr ?= lr_tdescr->get_table_line_type( ).

      CREATE DATA ms_data TYPE HANDLE lr_sdescr.

    ELSE.
*      BREAK c060863.
      MESSAGE 'Memória cheia. Reiniciar transação' TYPE 'E'.
    ENDIF.


  ENDMETHOD.


  METHOD create_input.

    DATA: ls_data       TYPE /qaps/s_var_input,
          ls_entry      TYPE /qaps/var_input,
          lt_entry      TYPE TABLE OF /qaps/var_input,
          ls_message    TYPE bapiret2,
          lr_data       TYPE REF TO data,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo,
          lv_one_month_ok  type abap_bool.


    CALL FUNCTION '/QAPS/FM_VAR_ELEM_INPUT'
      EXPORTING
        iv_action          = 'C'
        is_simulacao       = is_simulacao
        is_custo_elementar = is_data
      IMPORTING
        es_data            = ls_data
        es_message         = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_var_input = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial <= ms_periodo-final .

      if lv_periodo_inicial = ms_periodo-final.
        lv_one_month_ok = abap_true.
      endif.

      lv_id = sy-index.

      lv_pos = lv_pos + 1.

      IF lv_id = 1.
        lv_mes = ls_periodo-month.
        lv_ano = ls_periodo-year.
      ELSE.
        lv_mes =  lv_mes + 1.

        IF lv_mes > 12.
          lv_mes = '01'.
          lv_ano = lv_ano + 1.
        ENDIF.

        lv_periodo_inicial = lv_ano && lv_mes.

      ENDIF.

      check lv_periodo_inicial <= ms_periodo-final.

      ls_entry-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_entry TO lt_entry.

    ENDWHILE.

    MODIFY /qaps/var_input FROM TABLE lt_entry.

    IF sy-subrc IS INITIAL.

      COMMIT WORK AND WAIT.
      return = abap_true.

      data(lo_sync) = new /qaps/cl_rule_sincronizacao( ).
      lo_sync->sincronizar_input_create( is_data   = ls_entry ).

    ELSE.
      MESSAGE 'Entrada já existente' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD create_input_by_table.

    DATA: lv_id_simulacao type /qaps/ed_id_simulacao,
          ls_data       TYPE /qaps/s_var_input,
          ls_entry      TYPE /qaps/var_input,
          lt_entry      TYPE TABLE OF /qaps/var_input,
          ls_message    TYPE bapiret2,
          lr_data       TYPE REF TO data,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.


    lt_entry = CORRESPONDING #( it_data ).

    DATA(lv_id_var_input) = cl_system_uuid=>create_uuid_x16_static( ).

    LOOP AT lt_entry ASSIGNING FIELD-SYMBOL(<fs_entry>).

      if lv_id_simulacao is INITIAL.
        lv_id_simulacao = <fs_entry>-id_simulacao.
      endif.

      <fs_entry>-id_var_input = lv_id_var_input.
      lr_data = REF #( <fs_entry> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.

    MODIFY /qaps/var_input FROM TABLE lt_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD delete_input.

    DATA lr_id_var_input TYPE /qaps/r_id_var_input.
    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    lr_id_var_input = VALUE #( FOR wa IN it_data
                               ( sign = 'I' option = 'EQ' low = wa-id_var_input ) ).

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.


      DATA(lo_sync) = NEW /qaps/cl_rule_sincronizacao( ).
      DATA(lv_return) = lo_sync->sincronizar_input_delete( iv_id_simulacao = ms_simulacao-id_simulacao
                                                           ir_id_var_input = lr_id_var_input ).

      IF lv_return = abap_false.
        MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
        return = abap_false.
        RETURN.
      ENDIF.

*      "Exclusão Matriz Abastecimento
*      DATA(lo_matriz_abs) = NEW /qaps/cl_mdl_matriz_abast( ).
*      DATA(lt_var_input) = get_input_by_id( lr_id_var_input ).
*      lo_matriz_abs->delete_input( lt_var_input ).

      DELETE FROM /qaps/var_input WHERE id_var_input IN lr_id_var_input.
      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD delete_input_by_id.

    DATA lr_id_var_input TYPE /qaps/r_id_var_input.
    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    lr_id_var_input = VALUE #( FOR wa IN it_data
                               ( sign = 'I' option = 'EQ' low = wa-id_var_input ) ).

*    IF lines( it_data ) = 0.
*      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*    IF question( lv_message ) = abap_true.

    "Exclusão Matriz Abastecimento
    DATA(lo_matriz_abs) = NEW /qaps/cl_mdl_matriz_abast( ).
    DATA(lt_var_input) = get_input_by_id( lr_id_var_input ).
    lo_matriz_abs->delete_input( lt_var_input ).

    DELETE FROM /qaps/var_input WHERE id_var_input IN lr_id_var_input.
    COMMIT WORK AND WAIT.

    return = abap_true.
*    ELSE.
*      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
*      return = abap_false.
*    ENDIF.

    return = abap_true.

  ENDMETHOD.


  METHOD export_file.

    DATA lt_download TYPE /qaps/t_file_upload_multitab.
    DATA: lt_data    TYPE /qaps/t_file_var_input,
          lt_mapping TYPE /qaps/t_file_from_to,
          lt_trechos TYPE /qaps/t_trecho_help_values.

    IF is_simulacao IS INITIAL.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = VALUE #( type = 'E'
                             message = 'Nenhuma simulação foi selecionada' ).
    ENDIF.

    DATA(lt_variaveis) = get_variaveis( iv_id_simulacao = is_simulacao-id_simulacao ).

    IF lines( lt_variaveis ) = 0.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = VALUE #( type = 'E'
                             message = 'Nenhuma variável disponível para exportação' ).
    ENDIF.
    break c060863.
    APPEND VALUE /qaps/s_file_from_to( tipo = 'SIMULACAO'
                                       from = 'SIMULACAO'
                                       to   = is_simulacao-id_simulacao ) TO lt_mapping.

    LOOP AT lt_variaveis INTO DATA(ls_variaveis).

      DATA(lv_index) = sy-tabix.

      lt_data = get_input_by_simulacao( is_simulacao = is_simulacao
                                        is_custo_elementar = ls_variaveis ).

      DATA(lt_catalog) = create_dynamic_catalog( is_simulacao = is_simulacao
                                        is_custo_elementar = ls_variaveis ).
      create_dynamic_table( it_fcat = lt_catalog ).

      merge_data( it_data = lt_data
                  it_fcat = lt_catalog ).

      APPEND INITIAL LINE TO lt_download ASSIGNING FIELD-SYMBOL(<fs_download>).
      <fs_download>-sheet_name = name_treatment( CONV #( ls_variaveis-descricao ) ).

      <fs_download>-data = mt_data.
      DATA(lt_display_name) = get_display_name_mapping( mt_data ).
      <fs_download>-display_name = lt_display_name.

      APPEND VALUE /qaps/s_file_from_to(
        tipo = 'VARIAVEL'
        from = <fs_download>-sheet_name
        to   = ls_variaveis-id_custo_elementar
        sheet_id = lv_index ) TO lt_mapping.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'SYS_MAPPING'.
    <fs_download>-data = REF #( lt_mapping ).
    <fs_download>-invisible = abap_true.

    DATA(lt_help) = fill_help_values(  ).

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'HELP'.
    <fs_download>-data = REF #( lt_help ).

    lt_trechos = CORRESPONDING #( mo_logistica->get_trechos( ) ).

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'TRECHOS'.
    <fs_download>-data = REF #( lt_trechos ).

*    break c060863.
    TRY.
        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        lo_file->file_download_multi_tab( it_data = lt_download
                                          iv_filename = `Simulacao_` && is_simulacao-id_simulacao ).
      CATCH /qaps/cx_file_error.    "
    ENDTRY.

  ENDMETHOD.


  METHOD fill_help_values.

    DATA: lt_data    TYPE /qaps/t_file_help_values.

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_tp_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_PONTO' ).

    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).

    LOOP AT lt_tipo_regra INTO DATA(ls_domain).
      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'TIPO_REGRA'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.

    ENDLOOP.

    APPEND VALUE #( )  TO return.

    LOOP AT lt_tp_origem_destino INTO ls_domain.
      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'TIPO_ORIGEM'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.

    ENDLOOP.

    APPEND VALUE #( )  TO return.

    LOOP AT lt_tp_origem_destino INTO ls_domain.

      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'TIPO_DESTINO'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.
    ENDLOOP.

    APPEND VALUE #( )  TO return.

    LOOP AT lt_modal INTO ls_domain.

      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'MODAL'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.
    ENDLOOP.

    APPEND VALUE #( )  TO return.

    SELECT *
      FROM /qaps/custo_prc
      INTO TABLE @DATA(lt_custo_prc).

    SORT lt_custo_prc BY descricao ASCENDING.

    LOOP AT lt_custo_prc INTO DATA(ls_custo_prc).

      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'CUSTO'
          descricao = ls_custo_prc-descricao )  TO return.
    ENDLOOP.

*    SORT return BY campo valor ASCENDING.

  ENDMETHOD.


  METHOD get_display_name_mapping.

    data: lo_table type ref to cl_abap_tabledescr,
          lo_struct type ref to cl_abap_structdescr,
          lr_line type REF TO data.

    check not ir_data is INITIAL.

*    create data lr_line like LINE OF <ft>.
    lo_table ?= cl_abap_tabledescr=>describe_by_data_ref( ir_data ).
    lo_struct ?=  lo_table->get_table_line_type( ).

    loop at lo_struct->get_components( ) into data(ls_components).
      append INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-name = ls_components-name.
      case ls_components-name.
*        when ''. <fs_return> = value #( display_name =  ''.
        when 'TIPO_REGRA'.
          <fs_return> = value #( name = ls_components-name
                                 display_name = 'Tipo Regra'
                                 width = 15 ).
        when 'DSC_CATEGORIA'.
          <fs_return> = value #( name = ls_components-name
                                 display_name = 'Categoria'
                                 width = 15 ).
        when 'DSC_MODAL'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Modal'
                                 width = 15 ).
        when 'KEY_INPUT'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'ID'
                                 width = 15 ).
        when 'TIPO_ORIGEM'.
          <fs_return> = value #( name = ls_components-name
                                 display_name = 'Tipo Origem'
                                 width = 15 ).
        when 'DSC_ORIGEM'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Origem'
                                 width = 15 ).
        when 'TIPO_DESTINO'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Tipo Destino'
                                 width = 15 ).
        when 'DSC_DESTINO'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Destino'
                                 width = 15 ).
        when 'DSC_PROCESSO'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Processo'
                                 width = 15 ).
        when 'COD_TRECHO'.
          <fs_return> = value #( name = ls_components-name
                                 display_name =  'Trecho'
                                 width = 8 ).
      endcase.

      if ls_components-name(1) = '0' or ls_components-name(1) = '1'.
        <fs_return> = value #( name = ls_components-name
                               display_name =  ls_components-name
                               width = 10 ).
        TRANSLATE <fs_return>-display_name USING '_.'.
      endif.

    endloop.


  ENDMETHOD.


  METHOD get_input.

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @iv_id_simulacao
      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).

    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_trecho) = mo_logistica->get_trechos( ).

    SELECT *
      FROM /qaps/v_ponto
      INTO TABLE @DATA(lt_ponto).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      DATA(ls_origem) = VALUE #( lt_ponto[ tipo_ponto = <fs>-tipo_origem
                                           id_ponto   = <fs>-id_origem ] OPTIONAL ).
      IF NOT ls_origem IS INITIAL.
        <fs>-id_origem = ls_origem-id_ponto.
        <fs>-dsc_tipo_origem = ls_origem-dsc_tipo_ponto.
        <fs>-dsc_origem = ls_origem-codigo.
      ENDIF.

      DATA(ls_destino) = VALUE #( lt_ponto[ tipo_ponto = <fs>-tipo_destino
                                            id_ponto   = <fs>-id_destino ] OPTIONAL ).
      IF NOT ls_destino IS INITIAL.
        <fs>-id_destino = ls_destino-id_ponto.
        <fs>-dsc_tipo_destino = ls_destino-dsc_tipo_ponto.
        <fs>-dsc_destino = ls_destino-codigo.
      ENDIF.

      "Categoria
      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

      "Modal
      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

      "Processo
      <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

      "Trecho
      IF NOT <fs>-id_trecho IS INITIAL.
        <fs>-cod_trecho = VALUE #( lt_trecho[ id_trecho = <fs>-id_trecho ]-cod_trecho OPTIONAL ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_INPUT_BY_ID.

    SELECT *
      FROM /qaps/var_input
      WHERE id_var_input in @ir_id_var_input
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).

    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_ponto) = lo_logistica->get_pontos( ).
    DATA(lt_porto) = lo_logistica->get_portos( ).
    DATA(lt_regiao) = lo_logistica->get_regioes( ).
    DATA(lt_cais) = lo_logistica->get_cais( ).
    DATA(lt_grp_planta) = lo_logistica->get_grp_planta( ).

*    BREAK-POINT.
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = value #( lt_ponto[ id_ponto = <fs>-id_origem ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_origem = ls_ponto-lifnr.
*              <fs>-dsc_tipo_origem = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_origem = ls_ponto-kunnr.
*              <fs>-dsc_tipo_origem = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_origem = ls_ponto-werks.
*              <fs>-dsc_tipo_origem = 'Centro'.
*            WHEN 'I'.
*              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_origem = ls_cais-cod_cais.
*              <fs>-dsc_tipo_origem = 'Cais'.
*            when OTHERS.
*              clear: <fs>-id_origem,
*                     <fs>-tipo_origem.
*          ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs>-id_origem ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_origem = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_origem ]-codigo OPTIONAL ).
*      ENDCASE.

      <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = value #( lt_ponto[ id_ponto = <fs>-id_destino ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_destino = ls_ponto-lifnr.
*              <fs>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_destino = ls_ponto-kunnr.
*              <fs>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_destino = ls_ponto-werks.
*              <fs>-dsc_tipo_destino = 'Centro'.
*            WHEN 'I'.
*              ls_cais = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_destino = ls_cais-cod_cais.
*              <fs>-dsc_tipo_destino = 'Cais'.
*             when OTHERS.
*              clear: <fs>-id_destino,
*                     <fs>-tipo_destino.
*          ENDCASE.
*        WHEN 'R'.
*          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs>-id_destino ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_destino = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_destino ]-codigo OPTIONAL ).
*      ENDCASE.

      "Categoria
      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

      "Modal
      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

      "Processo
      <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_input_by_simulacao.

    SELECT *
      FROM /qaps/v_var_per
      WHERE id_simulacao = @is_simulacao-id_simulacao
      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).

    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

*      <fs>-dsc_custo_elm =
*          VALUE #( lt_custo_elem[ id_custo_elementar = <fs>-id_custo_elementar ]-descricao OPTIONAL ).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_input_by_simulacao_all.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @iv_id_simulacao
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
    DATA(lo_custo_elem) = NEW /qaps/cl_mdl_custo_elementar( ).

    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_ponto) = lo_logistica->get_pontos( ).
    DATA(lt_porto) = lo_logistica->get_portos( ).
    DATA(lt_regiao) = lo_logistica->get_regioes( ).
    DATA(lt_cais) = lo_logistica->get_cais( ).
    DATA(lt_grp_planta) = lo_logistica->get_grp_planta( ).
    DATA(lt_custo_elem) = lo_custo_elem->get_variaveis_by_tipo_lista(
                                          iv_id_tp_lista = ls_simulacao-id_tp_lista ).

*    BREAK-POINT.
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      <fs>-dsc_custo_elm =
          VALUE #( lt_custo_elem[ id_custo_elementar = <fs>-id_custo_elementar ]-descricao OPTIONAL ).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = lt_ponto[ id_ponto = <fs>-id_origem ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_origem = ls_ponto-lifnr.
*              <fs>-dsc_tipo_origem = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_origem = ls_ponto-kunnr.
*              <fs>-dsc_tipo_origem = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_origem = ls_ponto-werks.
*              <fs>-dsc_tipo_origem = 'Centro'.
*            WHEN 'I'.
*              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_origem = ls_cais-cod_cais.
*              <fs>-dsc_tipo_origem = 'Cais'.
*          ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs>-id_origem ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_origem = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_origem ]-codigo OPTIONAL ).
*      ENDCASE.

      <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = lt_ponto[ id_ponto = <fs>-id_destino ].
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_destino = ls_ponto-lifnr.
*              <fs>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_destino = ls_ponto-kunnr.
*              <fs>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_destino = ls_ponto-werks.
*              <fs>-dsc_tipo_destino = 'Centro'.
*            WHEN 'I'.
*              ls_cais = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_destino = ls_cais-cod_cais.
*              <fs>-dsc_tipo_destino = 'Cais'.
*          ENDCASE.
*        WHEN 'R'.
*          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs>-id_destino ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_destino = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_destino ]-codigo OPTIONAL ).
*      ENDCASE.

      "Categoria
      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

      "Modal
      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

      "Processo
      <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.


  ENDMETHOD.


  METHOD GET_INPUT_BY_SIMULACAO_MATRIZ.

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @iv_id_simulacao
*      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).

    SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
    SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_ponto) = lo_logistica->get_pontos( ).
    DATA(lt_porto) = lo_logistica->get_portos( ).
    DATA(lt_regiao) = lo_logistica->get_regioes( ).
    DATA(lt_cais) = lo_logistica->get_cais( ).
    DATA(lt_grp_planta) = lo_logistica->get_grp_planta( ).

*    BREAK-POINT.
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      CASE <fs>-tipo_regra.
        WHEN 'GP'.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_origem.
*        WHEN 'P'.
*          DATA(ls_ponto) = value #( lt_ponto[ id_ponto = <fs>-id_origem ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_origem = ls_ponto-lifnr.
*              <fs>-dsc_tipo_origem = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_origem = ls_ponto-kunnr.
*              <fs>-dsc_tipo_origem = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_origem = ls_ponto-werks.
*              <fs>-dsc_tipo_origem = 'Centro'.
*            WHEN 'I'.
*              DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_origem = ls_cais-cod_cais.
*              <fs>-dsc_tipo_origem = 'Cais'.
*            when OTHERS.
*              clear: <fs>-id_origem,
*                     <fs>-tipo_origem.
*          ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*        WHEN 'R'.
*          <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs>-id_origem ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_origem = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_origem ]-codigo OPTIONAL ).
*      ENDCASE.

      <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).

*      CASE <fs>-tipo_destino.
*        WHEN 'P'.
*          ls_ponto = value #( lt_ponto[ id_ponto = <fs>-id_destino ] OPTIONAL ).
*          CASE ls_ponto-tipo_ponto.
*            WHEN 'F'.
*              <fs>-dsc_destino = ls_ponto-lifnr.
*              <fs>-dsc_tipo_destino = 'Fornecedor'.
*            WHEN 'C'.
*              <fs>-dsc_destino = ls_ponto-kunnr.
*              <fs>-dsc_tipo_destino = 'Cliente'.
*            WHEN 'W'.
*              <fs>-dsc_destino = ls_ponto-werks.
*              <fs>-dsc_tipo_destino = 'Centro'.
*            WHEN 'I'.
*              ls_cais = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*              <fs>-dsc_destino = ls_cais-cod_cais.
*              <fs>-dsc_tipo_destino = 'Cais'.
*             when OTHERS.
*              clear: <fs>-id_destino,
*                     <fs>-tipo_destino.
*          ENDCASE.
*        WHEN 'R'.
*          <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*        WHEN 'S'.
*          <fs>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs>-id_destino ]-cod_porto OPTIONAL ).
*        WHEN 'G'.
*          <fs>-dsc_destino = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_destino ]-codigo OPTIONAL ).
*      ENDCASE.

      "Categoria
      <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

      "Modal
      <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

      "Processo
      <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_input_by_simul_premissa.

    DATA lr_custo TYPE RANGE OF /qaps/custo_elm-id_custo_elementar.

    IF NOT line_exists( mt_input[ id_simulacao = iv_id_simulacao ] ).

      SELECT *
        FROM /qaps/var_input
        WHERE id_simulacao = @iv_id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

      IF lines( return ) > 0.
        SELECT *
        FROM /qaps/custo_elm
        FOR ALL ENTRIES IN @return
        WHERE id_custo_elementar = @return-id_custo_elementar
        AND  ( importacao = 'X' OR nacional = 'X' )
        INTO TABLE @DATA(lt_custo).

        lr_custo = VALUE #( FOR wa IN lt_custo
                            ( sign = 'I' option = 'EQ' low = wa-id_custo_elementar ) ).

      ENDIF.

      DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).


      SELECT * FROM /qaps/categ_trns INTO TABLE @DATA(lt_categ).
      SELECT * FROM /qaps/custo_prc INTO TABLE @DATA(lt_processo).

      DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).
      DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_ORIGEM_DESTINO' ).
      DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
      DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
      DATA(lt_ponto) = mo_logistica->get_pontos( ).
      DATA(lt_porto) = mo_logistica->get_portos( ).
      DATA(lt_regiao) = mo_logistica->get_regioes( ).
      DATA(lt_cais) = mo_logistica->get_cais( ).
      DATA(lt_grp_planta) = mo_logistica->get_grp_planta( ).

      CHECK lines( lr_custo ) > 0.
*    BREAK-POINT.
      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

        CASE <fs>-tipo_regra.
          WHEN 'GP'.
            <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
          WHEN 'MP'.
            <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
          WHEN 'AG'.
            <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
          WHEN 'MA'.
            DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
            CONDENSE lv_matnr.
            <fs>-key_input = lv_matnr.
            <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
        ENDCASE.

        <fs>-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_origem ]-ddtext OPTIONAL ).

*        CASE <fs>-tipo_origem.
*          WHEN 'P'.
*            DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs>-id_origem ] OPTIONAL ).
*            CASE ls_ponto-tipo_ponto.
*              WHEN 'F'.
*                <fs>-dsc_origem = ls_ponto-lifnr.
*                <fs>-dsc_tipo_origem = 'Fornecedor'.
*              WHEN 'C'.
*                <fs>-dsc_origem = ls_ponto-kunnr.
*                <fs>-dsc_tipo_origem = 'Cliente'.
*              WHEN 'W'.
*                <fs>-dsc_origem = ls_ponto-werks.
*                <fs>-dsc_tipo_origem = 'Centro'.
*              WHEN 'I'.
*                DATA(ls_cais) = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*                <fs>-dsc_origem = ls_cais-cod_cais.
*                <fs>-dsc_tipo_origem = 'Cais'.
*              WHEN OTHERS.
*                CLEAR: <fs>-id_origem,
*                       <fs>-tipo_origem.
*            ENDCASE.
**          <fs>-dsc_origem = VALUE #( lt_ponto[ id_ponto = <fs>-id_ponto ]-dsc_ponto OPTIONAL ).
*          WHEN 'R'.
*            <fs>-dsc_origem = VALUE #( lt_regiao[ id_regiao = <fs>-id_origem ]-codigo OPTIONAL ).
*          WHEN 'S'.
*            <fs>-dsc_origem = VALUE #( lt_porto[ id_porto = <fs>-id_origem ]-cod_porto OPTIONAL ).
*          WHEN 'G'.
*            <fs>-dsc_origem = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_origem ]-codigo OPTIONAL ).
*        ENDCASE.

        <fs>-dsc_tipo_destino  = VALUE #( lt_origem_destino[ domvalue_l = <fs>-tipo_destino ]-ddtext OPTIONAL ).

*        CASE <fs>-tipo_destino.
*          WHEN 'P'.
*            ls_ponto = VALUE #( lt_ponto[ id_ponto = <fs>-id_destino ] OPTIONAL ).
*            CASE ls_ponto-tipo_ponto.
*              WHEN 'F'.
*                <fs>-dsc_destino = ls_ponto-lifnr.
*                <fs>-dsc_tipo_destino = 'Fornecedor'.
*              WHEN 'C'.
*                <fs>-dsc_destino = ls_ponto-kunnr.
*                <fs>-dsc_tipo_destino = 'Cliente'.
*              WHEN 'W'.
*                <fs>-dsc_destino = ls_ponto-werks.
*                <fs>-dsc_tipo_destino = 'Centro'.
*              WHEN 'I'.
*                ls_cais = VALUE #( lt_cais[ id_cais = ls_ponto-id_cais ] OPTIONAL ).
*                <fs>-dsc_destino = ls_cais-cod_cais.
*                <fs>-dsc_tipo_destino = 'Cais'.
*              WHEN OTHERS.
*                CLEAR: <fs>-id_destino,
*                       <fs>-tipo_destino.
*            ENDCASE.
*          WHEN 'R'.
*            <fs>-dsc_destino = VALUE #( lt_regiao[ id_regiao = <fs>-id_destino ]-codigo OPTIONAL ).
*          WHEN 'S'.
*            <fs>-dsc_destino = VALUE #( lt_porto[ id_porto = <fs>-id_destino ]-cod_porto OPTIONAL ).
*          WHEN 'G'.
*            <fs>-dsc_destino = VALUE #( lt_grp_planta[ id_grp_planta = <fs>-id_destino ]-codigo OPTIONAL ).
*        ENDCASE.

        "Categoria
        <fs>-dsc_categoria = VALUE #( lt_categ[ id_categoria = <fs>-id_categoria ]-descricao OPTIONAL ).

        "Modal
        <fs>-dsc_modal = VALUE #( lt_modal[ domvalue_l = <fs>-id_modal ]-ddtext OPTIONAL ).

        "Processo
        <fs>-dsc_processo = VALUE #( lt_processo[ id_processo = <fs>-id_processo ]-descricao OPTIONAL ).
        <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

      ENDLOOP.

      DELETE return WHERE NOT id_custo_elementar IN lr_custo.

      mt_input = return.

    ELSE.
      return = mt_input.
    ENDIF.

    DELETE return WHERE dsc_tipo_origem = 'Cais' OR dsc_tipo_origem = 'Porto'.

  ENDMETHOD.


  METHOD get_referencias.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @it_data
      WHERE id_simulacao = @it_data-id_original
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD get_simulacao.

    IF iv_id_simulacao IS INITIAL.

      SELECT *
        FROM /qaps/simulacao
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/simulacao
        WHERE id_simulacao = @iv_id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    IF lines( return ) > 0.

      DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
      DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista(  ).

      DATA(lt_ref) = get_referencias( return ).

      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

        CASE <fs>-status.
          WHEN 'A'. <fs>-icon = icon_green_light.
          WHEN 'F'. <fs>-icon = icon_locked.
          WHEN 'E'. <fs>-icon = icon_complete.
        ENDCASE.

        <fs>-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_SIMUL'
                                                                iv_value  = CONV #( <fs>-status ) ).

        <fs>-dsc_referencia = VALUE #( lt_ref[ id_simulacao = <fs>-id_original ]-descricao OPTIONAL ).

        <fs>-dsc_tp_lista = VALUE #( lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao OPTIONAL ).

      ENDLOOP.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD GET_TIPO_LISTA.

    IF iv_id_tipo_lista IS INITIAL.

      SELECT *
        FROM /qaps/tp_lista
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/tp_lista
        WHERE id_tp_lista = @iv_id_tipo_lista
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD get_variaveis.

    DATA lr_area TYPE RANGE OF /qaps/area_user-id_area.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @iv_id_simulacao
      INTO @DATA(ls_simulacao).

    CHECK NOT ls_simulacao IS INITIAL.

    ms_simulacao = ls_simulacao.

    SELECT /qaps/lista_area~id_area
      FROM /qaps/lista_area
      INNER JOIN /qaps/area_user
      ON  /qaps/area_user~id_tp_lista = /qaps/lista_area~id_tp_lista
      AND /qaps/area_user~id_area = /qaps/lista_area~id_area
      WHERE /qaps/lista_area~id_tp_lista = @ls_simulacao-id_tp_lista
      AND /qaps/lista_area~ativo = 'X'
      AND /qaps/area_user~ativo = 'X'
      AND uname = @iv_uname
      INTO TABLE @DATA(lt_area_user).

    CHECK lines( lt_area_user ) > 0.

    lr_area = VALUE #( FOR wa IN lt_area_user
                       ( sign = 'I' option = 'EQ' low = wa-id_area ) ).

    SELECT *
      FROM /qaps/custo_elm
      WHERE id_area IN @lr_area
      AND   origem_dado  = '1'
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).

    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).
    DATA(lt_areas) = lo_model_tp_lista->get_areas( iv_id_tp_lista = ls_simulacao-id_tp_lista ).

    SELECT *
      FROM /qaps/v_inp_cust
      WHERE id_simulacao = @iv_id_simulacao
      INTO TABLE @DATA(lt_check).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      DATA(ls_check) = VALUE #( lt_check[ id_custo_elementar = <fs>-id_custo_elementar ] OPTIONAL ).

      IF ls_check-qty > 1.
        <fs>-icon = icon_green_light.
      ELSE.
        <fs>-icon = icon_light_out.
      ENDIF.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao.
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_areas[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

    SORT return BY dsc_tipo_variavel descricao ASCENDING.

  ENDMETHOD.


  METHOD get_variaveis_by_input_id.

    DATA lr_area TYPE RANGE OF /qaps/area_user-id_area.

    SELECT *
      FROM /qaps/var_input
      FOR ALL ENTRIES IN @it_data
      WHERE id_var_input = @it_data-id_var_input
      INTO TABLE @DATA(lt_var_input).

    CHECK lines( lt_var_input ) > 0.

    DATA(ls_var_input) = lt_var_input[ 1 ].

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @ls_var_input-id_simulacao
      INTO @DATA(ls_simulacao).

    SELECT *
      FROM /qaps/custo_elm
      FOR ALL ENTRIES IN @lt_var_input
      WHERE id_custo_elementar = @lt_var_input-id_custo_elementar
      INTO CORRESPONDING FIELDS OF TABLE @return.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).

    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).
    DATA(lt_areas) = lo_model_tp_lista->get_areas( iv_id_tp_lista = ls_simulacao-id_tp_lista ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      <fs>-icon = icon_green_light.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao.
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_areas[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

    SORT return BY descricao ASCENDING.

  ENDMETHOD.


  METHOD import_file.

    TRY.

        IF is_simulacao IS INITIAL.
          RAISE EXCEPTION TYPE /qaps/cx_pricing_error
            EXPORTING
              message = VALUE #( type = 'E'
                                 message = 'Nenhuma simulação foi selecionada' ).
        ENDIF.

        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        DATA(lt_result) = lo_file->file_upload( ).
        FREE lo_file.

        "File Processing
        DATA(lo_processing) = NEW /qaps/cl_file_cst_elem_import( ).
        lo_processing->execute( is_simulacao = is_simulacao
                                ir_data = REF #( lt_result ) ).
        FREE lo_processing.

      CATCH /qaps/cx_file_error.    "
    ENDTRY.

  ENDMETHOD.


  METHOD merge_data.

    TYPES: BEGIN OF ts_var_input,
             id_var_input TYPE /qaps/var_input-id_var_input,
           END OF ts_var_input.

    DATA lo_sdescr_main     TYPE REF TO cl_abap_structdescr.
    DATA lo_sdescr_append     TYPE REF TO cl_abap_structdescr.
    DATA lr_line       TYPE REF TO data.
    DATA lv_name TYPE string.
    DATA lv_name_prefixo TYPE string.
    DATA lv_name_sufixo TYPE string.
    DATA lt_var_input TYPE TABLE OF ts_var_input.

    FIELD-SYMBOLS: <ft>      TYPE ANY TABLE,
                   <fs_line> TYPE any.

    ASSIGN mt_data->* TO <ft>.
    ASSIGN ms_data->* TO <fs_line>.


    lo_sdescr_main ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/VAR_INPUT' ).
    DATA(lt_components) = lo_sdescr_main->get_components( ).

    DELETE lt_components WHERE name = 'PERIODO'
                            OR name = 'MANDT'
                            OR name = 'VALOR'
                            OR name = 'PERCENTUAL'
                            OR name = ''.

    lo_sdescr_append ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/S_VAR_INPUT' ).
    DATA(lt_comp_append) = lo_sdescr_append->get_components( ).

    APPEND LINES OF lt_comp_append  TO lt_components.

    lt_var_input = VALUE #( FOR wa IN it_data
                            ( id_var_input = wa-id_var_input ) ).

    SORT lt_var_input BY id_var_input ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_var_input COMPARING id_var_input.

    DELETE lt_components WHERE as_include = 'X'.
    DATA(lv_lines) = lines( lt_components ).

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_variaveis) = lo_custo_elementar->get_variavel( ).

    LOOP AT lt_var_input INTO DATA(ls_var_input).

      DATA(ls_data) = it_data[ id_var_input = ls_var_input-id_var_input ].

      DO lv_lines TIMES.
        lv_name = lt_components[ sy-index ]-name.
        ASSIGN COMPONENT lv_name OF STRUCTURE ls_data TO FIELD-SYMBOL(<fv_src>).
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_trg>).

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDDO.

      "Fill Value
      LOOP AT it_data INTO ls_data
        WHERE id_var_input = ls_var_input-id_var_input.

        CHECK NOT ls_var_input-id_var_input IS INITIAL.

        DATA(ls_fcat) = it_fcat[ parameter1 = ls_data-periodo ].

        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.

        lv_name = ls_fcat-fieldname.

        DATA(lv_tipo_dado) = lt_variaveis[ id_custo_elementar = ls_data-id_custo_elementar ]-tipo_dado.

        CASE lv_tipo_dado.
          WHEN '1'.ASSIGN COMPONENT 'VALOR' OF STRUCTURE ls_data TO <fv_src>.
          WHEN '2'.ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE ls_data TO <fv_src>.
        ENDCASE.

        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

      "Fill Period
      LOOP AT it_data INTO ls_data
        WHERE id_var_input = ls_var_input-id_var_input.

        CHECK NOT ls_var_input-id_var_input IS INITIAL.

        ls_fcat = it_fcat[ parameter1 = ls_data-periodo ].

        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.

        lv_name = `PERIODO_` && lv_name_sufixo.
        ASSIGN COMPONENT 'PERIODO' OF STRUCTURE ls_data TO <fv_src>.
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.

    ENDLOOP.

  ENDMETHOD.


  METHOD name_treatment.

    return = iv_name.
*    TRANSLATE return TO UPPER CASE.
    REPLACE ALL OCCURRENCES OF '/' in return WITH ' '.
*    TRANSLATE return USING ' _'.
*    TRANSLATE return USING '%@'.
*    TRANSLATE return USING '/@'.
*    TRANSLATE return USING 'Ç@'.
*    TRANSLATE return USING 'Á@'.
*    TRANSLATE return USING 'Ã@'.
*    TRANSLATE return USING 'Ê@'.
*    TRANSLATE return USING 'É@'.
*    TRANSLATE return USING 'Í@'.

  ENDMETHOD.


  METHOD QUESTION.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR      = ' '
*       DIAGNOSE_OBJECT             = ' '
        text_question = iv_message
*       TEXT_BUTTON_1 = 'Ja'(001)
*       ICON_BUTTON_1 = ' '
*       TEXT_BUTTON_2 = 'Nein'(002)
*       ICON_BUTTON_2 = ' '
*       DEFAULT_BUTTON              = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN  = 25
*       START_ROW     = 6
*       POPUP_TYPE    =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer        = lv_answer
*     TABLES
*       PARAMETER     =
*     EXCEPTIONS
*       TEXT_NOT_FOUND              = 1
*       OTHERS        = 2
      .

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD set_periodo.
    ms_periodo = is_periodo.
  ENDMETHOD.


  METHOD SYNC_MATRIZ_ABASTECIMENTO.

*    CALL FUNCTION '/QAPS/FM_INIT_MATRIZ_ABAST'
*      DESTINATION 'NONE'
*      STARTING NEW TASK 'TSK_MATRIZ_ABAST'
*      EXPORTING
*        iv_id_simulacao = iv_id_simulacao.

  ENDMETHOD.


  METHOD update_input.

    DATA lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/var_input
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_var_input = @it_changed_data-id_var_input
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @DATA(lt_update).

    LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_changed_data) = VALUE #( it_changed_data[ id_var_input = <fs>-id_var_input
                                                        periodo      = <fs>-periodo ] OPTIONAL ).

      CHECK NOT ls_changed_data IS INITIAL.

      CASE ls_changed_data-tipo.
        WHEN 'V'.
          <fs>-valor = ls_changed_data-valor.
        WHEN 'P'.
          <fs>-percentual = ls_changed_data-percentual.
      ENDCASE.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/var_input FROM TABLE lt_update.

  ENDMETHOD.
ENDCLASS.
