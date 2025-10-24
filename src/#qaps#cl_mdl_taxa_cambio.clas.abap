class /QAPS/CL_MDL_TAXA_CAMBIO definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods ATIVAR_TAXA_CAMBIO
    importing
      !IS_DATA type /QAPS/S_TAXA_CAMBIO_VERSAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_TAXA_CAMBIO
    importing
      !IS_MOEDA_LOCAL type TCURT
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_HEADER_DATA
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
    returning
      value(RETURN) type TCURT
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_SINGLE_TAXA_CAMBIO
    importing
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO
      !IV_PERIODO type /QAPS/ED_PERIODO
    returning
      value(RETURN) type /QAPS/S_TAXA_CAMBIO .
  methods GET_TAXA_CAMBIO
    importing
      !IS_MOEDA_LOCAL type TCURT
    returning
      value(RETURN) type /QAPS/T_TAXA_CAMBIO .
  methods GET_VERSOES
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_PERIODO type /QAPS/ED_PERIODO
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO
    returning
      value(RETURN) type /QAPS/T_TAXA_CAMBIO_VERSAO .
  methods UPDATE_TAXA
    importing
      !IT_DATA type /QAPS/T_CHANGED_TAXA .
protected section.
private section.

  data MS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER .

  methods VERIFY_TAXA_CAMBIO
    importing
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO
      !IV_PERIODO type /QAPS/ED_PERIODO .
  methods GET_NEXT_VERSAO_ID
    importing
      !IV_MOEDA_LOCAL type /QAPS/ED_MOEDA_LOCAL
      !IV_PERIODO type /QAPS/ED_PERIODO
      !IV_ID_TAXA_CAMBIO type /QAPS/ED_ID_TAXA_CAMBIO
    returning
      value(RETURN) type /QAPS/ED_TX_VERSAO .
  methods GET_NEXT_NUMBER
    returning
      value(RETURN) type /QAPS/ED_COD_STD_PRODUCAO .
  methods FILL_DESCRICOES
    changing
      !CS_DATA type /QAPS/S_STD_PRODUCAO_HEADER .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
ENDCLASS.



CLASS /QAPS/CL_MDL_TAXA_CAMBIO IMPLEMENTATION.


  METHOD ativar_taxa_cambio.

    "Limpar todos
    UPDATE /qaps/tx_cmb_ver
    SET ativo = ''
        modified_by = sy-uname
        modified_in = sy-datum
        modified_on = sy-uzeit
     WHERE id_taxa_cambio = is_data-id_taxa_cambio
     AND    periodo       = is_data-periodo
     and   moeda_local    = is_data-moeda_local
     and   moeda_final    = is_data-moeda_final.

    "Marcar ativo
    UPDATE /qaps/tx_cmb_ver
    SET ativo = 'X'
        modified_by = sy-uname
        modified_in = sy-datum
        modified_on = sy-uzeit
     WHERE id_taxa_cambio = is_data-id_taxa_cambio
     AND    periodo       = is_data-periodo
     AND    versao        = is_data-versao
     and   moeda_local    = is_data-moeda_local
     and   moeda_final    = is_data-moeda_final.

    UPDATE /qaps/tx_cambio
    SET taxa = is_data-taxa
        modified_by = sy-uname
        modified_in = sy-datum
        modified_on = sy-uzeit
     WHERE id_taxa_cambio = is_data-id_taxa_cambio
     AND   periodo        = is_data-periodo
     and   moeda_local    = is_data-moeda_local
     and   moeda_final    = is_data-moeda_final.

    return = abap_true.

  ENDMETHOD.


  METHOD create_taxa_cambio.

    DATA: ls_data       TYPE /qaps/s_taxa_cambio,
          ls_entry      TYPE /qaps/tx_cambio,
          lt_entry      TYPE TABLE OF /qaps/tx_cambio,
          ls_versao     TYPE /qaps/tx_cmb_ver,
          lt_versao     TYPE TABLE OF /qaps/tx_cmb_ver,
          ls_message    TYPE bapiret2,
          lr_data       TYPE REF TO data,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.

    CALL FUNCTION '/QAPS/FM_TAXA_CAMBIO_INPUT'
      EXPORTING
        iv_action  = 'C'
*       IS_ITEM    =
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_taxa_cambio = cl_system_uuid=>create_uuid_x16_static( ).

    ls_entry-moeda_local = is_moeda_local-waers.

    GET TIME.
    ls_entry-data = sy-datlo.
    ls_entry-hora = sy-timlo.

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    DATA(lv_periodo_inicial) = is_periodo-inicial.

    ls_periodo-year = is_periodo-inicial(4).
    ls_periodo-month = is_periodo-inicial+4(2).

    WHILE lv_periodo_inicial <= is_periodo-final.

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

      check lv_periodo_inicial <= is_periodo-final.

      ls_entry-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_entry TO lt_entry.

      ls_versao = CORRESPONDING #( ls_entry ).
      ls_versao-versao = '1'.
      ls_versao-ativo = 'X'.
      APPEND ls_versao TO lt_versao.

    ENDWHILE.

    MODIFY /qaps/tx_cambio FROM TABLE lt_entry.
    MODIFY /qaps/tx_cmb_ver FROM TABLE lt_versao.

    return = abap_true.

  ENDMETHOD.


  METHOD FILL_DESCRICOES.

*    IF NOT cs_data-werks IS INITIAL.
*      cs_data-dsc_werks = /qaps/cl_helper_text=>get_werks_text( cs_data-werks ).
*    ELSEIF NOT cs_data-id_regiao IS INITIAL.
*      DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
*      DATA(lt_regiao) = lo_logistica->get_regioes( iv_id_regiao = cs_data-id_regiao ).
*
*      cs_data-dsc_regiao = VALUE #( lt_regiao[ 1 ]-descricao OPTIONAL ).
*
*    ENDIF.
*
*    cs_data-dsc_maktx = /qaps/cl_helper_text=>get_material_text( cs_data-matnr ).
*    cs_data-dsc_categoria = /qaps/cl_helper_text=>get_domain_text(
*                                                      iv_domain =  '/QAPS/D_CATEGORIA'
*                                                      iv_value  = CONV #( cs_data-categoria ) ).

  ENDMETHOD.


  METHOD get_header_data.

    SELECT SINGLE *
      FROM tcurt
      WHERE waers = @iv_moeda_local
      AND spras = @sy-langu
      INTO CORRESPONDING FIELDS OF @return.

  ENDMETHOD.


  METHOD GET_NEXT_NUMBER.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = '/QAPS/STDH'
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = return
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD get_next_versao_id.

    SELECT SINGLE MAX( versao ) as id
      from /qaps/tx_cmb_ver
      where id_taxa_cambio = @iv_id_taxa_cambio
      AND   moeda_local    = @iv_moeda_local
      AND   periodo        = @iv_periodo
      INTO @DATA(lv_id).

    return = lv_id + 1.

  ENDMETHOD.


  METHOD get_single_taxa_cambio.

    SELECT SINGLE *
      FROM /qaps/tx_cambio
      WHERE id_taxa_cambio = @iv_id_taxa_cambio
      AND   periodo = @iv_periodo
      INTO CORRESPONDING FIELDS OF @return.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM /qaps/fonte_cmb
        WHERE id_fonte = @return-id_fonte
        INTO @DATA(ls_fonte).

      return-dsc_fonte = ls_fonte-descricao.

    ENDIF.

  ENDMETHOD.


  METHOD get_taxa_cambio.

    SELECT *
      FROM /qaps/tx_cambio
      WHERE moeda_local = @is_moeda_local-waers
      INTO CORRESPONDING FIELDS OF TABLE @return.

    IF lines( return ) > 0.

      select *
        from /qaps/fonte_cmb
        into TABLE @data(lt_fonte).

        loop at return ASSIGNING FIELD-SYMBOL(<fs>).

          check not <fs>-id_fonte is INITIAL.
          <fs>-dsc_fonte = value #( lt_fonte[ id_fonte = <fs>-id_fonte ]-descricao OPTIONAL ).

        ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_versoes.

    verify_taxa_cambio( iv_id_taxa_cambio = iv_id_taxa_cambio
                        iv_periodo        = iv_periodo     ).

    SELECT *
      FROM /qaps/tx_cmb_ver
      WHERE id_taxa_cambio = @iv_id_taxa_cambio
      AND   moeda_local    = @iv_moeda_local
      AND   periodo        = @iv_periodo
      INTO CORRESPONDING FIELDS OF TABLE @return.

    SORT return BY versao DESCENDING.

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


  METHOD update_taxa.

    DATA: ls_data   TYPE /qaps/s_taxa_cambio,
          ls_entry  TYPE /qaps/tx_cambio,
          lt_entry  TYPE TABLE OF /qaps/tx_cambio,
          ls_versao TYPE /qaps/tx_cmb_ver,
          lt_versao TYPE TABLE OF /qaps/tx_cmb_ver,
          lr_data   TYPE REF TO data.


    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs>).

      verify_taxa_cambio( iv_id_taxa_cambio = <fs>-id_taxa_cambio
                          iv_periodo        = <fs>-periodo ).

      ls_data = get_single_taxa_cambio( iv_id_taxa_cambio = <fs>-id_taxa_cambio
                                        iv_periodo        = <fs>-periodo ).



      UPDATE /qaps/tx_cmb_ver
      SET ativo = ''
          modified_by = sy-uname
          modified_in = sy-datum
          modified_on = sy-uzeit
       WHERE id_taxa_cambio = <fs>-id_taxa_cambio
       AND    periodo       = <fs>-periodo.


      get time.

      ls_entry = CORRESPONDING #( ls_data ).
      ls_entry-taxa = <fs>-taxa.
      ls_entry-data = sy-datum.
      ls_entry-hora = sy-uzeit.
      lr_data = REF #( ls_entry ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).
      APPEND ls_entry TO lt_entry.

      ls_versao = CORRESPONDING #( ls_entry ).
      ls_versao-versao = get_next_versao_id( iv_moeda_local    = ls_versao-moeda_local
                                             iv_periodo        = ls_versao-periodo
                                             iv_id_taxa_cambio = ls_versao-id_taxa_cambio ).

      ls_versao-ativo = 'X'.
      APPEND ls_versao TO lt_versao.


    ENDLOOP.

    MODIFY /qaps/tx_cambio  FROM TABLE lt_entry.
    INSERT /qaps/tx_cmb_ver FROM TABLE lt_versao.


  ENDMETHOD.


  METHOD verify_taxa_cambio.

    DATA lr_data TYPE REF TO data.
    DATA ls_versao TYPE /qaps/tx_cmb_ver.

    SELECT SINGLE *
      FROM /qaps/tx_cambio
      WHERE id_taxa_cambio = @iv_id_taxa_cambio
      AND   periodo = @iv_periodo
      INTO @DATA(ls_cambio).

    CHECK sy-subrc NE 0.

    SELECT SINGLE *
      FROM /qaps/tx_cambio
      WHERE id_taxa_cambio = @iv_id_taxa_cambio
      INTO @ls_cambio.

*    ls_cambio-id_taxa_cambio = cl_system_uuid=>create_uuid_x16_static( ).
    ls_cambio-periodo = iv_periodo.
    CLEAR: ls_cambio-taxa,
           ls_cambio-created_by,
           ls_cambio-created_in,
           ls_cambio-created_on,
           ls_cambio-modified_by,
           ls_cambio-modified_in,
           ls_cambio-modified_on.

    lr_data = REF #( ls_cambio ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/tx_cambio FROM ls_cambio.

    ls_versao = CORRESPONDING #( ls_cambio ).
    ls_versao-ativo = 'X'.
    ls_versao-versao = '1'.
    MODIFY /qaps/tx_cmb_ver FROM ls_versao.

  ENDMETHOD.
ENDCLASS.
