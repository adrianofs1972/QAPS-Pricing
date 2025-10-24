class /QAPS/CL_MDL_MATRIZ_ABAST definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods COPY_INPUT
    importing
      !IS_DATA type /QAPS/S_MATRIZ_ITEM
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_ITEMS
    importing
      !IS_DATA type /QAPS/S_MATRIZ_HEADER
    returning
      value(RETURN) type /QAPS/T_MATRIZ_ITEM .
  methods GET_HEADER_ITEM
    importing
      !IV_ID_DISTRIBUICAO type /QAPS/ED_ID_DISTRIBUICAO optional
    returning
      value(RETURN) type /QAPS/T_MATRIZ_HEADER .
  methods GET_HEADER
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/T_MATRIZ_HEADER .
  methods GET_DISTRIBUICAO
    importing
      !IS_DATA type /QAPS/S_MATRIZ_ITEM
    returning
      value(RETURN) type /QAPS/T_MATRIZ_DISTRIB .
  methods CONSTRUCTOR
    importing
      !IV_ACTION type /QAPS/PROCESS_ACTION optional .
  methods DELETE_INPUT
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT
    returning
      value(RETURN) type ABAP_BOOL .
  methods UPDATE_INPUT
    importing
      !IT_CHANGED_DATA type /QAPS/T_MATRIZ_CHANGED_DATA .
  methods SET_PERIODO
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  methods GET_SIMULACAO
    importing
      !IV_ID_SIMULACAO type /QAPS/ED_ID_SIMULACAO optional
    returning
      value(RETURN) type /QAPS/S_MATRIZ_ABAST_SIMULACAO .
  methods CREATE_INPUT
    importing
      !IS_SIMULACAO type /QAPS/S_SIMULACAO
      !IS_DATA type /QAPS/S_CUSTO_ELEMENTAR
    returning
      value(RETURN) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  data MS_SIMULACAO type /QAPS/SIMULACAO .
  data MO_LOGISTICA type ref to /QAPS/CL_MDL_LOGISTICA .
  data MT_CATALOG type LVC_T_FCAT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MT_DATA type ref to DATA .
  data MS_DATA type ref to DATA .
  data MV_FIRST_TIME type ABAP_BOOL .

  methods UPDATE_PREMISSA
    importing
      !IT_DATA type /QAPS/T_MATRIZ_DST .
  methods SET_DISTRIBUICAO_INICIAL .
  methods EXECUTE_DISTRIB_SEM_DESTINO
    importing
      !IS_VAR_INPUT type /QAPS/V_MAT_IBAS
      !IS_ITEM type /QAPS/V_MAT_DBAS
      !IS_SIMULACAO type /QAPS/SIMULACAO
      !IV_FULL type ABAP_BOOL .
  methods EXECUTE_DISTRIBUICAO
    importing
      !IS_ORIGEM type /QAPS/V_MAT_IBAS
      !IS_DESTINO type /QAPS/V_MAT_DBAS
      !IS_SIMULACAO type /QAPS/SIMULACAO .
  methods COPY_DISTRIBUICAO
    importing
      !IS_SOURCE type /QAPS/V_MAT_HIER
      !IS_TARGET type /QAPS/V_MAT_HIER .
  methods EXECUTE_COPY
    importing
      !IS_DATA type /QAPS/S_MATRIZ_ITEM
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_SINGLE_HEADER
    importing
      !IV_ID_MATRIZ_ABAST type /QAPS/ED_ID_MATRIZ_ABAST
    returning
      value(RETURN) type /QAPS/MATRIZ_HDR .
  methods CREATE_DISTRIBUICAO
    importing
      !IS_SIMULACAO type /QAPS/SIMULACAO
      value(IS_DATA) type /QAPS/MATRIZ_DST .
  methods INITIALIZE_DISTRIBUICAO .
  methods INITIALIZE_HEADER .
  methods INITIALIZE_ITEM .
  methods INITIALIZE .
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
      !IT_DATA type /QAPS/T_MATRIZ_ABASTECIMENTO
      !IT_FCAT type LVC_T_FCAT .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods INITIALIZE_ITEM_SEM_DESTINO .
  methods INITIALIZE_ITEM_COM_DESTINO .
  methods INIT_DISTRIB_COM_DEST .
  methods INIT_DISTRIB_SEM_DEST .
ENDCLASS.



CLASS /QAPS/CL_MDL_MATRIZ_ABAST IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_logistica = NEW /qaps/cl_mdl_logistica( ).

    mv_first_time = abap_true.
  ENDMETHOD.


  METHOD copy_distribuicao.

    DATA lr_data TYPE REF TO data.

    SELECT *
      FROM /qaps/matriz_dst
      WHERE id_distribuicao = @is_source-id_distribuicao
      INTO TABLE @DATA(lt_source).

    SELECT *
      FROM /qaps/matriz_dst
      WHERE id_distribuicao = @is_target-id_distribuicao
      INTO TABLE @DATA(lt_target).

    LOOP AT lt_target ASSIGNING FIELD-SYMBOL(<fs_target>).
      DATA(ls_source) = VALUE #( lt_source[ periodo = <fs_target>-periodo ] OPTIONAL ).

      <fs_target>-percentual = ls_source-percentual.
      lr_data =  REF #( <fs_target> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/matriz_dst FROM TABLE lt_target.
    COMMIT WORK.

  ENDMETHOD.


  METHOD copy_input.

    DATA lv_message TYPE string.

    lv_message = TEXT-m04.

    IF question( lv_message ).
      execute_copy( is_data ).
      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD create_distribuicao.

    DATA lt_data TYPE TABLE OF /qaps/matriz_dst.
    DATA lr_data TYPE REF TO data.
    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/matriz_dst,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    DATA(ls_header) = get_single_header( is_data-id_matriz_abast ).

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    ls_per = VALUE /qaps/s_periodo_interval(
        inicial = is_simulacao-periodo_inicial
        final   = is_simulacao-periodo_final    ).

    lr_data = REF #( is_data ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    WHILE lv_periodo_inicial < ls_per-final.

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

      is_data-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND is_data TO lt_data.

    ENDWHILE.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/matriz_dst FROM TABLE lt_data.
      COMMIT WORK.
    ENDIF.

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

    ENDCASE.

    lv_pos = lines( lt_fcat ) + 1.

    DATA(lv_periodo_inicial) = is_simulacao-periodo_inicial.

    ls_periodo-year = is_simulacao-periodo_inicial(4).
    ls_periodo-month = is_simulacao-periodo_inicial+4(2).

    WHILE lv_periodo_inicial < is_simulacao-periodo_final.

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

      DATA(ls_val_new)    = ls_valor_template.

      ls_val_new-reptext = ls_val_new-scrtext_s
        = ls_val_new-scrtext_m = ls_val_new-scrtext_l = lv_periodo.
      ls_val_new-parameter0 = 'VALOR'.
      ls_val_new-parameter1 = lv_ano && lv_mes.

*      ls_val_new-fieldname   = ls_val_new-fieldname && `_` && lv_id.
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

    lr_tdescr  ?= cl_abap_tabledescr=>describe_by_data_ref( mt_data ).
    lr_sdescr ?= lr_tdescr->get_table_line_type( ).

    CREATE DATA ms_data TYPE HANDLE lr_sdescr.


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
          ls_periodo    TYPE /qaps/s_periodo.

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

    WHILE lv_periodo_inicial < ms_periodo-final.

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

      ls_entry-periodo = lv_periodo = lv_ano && lv_mes.
      APPEND ls_entry TO lt_entry.

    ENDWHILE.

    MODIFY /qaps/var_input FROM TABLE lt_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD delete_input.

    DATA: lt_delete_werks     TYPE /qaps/t_matriz_delete,
          lt_delete_destino   TYPE /qaps/t_matriz_delete,
          lr_id_abast_werks   TYPE RANGE OF /qaps/matriz_abs-id_matriz_abast,
          lr_id_abast_destino TYPE RANGE OF /qaps/matriz_abs-id_matriz_abast.

    LOOP AT it_data INTO DATA(ls_data).

      IF ls_data-id_destino IS INITIAL.
        IF NOT line_exists( lt_delete_werks[ id_var_input = ls_data-id_var_input ] ).
          APPEND INITIAL LINE TO lt_delete_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
          <fs_werks> = CORRESPONDING #( ls_data ).
          <fs_werks>-tipo_delecao = 'W'.
        ENDIF.
      ELSE.
        IF NOT line_exists( lt_delete_destino[ id_var_input = ls_data-id_var_input ] ).
          APPEND INITIAL LINE TO lt_delete_destino ASSIGNING FIELD-SYMBOL(<fs_destino>).
          <fs_destino> = CORRESPONDING #( ls_data ).
          <fs_destino>-tipo_delecao = 'D'.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lines( lt_delete_werks ) > 0.

      SELECT DISTINCT id_matriz_abast,id_simulacao,tipo_regra,
            key_input,id_ponto,id_porto,id_cais,
            id_destino,werks
        FROM /qaps/matriz_abs
        FOR ALL ENTRIES IN @lt_delete_werks
        WHERE id_simulacao = @lt_delete_werks-id_simulacao
        AND   tipo_regra = @lt_delete_werks-tipo_regra
        AND   key_input  = @lt_delete_werks-key_input
        AND   werks      <> ''
        AND   id_ponto   = @lt_delete_werks-id_origem
        INTO TABLE  @DATA(lt_matriz_werks).

      IF lines( lt_matriz_werks ) > 0.

        lr_id_abast_werks  = VALUE #( FOR wa_werks IN lt_matriz_werks
                                      ( sign = 'I'
                                        option = 'EQ'
                                        low = wa_werks-id_matriz_abast ) ).
*      break c060863.
        DELETE FROM /qaps/matriz_abs WHERE id_matriz_abast IN lr_id_abast_werks.

      ENDIF.

    ENDIF.

    IF lines( lt_delete_destino ) > 0.

      SELECT DISTINCT id_matriz_abast,id_simulacao,tipo_regra,
            key_input,id_ponto,id_porto,id_cais,
            id_destino,werks
        FROM /qaps/matriz_abs
        FOR ALL ENTRIES IN @lt_delete_destino
        WHERE id_simulacao = @lt_delete_destino-id_simulacao
        AND   tipo_regra = @lt_delete_destino-tipo_regra
        AND   key_input  = @lt_delete_destino-key_input
        AND   id_destino = @lt_delete_destino-id_destino
        AND   werks      = ''
        INTO TABLE  @DATA(lt_matriz_destino).

      IF lines( lt_matriz_destino ) > 0.

        lr_id_abast_destino  = VALUE #( FOR wa_destino IN lt_matriz_destino
                                      ( sign = 'I'
                                        option = 'EQ'
                                        low = wa_destino-id_matriz_abast ) ).

*      break c060863.
        DELETE FROM /qaps/matriz_abs WHERE id_matriz_abast IN lr_id_abast_destino.

      ENDIF.

    ENDIF.


    return = abap_true.

  ENDMETHOD.


  METHOD execute_copy.

    DATA: lv_message TYPE string,
          lv_where   TYPE string..

    SELECT SINGLE *
      FROM /qaps/matriz_hdr
      WHERE id_matriz_abast = @is_data-id_matriz_abast
      INTO @DATA(ls_header).

    CASE is_data-tipo_regra.
      WHEN 'GE'.
        lv_where = ` tipo_regra = 'GE' `.
      WHEN 'GP'.
        lv_where = ` ( tipo_regra = 'GP' ` &&
                ` and id_grupo_produto = '` && is_data-id_grupo_produto && `' )`.
      WHEN 'MP'.
        lv_where = ` ( tipo_regra = 'MP' ` &&
                ` and mat_planejado = '` && is_data-mat_planejado && `' )`.
      WHEN 'AG'.
        lv_where = ` ( tipo_regra = 'AG' ` &&
                ` and agregador = '` && is_data-agregador && `' )`.

      WHEN 'MT'.
        lv_where = ` ( tipo_regra = 'MT' ` &&
                   ` and matnr = '` && is_data-matnr && `' )`.
    ENDCASE.

    SELECT tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador, COUNT( * ) AS qty
      FROM /qaps/v_mat_hier
      WHERE id_grp_planta = @ls_header-id_grp_planta
      AND   id_centro <> @mc_guid_null
      AND   (lv_where)
      GROUP BY tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador
      INTO TABLE @DATA(lt_children).

    "Source
    SELECT *
      FROM /qaps/v_mat_hier
      WHERE id_grp_planta = @ls_header-id_grp_planta
      AND   id_centro = @mc_guid_null
      AND   (lv_where)
      INTO TABLE @DATA(lt_source).

**    Target
    SELECT *
      FROM /qaps/v_mat_hier
      WHERE id_grp_planta = @ls_header-id_grp_planta
      AND   id_centro <> @mc_guid_null
      AND   (lv_where)
      INTO TABLE @DATA(lt_target).

    LOOP AT lt_source INTO DATA(ls_source).

      DATA(lt_target_partial) = lt_target.
      DELETE lt_target_partial WHERE modalidade <> ls_source-modalidade
                          OR id_origem  <>  ls_source-id_origem.

      LOOP AT lt_target_partial INTO DATA(ls_target).
        copy_distribuicao( is_source = ls_source
                           is_target = ls_target ).
      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD execute_distribuicao.

    DATA lv_where_dest TYPE string.

    SELECT SINGLE *
      FROM /qaps/v_ponto
      WHERE id_ponto = @is_origem-id_origem
      INTO @DATA(ls_origem).

    IF ls_origem-tipo_ponto = 'P'.
      lv_where_dest = `( cod_porto = '` && ls_origem-codigo && `' )`   .
    ELSEIF ls_origem-tipo_ponto = 'I'.
      lv_where_dest = `( cod_cais = '` && ls_origem-codigo && `' AND cod_porto = '` && ls_origem-cod_porto && `' )`   .
    ENDIF.

    IF is_destino-id_grp_planta <> mc_guid_null AND is_destino-id_centro <> mc_guid_null.
      lv_where_dest = lv_where_dest && ` AND ( id_grp_planta = '` && is_destino-id_grp_planta && `'` &&
                      ` AND id_centro = '` && is_destino-id_centro && `' )`.
    ELSEIF is_destino-id_grp_planta <> mc_guid_null AND is_destino-id_centro = mc_guid_null.
      lv_where_dest = lv_where_dest && ` AND ( id_grp_planta = '` && is_destino-id_grp_planta && `' )`.
    ELSEIF is_destino-id_grp_planta = mc_guid_null AND is_destino-id_centro <> mc_guid_null.
      lv_where_dest = lv_where_dest && ` AND ( id_centro = '` && is_destino-id_centro && `' )`.
    ENDIF.

    SELECT *
      FROM /qaps/v_mat_od
      WHERE ativo = 'X'
      AND  (lv_where_dest)
      INTO TABLE @DATA(lt_origem_destino).

    CHECK lines( lt_origem_destino ) > 0.

    DATA(ls_data) = VALUE /qaps/matriz_dst(
        id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
        id_matriz_abast = is_destino-id_matriz_abast
        id_item         = is_destino-id_item
        modalidade      = COND #( WHEN is_origem-importacao = 'X' THEN 'I'
                                  WHEN is_origem-nacional = 'X' THEN 'N' )
        tipo_origem     = is_origem-tipo_origem
        id_origem       = is_origem-id_origem
*        percentual      = COND #( WHEN iv_full = abap_true THEN 100 ELSE 0 )
    ).

    create_distribuicao( is_simulacao = is_simulacao
                         is_data      = ls_data ).



  ENDMETHOD.


  METHOD execute_distrib_sem_destino.

    BREAK-POINT.

    DATA(ls_data) = VALUE /qaps/matriz_dst(
        id_distribuicao = cl_system_uuid=>create_uuid_x16_static( )
*            periodo         =
        id_matriz_abast = is_item-id_matriz_abast
        id_item         = is_item-id_item
        modalidade      = COND #( WHEN is_var_input-importacao = 'X' THEN 'I'
                                  WHEN is_var_input-nacional = 'X' THEN 'N' )
        tipo_origem     = is_var_input-tipo_origem
        id_origem       = is_var_input-id_origem
        percentual      = COND #( WHEN iv_full = abap_true THEN 100 ELSE 0 )
    ).

    create_distribuicao( is_simulacao = is_simulacao
                         is_data      = ls_data ).



  ENDMETHOD.


  METHOD get_distribuicao.

    SELECT *
      FROM /qaps/matriz_dst
      WHERE id_matriz_abast = @is_data-id_matriz_abast
      AND   id_item         = @is_data-id_item
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/v_ponto
      WHERE tipo_ponto IN ('I','P')
      INTO TABLE @DATA(lt_ponto).

    DATA(lt_porto) = mo_logistica->get_portos( ).
    DATA(lt_cais) = mo_logistica->get_cais( ).
    DATA(lt_modalidade) = /qaps/cl_helper_text=>get_domain_values( '/QAPS/D_MODALIDADE' ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_ponto) = VALUE #( lt_ponto[ id_ponto = <fs>-id_origem ]  OPTIONAL ).

      CASE <fs>-tipo_origem.
        WHEN 'P'.
          DATA(ls_porto) = lt_porto[ id_porto = ls_ponto-id_externo ].
          <fs>-cod_porto = ls_porto-cod_porto.
          <fs>-porto     = ls_porto-porto.
        WHEN 'I'.
          DATA(ls_cais) = lt_cais[ id_cais = ls_ponto-id_externo ].
          <fs>-cod_cais = ls_cais-cod_cais.
          <fs>-cais = ls_cais-cais.
          ls_porto = lt_porto[ id_porto = ls_cais-id_porto ].
          <fs>-cod_porto = ls_porto-cod_porto.
          <fs>-porto     = ls_porto-porto.
      ENDCASE.

      <fs>-dsc_modalidade = lt_modalidade[ domvalue_l = <fs>-modalidade ]-ddtext.

    ENDLOOP.

    sort return by cod_porto cod_cais.

  ENDMETHOD.


  METHOD get_header.

    data lt_parent TYPE /qaps/t_matriz_header.

    IF mv_first_time = abap_true.
      initialize( ).
      mv_first_time = abap_false.
    ENDIF.

    IF NOT iv_id_simulacao IS INITIAL.

      SELECT *
        FROM /qaps/v_mat_hdr
        WHERE id_simulacao = @iv_id_simulacao
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ELSE.

      SELECT *
      FROM /qaps/v_mat_hdr
      INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    CHECK lines( return ) > 0.

    SELECT *
      FROM /qaps/v_mt_st_rs
      FOR ALL ENTRIES IN @return
      WHERE id_matriz_abast = @return-id_matriz_abast
      INTO TABLE @DATA(lt_status).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(lt_aux) = lt_status.
      DELETE lt_aux WHERE id_matriz_abast <> <fs>-id_matriz_abast.

      IF line_exists( lt_aux[ red = 'X' ] ).
        <fs>-red = 'X'.
      ELSEIF line_exists( lt_aux[ yellow = 'X' ] ).
        <fs>-yellow = 'X'.
      ELSEIF line_exists( lt_aux[ green = 'X' ] ).
        <fs>-green = 'X'.
      ENDIF.

    ENDLOOP.

    "Grp Planta
    LOOP AT return ASSIGNING <fs>.

      IF <fs>-id_grp_planta <> mc_guid_null AND
         <fs>-id_centro  = mc_guid_null.

        SELECT *
          FROM /qaps/v_mat_hdr
          WHERE id_grp_planta = @<fs>-id_grp_planta
*        AND   id_centro = '00000000000000000000000000000000'"@mc_guid_null
          INTO CORRESPONDING FIELDS OF TABLE @lt_parent.

        SELECT *
          FROM /qaps/v_mt_st_rs
          FOR ALL ENTRIES IN @lt_parent
          WHERE id_matriz_abast = @lt_parent-id_matriz_abast
          INTO TABLE @lt_status.

          DELETE lt_parent WHERE id_centro <> mc_guid_null.
          DATA(ls_parent) = lt_parent[ 1 ].

          IF line_exists( lt_status[ red = 'X' ] ).
            <fs>-red = 'X'.
          ELSEIF line_exists( lt_status[ yellow = 'X' ] ).
            <fs>-yellow = 'X'.
          ELSEIF line_exists( lt_status[ green = 'X' ] ).
            <fs>-green = 'X'.
          ENDIF.

      ENDIF.

    ENDLOOP.


    SORT return BY id_simulacao ASCENDING
                   tipo DESCENDING
                   id_grp_planta ASCENDING
                   id_centro ASCENDING.

  ENDMETHOD.


  METHOD get_header_item.

    DATA: lt_parent TYPE /qaps/t_matriz_header,
          ls_return TYPE /qaps/s_mat_hdr.

    SELECT SINGLE *
      FROM /qaps/matriz_dst
      WHERE id_distribuicao = @iv_id_distribuicao
      INTO @DATA(ls_distrib).

    SELECT SINGLE *
      FROM /qaps/v_mat_hdr
      WHERE id_matriz_abast = @ls_distrib-id_matriz_abast
      INTO CORRESPONDING FIELDS OF @ls_return.

    SELECT *
      FROM /qaps/v_mt_st_rs
      WHERE id_matriz_abast = @ls_distrib-id_matriz_abast
      INTO TABLE @DATA(lt_status).

    IF line_exists( lt_status[ red = 'X' ] ).
      ls_return-red = 'X'.
    ELSEIF line_exists( lt_status[ yellow = 'X' ] ).
      ls_return-yellow = 'X'.
    ELSEIF line_exists( lt_status[ green = 'X' ] ).
      ls_return-green = 'X'.
    ENDIF.

    APPEND ls_return TO return.

    IF ls_return-id_grp_planta <> mc_guid_null AND
       ls_return-id_centro  <> mc_guid_null.

      SELECT *
        FROM /qaps/v_mat_hdr
        WHERE id_grp_planta = @ls_return-id_grp_planta
        INTO CORRESPONDING FIELDS OF TABLE @lt_parent.


      SELECT *
       FROM /qaps/v_mt_st_rs
       FOR ALL ENTRIES IN @lt_parent
       WHERE id_matriz_abast = @lt_parent-id_matriz_abast
       INTO TABLE @lt_status.

      DELETE lt_parent WHERE id_centro <> mc_guid_null.
      TRY.
          DATA(ls_parent) = lt_parent[ 1 ].

          IF line_exists( lt_status[ red = 'X' ] ).
            ls_parent-red = 'X'.
          ELSEIF line_exists( lt_status[ yellow = 'X' ] ).
            ls_parent-yellow = 'X'.
          ELSEIF line_exists( lt_status[ green = 'X' ] ).
            ls_parent-green = 'X'.
          ENDIF.

          APPEND ls_parent TO return.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

  ENDIF.

ENDMETHOD.


  METHOD get_input_by_simulacao.

    SELECT *
      FROM /qaps/var_input
      WHERE id_simulacao = @is_simulacao-id_simulacao
      AND   id_custo_elementar = @is_custo_elementar-id_custo_elementar
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
                                          iv_id_tp_lista = is_simulacao-id_tp_lista ).

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


  METHOD get_items.

*    TYPES: BEGIN OF ts_summary,
*             id_matriz_abast TYPE /qaps/matriz_itm-id_matriz_abast,
*             id_item         TYPE /qaps/matriz_itm-id_item,
*             qty             TYPE int8,
*             soma            TYPE tp_prozent,
*             media           TYPE tp_prozent,
*           END OF ts_summary.

    DATA: lv_show_button TYPE abap_bool,
          lr_items       TYPE RANGE OF /qaps/matriz_itm-id_item.
*          lt_summary     TYPE TABLE OF ts_summary.

    "Resumo Item
    SELECT item~id_matriz_abast,
           item~id_item,
           distrib~periodo,
           SUM( distrib~percentual ) AS soma
      FROM /qaps/matriz_itm AS item
      INNER JOIN /qaps/matriz_dst AS distrib ON item~id_item = distrib~id_item
      WHERE item~id_matriz_abast = @is_data-id_matriz_abast
      GROUP BY item~id_matriz_abast,
               item~id_item,
               distrib~periodo
      INTO TABLE @DATA(lt_summary).

    CHECK lines( lt_summary ) > 0.

    lr_items = VALUE #( FOR wa IN lt_summary
                        ( sign = 'I' option = 'EQ' low = wa-id_item ) ).

    SELECT *
        FROM /qaps/matriz_itm
        WHERE id_matriz_abast = @is_data-id_matriz_abast
        AND id_item IN @lr_items
        INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    IF is_data-id_grp_planta <> mc_guid_null
        AND is_data-id_centro =  mc_guid_null.

      SELECT tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador, COUNT( * ) AS qty
        FROM /qaps/v_mat_hier
        WHERE id_grp_planta = @is_data-id_grp_planta
        AND   id_centro <> @mc_guid_null
        GROUP BY tipo_regra,matnr,mat_planejado,id_grupo_produto,agregador
        INTO TABLE @DATA(lt_children).

      DATA(lt_style) = component_to_style( '/QAPS/S_MATRIZ_ITEM' ).

      ASSIGN lt_style[ fieldname = 'BTN_LINK' ] TO FIELD-SYMBOL(<fs_btn>).

      IF <fs_btn> IS ASSIGNED.
        <fs_btn>-style = cl_gui_alv_grid=>mc_style_button.
        <fs_btn>-style2 = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.
    ENDIF.

    DATA(lo_grp_prd) = NEW /qaps/cl_mdl_material( ).
    DATA(lt_grp_prd) = lo_grp_prd->get_grupo_produto( ).
    DATA(lt_tipo_regra) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_REGRA' ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      lv_show_button = abap_false.

      IF is_data-id_grp_planta <> mc_guid_null.

        CASE <fs>-tipo_regra.
          WHEN 'GE'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'GE' ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'GP'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'GP'
                                                              id_grupo_produto = <fs>-id_grupo_produto ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'MP'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'MP'
                                                              mat_planejado = <fs>-mat_planejado ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'AG'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'AG'
                                                              agregador = <fs>-agregador ] )
                                     THEN abap_true
                                     ELSE abap_false ).
          WHEN 'MT'.
            lv_show_button = COND #( WHEN line_exists( lt_children[ tipo_regra = 'MT'
                                                              matnr = <fs>-matnr ] )
                                     THEN abap_true
                                     ELSE abap_false ).
        ENDCASE.

      ENDIF.

      DATA(lt_summary_aux) = lt_summary.
      DELETE lt_summary_aux WHERE id_item <> <fs>-id_item.

      DATA(lt_summary_red) = lt_summary_aux.
      DATA(lt_summary_green) = lt_summary_aux.
      DATA(lt_summary_yellow) = lt_summary_aux.

      DELETE lt_summary_red   WHERE soma = 0 OR soma = 100.
      DELETE lt_summary_green WHERE soma <> 100.
      DELETE lt_summary_yellow WHERE soma <> 0.


      IF lines( lt_summary_red ) > 0.
        <fs>-icon = icon_red_light.
      ELSEIF lines( lt_summary_yellow ) > 0.
        <fs>-icon = icon_yellow_light.
      ELSEIF lines( lt_summary_green ) > 0.
        <fs>-icon = icon_green_light.
      ENDIF.

      IF lines( lt_style ) > 0 AND lv_show_button = abap_true.
        <fs>-style = lt_style.
        <fs>-btn_link = icon_insert_relation.
      ENDIF.

      CASE <fs>-tipo_regra.
        WHEN 'GE'.
          <fs>-ord = 1.
        WHEN 'GP'.
          <fs>-ord = 2.
          <fs>-key_input = <fs>-dsc_input = VALUE #( lt_grp_prd[ id_grupo_produto = <fs>-id_grupo_produto ]-descricao OPTIONAL ).
        WHEN 'MP'.
          <fs>-ord = 3.
          <fs>-key_input = <fs>-dsc_input = <fs>-mat_planejado.
        WHEN 'AG'.
          <fs>-ord = 4.
          <fs>-key_input = <fs>-dsc_input = <fs>-agregador.
        WHEN 'MA'.
          <fs>-ord = 5.
          DATA(lv_matnr) = |{ <fs>-matnr ALPHA = OUT }|.
          CONDENSE lv_matnr.
          <fs>-key_input = lv_matnr.
          <fs>-dsc_input = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
      ENDCASE.

      <fs>-dsc_tipo_regra  = VALUE #( lt_tipo_regra[ domvalue_l = <fs>-tipo_regra ]-ddtext OPTIONAL ).

    ENDLOOP.

    SORT return BY ord ASCENDING key_input ASCENDING.

  ENDMETHOD.


  METHOD get_simulacao.

    SELECT SINGLE *
      FROM /qaps/v_simul
      WHERE id_simulacao = @iv_id_simulacao
      INTO CORRESPONDING FIELDS OF @return.


  ENDMETHOD.


  METHOD get_single_header.

    SELECT SINGLE *
      FROM /qaps/matriz_hdr
      WHERE id_matriz_abast = @iv_id_matriz_abast
      INTO @return.

  ENDMETHOD.


  METHOD initialize.

*    initialize_header( ).
*    initialize_item( ).
*    initialize_distribuicao( ).

  ENDMETHOD.


  METHOD initialize_distribuicao.

    init_distrib_com_dest( ).
    init_distrib_sem_dest( ).

    "Quando for 100%
    set_distribuicao_inicial( ).

  ENDMETHOD.


  METHOD initialize_header.


    DATA: lr_data  TYPE REF TO data,
          lr_ponto TYPE RANGE OF /qaps/id_ponto.

    DATA lt_matriz_header TYPE TABLE OF /qaps/matriz_hdr.

    SELECT *
      FROM /qaps/v_mtz_dest
      INTO TABLE @DATA(lt_destino).

    SELECT id_simulacao
      FROM /qaps/simulacao
      INTO TABLE @DATA(lt_simulacao).

    IF lines( lt_simulacao ) > 0.

      SELECT *
        FROM /qaps/matriz_hdr
        INTO TABLE @DATA(lt_header).

      LOOP AT lt_simulacao INTO DATA(ls_simulacao).

        LOOP AT lt_destino INTO DATA(ls_destino).

          CASE ls_destino-tipo.
            WHEN 'C'.
              "Grp Planta
              IF NOT line_exists( lt_header[ id_simulacao     = ls_simulacao-id_simulacao
                                                id_grp_planta = ''
                                                id_centro     = ls_destino-id_centro ] )
                AND NOT line_exists( lt_matriz_header[ id_simulacao = ls_simulacao-id_simulacao
                                                       id_grp_planta = ''
                                                       id_centro    = ls_destino-id_centro ] ).


                APPEND VALUE /qaps/matriz_hdr(
                    id_matriz_abast  = cl_system_uuid=>create_uuid_x16_static( )
                    id_simulacao     = ls_simulacao-id_simulacao
                    id_centro        = ls_destino-id_centro ) TO lt_matriz_header.
              ENDIF.
            WHEN 'G'.

              "Grp Planta
              IF NOT line_exists( lt_header[ id_simulacao     = ls_simulacao-id_simulacao
                                                id_grp_planta = ls_destino-id_grp_planta
                                                id_centro     = '' ] )
                AND NOT line_exists( lt_matriz_header[ id_simulacao = ls_simulacao-id_simulacao
                                                       id_grp_planta = ls_destino-id_grp_planta
                                                       id_centro    = '' ] ).


                APPEND VALUE /qaps/matriz_hdr(
                    id_matriz_abast  = cl_system_uuid=>create_uuid_x16_static( )
                    id_simulacao     = ls_simulacao-id_simulacao
                    id_grp_planta    = ls_destino-id_grp_planta ) TO lt_matriz_header.
              ENDIF.

              "Grp Planta e/ou Centro
              IF NOT line_exists( lt_header[ id_simulacao  = ls_simulacao-id_simulacao
                                                id_grp_planta = ls_destino-id_grp_planta
                                                id_centro     = ls_destino-id_centro ] )
                AND NOT line_exists( lt_matriz_header[ id_simulacao  = ls_simulacao-id_simulacao
                                                       id_grp_planta = ls_destino-id_grp_planta
                                                       id_centro     = ls_destino-id_centro ] ).


                APPEND VALUE /qaps/matriz_hdr(
                    id_matriz_abast  = cl_system_uuid=>create_uuid_x16_static( )
                    id_simulacao     = ls_simulacao-id_simulacao
                    id_grp_planta    = ls_destino-id_grp_planta
                    id_centro        = ls_destino-id_centro ) TO lt_matriz_header.

              ENDIF.

          ENDCASE.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    IF lines( lt_matriz_header ) > 0.
      LOOP AT lt_matriz_header REFERENCE INTO lr_data.
        preencher_dados_controle( CHANGING cr_data = lr_data ).
      ENDLOOP.

      MODIFY /qaps/matriz_hdr FROM TABLE lt_matriz_header.
      COMMIT WORK AND WAIT.

    ENDIF.


  ENDMETHOD.


  METHOD initialize_item.

    initialize_item_com_destino( ).
    initialize_item_sem_destino( ).

  ENDMETHOD.


  METHOD initialize_item_com_destino.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/matriz_itm,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_mat_hdr
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/matriz_itm
      FOR ALL ENTRIES IN @lt_header
      WHERE id_matriz_abast  = @lt_header-id_matriz_abast
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_inp_mtz
      WHERE id_destino <> @mc_guid_null
      INTO TABLE @DATA(lt_var_input).

    LOOP AT lt_header INTO DATA(ls_header).

      DATA(lt_filtered_input) = lt_var_input.

      CASE ls_header-tipo.
        WHEN 'G'.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta
                                      or id_centro <> ls_header-id_centro.
        WHEN 'C'.
          DELETE lt_filtered_input WHERE id_grp_planta <> ls_header-id_grp_planta
                                      or id_centro <> ls_header-id_centro.
      ENDCASE.

      LOOP AT lt_filtered_input INTO DATA(ls_filtered_input).

        CHECK NOT line_exists( lt_item[ id_matriz_abast  = ls_header-id_matriz_abast
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] )
          AND NOT line_exists( lt_data[ id_matriz_abast  = ls_header-id_matriz_abast
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] ).

        DATA(ls_data) =  VALUE /qaps/matriz_itm( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                 id_matriz_abast  = ls_header-id_matriz_abast
                                                 tipo_regra       = ls_filtered_input-tipo_regra
                                                 matnr            = ls_filtered_input-matnr
                                                 id_grupo_produto = ls_filtered_input-id_grupo_produto
                                                 agregador        = ls_filtered_input-agregador
                                                 mat_planejado    = ls_filtered_input-mat_planejado ).

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/matriz_itm FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD initialize_item_sem_destino.

    DATA lv_key_input TYPE /qaps/key_input.
    DATA: lt_data TYPE TABLE OF /qaps/matriz_itm,
          lr_data TYPE REF TO data.

    SELECT *
        FROM /qaps/v_mat_hdr
        INTO TABLE @DATA(lt_header).

    CHECK lines( lt_header ) > 0.

    SELECT *
      FROM /qaps/matriz_itm
      FOR ALL ENTRIES IN @lt_header
      WHERE id_matriz_abast  = @lt_header-id_matriz_abast
      INTO TABLE @DATA(lt_item).

    SELECT *
      FROM /qaps/v_inp_mtz
      WHERE id_destino = @mc_guid_null
      INTO TABLE @DATA(lt_var_input).

    LOOP AT lt_header INTO DATA(ls_header).

      LOOP AT lt_var_input INTO DATA(ls_filtered_input).

        CHECK NOT line_exists( lt_item[ id_matriz_abast  = ls_header-id_matriz_abast
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] )
          AND NOT line_exists( lt_data[ id_matriz_abast  = ls_header-id_matriz_abast
                                        tipo_regra       = ls_filtered_input-tipo_regra
                                        matnr            = ls_filtered_input-matnr
                                        id_grupo_produto = ls_filtered_input-id_grupo_produto
                                        agregador        = ls_filtered_input-agregador
                                        mat_planejado    = ls_filtered_input-mat_planejado    ] ).

        DATA(ls_data) =  VALUE /qaps/matriz_itm( id_item          = cl_system_uuid=>create_uuid_x16_static( )
                                                 id_matriz_abast  = ls_header-id_matriz_abast
                                                 tipo_regra       = ls_filtered_input-tipo_regra
                                                 matnr            = ls_filtered_input-matnr
                                                 id_grupo_produto = ls_filtered_input-id_grupo_produto
                                                 agregador        = ls_filtered_input-agregador
                                                 mat_planejado    = ls_filtered_input-mat_planejado ).

        lr_data = REF #( ls_data ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).
        APPEND ls_data TO lt_data.

      ENDLOOP.

    ENDLOOP.

    IF lines( lt_data ) > 0.
      MODIFY /qaps/matriz_itm FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD init_distrib_com_dest.

    DATA lt_origem TYPE /qaps/t_mat_ibas.

    "Item
    SELECT *
      FROM /qaps/v_mat_dbas
      INTO TABLE @DATA(lt_destino).

    CHECK lines( lt_destino ) > 0.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_simulacao = @lt_destino-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_item = @lt_destino-id_item
      AND   id_matriz_abast = @lt_destino-id_matriz_abast
      INTO TABLE @DATA(lt_distrib).

    SORT lt_destino BY id_matriz_abast id_item.

    LOOP AT lt_destino INTO DATA(ls_destino).

      CHECK NOT line_exists( lt_distrib[ id_matriz_abast = ls_destino-id_matriz_abast
                                         id_item         = ls_destino-id_item ] ).

      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_destino-id_simulacao ].

      IF ls_destino-tipo_regra <> 'GE'.

        SELECT *
          FROM /qaps/v_mat_ibas
          WHERE ( ( tipo_regra = 'GP' AND id_grupo_produto = @ls_destino-id_grupo_produto ) OR
                ( tipo_regra = 'AG'   AND agregador = @ls_destino-agregador ) OR
                ( tipo_regra = 'MP'   AND mat_planejado = @ls_destino-mat_planejado ) OR
                ( tipo_regra = 'MA'   AND matnr = @ls_destino-matnr ) )
          AND id_simulacao = @ls_destino-id_simulacao
          AND has_destino = 'X'
          INTO TABLE @lt_origem.
      ELSE.
        SELECT *
        FROM /qaps/v_mat_ibas
        WHERE (  tipo_regra = 'GE' )
        AND id_simulacao = @ls_destino-id_simulacao
        AND has_destino = 'X'
        INTO TABLE @lt_origem.
      ENDIF.

      CHECK lines( lt_origem ) > 0.

      IF lines( lt_origem ) = 1.
        DATA(lv_full) = abap_true.
      ELSE.
        lv_full = abap_false.
      ENDIF.

*      BREAK-POINT.

      LOOP AT lt_origem INTO DATA(ls_origem).

        execute_distribuicao( is_origem    = ls_origem
                              is_destino   = ls_destino
                              is_simulacao = ls_simulacao ).

      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD init_distrib_sem_dest.

    DATA lt_origem TYPE /qaps/t_mat_ibas.

    "Item
    SELECT *
      FROM /qaps/v_mat_dbas
      INTO TABLE @DATA(lt_destino).

    CHECK lines( lt_destino ) > 0.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_simulacao = @lt_destino-id_simulacao
      INTO TABLE @DATA(lt_simulacao).

    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @lt_destino
      WHERE id_item = @lt_destino-id_item
      AND   id_matriz_abast = @lt_destino-id_matriz_abast
      INTO TABLE @DATA(lt_distrib).

    SORT lt_destino BY id_matriz_abast id_item.

*    BREAK-POINT.
    LOOP AT lt_destino INTO DATA(ls_destino).

      CHECK NOT line_exists( lt_distrib[ id_matriz_abast = ls_destino-id_matriz_abast
                                         id_item         = ls_destino-id_item ] ).

      DATA(ls_simulacao) = lt_simulacao[ id_simulacao = ls_destino-id_simulacao ].

      IF ls_destino-tipo_regra <> 'GE'.

        SELECT *
          FROM /qaps/v_mat_ibas
          WHERE ( ( tipo_regra = 'GP' AND id_grupo_produto = @ls_destino-id_grupo_produto ) OR
                ( tipo_regra = 'AG'   AND agregador = @ls_destino-agregador ) OR
                ( tipo_regra = 'MP'   AND mat_planejado = @ls_destino-mat_planejado ) OR
                ( tipo_regra = 'MA'   AND matnr = @ls_destino-matnr ) )
          AND id_simulacao = @ls_destino-id_simulacao
          AND has_destino = ''
          INTO TABLE @lt_origem.
      ELSE.
        SELECT *
        FROM /qaps/v_mat_ibas
        WHERE (  tipo_regra = 'GE' )
        AND id_simulacao = @ls_destino-id_simulacao
        AND has_destino = ''
        INTO TABLE @lt_origem.
      ENDIF.

      CHECK lines( lt_origem ) > 0.

      LOOP AT lt_origem INTO DATA(ls_origem).

        execute_distribuicao( is_origem    = ls_origem
                              is_destino   = ls_destino
                              is_simulacao = ls_simulacao ).

      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD merge_data.

    TYPES: BEGIN OF ts_matriz_abs,
             id_matriz_abast TYPE /qaps/matriz_abs-id_matriz_abast,
           END OF ts_matriz_abs.

    DATA lo_sdescr_main     TYPE REF TO cl_abap_structdescr.
    DATA lo_sdescr_append     TYPE REF TO cl_abap_structdescr.
    DATA lr_line       TYPE REF TO data.
    DATA lv_name TYPE string.
    DATA lv_name_prefixo TYPE string.
    DATA lv_name_sufixo TYPE string.
    DATA lt_matriz_abs TYPE TABLE OF ts_matriz_abs.

    FIELD-SYMBOLS: <ft>      TYPE ANY TABLE,
                   <fs_line> TYPE any.

    ASSIGN mt_data->* TO <ft>.
    ASSIGN ms_data->* TO <fs_line>.


    lo_sdescr_main ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/S_MATRIZ_ABASTECIMENTO' ).
    DATA(lt_components) = lo_sdescr_main->get_components( ).

    DELETE lt_components WHERE name = 'PERIODO'
                            OR name = 'MANDT'
                            OR name = 'VALOR'
                            OR name = 'PERCENTUAL'
                            OR name = ''.

    lo_sdescr_append ?= cl_abap_structdescr=>describe_by_name( p_name = '/QAPS/S_MATRIZ_ABASTECIMENTO' ).
    DATA(lt_comp_append) = lo_sdescr_append->get_components( ).

    APPEND LINES OF lt_comp_append  TO lt_components.

    lt_matriz_abs = VALUE #( FOR wa IN it_data
                            ( id_matriz_abast = wa-id_matriz_abast ) ).

    SORT lt_matriz_abs BY id_matriz_abast ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_matriz_abs COMPARING id_matriz_abast.

    DELETE lt_components WHERE as_include = 'X'.
    DATA(lv_lines) = lines( lt_components ).

    DATA(lo_custo_elementar) = NEW /qaps/cl_mdl_custo_elementar( ).
    DATA(lt_variaveis) = lo_custo_elementar->get_variavel( ).

    LOOP AT lt_matriz_abs INTO DATA(ls_matriz_abs).

      DATA(ls_data) = it_data[ id_matriz_abast = ls_matriz_abs-id_matriz_abast ].

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
        WHERE id_matriz_abast = ls_matriz_abs-id_matriz_abast.

        CHECK NOT ls_matriz_abs-id_matriz_abast IS INITIAL.

        DATA(ls_fcat) = it_fcat[ parameter1 = ls_data-periodo ].

        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.

        lv_name = ls_fcat-fieldname.

        ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE ls_data TO <fv_src>.
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

      "Fill Period
*      LOOP AT it_data INTO ls_data
*        WHERE id_var_input = ls_matriz_abs-id_matriz_abast.
*
*        CHECK NOT ls_matriz_abs-id_matriz_abast IS INITIAL.
*
*        ls_fcat = it_fcat[ parameter1 = ls_data-periodo ].
*
*        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.
*
*        lv_name = `PERIODO_` && lv_name_sufixo.
*        ASSIGN COMPONENT 'PERIODO' OF STRUCTURE ls_data TO <fv_src>.
*        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.
*
*        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.
*
*        <fv_trg> = <fv_src>.
*        UNASSIGN: <fv_src>, <fv_trg>.
*
*      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.

    ENDLOOP.

  ENDMETHOD.


  METHOD question.

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


  METHOD set_distribuicao_inicial.

    SELECT DISTINCT id_item
      FROM /qaps/matriz_itm
      INTO TABLE @DATA(lt_itm).

    SELECT DISTINCT id_item, id_distribuicao
      FROM /qaps/matriz_dst
      INTO TABLE @DATA(lt_distrib).

*    BREAK-POINT.

    LOOP AT lt_itm INTO DATA(ls_itm).

      DATA(lt_distrib_aux) = lt_distrib.
      DELETE lt_distrib_aux WHERE id_item <> ls_itm-id_item.

      CHECK lines( lt_distrib_aux ) = 1.
      UPDATE /qaps/matriz_dst
      SET percentual = 100
      WHERE id_item = ls_itm-id_item.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD set_periodo.
    ms_periodo = is_periodo.
  ENDMETHOD.


  METHOD update_input.

    DATA lr_data TYPE REF TO data.
    DATA lt_update TYPE /qaps/t_matriz_dst.

    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_distribuicao = @it_changed_data-id_distribuicao
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @lt_update.

    LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_changed_data) = it_changed_data[ id_distribuicao = <fs>-id_distribuicao
                                               periodo         = <fs>-periodo ].

      <fs>-percentual = ls_changed_data-percentual.

      lr_data = REF #( <fs> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

    ENDLOOP.

    MODIFY /qaps/matriz_dst FROM TABLE lt_update.
    COMMIT WORK AND WAIT.

    update_premissa( lt_update ).

    SELECT *
      FROM /qaps/matriz_dst
      FOR ALL ENTRIES IN @it_changed_data
      WHERE id_parent = @it_changed_data-id_distribuicao
      AND   periodo      = @it_changed_data-periodo
      INTO TABLE @lt_update.

    IF lines( lt_update ) > 0.

      LOOP AT lt_update ASSIGNING <fs>.

        ls_changed_data = it_changed_data[ id_distribuicao = <fs>-id_parent
                                           periodo         = <fs>-periodo ].

        <fs>-percentual = ls_changed_data-percentual.

        lr_data = REF #( <fs> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

      ENDLOOP.

      MODIFY /qaps/matriz_dst FROM TABLE lt_update.
      COMMIT WORK AND WAIT.

      update_premissa( it_data = lt_update ).

    ENDIF.

  ENDMETHOD.


  METHOD update_premissa.

    DATA lv_final TYPE abap_bool.

    SELECT *
      FROM /qaps/prem_distr
      FOR ALL ENTRIES IN @it_data
      WHERE id_distr_matriz = @it_data-id_distribuicao
      AND   periodo   = @it_data-periodo
      INTO TABLE @DATA(lt_update).

    CHECK lines( lt_update ) > 0.

    LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<fs>).

      DATA(ls_data) = it_data[ id_distribuicao = <fs>-id_distr_matriz
                               periodo         = <fs>-periodo ].

      CHECK sy-subrc EQ 0.

      <fs>-percentual = ls_data-percentual.

    ENDLOOP.

    MODIFY /qaps/prem_distr FROM TABLE lt_update.
    COMMIT WORK AND WAIT.

    BREAK c060863.
    WHILE lv_final = abap_false.

      "Herança
      SELECT *
        FROM /qaps/prem_distr
        FOR ALL ENTRIES IN @lt_update
        WHERE id_parent = @lt_update-id_distribuicao
        AND   periodo   = @lt_update-periodo
        INTO TABLE @DATA(lt_heranca).

      if lines( lt_heranca ) = 0.
        lv_final = abap_true.
        exit.
      endif.

      LOOP AT lt_heranca ASSIGNING <fs>.

        DATA(ls_update) = lt_update[ id_distribuicao = <fs>-id_parent
                                     periodo   = <fs>-periodo ].

        CHECK sy-subrc EQ 0.

        <fs>-percentual = ls_update-percentual.

      ENDLOOP.

      MODIFY /qaps/prem_distr FROM TABLE lt_heranca.
      COMMIT WORK AND WAIT.

      SELECT *
       FROM /qaps/prem_distr
       FOR ALL ENTRIES IN @lt_heranca
       WHERE id_parent = @lt_heranca-id_distribuicao
       AND   periodo   = @lt_heranca-periodo
       INTO TABLE @DATA(lt_new).

      IF sy-subrc NE 0.
        lv_final = abap_true.
      ELSE.
        lt_update = lt_heranca.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
