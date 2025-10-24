class /QAPS/CL_MDL_REL_0002 definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods EXPORTAR
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods UPDATE_CONTENT
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
      !IV_CONTENT type STRING
      !IV_STATUS type /QAPS/ED_STATUS_LISTA_CUSTO .
  methods GET_SINGLE_LISTA_CUSTO
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO optional
    returning
      value(RETURN) type /QAPS/S_LISTA_HEADER .
  methods GET_LISTA_CUSTO
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO optional
    returning
      value(RETURN) type /QAPS/T_LISTA_HEADER .
  methods SET_PERIODO
    returning
      value(RETURN) type /QAPS/S_PERIODO .
  methods GET_ITEMS
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
      !IS_EXIBICAO type /QAPS/S_LISTA_CUSTO_EXIBICAO
    returning
      value(RETURN) type /QAPS/T_REL_0002 .
protected section.
private section.

  data MS_DATA type ref to DATA .
  data MS_PERIODO type /QAPS/S_PERIODO .
  data MT_CATALOG type LVC_T_FCAT .
  data MT_DATA type ref to DATA .

  methods DATA_TREATMENT_FOR_EXPORT
    changing
      !CT_DATA type /QAPS/T_REL_0002 .
  methods UPDATE_STATUS_LISTA_CUSTO
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
      !IV_STATUS type /QAPS/ED_STATUS_LISTA_CUSTO
      !IV_CLEAR_CONTENT type ABAP_BOOL default ABAP_FALSE .
  methods CREATE_DYNAMIC_CATALOG
    importing
      !IS_TIPO_LISTA type /QAPS/TP_LISTA
    returning
      value(RETURN) type LVC_T_FCAT .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT .
  methods GET_DISPLAY_NAME_MAPPING
    returning
      value(RETURN) type /QAPS/T_DISPLAY_NAME .
  methods GET_TAB_HEADER
    importing
      !IS_DATA type /QAPS/S_LISTA_HEADER
    returning
      value(RETURN) type /QAPS/T_CHAVE_VALOR .
  methods GET_TAB_HELP
    importing
      !IS_DATA type /QAPS/S_LISTA_HEADER
    returning
      value(RETURN) type /QAPS/T_HELP_EXPRESSAO .
  methods MERGE_DATA
    importing
      !IV_MOEDA type /QAPS/ED_MOEDA_CALCULO
      !IV_PERIODO type /QAPS/ED_PER_INICIAL
      !IV_MOEDA_FINAL type ABAP_BOOL
      !IS_DATA type /QAPS/S_RETORNO_FINAL
      !IT_FCAT type LVC_T_FCAT
    returning
      value(RETURN) type ref to DATA .
  methods GET_TAXA_CAMBIO
    importing
      !IS_DATA type /QAPS/S_LISTA_HEADER
    returning
      value(RETURN) type /QAPS/T_TAXA_CAMBIO_PERIODO .
  methods GENERATE_LISTA_CUSTO
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_NEXT_ID
    returning
      value(RETURN) type /QAPS/ED_ID_LISTA_CUSTO .
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



CLASS /QAPS/CL_MDL_REL_0002 IMPLEMENTATION.


  METHOD CREATE_DYNAMIC_CATALOG.

    DATA: lt_fcat          TYPE lvc_t_fcat,
          lt_fcat_template TYPE lvc_t_fcat,
          lt_fcat_v        TYPE lvc_t_fcat,
          lt_fcat_c        TYPE lvc_t_fcat,
          lt_fcat_e        TYPE lvc_t_fcat,
          lv_id(2)         TYPE n,
          lv_mes(2)        TYPE n,
          lv_ano(4)        TYPE n,
          lv_periodo(7)    TYPE c,
          lv_pos(2)        TYPE n,
          ls_periodo       TYPE /qaps/s_periodo.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_LISTA_CUSTO'
      CHANGING
        ct_fieldcat      = lt_fcat.

    DELETE lt_fcat WHERE datatype = 'RAW'.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_VALUES'
      CHANGING
        ct_fieldcat      = lt_fcat_template.

    DATA(ls_valor_template) = lt_fcat_template[ fieldname = 'VALOR' ].
    DATA(ls_perc_template)  = lt_fcat_template[ fieldname = 'PERCENTUAL' ].

    CLEAR: ls_valor_template-ref_table,
           ls_perc_template-ref_table.

    REFRESH lt_fcat_template.

    DATA(lt_calc_node) = NEW /qaps/cl_mdl_input_custo_calc( )->get_nodes( is_tipo_lista-id_tp_lista ).

    LOOP AT lt_calc_node INTO DATA(ls_calc_node).

      CASE ls_calc_node-tipo_node.
        WHEN 'E'.
          IF ls_calc_node-tipo_dado = 1.
            ls_valor_template-fieldname = ls_calc_node-fieldname.
            ls_valor_template-reptext = ls_calc_node-dsc_custo_elementar.
            APPEND ls_valor_template TO lt_fcat_e.
          ELSEIF ls_calc_node-tipo_dado = 2.
            ls_perc_template-fieldname = ls_calc_node-fieldname.
            ls_perc_template-reptext = ls_calc_node-dsc_custo_elementar.
            APPEND ls_perc_template TO lt_fcat_e.
          ENDIF.

        WHEN 'C'.
          ls_valor_template-fieldname = ls_calc_node-fieldname.
          ls_valor_template-reptext = ls_calc_node-descricao.

          IF ls_calc_node-price_field = 'X'.
            APPEND ls_valor_template TO lt_fcat_v.
          ELSE.
            APPEND ls_valor_template TO lt_fcat_c.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    SORT lt_fcat_v BY fieldname ASCENDING.
    SORT lt_fcat_c BY fieldname ASCENDING.
    SORT lt_fcat_e BY fieldname ASCENDING.

    APPEND LINES OF lt_fcat_v TO lt_fcat.
    APPEND LINES OF lt_fcat_c TO lt_fcat.
    APPEND LINES OF lt_fcat_e TO lt_fcat.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-col_pos = sy-tabix.
    ENDLOOP.

    mt_catalog = lt_fcat.
    return = lt_fcat.

  ENDMETHOD.


  METHOD CREATE_DYNAMIC_TABLE.

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

    cl_alv_table_create=>create_dynamic_table(
      EXPORTING
        it_fieldcatalog = it_fcat
      IMPORTING
        ep_table        = mt_data "lr_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2 ).

    CHECK NOT mt_data IS INITIAL.

    ASSIGN mt_data->* TO <fs_table>.
    CREATE DATA lr_line LIKE LINE OF <fs_table>.

    IF sy-subrc IS INITIAL.

      lr_tdescr  ?= cl_abap_tabledescr=>describe_by_data_ref( mt_data ).
      lr_sdescr ?= lr_tdescr->get_table_line_type( ).

      CREATE DATA ms_data TYPE HANDLE lr_sdescr.

    ELSE.
      MESSAGE 'Memória cheia. Reiniciar transação' TYPE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD data_treatment_for_export.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-material = |{ <fs>-material ALPHA = OUT }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD exportar.

    DATA: lt_download         TYPE /qaps/t_file_upload_multitab.

    DATA(lt_lista_custo) = get_lista_custo( iv_cod_lista_custo  ).
    DATA(ls_lista_custo) = lt_lista_custo[ 1 ].

    "Header
    APPEND INITIAL LINE TO lt_download ASSIGNING FIELD-SYMBOL(<fs_help>).
    DATA(lt_header) = get_tab_header( ls_lista_custo ).
    <fs_help>-sheet_name = 'Header'.
    <fs_help>-data = REF #( lt_header ).

    "Moeda Local
    DATA(lt_moeda_local) = get_items( iv_cod_lista_custo = ls_lista_custo-cod_lista_custo
                                      is_exibicao = VALUE #( moeda = 'MOEDA_LOCAL' ) ).
    data_treatment_for_export( CHANGING ct_data = lt_moeda_local ).

    APPEND INITIAL LINE TO lt_download ASSIGNING FIELD-SYMBOL(<fs_data>).
    DATA(lv_sheet_name) = `Lista ` && ls_lista_custo-moeda_calculo.
    <fs_data>-sheet_name = lv_sheet_name.
    <fs_data>-data = REF #( lt_moeda_local ).
    <fs_data>-display_name = get_display_name_mapping( ).

    "Moeda Lista
    DATA(lt_moeda_final) = get_items( iv_cod_lista_custo = ls_lista_custo-cod_lista_custo
                                   is_exibicao = VALUE #( moeda = 'MOEDA_FINAL' ) ).
    data_treatment_for_export( CHANGING ct_data = lt_moeda_final ).

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_data>.
    lv_sheet_name = `Lista ` && ls_lista_custo-moeda_lista.
    <fs_data>-sheet_name = lv_sheet_name.
    <fs_data>-data = REF #( lt_moeda_final ).
    <fs_data>-display_name = get_display_name_mapping( ).

    TRY.
        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        lo_file->file_download_multi_tab( it_data = lt_download
                                          iv_filename = `Lista_Custo_` && ls_lista_custo-cod_lista_custo ).
      CATCH /qaps/cx_file_error.    "
    ENDTRY.


  ENDMETHOD.


  METHOD GENERATE_LISTA_CUSTO.

    DATA: lv_jobname  TYPE tbtcjob-jobname,
          lv_jobcount TYPE tbtcjob-jobcount.

    lv_jobname = `GER_LISTA_CUSTO_` && sy-datum && `_` && sy-uzeit.

    " Open Job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

    SUBMIT /qaps/job_geracao_lista
      WITH p_codigo = iv_cod_lista_custo
      VIA JOB lv_jobname
      NUMBER lv_jobcount
      USER sy-uname
      AND RETURN.

    "     Schedule and close job.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount  = lv_jobcount
        jobname   = lv_jobname
        sdlstrtdt = sy-datum
        sdlstrttm = sy-uzeit.

    WAIT UP TO 2 SECONDS.

  ENDMETHOD.


  METHOD get_display_name_mapping.

    FIELD-SYMBOLS <fs_return> TYPE /qaps/s_display_name.

    DEFINE set_name.
      APPEND INITIAL LINE TO return ASSIGNING <fs_return>.
      <fs_return>-name         = &1.
      <fs_return>-display_name = &2.
    END-OF-DEFINITION.

    set_name 'COD_LISTA_CUSTO' 'Código Lista Custo'.
    set_name 'DESCRICAO' 'Lista de Custo'.
    set_name 'PERIODO' 'Mês'.
    set_name 'WERKS' 'Centro'.
    set_name 'COD_GRP_PLANTA' 'Código Grupo de Plantas'.
    set_name 'MATERIAL' 'Material'.
    set_name 'AGREGADOR' 'Agregador'.
    set_name 'ID_GRUPO_PRODUTO' ''.
    set_name 'DSC_GRUPO_PRODUTO' 'Grupo de Produto'.
    set_name 'CUSTO_GERENCIAL' 'Custo Gerencial'.
    set_name 'MARKUP_VAL' 'Markup'.
    set_name 'MARKUP_PERC' 'Markup %'.
    set_name 'MERCADO_VAL' 'Mercado'.
    set_name 'MERCADO_PERC' 'Mercado %'.
    set_name 'TOTAL_GERENCIAL' 'Total Gerencial'.

  ENDMETHOD.


  METHOD get_items.

    DATA: ls_content TYPE /qaps/s_retorno_final,
          lt_data    TYPE /qaps/t_retorno_calculo,
          lr_data    TYPE REF TO data.
    DATA ls_template type /qaps/s_rel_0002.

    DATA: lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_entry      TYPE /qaps/prem_distr,
          ls_periodo    TYPE /qaps/s_periodo,
          ls_per        TYPE /qaps/s_periodo_interval.

    DATA(ls_lista_custo) = NEW /qaps/cl_mdl_lista_custo( )->get_single_lista_custo( iv_cod_lista_custo ).
    DATA(lt_grupo_produto) =  NEW /qaps/cl_mdl_material( )->get_grupo_produto(  ).
    DATA(lt_material) = NEW /qaps/cl_mdl_material( )->get_materiais_all( ir_matnr = VALUE #( ) ).
    DATA(ls_simulacao) = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( ls_lista_custo-id_simulacao ).

    lr_data = REF #( ls_content ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml = ls_lista_custo-content
                                         CHANGING cr_data = lr_data ).

    APPEND LINES OF: ls_content-t_importado TO lt_data,
                     ls_content-t_nacional TO lt_data,
                     ls_content-t_producao_conversao TO lt_data,
                     ls_content-t_producao_producao TO lt_data,
                     ls_content-t_transf_importacao TO lt_data,
                     ls_content-t_transf_nacional TO lt_data,
                     ls_content-t_transf_std_producao TO lt_data.

    DELETE lt_data WHERE ponderacao <> 'X'.

    select matnr,maktx
      from makt
      FOR ALL ENTRIES IN @lt_data
      where matnr = @lt_data-material
      and spras = @sy-langu
      into TABLE @data(lt_makt).

    LOOP AT lt_data INTO DATA(ls_data).

      DATA(ls_material) = lt_material[ matnr = ls_data-matnr ].

      clear ls_template.

      ls_template = VALUE /qaps/s_rel_0002( cod_lista_custo   = ls_lista_custo-cod_lista_custo
                                            descricao         = ls_lista_custo-descricao
                                            werks             = ls_data-werks
                                            cod_grp_planta    = ls_data-cod_grp_planta
                                            material          = ls_data-matnr
                                            maktx = value #( lt_makt[ matnr = ls_data-matnr ]-maktx OPTIONAL )
                                            agregador         = ls_material-agregador ).
*                                                  id_grupo_produto  = ls_data-id_grupo_produto ).

      ls_template-dsc_grupo_produto
              = VALUE #( lt_grupo_produto[ id_grupo_produto = ls_material-id_grupo_produto ]-descricao OPTIONAL ).

      DATA(lv_periodo_inicial) = ls_simulacao-periodo_inicial.

      ls_periodo-year = ls_simulacao-periodo_inicial(4).
      ls_periodo-month = ls_simulacao-periodo_inicial+4(2).

      ls_per = VALUE /qaps/s_periodo_interval(
          inicial = ls_simulacao-periodo_inicial
          final   = ls_simulacao-periodo_final    ).

*      lr_data = REF #( is_data ).
*      preencher_dados_controle( CHANGING cr_data = lr_data ).
*      BREAK C060863.
      WHILE lv_periodo_inicial <= ls_per-final.

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

        ls_template-periodo = lv_ano && lv_mes.

        check lv_periodo_inicial <= ls_per-final.

        LOOP AT ls_data-t_expressao INTO DATA(ls_expressao).

          CHECK ( ls_expressao-tipo_node = 'C' AND ls_expressao-price_field = 'X' ).

          DATA(ls_valores) = ls_expressao-t_valores[ periodo = ls_template-periodo ].

*          break c060863.
          IF is_exibicao-moeda = 'MOEDA_LOCAL'.

            CASE ls_expressao-fieldname.
              WHEN 'VLR_CUSTO_GERENCIAL'.
                ls_template-custo_gerencial = ls_valores-valor.
              WHEN 'VLR_TOTAL_GERENCIAL'.
                ls_template-total_gerencial = ls_valores-valor.
              WHEN 'VLR_TOTAL_MARKUP'.
                ls_template-markup_val = ls_valores-valor.
              WHEN 'VLR_TOTAL_MERCADO'.
                ls_template-mercado_val = ls_valores-valor.
            ENDCASE.

          ELSEIF is_exibicao-moeda = 'MOEDA_FINAL'.

            CASE ls_expressao-fieldname.
              WHEN 'VLR_CUSTO_GERENCIAL'.
                ls_template-custo_gerencial = ls_valores-valor_moeda_final.
              WHEN 'VLR_TOTAL_GERENCIAL'.
                ls_template-total_gerencial = ls_valores-valor_moeda_final.
              WHEN 'VLR_TOTAL_MARKUP'.
                ls_template-markup_val = ls_valores-valor_moeda_final.
              WHEN 'VLR_TOTAL_MERCADO'.
                ls_template-mercado_val = ls_valores-valor_moeda_final.
            ENDCASE.

          ENDIF.

        ENDLOOP.

        APPEND ls_template TO return.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_LISTA_CUSTO.

    IF iv_cod_lista_custo IS INITIAL.
      SELECT *
        FROM /qaps/lista_hdr
        INTO CORRESPONDING FIELDS OF TABLE @return.
    ELSE.
      SELECT *
        FROM /qaps/lista_hdr
        WHERE cod_lista_custo = @iv_cod_lista_custo
        INTO CORRESPONDING FIELDS OF TABLE @return.
    ENDIF.

    IF lines( return ) > 0.

      SELECT * FROM /qaps/fonte_cmb INTO TABLE @DATA(lt_fonte).

      LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

        CASE <fs>-status.
          WHEN 'G'. <fs>-icon = icon_light_out.
          WHEN 'A'. <fs>-icon = icon_green_light.
          WHEN 'F'. <fs>-icon = icon_locked.
          WHEN 'E'. <fs>-icon = icon_green_light.
          WHEN 'I'. <fs>-icon = icon_light_out.
        ENDCASE.

        <fs>-dsc_status = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_STATUS_LISTA_CUSTO'
                                                                 iv_value  = CONV #( <fs>-status ) ).

        <fs>-dsc_fonte = VALUE #( lt_fonte[ id_fonte = <fs>-id_fonte ]-descricao OPTIONAL ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD GET_NEXT_ID.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = '/QAPS/LISC'
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.


  ENDMETHOD.


  METHOD GET_REFERENCIAS.

    SELECT *
      FROM /qaps/simulacao
      FOR ALL ENTRIES IN @it_data
      WHERE id_simulacao = @it_data-id_original
      INTO CORRESPONDING FIELDS OF TABLE @return.

  ENDMETHOD.


  METHOD GET_SINGLE_LISTA_CUSTO.

    SELECT SINGLE *
      FROM /qaps/lista_hdr
      WHERE cod_lista_custo = @iv_cod_lista_custo
      INTO CORRESPONDING FIELDS OF @return.

    SELECT *
      FROM /qaps/fonte_cmb
      INTO TABLE @DATA(lt_fonte).

    return-dsc_fonte = VALUE #( lt_fonte[ id_fonte = return-id_fonte ]-descricao OPTIONAL ).

  ENDMETHOD.


  METHOD get_tab_header.

*    DATA: lr_data             TYPE REF TO data,
*          is_data             TYPE /qaps/s_retorno_final.

*     lr_data = REF #( is_data ).
*    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = is_data-content
*                                         CHANGING  cr_data = lr_data ).
    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.

    lr_data = REF #( ls_data ).
    IF NOT is_data-content IS INITIAL.
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  =  is_data-content
                                           CHANGING  cr_data = lr_data ).
    ENDIF.

     lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = is_data-content
                                         CHANGING  cr_data = lr_data ).

    APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs>).
    <fs>-chave = 'Lista de Custos'.
    <fs>-valor = is_data-cod_lista_custo && ` ` && is_data-descricao.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Status'.
    <fs>-valor = /qaps/cl_helper_text=>get_domain_text(
                 iv_domain = '/QAPS/D_STATUS_LISTA_CUSTO'
                 iv_value  = Conv #( is_data-status ) ).

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Tipo de Lista'.
    <fs>-valor = ls_data-tipo_lista-cod_tp_lista && ` ` && ls_data-tipo_lista-descricao.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Simulação'.
    <fs>-valor = ls_data-simulacao-id_simulacao && ` ` && ls_data-simulacao-descricao.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Std Produção'.
    <fs>-valor = ls_data-std_producao-codigo  && ` ` &&  ls_data-std_producao-descricao.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Fonte Câmbio'.
    <fs>-valor = is_data-dsc_fonte.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Moeda (Local/Final)'.
    <fs>-valor = is_data-moeda_calculo && `/` && is_data-moeda_lista.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = ''.
    <fs>-valor = ''.

    APPEND INITIAL LINE TO return ASSIGNING <fs>.
    <fs>-chave = 'Período'.
    <fs>-valor = 'Taxa'.

    sort ls_data-t_taxa_cambio by periodo ASCENDING.

    LOOP AT ls_data-t_taxa_cambio INTO DATA(ls_cambio).

      APPEND INITIAL LINE TO return ASSIGNING <fs>.
      <fs>-chave = ls_cambio-periodo+4(2) && `.` && ls_cambio-periodo(4).
      <fs>-valor = ls_cambio-taxa.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_TAB_HELP.

    DATA: lr_data TYPE REF TO data,
          ls_data TYPE /qaps/s_retorno_final.


    lr_data = REF #( ls_data ).
    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = is_data-content
                                         CHANGING  cr_data = lr_data ).


    DATA(lt_expressao) = NEW /qaps/cl_mdl_input_custo_calc( )->get_nodes( ls_data-tipo_lista-id_tp_lista ).

    SORT lt_expressao BY fieldname.

    LOOP AT lt_expressao INTO DATA(ls_expressao).

      CHECK ls_expressao-price_field = 'X'.

      APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-descricao = ls_expressao-descricao.
      <fs>-id = ls_expressao-fieldname.
      <fs>-expressao = ls_expressao-expressao.

    ENDLOOP.

    LOOP AT lt_expressao INTO ls_expressao.

      CHECK ls_expressao-price_field = '' and ls_expressao-tipo_node = 'C'.

      APPEND INITIAL LINE TO return ASSIGNING <fs>.
      <fs>-descricao = ls_expressao-descricao.
      <fs>-id = ls_expressao-fieldname.
      <fs>-expressao = ls_expressao-expressao.

    ENDLOOP.

    LOOP AT lt_expressao INTO ls_expressao.

      CHECK ls_expressao-price_field = '' and ls_expressao-tipo_node = 'E'.

      APPEND INITIAL LINE TO return ASSIGNING <fs>.
      <fs>-descricao = ls_expressao-dsc_custo_elementar.
      <fs>-id = ls_expressao-fieldname.
      <fs>-expressao = ls_expressao-expressao.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_TAXA_CAMBIO.

    SELECT SINGLE *
      FROM /qaps/simulacao
      WHERE id_simulacao = @is_data-id_simulacao
      INTO @DATA(ls_simulacao).

*    SELECT SINGLE *
*      FROM /qaps/fonte_cmb
*      WHERE id_fonte = @is_data-id_fonte
*      INTO @DATA(ls_fonte).

    IF is_data-moeda_calculo <> is_data-moeda_lista.
      SELECT *
        FROM /qaps/v_tx_cmb
        WHERE id_fonte = @is_data-id_fonte
        AND   moeda_local = @is_data-moeda_calculo
        AND   moeda_final = @is_data-moeda_lista
        AND   periodo >= @ls_simulacao-periodo_inicial
        AND   periodo <= @ls_simulacao-periodo_final
        INTO TABLE @return.
    ELSE.
      "Carrega para conversões, quando houver variáveis em outra moeda
      SELECT *
        FROM /qaps/v_tx_cmb
        WHERE id_fonte = @is_data-id_fonte
        AND   moeda_local = @is_data-moeda_calculo
*        AND   moeda_final = @iv_moeda_final
        AND   periodo >= @ls_simulacao-periodo_inicial
        AND   periodo <= @ls_simulacao-periodo_final
        INTO TABLE @return.
    ENDIF.

  ENDMETHOD.


  METHOD MERGE_DATA.

    DATA: lr_tdescr     TYPE REF TO cl_abap_tabledescr.

    DATA: lv_valor TYPE lvc_fname,
          lv_moeda TYPE lvc_fname.

    FIELD-SYMBOLS: <ft>      TYPE ANY TABLE,
                   <fs_line> TYPE any.

    lr_tdescr  ?= cl_abap_tabledescr=>describe_by_data_ref( mt_data ).
    CREATE DATA return TYPE HANDLE lr_tdescr.

    ASSIGN return->* TO <ft>.
    ASSIGN ms_data->* TO <fs_line>.

    REFRESH <ft>.


    "Importado
    DATA lv_tipo TYPE /qaps/s_lista_custo-tipo.
    lv_tipo = 'Importação'.

    LOOP AT is_data-t_importado INTO DATA(ls_data).

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.

      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_tipo>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = lv_tipo.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO DATA(ls_expressao).
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_value>).

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Nacional
    LOOP AT is_data-t_nacional INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      ls_data-lifnr = |{ ls_data-lifnr ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Nacional'.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Produção - Conversão
    LOOP AT is_data-t_producao_conversao INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      ls_data-lifnr = |{ ls_data-lifnr ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Prod. - Conversão'.
      ENDIF.

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_matnr>).
      IF <fv_matnr> IS ASSIGNED.
        <fv_matnr> = |{ ls_data-std_prd_pa ALPHA = OUT }|.
      ENDIF.

      ASSIGN COMPONENT 'COMPONENTE' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_comp>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_comp> = |{ ls_data-std_prd_cp ALPHA = OUT }|.
      ENDIF.

      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_menge>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_menge> = ls_data-std_prd_menge.
      ENDIF.

      ASSIGN COMPONENT 'MEINS' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_meins>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_meins> = ls_data-std_prd_meins.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Produção - Produção
    LOOP AT is_data-t_producao_producao INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      ls_data-lifnr = |{ ls_data-lifnr ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Prod. - Produção'.
      ENDIF.

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <fs_line> TO <fv_matnr>.
      IF <fv_matnr> IS ASSIGNED.
        <fv_matnr> = |{ ls_data-std_prd_pa ALPHA = OUT }|.
      ENDIF.

      ASSIGN COMPONENT 'COMPONENTE' OF STRUCTURE <fs_line> TO <fv_comp>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_comp> = |{ ls_data-std_prd_cp ALPHA = OUT }|.
      ENDIF.

      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_line> TO <fv_menge>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_menge> = ls_data-std_prd_menge.
      ENDIF.

      ASSIGN COMPONENT 'MEINS' OF STRUCTURE <fs_line> TO <fv_meins>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_meins> = ls_data-std_prd_meins.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.


    "Transf Importaçãp
    LOOP AT is_data-t_transf_importacao INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Importação'.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Transf Nacional
    LOOP AT is_data-t_transf_nacional INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      ls_data-material = |{ ls_data-lifnr ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Nacional'.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Transf Std Produção
    LOOP AT is_data-t_transf_std_producao INTO ls_data.

      CLEAR <fs_line>.

      ls_data-matnr = |{ ls_data-matnr ALPHA = OUT }|.
      ls_data-material = |{ ls_data-material ALPHA = OUT }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Std Produção'.
      ENDIF.

      LOOP AT ls_data-t_expressao INTO ls_expressao.
        ASSIGN COMPONENT ls_expressao-fieldname OF STRUCTURE <fs_line> TO <fv_value>.

        CHECK sy-subrc IS INITIAL.
        IF iv_moeda_final = abap_false.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor.
        ELSE.
          <fv_value> = ls_expressao-t_valores[ periodo = iv_periodo ]-valor_moeda_final.
        ENDIF.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

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


  METHOD SET_PERIODO.

    DATA ls_message     TYPE bapiret2.

    CHECK ms_periodo IS INITIAL.

    CALL FUNCTION '/QAPS/FM_PERIODO_INPUT'
      IMPORTING
        es_data    = ms_periodo
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ELSE.
      return = ms_periodo.
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_CONTENT.

    UPDATE /qaps/lista_hdr
    SET content = iv_content
        status = iv_status
    WHERE cod_lista_custo = iv_cod_lista_custo.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD UPDATE_STATUS_LISTA_CUSTO.

    DATA: ls_data    TYPE /qaps/s_lista_header,
          ls_entry   TYPE /qaps/lista_hdr,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/lista_hdr
      WHERE cod_lista_custo = @iv_cod_lista_custo
      INTO @ls_entry.

    CHECK sy-subrc EQ 0.

    ls_entry-status = iv_status.

    if iv_clear_content = abap_true.
      ls_entry-content = ''.
    endif.

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/lista_hdr FROM ls_entry.

  ENDMETHOD.
ENDCLASS.
