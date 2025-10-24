class /QAPS/CL_FILE_CST_ELEM_IMPORT definition
  public
  inheriting from /QAPS/CL_FILE_PROCESSING_BASE
  final
  create public .

public section.

  methods CONSTRUCTOR .

  methods EXECUTE
    redefinition .
protected section.

  methods GENERIC_TO_TYPED_DATA
    redefinition .
  methods PRE_PROCESSING
    redefinition .
  methods PROCESSING
    redefinition .
  methods PROCESS_SHEET
    redefinition .
  methods SHOW_LOG
    redefinition .
  methods MAPPING_COLUMNS
    redefinition .
private section.

  data MO_GR_ALV type ref to CL_SALV_TABLE .
  data MT_TRECHO type /QAPS/T_TRECHO .
  data MO_MATERIAL type ref to /QAPS/CL_MDL_MATERIAL .
  data MS_CUSTO_ELM type /QAPS/S_CUSTO_ELEMENTAR .
  data MT_CATEG_TRANSPORTE type /QAPS/T_CATEG_TRNS .
  data MT_CUSTO_PROCESSO type /QAPS/T_CUSTO_PRC .
  data MT_ERRORS type BAPIRET2_T .
  data MV_ID type INT2 .
  data MO_LOGISTICA type ref to /QAPS/CL_MDL_LOGISTICA .
  data MS_MAPPING type /QAPS/S_FILE_FROM_TO .
  data MT_CUSTO_ELEMENTAR type /QAPS/T_CUSTO_ELEMENTAR .
  data MT_DATA_PARTIAL type /QAPS/T_FILE_CST_ELEM_LOG .
  data MT_DATA type /QAPS/T_FILE_CST_ELEM_LOG .
  data MS_SIMULACAO type /QAPS/S_SIMULACAO .
  data MT_PERIODO type /QAPS/T_FILE_MAPPING .

  methods GET_TRECHO
    returning
      value(RETURN) type /QAPS/T_TRECHO .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_CUSTO_PROCESSO
    returning
      value(RETURN) type /QAPS/T_CUSTO_PRC .
  methods GET_CATEG_TRANSPORTE
    returning
      value(RETURN) type /QAPS/T_CATEG_TRNS .
  methods GET_CUSTO_ELEMENTAR
    importing
      !IV_ID_CUSTO_ELEMENTAR type /QAPS/ID_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/S_CUSTO_ELEMENTAR .
  methods ON_ADDED_FUNCTION
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION
      !SENDER .
  methods VALIDATE
    changing
      !CS_DATA type /QAPS/S_FILE_CST_ELEM_LOG .
ENDCLASS.



CLASS /QAPS/CL_FILE_CST_ELEM_IMPORT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_logistica = NEW /qaps/cl_mdl_logistica( ).
    mo_material = NEW /qaps/cl_mdl_material( ).
  ENDMETHOD.


  METHOD execute.

    DATA lv_id_simulacao TYPE /qaps/ed_id_simulacao.
    FIELD-SYMBOLS: <ft_file>    TYPE /qaps/t_file_upload,
                   <ft_mapping> TYPE /qaps/t_file_from_to.

    REFRESH mt_errors.

    ASSIGN ir_data->* TO <ft_file>.
    DATA(ls_mapping) = VALUE #( <ft_file>[ sheet_name = 'SYS_MAPPING' ] OPTIONAL ).

    ASSIGN ls_mapping-data->* TO <ft_mapping>.

    IF <ft_mapping> IS ASSIGNED.

      lv_id_simulacao = CONV #( <ft_mapping>[ tipo = 'SIMULACAO' ]-to ).
      ms_simulacao = NEW /qaps/cl_mdl_simulacao( )->get_simulacao_by_id( lv_id_simulacao ).

      if ms_simulacao-id_simulacao <> is_simulacao-id_simulacao.
        RAISE EXCEPTION TYPE /qaps/cx_pricing_error
            EXPORTING
              message = VALUE #( type = 'E'
                                 message = 'Simulação selecionada é diferente do arquivo, fazer o export antes' ).
      endif.

      "Verficar variáveis por permissão
      DATA(lt_variaveis) = NEW /qaps/cl_mdl_input_custo_elem( )->get_variaveis( lv_id_simulacao ).

      DELETE <ft_mapping> WHERE tipo <> 'VARIAVEL'.

      LOOP AT <ft_mapping> ASSIGNING FIELD-SYMBOL(<fs_mapping>).

        CHECK line_exists( <ft_file>[ sheet_name = <fs_mapping>-from ] ).

        ls_mapping = VALUE #( <ft_file>[ sheet_name = <fs_mapping>-from ] OPTIONAL ).

        IF line_exists( lt_variaveis[ id_custo_elementar = <fs_mapping>-to ] )."descricao = ls_mapping-sheet_name ] ).
          CLEAR mv_id.
          mv_id = mv_id + 1.
          ms_mapping = <fs_mapping>.
          process_sheet( ls_mapping-data ).
        ELSE.
          APPEND VALUE bapiret2( type = 'E'
                                 message = `Custo Elementar não permitido - ` && ls_mapping-sheet_name
                                  ) TO mt_errors.
        ENDIF.

      ENDLOOP.

      IF lines( mt_errors ) > 0.
        CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
          TABLES
            it_return = mt_errors.
      ENDIF.

      "Exibir dados a efetivar
      DATA(lv_return) = show_log( ).

      IF lv_return = abap_true.
        processing( ).
      ELSE.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD generic_to_typed_data.

    DATA lt_data TYPE /qaps/t_file_cst_elem_log.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    DATA(lt_grp_produto) = mo_material->get_grupo_produto( ).

    "data
*    BREAK C060863.
    LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs>).

      CHECK sy-tabix > 1.

      CHECK NOT <fs> IS INITIAL.

      mv_id = mv_id + 1.

      LOOP AT mt_periodo INTO DATA(ls_periodo).

        APPEND INITIAL LINE TO mt_data_partial ASSIGNING FIELD-SYMBOL(<fs_data>).

        <fs_data>-id = mv_id.
        <fs_data>-id_simulacao = ms_simulacao-id_simulacao.
        <fs_data>-id_custo_elementar = CONV #( ms_mapping-to ).
        DATA(lv_periodo) = ls_periodo-table_column+3(4) && ls_periodo-table_column(2).
        <fs_data>-periodo = lv_periodo.

        LOOP AT mt_mapping INTO DATA(ls_mapping).

          ASSIGN COMPONENT ls_mapping-excel_column OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_src>).
          ASSIGN COMPONENT ls_mapping-table_column OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fv_trg>).
          CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

          <fv_trg> = <fv_src>.
          UNASSIGN: <fv_src>, <fv_trg>.

        ENDLOOP.

        CASE ms_custo_elm-tipo_variavel.
          WHEN 'G'."Geral
            ASSIGN COMPONENT 'D' OF STRUCTURE <fs> TO <fv_src>.
          WHEN 'L'."Logistica
            ASSIGN COMPONENT 'D' OF STRUCTURE <fs> TO <fv_src>.
          WHEN 'P'."Produtivo
            ASSIGN COMPONENT 'C' OF STRUCTURE <fs> TO <fv_src>.
          WHEN 'F'."Frete
            ASSIGN COMPONENT 'B' OF STRUCTURE <fs> TO <fv_src>.
          WHEN 'C'."Compras
            ASSIGN COMPONENT 'D' OF STRUCTURE <fs> TO <fv_src>.
        ENDCASE.

        CASE <fs_data>-tipo_regra.
          WHEN 'GE'."	Geral
          WHEN 'GP'."	Grupo de Produto
            <fs_data>-id_grupo_produto = VALUE #( lt_grp_produto[ descricao =  <fv_src> ]-id_grupo_produto OPTIONAL ).
            <fs_data>-dsc_input = <fv_src>.
          WHEN 'MP'."	Material Planejado
            <fs_data>-mat_planejado = <fv_src>.
          WHEN 'AG'."	Agregador
            <fs_data>-agregador = <fv_src>.
          WHEN 'MA'."	Material
            <fs_data>-matnr = <fv_src>.
        ENDCASE.

        DATA(lv_tipo_dado) = mt_custo_elementar[ id_custo_elementar = <fs_data>-id_custo_elementar ]-tipo_dado.

        CASE lv_tipo_dado.
          WHEN '1'.ASSIGN COMPONENT 'VALOR' OF STRUCTURE <fs_data> TO <fv_trg>.
          WHEN '2'.ASSIGN COMPONENT 'PERCENTUAL' OF STRUCTURE <fs_data> TO <fv_trg>.
        ENDCASE.

        ASSIGN COMPONENT ls_periodo-excel_column OF STRUCTURE <fs> TO <fv_src>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        IF <fv_src> CO '0123456789.'.
          <fv_trg> = <fv_src>.
        ELSE.
          <fs_data>-icon = icon_red_light.
          <fs_data>-message = `Valor '` && <fv_src> && `' é inválido`.
        ENDIF.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

    ENDLOOP.

    DELETE mt_data_partial WHERE periodo IS INITIAL.

  ENDMETHOD.


  METHOD get_categ_transporte.

    IF lines( mt_categ_transporte ) = 0.
      SELECT *
        FROM /qaps/categ_trns
        INTO TABLE @mt_categ_transporte.
    ENDIF.

    return = mt_categ_transporte.

  ENDMETHOD.


  METHOD get_custo_elementar.

    IF mt_custo_elementar IS INITIAL.
      DATA(lo_custo_elm) = NEW /qaps/cl_mdl_custo_elementar( ).
      mt_custo_elementar = lo_custo_elm->get_variavel( ).
    ENDIF.

    return = VALUE #( mt_custo_elementar[ id_custo_elementar = iv_id_custo_elementar ] OPTIONAL ).

  ENDMETHOD.


  METHOD get_custo_processo.

    IF lines( mt_custo_processo ) = 0.
      SELECT *
        FROM /qaps/custo_prc
        INTO TABLE @mt_custo_processo.
    ENDIF.

    return = mt_custo_processo.

  ENDMETHOD.


  METHOD get_trecho.

    IF lines( mt_trecho ) = 0.
      mt_trecho = mo_logistica->get_trechos( ).
    ENDIF.

    return = mt_trecho.

  ENDMETHOD.


  METHOD mapping_columns.

    REFRESH: mt_mapping,
             mt_periodo.

    DEFINE add_mapping.
      APPEND VALUE /qaps/s_file_mapping(
                excel_column = &1
                table_column = &2 ) TO mt_mapping.
    END-OF-DEFINITION.

    DEFINE add_periodo.
      APPEND VALUE /qaps/s_file_mapping(
                excel_column = &1
                table_column = &2 ) TO mt_periodo.
    END-OF-DEFINITION.

    FIELD-SYMBOLS: <ft> TYPE ANY TABLE,
                   <fs> TYPE any.

    ASSIGN ir_data->* TO <ft>.
    LOOP AT <ft> ASSIGNING <fs>.
      EXIT.
    ENDLOOP.

    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( ir_data ) ).    .
    DATA(lo_periodo) = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) ).
    DATA(lt_periodo) = lo_periodo->get_components( ).

    ms_custo_elm = get_custo_elementar( CONV #( ms_mapping-to ) ).

    CASE ms_custo_elm-tipo_variavel.
      WHEN 'G'.	"Geral
        add_mapping 'A' 'TIPO_REGRA'.
        add_mapping 'B' 'TIPO_ORIGEM'."Tipo Origem
        add_mapping 'C' 'TIPO_DESTINO'.	"Tipo Destino

        add_mapping 'E' 'DSC_ORIGEM'.   "Origem
        add_mapping 'F' 'DSC_DESTINO'.    "Destino

        LOOP AT lt_periodo INTO DATA(ls_periodo) FROM 7.
          ASSIGN COMPONENT ls_periodo-name OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_periodo>).
          add_periodo ls_periodo-name <fv_periodo>."Tipo Origem
        ENDLOOP.

      WHEN 'L'.	"	Logística
        add_mapping 'A' 'TIPO_REGRA'.
        add_mapping 'B' 'TIPO_ORIGEM'. "Tipo Origem
        add_mapping 'C' 'TIPO_DESTINO'. "Tipo Destino
*B                                      ID
        add_mapping 'E' 'DSC_MODAL'.
        add_mapping 'F' 'DSC_CATEGORIA'.
        add_mapping 'G' 'DSC_ORIGEM'.
        add_mapping 'H' 'DSC_DESTINO'.

        LOOP AT lt_periodo INTO ls_periodo FROM 9.
          ASSIGN COMPONENT ls_periodo-name OF STRUCTURE <fs> TO <fv_periodo>.
          add_periodo ls_periodo-name <fv_periodo>."Tipo Origem
        ENDLOOP.

      WHEN 'P'.	"	Produtiva
        add_mapping 'A' 'TIPO_REGRA'.
        add_mapping 'B' 'TIPO_DESTINO'. "Tipo Destino
        add_mapping 'D' 'DSC_DESTINO'."Destino
        add_mapping 'E'	'DSC_PROCESSO'. "Processo

        LOOP AT lt_periodo INTO ls_periodo FROM 6.
          ASSIGN COMPONENT ls_periodo-name OF STRUCTURE <fs> TO <fv_periodo>.
          add_periodo ls_periodo-name <fv_periodo>."Tipo Origem
        ENDLOOP.

      WHEN 'F'.	"	Frete
        add_mapping 'A' 'TIPO_REGRA'.
        add_mapping 'B' 'KEY_INPUT'. "Tipo Origem
        add_mapping 'C' 'COD_TRECHO'. "Tipo Destino
*B                                      ID
*        add_mapping 'E' 'DSC_MODAL'.
*        add_mapping 'F' 'DSC_CATEGORIA'.
*        add_mapping 'F' 'DSC_ORIGEM'.
*        add_mapping 'G' 'DSC_DESTINO'.

        LOOP AT lt_periodo INTO ls_periodo FROM 4.
          ASSIGN COMPONENT ls_periodo-name OF STRUCTURE <fs> TO <fv_periodo>.
          add_periodo ls_periodo-name <fv_periodo>."Tipo Origem
        ENDLOOP.

      WHEN 'C'.	"Compras
*        BREAK c060863.
        add_mapping 'A' 'TIPO_REGRA'.
        add_mapping 'B' 'TIPO_ORIGEM'.
        add_mapping 'C' 'TIPO_DESTINO'.
        add_mapping 'D' 'KEY_INPUT'. "Tipo Origem
        add_mapping 'E' 'DSC_ORIGEM'.    "Destino
        add_mapping 'F' 'DSC_DESTINO'.    "Destino

        LOOP AT lt_periodo INTO ls_periodo FROM 7.
          ASSIGN COMPONENT ls_periodo-name OF STRUCTURE <fs> TO <fv_periodo>.
          add_periodo ls_periodo-name <fv_periodo>."Tipo Origem
        ENDLOOP.

      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.


  METHOD on_added_function.

    DATA: lo_filters TYPE REF TO cl_salv_filters.
    DATA lt_data TYPE /qaps/t_file_cst_elem_exp.

    CASE e_salv_function.
      WHEN 'FECHAR'.
        LEAVE TO SCREEN 0.
      WHEN 'EXECUTE'.
        processing( ).
        MESSAGE 'Importação efetuada com sucesso' TYPE 'S'.
        LEAVE TO SCREEN 0.
      WHEN 'EXPORT'.

        loop at mt_data into data(ls_data).
          append INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<fs>).
          <fs> = CORRESPONDING #( ls_data ).
          case ls_data-icon.
            when icon_green_light. <fs>-status = 'S'.
            when icon_red_light. <fs>-status = 'E'.
          endcase.
        ENDLOOP.

        export_log( REF #( lt_data ) ).
      WHEN 'TODOS'.
        lo_filters = mo_gr_alv->get_filters( ).
        lo_filters->clear( ).
      WHEN 'ERRORS'.
        lo_filters = mo_gr_alv->get_filters( ).

        TRY.
            CALL METHOD lo_filters->add_filter
              EXPORTING
                columnname = 'ICON'
                sign       = 'I'
                option     = 'EQ'
                low        = '@0A@' "red light
*               high       =
              .
          CATCH cx_salv_not_found .                     "#EC NO_HANDLER
          CATCH cx_salv_data_error .                    "#EC NO_HANDLER
          CATCH cx_salv_existing .                      "#EC NO_HANDLER
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD pre_processing.

    LOOP AT mt_data_partial ASSIGNING FIELD-SYMBOL(<fs>).
      validate( CHANGING cs_data = <fs> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD processing.

    TYPES: BEGIN OF ts_group,
             id                 TYPE int2,
             id_simulacao       TYPE /qaps/var_input-id_simulacao,
             id_custo_elementar TYPE /qaps/var_input-id_custo_elementar,
           END OF ts_group.

    DATA: ls_entry     TYPE /qaps/s_var_input,
          lt_entry     TYPE /qaps/t_var_input,
          lt_group     TYPE TABLE OF ts_group,
          lt_var_input TYPE /qaps/t_var_input_selected.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF question( lv_message ) = abap_true.

*      BREAK c060863.

      "Deleção
      LOOP AT mt_data INTO DATA(ls_data).
        CHECK NOT line_exists( lt_group[ id = ls_data-id
                                         id_simulacao = ls_data-id_simulacao
                                         id_custo_elementar = ls_data-id_custo_elementar ] ).

        APPEND VALUE ts_group( id = ls_data-id
                               id_simulacao       = ls_data-id_simulacao
                               id_custo_elementar = ls_data-id_custo_elementar ) TO lt_group.

        SELECT id_var_input
        FROM /qaps/var_input
        WHERE id_simulacao = @ls_data-id_simulacao
        AND   id_custo_elementar = @ls_data-id_custo_elementar
        APPENDING TABLE @lt_var_input.

        SORT lt_var_input BY id_var_input ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_var_input COMPARING id_var_input.

      ENDLOOP.

      DATA(lo_input) = NEW /qaps/cl_mdl_input_custo_elem( ).
      lo_input->delete_input_by_id( lt_var_input ).

      LOOP AT lt_group INTO DATA(ls_group).

        REFRESH lt_entry.

        LOOP AT mt_data INTO ls_data WHERE id = ls_group-id
                                       AND id_custo_elementar = ls_group-id_custo_elementar.
          ls_entry = CORRESPONDING #( ls_data ).
          APPEND ls_entry TO lt_entry.
        ENDLOOP.

        lo_input->create_input_by_table( lt_entry ).

      ENDLOOP.

      break c060863.
      lo_input->sync_matriz_abastecimento( ls_data-id_simulacao ).

    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
*      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD process_sheet.

    REFRESH mt_data_partial.

    "Mapear colunas
    mapping_columns( ir_data = ir_line
                     is_target_type_struct = '/QAPS/S_FILE_CST_ELEM' ).

    "Populate Data
    generic_to_typed_data( ir_line ).

    "Processar dados
    pre_processing( ).

    APPEND LINES OF mt_data_partial TO mt_data.

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


  METHOD show_log.

    DATA:
*          lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table,
          lv_pf_status  TYPE sypfkey.

    DATA lt_fcat TYPE lvc_t_fcat.

*    BREAK-POINT.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = mo_gr_alv
          CHANGING
            t_table      = mt_data.
      CATCH cx_salv_msg.
    ENDTRY.

    data: lo_layout    TYPE REF TO cl_salv_form_layout_grid,
          lo_element   TYPE REF TO cl_salv_form_element.

    CREATE OBJECT lo_layout.

    " Adicionar uma linha no grid do form
    data(lv_simulacao) = `Simulação: ` && |{ ms_simulacao-id_simulacao ALPHA = OUT }|.
    lo_element = lo_layout->create_label( row = 1 column = 1 text = lv_simulacao ).
    mo_gr_alv->set_top_of_list( lo_layout ).

    IF line_exists( mt_data[ icon = icon_red_light ] ).
      lv_pf_status = '1001'.
    ELSE.
      lv_pf_status = '1000'.
    ENDIF.

    mo_gr_alv->set_screen_status( report        = '/QAPS/REP_SALV_LOG_BASE'
                                  pfstatus      = lv_pf_status
                                  set_functions = mo_gr_alv->c_functions_all ).

    DATA(lo_events) = mo_gr_alv->get_event( ).
    SET HANDLER on_added_function FOR lo_events.

    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( mt_data ) ).    .
    DATA(lo_struct) = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) ).

    DATA(lv_name) = lo_struct->get_relative_name( ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_FILE_CST_ELEM_LOG'
      CHANGING
        ct_fieldcat      = lt_fcat.

    lo_columns = mo_gr_alv->get_columns( ).

    LOOP AT lt_fcat INTO DATA(ls_fcat).

      TRY.
          lo_column ?= lo_columns->get_column( columnname = ls_fcat-fieldname  ).
        CATCH cx_salv_not_found .
      ENDTRY.

      CASE ls_fcat-datatype.
        WHEN 'RAW'.
          lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
        WHEN 'DDTEXT'.
          lo_column->set_output_length( value = 10 ).
        WHEN 'CHAR'.
          IF ls_fcat-intlen >= 18.
            lo_column->set_output_length( value = 15 ).
          ENDIF.
      ENDCASE.

      CASE ls_fcat-fieldname.
        WHEN 'ICON'.
          lo_column->set_output_length( 10 ).
          lo_column->set_alignment( value = if_salv_c_alignment=>centered ).
          lo_column->set_long_text( 'Status' ).
          lo_column->set_medium_text( 'Status' ).
          lo_column->set_short_text( 'Status' ).
         WHEN 'ID'.
          lo_column->set_output_length( 8 ).
          lo_column->set_alignment( value = if_salv_c_alignment=>centered ).
          lo_column->set_long_text( 'Linha' ).
          lo_column->set_medium_text( 'Linha' ).
          lo_column->set_short_text( 'Linha' ).
        WHEN 'MESSAGE'.
          lo_columns->set_column_position(
            EXPORTING
              columnname = ls_fcat-fieldname    " ALV Control: Field Name of Internal Table Field
              position   = 2
          ).
          lo_column->set_long_text( 'Mensagem' ).
          lo_column->set_output_length( 30 ).
          lo_column->set_medium_text( 'Mensagem' ).
          lo_column->set_short_text( 'Mensagem' ).
        WHEN 'MATNR' OR 'AGREGADOR' OR 'MAT_PLANEJADO' OR 'MAKTX' OR 'TIPO_REGRA'
            OR 'TIPO_ORIGEM' OR 'TIPO_DESTINO' OR 'ID_MODAL'
            OR 'ID_SIMULACAO' OR 'DSC_SIMULACAO'.
          lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
      ENDCASE.

    ENDLOOP.


    DATA(lo_display_settings) = mo_gr_alv->get_display_settings( ).
    lo_display_settings->set_striped_pattern( value = abap_true ).

    mo_gr_alv->display( ).

  ENDMETHOD.


  METHOD validate.

    DEFINE add_error.
      cs_data-icon = icon_red_light.
      cs_data-message = &1.
      RETURN.
    END-OF-DEFINITION.

    "Descrições
    DATA(lt_grupo_produto) = mo_material->get_grupo_produto( ).
    DATA(lt_material) = mo_material->get_materiais_all( ir_matnr = VALUE #( ) ).

    DATA(lt_ponto) = mo_logistica->get_view_ponto( ).

    DATA(lt_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_PONTO' ).
    DATA(lt_modal) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_MODAL' ).
    DATA(lt_categ_trans) = get_categ_transporte( ).
    DATA(lt_custo_processo) = get_custo_processo( ).
    DATA(ls_custo_elementar) = get_custo_elementar( cs_data-id_custo_elementar ).
    DATA(lt_trecho) = get_trecho( ).

*    "to upper
    LOOP AT lt_grupo_produto ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-descricao = to_upper( <fs>-descricao ).
    ENDLOOP.

    cs_data-dsc_simulacao = ms_simulacao-descricao.
    cs_data-dsc_custo_elm = mt_custo_elementar[ id_custo_elementar = cs_data-id_custo_elementar ]-descricao.

    cs_data-dsc_tipo_regra = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_TIPO_REGRA'
                                                                    iv_value  = CONV #( cs_data-tipo_regra ) ).

    CHECK cs_data-icon IS INITIAL.

    IF cs_data-dsc_tipo_regra IS INITIAL.
      DATA(lv_message) = `Tipo de Regra ` && cs_data-tipo_regra && ` inválido`.
      add_error lv_message.
    ENDIF.

    CASE cs_data-tipo_regra.
      WHEN 'GE'."	Geral
      WHEN 'GP'."	Grupo de Produto
        DATA(lv_upper) = to_upper( cs_data-dsc_input ).
        DATA(ls_grupo_produto) = VALUE #( lt_grupo_produto[ descricao = lv_upper ] OPTIONAL ).
        cs_data-id_grupo_produto = ls_grupo_produto-id_grupo_produto.
        cs_data-key_input = cs_data-dsc_input.

        IF ls_grupo_produto-id_grupo_produto IS INITIAL.
          lv_message = `Grp Produto ` && cs_data-key_input && ` inválido`.
          add_error lv_message.
        ENDIF.
      WHEN 'MP'."	Material Planejado
        cs_data-key_input = cs_data-mat_planejado.
        IF NOT line_exists( lt_material[ mat_planejado = cs_data-key_input ] ).
          lv_message = `Mat Planejado ` && cs_data-key_input && ` inválido`.
          add_error lv_message.
        ELSE.
          cs_data-dsc_input = cs_data-key_input.
        ENDIF.
      WHEN 'AG'."	Agregador
        cs_data-key_input = cs_data-agregador.
        IF NOT line_exists( lt_material[ agregador = cs_data-key_input ] ).
          lv_message = `Agregador ` && cs_data-key_input && ` inválido`.
          add_error lv_message.
        ELSE.
          cs_data-dsc_input = cs_data-key_input.
        ENDIF.
      WHEN 'MA'."	Material
        TRY.
            cs_data-matnr = |{ cs_data-matnr ALPHA = IN WIDTH = 18 }|.
            cs_data-key_input = cs_data-matnr.
            cs_data-dsc_input = /qaps/cl_helper_text=>get_material_text( iv_matnr = cs_data-matnr ).
          CATCH /qaps/cx_general.  "
            lv_message = `Material ` && cs_data-matnr && ` inválido`.
            add_error lv_message.
        ENDTRY.
    ENDCASE.

    IF ls_custo_elementar-tipo_variavel = 'F'.

      IF cs_data-cod_trecho IS INITIAL.
        lv_message = `Trecho é obrigatório`.
        add_error lv_message.
      ENDIF.

      DATA(ls_trecho) = VALUE #( lt_trecho[ cod_trecho = cs_data-cod_trecho ] OPTIONAL ).

      IF ls_trecho IS INITIAL.
        lv_message = `Cod Trecho ` && cs_data-cod_trecho && ` inválido`.
        add_error lv_message.
      ENDIF.

*      break c060863.
      cs_data-id_trecho = ls_trecho-id_trecho.
      cs_data-tipo_origem = ls_trecho-tipo_origem.
      cs_data-id_origem = ls_trecho-id_origem.
      cs_data-dsc_origem = VALUE #( lt_ponto[ id_ponto = ls_trecho-id_origem ]-descricao OPTIONAL ).
      cs_data-tipo_destino = ls_trecho-tipo_destino.
      cs_data-id_destino = ls_trecho-id_destino.
      cs_data-dsc_destino = VALUE #( lt_ponto[ id_ponto = ls_trecho-id_destino ]-descricao OPTIONAL ).
      cs_data-id_modal = ls_trecho-id_modal.

    ENDIF.

    IF NOT cs_data-tipo_origem IS INITIAL.
      cs_data-dsc_tipo_origem = VALUE #( lt_origem_destino[ domvalue_l = cs_data-tipo_origem ]-ddtext OPTIONAL ).

      IF cs_data-dsc_tipo_origem IS INITIAL.
        lv_message = `Tipo de Origem ` && cs_data-tipo_origem && ` inválido`.
        add_error lv_message.
      ENDIF.
    ENDIF.

    IF cs_data-tipo_origem IS INITIAL AND NOT cs_data-dsc_origem IS INITIAL.
      lv_message = `Determinar Tipo de origem para '` && cs_data-dsc_origem && `'`.
      add_error lv_message.
    ELSEIF NOT cs_data-tipo_origem IS INITIAL AND NOT cs_data-dsc_origem IS INITIAL.

      CASE cs_data-tipo_origem.
        WHEN 'F' OR 'K'.
          DATA(lv_codigo_origem) = |{ cs_data-dsc_origem ALPHA = IN WIDTH = '10' }|.
        WHEN OTHERS.
          lv_codigo_origem = cs_data-dsc_origem.
      ENDCASE.

      DATA(ls_ponto_origem) = VALUE #( lt_ponto[ codigo = lv_codigo_origem
                                                 tipo_ponto = cs_data-tipo_origem ] OPTIONAL ).

      IF NOT ls_ponto_origem IS INITIAL.
        cs_data-id_origem =  ls_ponto_origem-id_ponto.
        cs_data-dsc_origem = ls_ponto_origem-descricao.
        cs_data-dsc_tipo_origem = ls_ponto_origem-dsc_tipo_ponto.
      ENDIF.

    ENDIF.

*    if cs_data-tipo_regra = 'AG'.
*      BREAK-POINT.
*    endif.

    IF NOT cs_data-tipo_destino IS INITIAL.
      cs_data-dsc_tipo_destino = VALUE #( lt_origem_destino[ domvalue_l = cs_data-tipo_destino ]-ddtext OPTIONAL ).

      IF cs_data-dsc_tipo_destino IS INITIAL.
        lv_message = `Tipo de destino ` && cs_data-tipo_destino && ` inválido`.
        add_error lv_message.
      ENDIF.
    ENDIF.

    "Destino
    IF cs_data-tipo_destino IS INITIAL AND NOT cs_data-dsc_destino IS INITIAL.
      lv_message = `Determinar Tipo de destino para '` && cs_data-dsc_destino && `'`.
      add_error lv_message.
    ELSEIF NOT cs_data-tipo_destino IS INITIAL AND NOT cs_data-dsc_destino IS INITIAL.

      CASE cs_data-tipo_destino.
        WHEN 'F' OR 'K'.
          DATA(lv_codigo_destino) = |{ cs_data-dsc_destino ALPHA = IN WIDTH = '10' }|.
        WHEN OTHERS.
          lv_codigo_destino = cs_data-dsc_destino.
      ENDCASE.

      DATA(ls_ponto_destino) = VALUE #( lt_ponto[ codigo = lv_codigo_destino
                                                  tipo_ponto = cs_data-tipo_destino ] OPTIONAL ).

      IF NOT ls_ponto_destino IS INITIAL.
        cs_data-id_destino =  ls_ponto_destino-id_ponto.
        cs_data-dsc_destino = ls_ponto_destino-descricao.
        cs_data-dsc_tipo_destino = ls_ponto_destino-dsc_tipo_ponto.
      ENDIF.

    ENDIF.

    IF NOT cs_data-dsc_modal IS INITIAL.
*      BREAK c060863.
      cs_data-id_modal = cs_data-dsc_modal." VALUE #( lt_modal[ ddtext = cs_data-dsc_modal ]-domvalue_l OPTIONAL ).

      IF cs_data-dsc_modal IS INITIAL.
        lv_message = `Modal ` && cs_data-dsc_modal && ` inválido`.
        add_error lv_message.
      ENDIF.

    ENDIF.

    IF NOT cs_data-dsc_categoria IS INITIAL.
      cs_data-id_categoria = VALUE #( lt_categ_trans[ descricao = cs_data-dsc_categoria ]-id_categoria OPTIONAL ).

      IF cs_data-id_categoria IS INITIAL.
        lv_message = `Categoria ` && cs_data-dsc_categoria && ` inválido`.
        add_error lv_message.
      ENDIF.
    ENDIF.

    IF NOT cs_data-dsc_processo IS INITIAL.

      cs_data-id_processo = VALUE #( lt_custo_processo[ descricao = cs_data-dsc_processo ]-id_processo OPTIONAL ).

      IF cs_data-id_processo IS INITIAL.
        lv_message = `Processo ` && cs_data-dsc_processo && ` inválido`.
        add_error lv_message.
      ENDIF.

    ENDIF.

    cs_data-message = 'OK'.
    cs_data-icon = icon_green_light.

  ENDMETHOD.
ENDCLASS.
