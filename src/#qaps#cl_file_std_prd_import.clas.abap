class /QAPS/CL_FILE_STD_PRD_IMPORT definition
  public
  inheriting from /QAPS/CL_FILE_PROCESSING_BASE
  final
  create public .

public section.

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
  data MT_DATA type /QAPS/T_FILE_STD_PRODUCAO_LOG .

  methods ON_ADDED_FUNCTION
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods VALIDATE
    changing
      !CS_DATA type /QAPS/S_FILE_STD_PRODUCAO_LOG .
ENDCLASS.



CLASS /QAPS/CL_FILE_STD_PRD_IMPORT IMPLEMENTATION.


  METHOD execute.

    FIELD-SYMBOLS: <ft_file>    TYPE /qaps/t_file_upload,
                   <ft_mapping> TYPE /qaps/t_file_from_to.

    ASSIGN ir_data->* TO <ft_file>.
    DATA(ls_std_producao) = VALUE #( <ft_file>[ sheet_name = 'STD_PRODUCAO' ] OPTIONAL ).

    IF NOT ls_std_producao IS INITIAL.

      process_sheet( ls_std_producao-data ).

*      "Exibir dados a efetivar
      DATA(lv_return) = show_log( ).

      IF lv_return = abap_true.
        return = mr_result.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD generic_to_typed_data.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    "data
    LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs>).

      CHECK sy-tabix > 1.

      APPEND INITIAL LINE TO mt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      LOOP AT mt_mapping INTO DATA(ls_mapping).

        ASSIGN COMPONENT ls_mapping-excel_column OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_src>).
        ASSIGN COMPONENT ls_mapping-table_column OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fv_trg>).
        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD mapping_columns.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <ft>.

    DEFINE add_mapping.
      APPEND VALUE /qaps/s_file_mapping(
            excel_column = &1
            table_column = &2 ) TO mt_mapping.
    END-OF-DEFINITION.

    add_mapping 'A' 'MATNR'.
    add_mapping 'B' 'MEINS_PA'.
    add_mapping 'C' 'TIPO_DESTINO'.
    add_mapping 'D' 'COD_DESTINO'.
    add_mapping 'E' 'CATEGORIA'.
    add_mapping 'F' 'COMPONENTE'.
    add_mapping 'G' 'MENGE'.
    add_mapping 'H' 'MEINS_CP'.

  ENDMETHOD.


  METHOD on_added_function.

    DATA lo_filters TYPE REF TO cl_salv_filters.
    DATA lt_data TYPE /qaps/t_file_std_producao_exp.

    CASE e_salv_function.
      WHEN 'FECHAR'.
        mv_result = abap_false.
        LEAVE TO SCREEN 0.
      WHEN 'EXECUTE'.
        mv_result = abap_true.
*        processing( ).
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

    LOOP AT mt_data ASSIGNING FIELD-SYMBOL(<fs>).
      validate( CHANGING cs_data = <fs> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD processing.
    BREAK-POINT.
  ENDMETHOD.


  METHOD process_sheet.

    "Mapear colunas
    mapping_columns( ir_data =  ir_line
                     is_target_type_struct = '/QAPS/S_FILE_STD_PRODUCAO' ).

    "Populate Data
    generic_to_typed_data( ir_line ).

    "Processar dados
    pre_processing( ).

  ENDMETHOD.


  METHOD show_log.

    DATA:
*          mo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table,
          lv_pf_status  TYPE sypfkey,
          lt_fcat       TYPE lvc_t_fcat.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = mo_gr_alv
          CHANGING
            t_table      = mt_data.
      CATCH cx_salv_msg.
    ENDTRY.

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
        i_structure_name = '/QAPS/S_FILE_STD_PRODUCAO_LOG'
      CHANGING
        ct_fieldcat      = lt_fcat.

    lo_columns = mo_gr_alv->get_columns( ).
*    BREAK C060863.
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
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 1 ).
          lo_column->set_output_length( 10 ).
          lo_column->set_alignment( value = if_salv_c_alignment=>centered ).
          lo_column->set_long_text( 'Status' ).
          lo_column->set_medium_text( 'Status' ).
          lo_column->set_short_text( 'Status' ).
        WHEN 'MATNR'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 2 ).
        WHEN 'DSC_MAKTX'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 3 ).
          lo_column->set_long_text( 'Prod Acabado' ).
          lo_column->set_medium_text( 'Prod Acabado' ).
          lo_column->set_short_text( value = 'Prod Acab' ).
        WHEN 'MEINS_PA'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 4 ).
        WHEN 'DSC_MEINS_PA'.
          lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 5 ).
        WHEN 'TIPO_DESTINO'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 6 ).
          lo_column->set_long_text( 'Tipo Destino' ).
          lo_column->set_medium_text( 'Tipo Destino' ).
          lo_column->set_short_text( value = 'Tp Destino' ).
*         WHEN 'DSC_TIPO_DESTINO'.
*           lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 7 ).
        WHEN 'COD_DESTINO'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 7 ).
          lo_column->set_long_text( 'Destino' ).
          lo_column->set_medium_text( 'Destino' ).
          lo_column->set_short_text( 'Destino' ).
        WHEN 'CATEGORIA'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 8 ).
          lo_column->set_output_length( 8 ).
        WHEN 'DSC_CATEGORIA'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 9 ).
          lo_column->set_output_length( 10 ).
        WHEN 'COMPONENTE'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 10 ).
        WHEN 'DSC_COMPONENTE'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 11 ).
          lo_column->set_long_text( 'Componente' ).
          lo_column->set_medium_text( 'Componente' ).
          lo_column->set_short_text( value = 'Componente' ).
        WHEN 'MENGE'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 12 ).
          lo_column->set_output_length( 10 ).
        WHEN 'MEINS_CP'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 13 ).
        WHEN 'MESSAGE'.
          lo_columns->set_column_position( columnname = ls_fcat-fieldname position = 14 ).
          lo_column->set_long_text( 'Mensagem' ).
          lo_column->set_output_length( 40 ).
          lo_column->set_medium_text( 'Mensagem' ).
          lo_column->set_short_text( 'Mensagem' ).
        WHEN OTHERS.
          lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
      ENDCASE.

    ENDLOOP.


    DATA(lo_display_settings) = mo_gr_alv->get_display_settings( ).
    lo_display_settings->set_striped_pattern( value = abap_true ).

    mo_gr_alv->display( ).

    IF mv_result = abap_true.
      mr_result = REF #( mt_data ).
    ENDIF.

    return = mv_result.

  ENDMETHOD.


  METHOD validate.

    DATA lv_message TYPE string.

    DEFINE add_error.
      cs_data-icon = icon_red_light.
      cs_data-message = &1.
      RETURN.
    END-OF-DEFINITION.

    data(lt_categoria) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_CATEGORIA' ).

    TRY.
        cs_data-dsc_maktx = /qaps/cl_helper_text=>get_material_text( cs_data-matnr ).
      CATCH /qaps/cx_general.  "
        lv_message = `Produto Acabado ` && cs_data-matnr &&  ` não existe` .
        add_error lv_message.
    ENDTRY.

     TRY.
        cs_data-dsc_meins_pa = /qaps/cl_helper_text=>get_meins_text( cs_data-meins_pa ).
      CATCH /qaps/cx_general.  "
        lv_message = `Unid Medida PA ` && cs_data-meins_pa &&  ` não existe` .
        add_error lv_message.
    ENDTRY.

    cs_data-dsc_categoria = value #( lt_categoria[ domvalue_l = cs_data-categoria ]-ddtext OPTIONAL ).

    if cs_data-dsc_categoria is INITIAL.
      lv_message = `Categoria ` && cs_data-categoria &&  ` não existe` .
        add_error lv_message.
    endif.

    TRY.
        cs_data-dsc_componente = /qaps/cl_helper_text=>get_material_text( cs_data-componente ).
      CATCH /qaps/cx_general.  "
        lv_message = `Componente ` && cs_data-componente &&  ` não existe` .
        add_error lv_message.
    ENDTRY.

    TRY.
        cs_data-dsc_meins_cp = /qaps/cl_helper_text=>get_meins_text( cs_data-meins_cp ).
      CATCH /qaps/cx_general.  "
        lv_message = `Unid Medida CP ` && cs_data-meins_cp &&  ` não existe` .
        add_error lv_message.
    ENDTRY.

    cs_data-icon = icon_green_light.

*    BREAK-POINT.

  ENDMETHOD.
ENDCLASS.
