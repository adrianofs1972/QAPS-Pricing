class /QAPS/CL_MDL_STD_PRODUCAO definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods CREATE_STD_PRODUCAO_BY_FILE
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
      value(IT_FILE) type /QAPS/T_FILE_STD_PRODUCAO_LOG
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_COMPONENTES
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
      !IV_ID_STD_PROD_PA type /QAPS/ED_ID_STD_PROD_PA
    returning
      value(RETURN) type /QAPS/T_STD_PRODUCAO_CP .
  methods GET_PRODUTOS_ACABADOS
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/T_STD_PRODUCAO_PA .
  methods DELETE_STD_PRODUCAO_CP
    importing
      !IT_DATA type /QAPS/T_STD_PRODUCAO_CP
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_STD_PRODUCAO_PA
    importing
      !IT_DATA type /QAPS/T_STD_PRODUCAO_PA
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_STD_PRODUCAO_CP
    importing
      !IS_DATA type /QAPS/S_STD_PRODUCAO_CP
      !IS_PRODUTO_ACABADO type /QAPS/S_STD_PRODUCAO_PA
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_STD_PRODUCAO_PA
    importing
      !IS_DATA type /QAPS/S_STD_PRODUCAO_PA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_STD_PRODUCAO_CP
    importing
      !IS_PRODUTO_ACABADO type /QAPS/S_STD_PRODUCAO_PA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_STD_PRODUCAO_PA
    importing
      !IV_ID_STD_PRODUCAO type /QAPS/ED_ID_STD_PRODUCAO
    returning
      value(RETURN) type ABAP_BOOL .
  methods IMPORT_FILE
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods EXPORT_FILE
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_ALL
    returning
      value(RETURN) type /QAPS/T_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods GET_HEADER
    importing
      !IV_CODIGO type /QAPS/ED_COD_STD_PRODUCAO
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods DELETE_STD_PRODUCAO
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER
    raising
      /QAPS/CX_PRICING_ERROR .
  methods UPDATE_HEADER
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER .
protected section.
private section.

  data MT_CENTRO type /QAPS/T_CENTRO .
  data MT_GRP_PLANTA type /QAPS/T_GRP_PLANTA .
  data MT_REGIAO type /QAPS/T_REGIAO .
  data MS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER .

  methods FILL_AUXILIAR_TABLES .
  methods CONVERT_LOG_TO_PA
    importing
      !IT_FILE type /QAPS/T_FILE_STD_PRODUCAO_LOG
    returning
      value(RETURN) type /QAPS/T_STD_PRD_PA .
  methods SAVE_STD_PRODUCAO_BY_FILE_INS
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER
      value(IT_PA) type /QAPS/T_STD_PRD_PA
      value(IT_FILE) type /QAPS/T_FILE_STD_PRODUCAO_LOG
    returning
      value(RETURN) type ABAP_BOOL .
  methods SAVE_STD_PRODUCAO_BY_FILE_UPD
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER
      value(IT_PA) type /QAPS/T_STD_PRD_PA
      value(IT_FILE) type /QAPS/T_FILE_STD_PRODUCAO_LOG
    returning
      value(RETURN) type ABAP_BOOL .
  methods SAVE_STD_PRODUCAO_BY_FILE
    importing
      !IS_HEADER type /QAPS/S_STD_PRODUCAO_HEADER
      value(IT_PA) type /QAPS/T_STD_PRD_PA
      value(IT_FILE) type /QAPS/T_FILE_STD_PRODUCAO_LOG
      !IV_OPTION type CHAR1
    returning
      value(RETURN) type ABAP_BOOL .
  methods FILL_HELP_VALUES
    returning
      value(RETURN) type /QAPS/T_FILE_HELP_VALUES .
  methods GET_DISPLAY_NAME_MAPPING
    importing
      !IR_DATA type ref to DATA
    returning
      value(RETURN) type /QAPS/T_DISPLAY_NAME .
  methods GET_NEXT_NUMBER
    returning
      value(RETURN) type /QAPS/ED_COD_STD_PRODUCAO .
  methods CREATE_STD_PRODUCAO_INTERNAL
    importing
      !IS_DATA type /QAPS/S_STD_PRODUCAO_HEADER
    returning
      value(RETURN) type /QAPS/S_STD_PRODUCAO_HEADER .
  methods FILL_DESCRICOES
    changing
      !CS_DATA type /QAPS/S_STD_PRODUCAO_HEADER .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
      !IV_TEXT_BUTTON_1 type CHAR12 default 'Sim'
      !IV_TEXT_BUTTON_2 type CHAR12 default 'Não'
    returning
      value(RETURN) type ABAP_BOOL .
ENDCLASS.



CLASS /QAPS/CL_MDL_STD_PRODUCAO IMPLEMENTATION.


  METHOD convert_log_to_pa.

    fill_auxiliar_tables( ).

    LOOP AT it_file INTO DATA(ls_file).

      APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs>).

      <fs> = VALUE /qaps/std_prd_pa(
          matnr           =  |{ ls_file-matnr ALPHA = in WIDTH = 18 }|
          categoria       = ls_file-categoria
          id_regiao       = VALUE #( mt_regiao[ codigo = ls_file-cod_destino ]-id_regiao OPTIONAL )
          id_grp_planta   = VALUE #( mt_grp_planta[ codigo = ls_file-cod_destino ]-id_grp_planta OPTIONAL )
          werks           = VALUE #( mt_centro[ werks = ls_file-cod_destino ]-werks OPTIONAL )
          meins           = ls_file-meins_pa
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD create_std_producao.

    DATA lr_data TYPE REF TO data.
    DATA:
*          ls_data     TYPE /qaps/s_calculation_node,
      ls_entry   TYPE /qaps/std_prd_h,
      ls_message TYPE bapiret2,
      lt_sval    TYPE TABLE OF sval,
      lv_return  TYPE char1.

    APPEND VALUE sval( tabname = '/QAPS/STD_PRD_H'
                      fieldname = 'DESCRICAO'
                      fieldtext = 'Descrição' ) TO lt_sval.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Std Produção'
        start_column    = '15'
*       START_ROW       = '5'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF ls_message-type = 'A' OR sy-ucomm = 'CANC'.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = VALUE #( type = 'E'
                             message = 'Operação cancelada pelo usuário' ).
    ENDIF.

    ls_entry = VALUE /qaps/std_prd_h(
        id_std_producao = cl_system_uuid=>create_uuid_x16_static( )
        codigo          = get_next_number( )
        descricao       = lt_sval[ 1 ]-value ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data  ).
    INSERT /qaps/std_prd_h FROM ls_entry.

    return = CORRESPONDING #( ls_entry ).

  ENDMETHOD.


  METHOD create_std_producao_by_file.
    DATA: lv_message TYPE string,
          lv_option  TYPE c.

    lv_message = TEXT-m04.
    IF question( lv_message ) = abap_true.

      DATA(ls_std_producao_header) = get_header( iv_codigo ).

      DATA(lt_pa) = convert_log_to_pa( it_file ).

      SELECT *
        FROM /qaps/std_prd_pa
        FOR ALL ENTRIES IN @lt_pa
        WHERE matnr = @lt_pa-matnr
        AND id_std_producao = @ls_std_producao_header-id_std_producao
        AND id_regiao       = @lt_pa-id_regiao
        AND id_grp_planta   = @lt_pa-id_grp_planta
        AND werks           = @lt_pa-werks
        INTO TABLE @DATA(lt_qty).

      IF lines( lt_qty ) > 0.

        lv_message = TEXT-m05.
        IF question( iv_message = lv_message
                     iv_text_button_1 = 'Sobrescrever'
                     iv_text_button_2 = 'Atualizar' ) = abap_true.
          lv_option = 'S'.
        ELSE.
          lv_option = 'U'.
        ENDIF.

      ELSE.
        lv_option = 'S'.
      ENDIF.

      return = save_std_producao_by_file( is_header = ls_std_producao_header
                                          it_pa   = lt_pa
                                          it_file   = it_file
                                          iv_option = lv_option    " S - Sobrescrever/ U -  Atualizar
                                         ).

      return  = abap_true.

      MESSAGE 'Importação efetuada com sucesso' TYPE 'S'.

    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD create_std_producao_cp.

    DATA: ls_data    TYPE /qaps/s_std_producao_cp,
          ls_entry   TYPE /qaps/std_prd_cp,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_STD_PRD_CP_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'C'
*       IS_ITEM    =
        is_pa      = is_produto_acabado
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-componente =  |{ ls_entry-componente ALPHA = IN WIDTH = 18 }|.
    ls_entry-id_std_producao = is_produto_acabado-id_std_producao.
    ls_entry-id_std_prod_pa = is_produto_acabado-id_std_prod_pa.
    ls_entry-id_std_prod_cp = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/std_prd_cp FROM ls_entry.

    IF sy-subrc IS INITIAL.
      return = abap_true.
    ELSE.
      MESSAGE 'Componente já existe' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD create_std_producao_internal.

    DATA lr_data TYPE REF TO data.
    DATA ls_entry TYPE /qaps/std_prd_h.

    ls_entry = CORRESPONDING #( is_data ).
    ls_entry-id_std_producao = cl_system_uuid=>create_uuid_x16_static( ).
    lr_data = REF #( ls_entry ).

    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/std_prd_h FROM ls_entry.

    return = CORRESPONDING #( ls_entry ).

    fill_descricoes( CHANGING cs_data = return ).

  ENDMETHOD.


  METHOD create_std_producao_pa.

    DATA: ls_data    TYPE /qaps/s_std_producao_pa,
          ls_entry   TYPE /qaps/std_prd_pa,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_STD_PRD_PA_INPUT'
*      DESTINATION 'NONE'
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

*    break c060863.
    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-matnr = |{ ls_entry-matnr ALPHA = IN WIDTH = 18 }|.
    ls_entry-id_std_prod_pa = cl_system_uuid=>create_uuid_x16_static( ).
    ls_entry-id_std_producao = iv_id_std_producao.

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/std_prd_pa FROM ls_entry.

    IF sy-subrc IS INITIAL.
      return = abap_true.
    ELSE.
      MESSAGE 'Componente já existe' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_std_producao.

    DATA lv_message TYPE string.

    lv_message = 'Deseja excluir este Std Produção?'.

    IF question( lv_message ) = abap_true.

      DELETE FROM /qaps/std_prd_cp WHERE id_std_producao = is_header-id_std_producao.
      DELETE FROM /qaps/std_prd_pa WHERE id_std_producao = is_header-id_std_producao.
      DELETE FROM /qaps/std_prd_h WHERE id_std_producao = is_header-id_std_producao.

      COMMIT WORK AND WAIT.

    ELSE.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = VALUE #( type = 'E'
                             message = 'Operação cancelada pelo usuário' ).

    ENDIF.

  ENDMETHOD.


  METHOD DELETE_STD_PRODUCAO_CP.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/std_prd_cp WHERE id_std_prod_cp = ls_data-id_std_prod_cp.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD DELETE_STD_PRODUCAO_PA.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/std_prd_cp WHERE id_std_prod_pa = ls_data-id_std_prod_pa.
        DELETE FROM /qaps/std_prd_pa WHERE id_std_prod_pa = ls_data-id_std_prod_pa.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD edit_std_producao_cp.

    DATA: ls_data    TYPE /qaps/s_std_producao_cp,
          ls_entry   TYPE /qaps/std_prd_cp,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_STD_PRD_CP_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_item    = is_data
        is_pa      = is_produto_acabado
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
*    ls_entry-id_std_producao = is_produto_acabado-id_std_producao.
*    ls_entry-id_std_prod_pa = is_produto_acabado-id_std_prod_pa.
*    ls_entry-id_std_prod_cp = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/std_prd_cp FROM ls_entry.

    IF sy-subrc IS INITIAL.
      return = abap_true.
    ELSE.
      MESSAGE 'Componente já existe' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD edit_std_producao_pa.

    DATA: ls_data    TYPE /qaps/s_std_producao_pa,
          ls_entry   TYPE /qaps/std_prd_pa,
          ls_message TYPE bapiret2,
          lr_data    TYPE REF TO data.

    CALL FUNCTION '/QAPS/FM_STD_PRD_PA_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_action  = 'E'
        is_item    = is_data
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/std_prd_pa FROM ls_entry.

    IF sy-subrc IS INITIAL.
      return = abap_true.
    ELSE.
      MESSAGE 'Componente já existe' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD export_file.

    DATA lt_download TYPE /qaps/t_file_upload_multitab.
    DATA: lt_export  TYPE /qaps/t_file_std_producao,
          lt_mapping TYPE /qaps/t_file_from_to.

    SELECT *
      FROM /qaps/v_std_file
      WHERE codigo = @iv_codigo
      INTO TABLE @DATA(lt_data).

    IF sy-subrc = 0.

      DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
      DATA(lt_regiao) = lo_logistica->get_regioes( ).
      DATA(lt_grp_planta) = lo_logistica->get_grp_planta( ).

      LOOP AT lt_data INTO DATA(ls_data).
        APPEND INITIAL LINE TO lt_export ASSIGNING FIELD-SYMBOL(<fs_export>).
        <fs_export> = CORRESPONDING #( ls_data ).

        IF NOT ls_data-id_regiao IS INITIAL.
          <fs_export>-tipo_destino = 'R'.
          <fs_export>-cod_destino = VALUE #( lt_regiao[ id_regiao = ls_data-id_regiao ]-codigo OPTIONAL ).
        ELSEIF NOT ls_data-id_grp_planta IS INITIAL.
          <fs_export>-tipo_destino = 'G'.
          <fs_export>-cod_destino = VALUE #( lt_grp_planta[ id_grp_planta = ls_data-id_grp_planta ]-codigo OPTIONAL ).
        ELSEIF NOT ls_data-werks IS INITIAL.
          <fs_export>-tipo_destino = 'W'.
          <fs_export>-cod_destino = ls_data-werks.
        ENDIF.

      ENDLOOP.

    ENDIF.

    DATA(lt_display_name) = get_display_name_mapping( REF #( lt_export ) ).

    APPEND INITIAL LINE TO lt_download ASSIGNING FIELD-SYMBOL(<fs_download>).
    <fs_download>-sheet_name = 'STD_PRODUCAO'.
    <fs_download>-data = REF #( lt_export ).
    <fs_download>-display_name = lt_display_name.

    APPEND VALUE /qaps/s_file_from_to( tipo = 'STD_PRODUCAO'
                                       from = 'STD_PRODUCAO'
                                       to   = iv_codigo  ) TO lt_mapping.

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'SYS_MAPPING'.
    <fs_download>-data = REF #( lt_mapping ).
    <fs_download>-invisible = 'X'.

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'DISPLAY_MAPPING'.
    <fs_download>-data = REF #( lt_display_name ).
    <fs_download>-invisible = 'X'.

    DATA(lt_help) = fill_help_values(  ).

    APPEND INITIAL LINE TO lt_download ASSIGNING <fs_download>.
    <fs_download>-sheet_name = 'HELP'.
    <fs_download>-data = REF #( lt_help ).


    TRY.
        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        lo_file->file_download_multi_tab( it_data = lt_download
                                          iv_filename =  `Std_Prd_` && iv_codigo ).
      CATCH /qaps/cx_file_error.    "
    ENDTRY.


  ENDMETHOD.


  METHOD fill_auxiliar_tables.

    DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).

    if lines( mt_regiao ) = 0.
      mt_regiao = lo_logistica->get_regioes( ).
    endif.

    if lines( mt_centro ) = 0.
      mt_centro = lo_logistica->get_centros( ).
    endif.

    if lines( mt_grp_planta ) = 0.
      mt_grp_planta = lo_logistica->get_grp_planta( ).
    endif.

  ENDMETHOD.


  METHOD fill_descricoes.

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


  METHOD fill_help_values.

    DATA(lt_categoria) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_CATEGORIA' ).
    DATA(lt_tp_origem_destino) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_TIPO_PONTO' ).

    LOOP AT lt_categoria INTO DATA(ls_domain).
      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'CATEGORIA'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.

    ENDLOOP.

    APPEND VALUE #( )  TO return.


    LOOP AT lt_tp_origem_destino INTO ls_domain.

      check ls_domain-domvalue_l = 'R' or
            ls_domain-domvalue_l = 'G' or
            ls_domain-domvalue_l = 'W'.

      APPEND VALUE /qaps/s_file_help_values(
          campo     = 'TIPO_DESTINO'
          valor     = ls_domain-domvalue_l
          descricao = ls_domain-ddtext )  TO return.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_all.

    SELECT *
      FROM /qaps/std_prd_h
      INTO CORRESPONDING FIELDS OF TABLE @return.


  ENDMETHOD.


  METHOD GET_COMPONENTES.

    SELECT *
      FROM /qaps/std_prd_cp
      WHERE id_std_producao = @iv_id_std_producao
      and   id_std_prod_pa  = @iv_id_std_prod_pa
      INTO CORRESPONDING FIELDS OF TABLE @return.

*    data(lo_logistica) = new /qaps/cl_mdl_logistica( ).
*
    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
*
*      TRY.
          <fs>-dsc_maktx = /qaps/cl_helper_text=>get_material_text( <fs>-componente ).
*          <fs>-dsc_categoria = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_CATEGORIA'
*                                                                        iv_value  = CONV #( <FS>-categoria ) ).
*
*          IF NOT <fs>-werks IS INITIAL.
*            <fs>-tipo_destino = 'Planta'.
*            <fs>-cod_destino = <fs>-werks.
*
*            TRY.
*                <fs>-dsc_destino = /qaps/cl_helper_text=>get_werks_text( <fs>-werks ).
*              CATCH /qaps/cx_general.  "
*            ENDTRY.
*          ELSEIF NOT <fs>-id_regiao IS INITIAL.
*            data(ls_regiao) = lo_logistica->get_regiao_by_id( <fs>-id_regiao ).
*            <fs>-tipo_destino = 'Região'.
*            <fs>-cod_destino = ls_regiao-codigo.
*            <fs>-dsc_destino = ls_regiao-descricao.
*          ENDIF.
*
*        CATCH /qaps/cx_general.  "
*      ENDTRY.
*
    ENDLOOP.

  ENDMETHOD.


  METHOD get_display_name_mapping.

    return = VALUE #(
                    ( name = 'MATNR' display_name = 'Produto Acabado' width = 17 )
                    ( name = 'CATEGORIA' display_name = 'Categoria' width = 15 )
                    ( name = 'MEINS_PA' display_name = 'Un Medida PA' width = 15 )
                    ( name = 'TIPO_DESTINO' display_name = 'Tipo Destino' width = 15 )
                    ( name = 'COD_DESTINO' display_name = 'Destino' width = 15 )
                    ( name = 'COMPONENTE' display_name = 'Componente' width = 15 )
                    ( name = 'MENGE' display_name = 'Qtde' width = 15 )
                    ( name = 'MEINS_CP' display_name = 'Un Medida CP' width = 15 ) ).

  ENDMETHOD.


  METHOD get_header.

    DATA lv_question TYPE string.
    DATA ls_data TYPE /qaps/s_std_producao_header.

    SELECT SINGLE *
      FROM /qaps/std_prd_h
      WHERE codigo = @iv_codigo
      INTO CORRESPONDING FIELDS OF @return.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message = VALUE #( type = 'E'
                             message = 'Std Produção não existe' ).

    ENDIF.


ENDMETHOD.


  METHOD get_next_number.

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


  METHOD get_produtos_acabados.

    SELECT *
      FROM /qaps/std_prd_pa
      WHERE id_std_producao = @iv_id_std_producao
      INTO CORRESPONDING FIELDS OF TABLE @return.

    data(lo_logistica) = new /qaps/cl_mdl_logistica( ).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      TRY.
          <fs>-dsc_maktx = /qaps/cl_helper_text=>get_material_text( <fs>-matnr ).
          <fs>-dsc_categoria = /qaps/cl_helper_text=>get_domain_text( iv_domain = '/QAPS/D_CATEGORIA'
                                                                        iv_value  = CONV #( <FS>-categoria ) ).

          IF NOT <fs>-werks IS INITIAL.
            <fs>-tipo_destino = 'Planta'.
            <fs>-cod_destino = <fs>-werks.

            TRY.
                <fs>-dsc_destino = /qaps/cl_helper_text=>get_werks_text( <fs>-werks ).
              CATCH /qaps/cx_general.  "
            ENDTRY.
          ELSEIF NOT <fs>-id_regiao IS INITIAL.
            data(ls_regiao) = lo_logistica->get_regiao_by_id( <fs>-id_regiao ).
            <fs>-tipo_destino = 'Região'.
            <fs>-cod_destino = ls_regiao-codigo.
            <fs>-dsc_destino = ls_regiao-descricao.
          ELSEIF NOT <fs>-id_grp_planta IS INITIAL.
            data(lt_grp_planta) = lo_logistica->get_grp_planta( ).
            data(ls_grp_planta) = value #( lt_grp_planta[ id_grp_planta = <fs>-id_grp_planta ] OPTIONAL ).
            <fs>-tipo_destino = 'Grp Plantas'.
            <fs>-cod_destino = ls_grp_planta-codigo.
            <fs>-dsc_destino = ls_grp_planta-descricao.
          ENDIF.

        CATCH /qaps/cx_general.  "
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD import_file.

    FIELD-SYMBOLS <ft_return> TYPE /qaps/t_file_std_producao_log.

    TRY.
        DATA(lo_file) = NEW /qaps/cl_helper_file( ).
        DATA(lt_result) = lo_file->file_upload( ).

        "File Processing
        DATA(lo_processing) = NEW /qaps/cl_file_std_prd_import( ).

        DATA(lr_return) = lo_processing->execute( ir_data = REF #( lt_result ) ).

*        BREAK-POINT.
        IF NOT lr_return IS INITIAL.
          ASSIGN lr_return->* TO <ft_return>.
          create_std_producao_by_file( EXPORTING iv_codigo = iv_codigo
                                                 it_file   = <ft_return> ).
        ENDIF.

      CATCH /qaps/cx_file_error.    "
    ENDTRY.


  ENDMETHOD.


  METHOD question.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR              = ' '
*       DIAGNOSE_OBJECT       = ' '
        text_question         = iv_message
        text_button_1         = iv_text_button_1 "'Ja'(001)
*       ICON_BUTTON_1         = ' '
        text_button_2         = iv_text_button_2 "'Nein'(002)
*       ICON_BUTTON_2         = ' '
*       DEFAULT_BUTTON        = '1'
        display_cancel_button = ''
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN          = 25
*       START_ROW             = 6
*       POPUP_TYPE            =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        answer                = lv_answer
*     TABLES
*       PARAMETER             =
*     EXCEPTIONS
*       TEXT_NOT_FOUND        = 1
*       OTHERS                = 2
      .

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD save_std_producao_by_file.

    DATA: lt_pa TYPE TABLE OF /qaps/std_prd_pa,
          lt_cp TYPE TABLE OF /qaps/std_prd_cp.

    CASE iv_option.
      WHEN 'S'.
        return = save_std_producao_by_file_ins( is_header = is_header
                                                it_pa = it_pa
                                                it_file   = it_file ).
      WHEN 'U'.
        return = save_std_producao_by_file_upd( is_header = is_header
                                                it_pa = it_pa
                                                it_file   = it_file ).
    ENDCASE.

  ENDMETHOD.


  METHOD save_std_producao_by_file_ins.

    DATA: lt_pa TYPE TABLE OF /qaps/std_prd_pa,
          lt_cp TYPE TABLE OF /qaps/std_prd_cp.

    SORT it_file BY matnr ASCENDING.

    DELETE FROM /qaps/std_prd_cp WHERE id_std_producao = is_header-id_std_producao.
    DELETE FROM /qaps/std_prd_pa WHERE id_std_producao = is_header-id_std_producao.
    COMMIT WORK AND WAIT.

    DATA lr_data TYPE REF TO data.

    SORT it_file BY matnr ASCENDING.

    DATA(lt_file_pa) = it_file.
    DATA(lt_file_cp) = it_file.

*    BREAK c060863.

    SORT lt_file_pa BY matnr meins_pa tipo_destino cod_destino categoria.
    DELETE ADJACENT DUPLICATES FROM lt_file_pa COMPARING matnr meins_pa tipo_destino cod_destino categoria.

    LOOP AT lt_file_pa INTO DATA(ls_file_pa).

      APPEND INITIAL LINE TO lt_pa ASSIGNING FIELD-SYMBOL(<fs_new_entry>).
      <fs_new_entry> = CORRESPONDING #( ls_file_pa ).
      <fs_new_entry>-matnr = |{ <fs_new_entry>-matnr ALPHA = in WIDTH = 18 }|.
      <fs_new_entry>-id_std_producao = is_header-id_std_producao.
      <fs_new_entry>-id_std_prod_pa = cl_system_uuid=>create_uuid_x16_static( ).
      <fs_new_entry>-meins = ls_file_pa-meins_pa.

      case ls_file_pa-tipo_destino.
        when 'R'.
          DATA(ls_regiao)     = VALUE #( mt_regiao[ codigo     = ls_file_pa-cod_destino ] OPTIONAL ).
        when 'G'.
          DATA(ls_grp_planta) = VALUE #( mt_grp_planta[ codigo = ls_file_pa-cod_destino ] OPTIONAL ).
        when 'W'.
          DATA(ls_werks)      = VALUE #( mt_centro[ werks      = ls_file_pa-cod_destino ] OPTIONAL ).
      endcase.

      IF NOT ls_regiao IS INITIAL.
        <fs_new_entry>-id_regiao = ls_regiao-id_regiao.
      ELSEIF NOT ls_grp_planta IS INITIAL.
        <fs_new_entry>-id_grp_planta = ls_grp_planta-id_grp_planta.
      ELSEIF NOT ls_werks IS INITIAL.
        <fs_new_entry>-werks = ls_werks-werks.
      ENDIF.

      lr_data = REF #( <fs_new_entry> ).
      preencher_dados_controle( CHANGING cr_data = lr_data ).

      LOOP AT lt_file_cp INTO DATA(ls_file_cp).

        CHECK ls_file_pa-matnr        = ls_file_cp-matnr
          AND ls_file_pa-tipo_destino = ls_file_cp-tipo_destino
          AND ls_file_pa-cod_destino  = ls_file_cp-cod_destino.

        APPEND INITIAL LINE TO lt_cp ASSIGNING FIELD-SYMBOL(<fs_cp>).
        <fs_cp> = VALUE /qaps/std_prd_cp(
            id_std_prod_cp  = cl_system_uuid=>create_uuid_x16_static( )
            id_std_producao = <fs_new_entry>-id_std_producao
            id_std_prod_pa  = <fs_new_entry>-id_std_prod_pa
            componente      = |{ ls_file_cp-componente ALPHA = in WIDTH = 18 }|
            menge           = ls_file_cp-menge
            meins           = ls_file_cp-meins_cp ).

        lr_data = REF #( <fs_cp> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

      ENDLOOP.

    ENDLOOP.

    MODIFY /qaps/std_prd_pa FROM TABLE lt_pa.
    MODIFY /qaps/std_prd_cp FROM TABLE lt_cp.


  ENDMETHOD.


  METHOD save_std_producao_by_file_upd.

    DATA: lt_pa TYPE TABLE OF /qaps/std_prd_pa,
          lt_cp TYPE TABLE OF /qaps/std_prd_cp.

    DATA lr_data TYPE REF TO data.

    SORT it_file BY matnr ASCENDING.

    SELECT *
        FROM /qaps/std_prd_pa
        FOR ALL ENTRIES IN @it_pa
        WHERE matnr           = @it_pa-matnr
        AND   id_std_producao = @is_header-id_std_producao
        AND   id_regiao       = @it_pa-id_regiao
        AND   id_grp_planta   = @it_pa-id_grp_planta
        AND   werks           = @it_pa-werks
        INTO TABLE @lt_pa.

    LOOP AT it_pa ASSIGNING FIELD-SYMBOL(<fs_pa>).

      "Update
      IF line_exists( lt_pa[ matnr           = <fs_pa>-matnr
                             id_std_producao = is_header-id_std_producao
                             id_regiao       = <fs_pa>-id_regiao
                             id_grp_planta   = <fs_pa>-id_grp_planta
                             werks           = <fs_pa>-werks ] ).

        DATA(ls_pa) = lt_pa[ matnr           = <fs_pa>-matnr
                             id_std_producao = is_header-id_std_producao
                             id_regiao       = <fs_pa>-id_regiao
                             id_grp_planta   = <fs_pa>-id_grp_planta
                             werks           = <fs_pa>-werks ].

        lr_data = REF #( <fs_pa> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

        <fs_pa>-id_std_producao = ls_pa-id_std_producao.
        <fs_pa>-id_std_prod_pa  = ls_pa-id_std_prod_pa.

        DELETE FROM /qaps/std_prd_cp WHERE id_std_producao = ls_pa-id_std_producao
                                       AND id_std_prod_pa  = ls_pa-id_std_prod_pa.

        LOOP AT it_file INTO DATA(ls_file).

          CHECK ls_file-matnr = <fs_pa>-matnr.

          CASE ls_file-tipo_destino.
            WHEN 'R'.
              DATA(ls_regiao)     = VALUE #( mt_regiao[ codigo     = ls_file-cod_destino ] OPTIONAL ).
            WHEN 'G'.
              DATA(ls_grp_planta) = VALUE #( mt_grp_planta[ codigo = ls_file-cod_destino ] OPTIONAL ).
            WHEN 'W'.
              DATA(ls_werks)      = VALUE #( mt_centro[ werks      = ls_file-cod_destino ] OPTIONAL ).
          ENDCASE.

          CHECK ( ls_regiao-id_regiao         = <fs_pa>-id_regiao AND NOT <fs_pa>-id_regiao IS INITIAL ) OR
                ( ls_grp_planta-id_grp_planta = <fs_pa>-id_grp_planta AND NOT <fs_pa>-id_grp_planta IS INITIAL ) OR
                ( ls_werks-werks              = <fs_pa>-werks AND NOT <fs_pa>-werks IS INITIAL ).

          APPEND INITIAL LINE TO lt_cp ASSIGNING FIELD-SYMBOL(<fs_cp>).
          <fs_cp> = VALUE /qaps/std_prd_cp(
              id_std_prod_cp  = cl_system_uuid=>create_uuid_x16_static( )
              id_std_producao = <fs_pa>-id_std_producao
              id_std_prod_pa  = <fs_pa>-id_std_prod_pa
              componente      = ls_file-componente
              menge           = ls_file-menge
              meins           = ls_file-meins_cp ).

          lr_data = REF #( <fs_cp> ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).

        ENDLOOP.

      ELSE. "Insert

*        BREAK-POINT.
        APPEND INITIAL LINE TO lt_pa ASSIGNING FIELD-SYMBOL(<fs_new_entry>).
        <fs_new_entry> = CORRESPONDING #( <fs_pa> ).
        <fs_new_entry>-id_std_producao = is_header-id_std_producao.
        <fs_new_entry>-id_std_prod_pa = cl_system_uuid=>create_uuid_x16_static( ).

        lr_data = REF #( <fs_new_entry> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

        LOOP AT it_file INTO ls_file.

          CHECK ls_file-matnr = <fs_new_entry>-matnr.

          ls_regiao     = VALUE #( mt_regiao[ codigo     = ls_file-cod_destino ] OPTIONAL ).
          ls_grp_planta = VALUE #( mt_grp_planta[ codigo = ls_file-cod_destino ] OPTIONAL ).
          ls_werks      = VALUE #( mt_centro[ werks      = ls_file-cod_destino ] OPTIONAL ).

          CHECK ( ls_regiao-id_regiao         = <fs_pa>-id_regiao AND NOT <fs_pa>-id_regiao IS INITIAL ) OR
                ( ls_grp_planta-id_grp_planta = <fs_pa>-id_grp_planta AND NOT <fs_pa>-id_grp_planta IS INITIAL ) OR
                ( ls_werks-werks              = <fs_pa>-werks AND NOT <fs_pa>-werks IS INITIAL ).

          APPEND INITIAL LINE TO lt_cp ASSIGNING <fs_cp>.
          <fs_cp> = VALUE /qaps/std_prd_cp(
              id_std_prod_cp  = cl_system_uuid=>create_uuid_x16_static( )
              id_std_producao = <fs_new_entry>-id_std_producao
              id_std_prod_pa  = <fs_new_entry>-id_std_prod_pa
              componente      = ls_file-componente
              menge           = ls_file-menge
              meins           = ls_file-meins_cp ).

          lr_data = REF #( <fs_cp> ).
          preencher_dados_controle( CHANGING cr_data = lr_data ).

        ENDLOOP.

      ENDIF.


    ENDLOOP.

    MODIFY /qaps/std_prd_pa FROM TABLE lt_pa.
    MODIFY /qaps/std_prd_cp FROM TABLE lt_cp.

*    BREAK-POINT.


  ENDMETHOD.


  METHOD update_header.

*    DATA: ls_entry TYPE /qaps/std_prd_h,
*          lr_data  TYPE REF TO data.
*
*    ls_entry = CORRESPONDING #( is_header ).
*    lr_data = REF #( ls_entry ).
*    preencher_dados_controle( CHANGING cr_data = lr_data ).
*
*    UPDATE /qaps/std_prd_h
*    SET    meins = ls_entry-meins
*           modified_by = ls_entry-modified_by
*           modified_in = ls_entry-modified_in
*           modified_on = ls_entry-modified_on
*     WHERE id_std_producao = is_header-id_std_producao.

  ENDMETHOD.
ENDCLASS.
