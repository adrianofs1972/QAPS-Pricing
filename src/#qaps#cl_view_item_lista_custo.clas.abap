class /QAPS/CL_VIEW_ITEM_LISTA_CUSTO definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods SET_SESSION_DATA
    importing
      !IS_EXIBICAO type /QAPS/S_LISTA_CUSTO_EXIBICAO
      !IS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER
      !IV_PREVIOUS_FCAT type STRING .

  methods RESET
    redefinition .
  methods SET_DATA
    redefinition .
protected section.

  methods CUSTOMIZE_CATALOG
    redefinition .
  methods DISPLAY_ALV
    redefinition .
  methods GET_LAYOUT
    redefinition .
  methods GET_SORT
    redefinition .
  methods HOTSPOT_CLICK
    redefinition .
  methods MENU_BUTTON
    redefinition .
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
  methods DOUBLE_CLICK
    redefinition .
private section.

  data MS_EXIBICAO type /QAPS/S_LISTA_CUSTO_EXIBICAO .
  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MT_PREVIOUS_FCAT type LVC_T_FCAT .
  data MV_HAS_ERROR type ABAP_BOOL .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MV_SELECTED_VALOR type UI_FUNC .
  data MV_SELECTED_MOEDA type UI_FUNC .
  data MT_DATA type ref to DATA .
  data MS_DATA type ref to DATA .
  data MT_CATALOG type LVC_T_FCAT .

  methods GET_COLOR_TABLE
    importing
      !IR_DATA type ref to DATA
      !IV_PRODUCAO type CHAR1 default ''
    returning
      value(RETURN) type LVC_T_SCOL .
  methods GET_LINE_PONDERACAO
    importing
      !IT_PONDERACAO type /QAPS/T_PONDERACAO
      !IS_LISTA_CUSTO type /QAPS/S_RETORNO_CALCULO
      !IV_PRODUCAO type ABAP_BOOL
    returning
      value(RETURN) type /QAPS/S_PONDERACAO .
  methods GET_ID_PONDERACAO
    importing
      !IT_PONDERACAO type /QAPS/T_PONDERACAO
      !IS_LISTA_CUSTO type /QAPS/S_RETORNO_CALCULO
      !IV_PRODUCAO type ABAP_BOOL
    returning
      value(RETURN) type /QAPS/S_PONDERACAO .
  methods CREATE_DYNAMIC_CATALOG
    returning
      value(RETURN) type LVC_T_FCAT .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT .
  methods FILL_PORTO_CAIS_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  methods FILL_MATERIAL_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  methods MERGE_DATA
    importing
      !IS_DATA type /QAPS/S_RETORNO_FINAL
      !IT_FCAT type LVC_T_FCAT .
ENDCLASS.



CLASS /QAPS/CL_VIEW_ITEM_LISTA_CUSTO IMPLEMENTATION.


  METHOD create_dynamic_catalog.

    DATA: lt_fcat_init  TYPE lvc_t_fcat,
          lt_fcat       TYPE lvc_t_fcat,
          lt_fcat_comp  TYPE lvc_t_fcat,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_LISTA_CUSTO'
      CHANGING
        ct_fieldcat      = lt_fcat.

    APPEND LINES OF lt_fcat_init TO lt_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_VALUES'
      CHANGING
        ct_fieldcat      = lt_fcat_comp.

    DATA(ls_valor_template)      = lt_fcat_comp[ fieldname = 'VALOR' ].

    DELETE lt_fcat WHERE datatype = 'RAW'.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

      "Campos Chave
      if sy-tabix <= 5.
        <fs_fcat>-key = 'X'.
      endif.

      CASE <fs_fcat>-fieldname.
        when 'WERKS' OR 'COD_GRP_PLANTA' OR 'COD_PORTO'.
          <fs_fcat>-outputlen = 6.
         when 'COD_CAIS'.
          <fs_fcat>-outputlen = 7.
        when 'AGREGADOR'.
          <fs_fcat>-outputlen = 8.
        when 'DSC_GRUPO_PRODUTO'.
          <fs_fcat>-outputlen = 10.
        when 'PORTO'.
          <fs_fcat>-outputlen = 15.
        when 'CAIS'.
          <fs_fcat>-outputlen = 18.
        when 'TIPO'.
          <fs_fcat>-outputlen = 19.
        WHEN OTHERS.
          IF <fs_fcat>-fieldname(3) = 'DSC' OR <fs_fcat>-fieldname(4) = 'DESC'.
            <fs_fcat>-outputlen = 20.
          ELSE.
            <fs_fcat>-outputlen = 10.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    SORT lt_fcat BY col_pos.

    lv_pos = lines( lt_fcat ) + 1.

    DATA(lv_periodo_inicial) = ms_periodo-inicial.

    ls_periodo-year = ms_periodo-inicial(4).
    ls_periodo-month = ms_periodo-inicial+4(2).

    WHILE lv_periodo_inicial <= ms_periodo-final.

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

      check lv_periodo_inicial <= ms_periodo-final.

      DATA(ls_val_new)    = ls_valor_template.

      ls_val_new-reptext = ls_val_new-scrtext_s
        = ls_val_new-scrtext_m = ls_val_new-scrtext_l = lv_periodo.
      ls_val_new-parameter0 = 'VALOR'.
      ls_val_new-parameter1 = lv_ano && lv_mes.

      ls_val_new-fieldname   = ls_val_new-fieldname && `_` && lv_id.

      CLEAR: ls_val_new-ref_table.
      ls_val_new-col_pos = lv_pos.
      ls_val_new-hotspot = 'X'.

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

    cl_alv_table_create=>create_dynamic_table(
      EXPORTING
        it_fieldcatalog = it_fcat
      IMPORTING
        ep_table        = lr_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2 ).

    CHECK NOT lr_table IS INITIAL.

    ASSIGN lr_table->* TO <fs_table>.
    CREATE DATA lr_line LIKE LINE OF <fs_table>.

    "Estrutura Append
    APPEND INITIAL LINE TO lt_components ASSIGNING <fs_comp>.
    <fs_comp>-type ?= cl_abap_structdescr=>describe_by_data_ref( lr_line ).
    <fs_comp>-name = 'ALV_FIELDS'.
    <fs_comp>-as_include = abap_true.

    "Style
    APPEND INITIAL LINE TO lt_components ASSIGNING <fs_comp>.
    <fs_comp>-type ?= cl_abap_structdescr=>describe_by_data( lt_color ).
    <fs_comp>-name = 'COLOR'.

    "Color
    APPEND INITIAL LINE TO lt_components ASSIGNING <fs_comp>.
    <fs_comp>-type ?= cl_abap_structdescr=>describe_by_data( lt_style ).
    <fs_comp>-name = 'STYLE'.

    lr_sdescr  = cl_abap_structdescr=>create( lt_components ).
    lr_tdescr  = cl_abap_tabledescr=>create( lr_sdescr ).

    CREATE DATA ms_data TYPE HANDLE lr_sdescr.
    CREATE DATA mt_data TYPE HANDLE lr_tdescr.

  ENDMETHOD.


  METHOD CUSTOMIZE_CATALOG.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                    OR fieldname = 'STYLE'
                    OR fieldname = 'COLOR'
                    OR fieldname = 'ORD'
                    OR fieldname = 'MANDT'
                    OR fieldname = 'MESSAGE'
                    OR fieldname = 'CREATED_BY'
                    OR fieldname = 'CREATED_IN'
                    OR fieldname = 'CREATED_ON'
                    OR fieldname = 'MODIFIED_BY'
                    OR fieldname = 'MODIFIED_IN'
                    OR fieldname = 'MODIFIED_ON'
                    OR fieldname = 'ID_TP_LISTA'
                    OR fieldname = 'ID_STD_PRODUCAO'
                    OR fieldname = 'STATUS'
                    OR fieldname = 'DSC_TP_LISTA'
                    OR fieldname = 'MODALIDADE'
                    OR fieldname = 'HAS_ERROR'
                    OR fieldname = 'ID_ITEM_LISTA_CUSTO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'PONDERACAO'.
          <fs>-col_pos = 1.
          <fs>-checkbox = 'X'.
          <fs>-outputlen = '3'.
        WHEN 'ID_PONDERACAO'.
          <fs>-no_out = 'X'.
        when 'COD_GRP_PLANTA' OR 'WERKS' OR 'MATERIAL' or 'ID_PONDERACAO'.
          <fs>-no_merging = 'X'.
        WHEN 'DSC_STATUS'.
          <fs>-col_pos = 2.
          <fs>-outputlen = '10'.
          <fs>-just = 'C'.
        WHEN 'ID_SIMULACAO'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '10'.
          <fs>-col_pos = 3.
        WHEN 'DESCRICAO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = '18'.
          <fs>-hotspot = 'X'.
        WHEN 'ID_ORIGINAL'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '12'.
          <fs>-col_pos = 5.
        WHEN 'DSC_REFERENCIA'.
          <fs>-col_pos = 6.
          <fs>-outputlen = '15'.
        WHEN 'PERIODO_INICIAL'.
          <fs>-col_pos = 7.
          <fs>-outputlen = '12'.
        WHEN 'PERIODO_FINAL'.
          <fs>-col_pos = 8.
          <fs>-outputlen = '12'.
        WHEN 'COD_STD_PRODUCAO'.
          <fs>-col_pos = 9.
          <fs>-outputlen = '10'.
        WHEN 'DSC_STD_PRODUCAO'.
          <fs>-col_pos = 10.
          <fs>-outputlen = '18'.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.

    DATA lt_fcat TYPE lvc_t_fcat.
    FIELD-SYMBOLS <fs> TYPE ANY TABLE.

    ASSIGN mt_data->* TO <fs>.

    CHECK <fs> IS ASSIGNED.

    IF lines( mt_previous_fcat ) = 0.
      lt_fcat = it_catalog.
    ELSE.
      lt_fcat = mt_previous_fcat.
    ENDIF.

    mo_alv->set_table_for_first_display(
      EXPORTING
*       i_buffer_active =
*       i_bypassing_buffer            =
*       i_consistency_check           =
*       i_structure_name              =
*       is_variant      =
*       i_save          =
*       i_default       = 'X'
        is_layout       = is_layout
*       is_print        =
*       it_special_groups             =
*       it_toolbar_excluding          =
*       it_hyperlink    =
*       it_alv_graphics =
*       it_except_qinfo =
*       ir_salv_adapter =
      CHANGING
        it_outtab       = <fs> "mt_data
        it_fieldcatalog = lt_fcat
        it_sort         = it_sort
*       it_filter       =
*      EXCEPTIONS
*       invalid_parameter_combination = 1
*       program_error   = 2
*       too_many_lines  = 3
*       others          = 4
    ).


    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD double_click.

    FIELD-SYMBOLS <ft> TYPE STANDARD TABLE.

    ASSIGN mt_data->* TO <ft>.
    ASSIGN <ft>[ e_row ] TO FIELD-SYMBOL(<fs>).

    ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_has_error>).

    CHECK <fv_has_error> = abap_true.

*    BREAK c060863.

    ASSIGN COMPONENT 'ID_ITEM_LISTA_CUSTO' OF STRUCTURE <fs>
                              TO FIELD-SYMBOL(<fv_id_item_lista_custo>).

    raise EVENT on_double_click
      EXPORTING
        e_row                = e_row
        e_column             = e_column
        es_row_no            = es_row_no
        iv_additional_data_1 = CONV #( <fv_id_item_lista_custo> )
*        iv_additional_data_2 =
*        iv_additional_data_3 =
*        iv_additional_data_4 =
    .


  ENDMETHOD.


  METHOD fill_material_data.

    CHECK lines( ct_data ) > 0.

    SELECT *
      FROM /qaps/v_mat_all
      INTO TABLE @DATA(lt_material).

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_matnr>).

      CHECK sy-subrc EQ 0.

      DATA(ls_matnr) = VALUE #( lt_material[ matnr = <fv_matnr> ]  OPTIONAL ).

      ASSIGN COMPONENT 'AGREGADOR' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_agregador>).

      IF sy-subrc EQ 0.
        <fv_agregador> = ls_matnr-agregador.
      ENDIF.

      ASSIGN COMPONENT 'DSC_GRUPO_PRODUTO' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_dsc_grupo_produto>).

      IF sy-subrc EQ 0.
        <fv_dsc_grupo_produto> = ls_matnr-grp_produto.
      ENDIF.

      ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_maktx>).

      IF sy-subrc EQ 0.
        <fv_maktx> = ls_matnr-dsc_material.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_porto_cais_data.

*    BREAK c060863.

    CHECK lines( ct_data ) > 0.

    SELECT *
      FROM /qaps/v_prt_cais
      INTO TABLE @DATA(lt_porto_cais).

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).

      ASSIGN COMPONENT 'COD_PORTO' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_cod_porto>).

      IF sy-subrc EQ 0.

        DATA(ls_porto) = VALUE #( lt_porto_cais[ cod_porto = <fv_cod_porto> ]  OPTIONAL ).

        ASSIGN COMPONENT 'PORTO' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_porto>).

        IF sy-subrc EQ 0.
          <fv_porto> = ls_porto-porto.
        ENDIF.

      ENDIF.

      ASSIGN COMPONENT 'COD_CAIS' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_cod_cais>).

      IF sy-subrc EQ 0.

        DATA(ls_cais) = VALUE #( lt_porto_cais[ cod_cais = <fv_cod_cais> ]  OPTIONAL ).

        ASSIGN COMPONENT 'CAIS' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_cais>).

        IF sy-subrc EQ 0.
          <fv_cais> = ls_cais-cais.
        ENDIF.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD get_color_table.

    DATA: lo_typedescr   TYPE REF TO cl_abap_structdescr,
          lo_comp_struct TYPE REF TO cl_abap_structdescr.

    DATA ls_scol TYPE lvc_s_scol.

    lo_typedescr ?= cl_abap_typedescr=>describe_by_data_ref( ir_data  ).

    DATA(lt_comp) = lo_typedescr->get_components( ).

    SORT lt_comp BY name.
    lo_comp_struct ?= lt_comp[ name = 'ALV_FIELDS' ]-type.

    DATA(lt_comp_struct) = lo_comp_struct->get_components( ).

    LOOP AT lt_comp_struct INTO DATA(ls_comp).

      CHECK NOT ls_comp-name IS INITIAL.
      ls_scol-fname = ls_comp-name.
      CASE iv_producao.
        WHEN 'I'.
          ls_scol-color = VALUE lvc_s_colo(
              col = 7
              int = 0
              inv = 0
          ).
        WHEN 'T'.
          ls_scol-color = VALUE lvc_s_colo(
              col = 3
              int = 0
              inv = 0
          ).
        WHEN 'E'. "Error
          ls_scol-color = VALUE lvc_s_colo(
              col = 6
              int = 0
              inv = 0
          ).
        WHEN 'P'. "Error
          ls_scol-color = VALUE lvc_s_colo(
              col = 5
              int = 0
              inv = 0
          ).
        WHEN OTHERS.
          ls_scol-color = VALUE lvc_s_colo(
              col = 2
              int = 0
              inv = 0
          ).
      ENDCASE.
      APPEND ls_scol TO return.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_id_ponderacao.

    DATA lv_producao TYPE abap_bool.
    data lv_matnr type matnr.

    if iv_producao = ''.
      lv_matnr  = |{ is_lista_custo-matnr ALPHA = in WIDTH = 18 }|.
    else.
      lv_matnr  = |{ is_lista_custo-std_prd_pa ALPHA = in WIDTH = 18 }|.
    endif.

    lv_producao = iv_producao.

    return = VALUE #( it_ponderacao[ cod_grp_planta = is_lista_custo-cod_grp_planta
                                     werks          = is_lista_custo-werks
                                     matnr          = lv_matnr
                                     producao       = lv_producao ] OPTIONAL ).

    IF return IS INITIAL AND lv_producao = abap_false.
      lv_producao = abap_true.
    ELSEIF return IS INITIAL AND lv_producao = abap_true.
      lv_producao = abap_false.
    ELSE.
      RETURN.
    ENDIF.

    return = VALUE #( it_ponderacao[ cod_grp_planta = is_lista_custo-cod_grp_planta
                            werks          = is_lista_custo-werks
                            matnr          = lv_matnr
                            producao       = lv_producao ] OPTIONAL ).

  ENDMETHOD.


  METHOD get_layout.
    return = super->get_layout( ).
    return-sel_mode = 'A'.
*    return-no_toolbar = 'X'.

    DATA(lv_cod_lista_custo) = |{ ms_lista_custo-cod_lista_custo ALPHA = OUT }|.
    data(lv_descricao) = |{ ms_lista_custo-descricao ALPHA = OUT }|.
    CONDENSE: lv_cod_lista_custo NO-GAPS,
              lv_descricao.
    DATA(lv_title) = |{ lv_cod_lista_custo } - | &&
                     | { lv_descricao } | &&
                     | - Status: { ms_lista_custo-dsc_status } |.
    return-grid_title = lv_title.

  ENDMETHOD.


  METHOD get_sort.

    APPEND VALUE lvc_s_sort(
        spos       = 1
        fieldname  = 'ID_PONDERACAO' ) TO return.

    APPEND VALUE lvc_s_sort(
        spos       = 2
        fieldname  = 'COD_GRP_PLANTA' ) TO return.

    APPEND VALUE lvc_s_sort(
        spos       = 3
        fieldname  = 'WERKS' ) TO return.

    APPEND VALUE lvc_s_sort(
        spos       = 4
        fieldname  = 'MATERIAL' ) TO return.

    APPEND VALUE lvc_s_sort(
        spos       = 5
        fieldname  = 'PONDERACAO' ) TO return.

  ENDMETHOD.


  METHOD hotspot_click.


    FIELD-SYMBOLS <ft> TYPE ANY TABLE.
    DATA lr_data TYPE REF TO data.

    ASSIGN mt_data->* TO <ft>.

    LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs>).
      CHECK sy-tabix = es_row_no-row_id.
      lr_data = REF #( <fs> ).
      EXIT.
    ENDLOOP.

    DATA(ls_fcat) = mt_catalog[ fieldname = e_column_id ].
    DATA(lv_periodo) = ls_fcat-parameter1.

    ASSIGN COMPONENT 'ID_ITEM_LISTA_CUSTO'  OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_item>).

    RAISE EVENT on_hotspot_click
      EXPORTING
        iv_source    = 'LISTA_CUSTO_DETALHE'
        is_row_id    = e_row_id
        is_column_id = e_column_id
        is_row_no    = es_row_no
        ir_data      = lr_data
        iv_additional_data_1 = CONV #( lv_periodo )
        iv_additional_data_2 = CONV #( <fv_item> ).

  ENDMETHOD.


  METHOD menu_button.

   IF e_ucomm = '&VALOR'.
*      LOOP AT mt_fields INTO DATA(ls_field).
      DO 3 TIMES.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = `teste ` && sy-index
            text  = `teste ` && sy-index.
      ENDDO.
*      ENDLOOP.
    ENDIF.



  ENDMETHOD.


  METHOD merge_data.

    DATA lo_sdescr_main     TYPE REF TO cl_abap_structdescr.
    DATA lo_sdescr_append     TYPE REF TO cl_abap_structdescr.
    DATA lr_line       TYPE REF TO data.
    DATA lv_name TYPE string.
    DATA lv_name_prefixo TYPE string.
    DATA lv_name_sufixo TYPE string.


    DATA: lv_valor TYPE lvc_fname,
          lv_moeda TYPE lvc_fname.

    FIELD-SYMBOLS: <ft>            TYPE ANY TABLE,
                   <fs_line>       TYPE any,
                   <fv_ponderacao> TYPE any,
                   <fv_color>      TYPE lvc_t_scol.

    DEFINE set_ponderado.
      ASSIGN COMPONENT 'PONDERACAO' OF STRUCTURE <fs_line> TO <fv_ponderacao>.
       IF <fv_ponderacao> IS ASSIGNED.
         <fv_ponderacao> = 'X'.
       ENDIF.

       ASSIGN COMPONENT 'COLOR' OF STRUCTURE <fs_line> TO <fv_color>.
       IF <fv_color> IS ASSIGNED.
         CASE &1.
           WHEN abap_false.
             <fv_color> = lt_color.
           WHEN abap_true.
             <fv_color> = lt_color_prd.
           WHEN 'E'.
             <fv_color> = lt_color_err.
         ENDCASE.
*         IF &1 = abap_false.
*           <fv_color> = lt_color.
*         ELSE.
*           <fv_color> = lt_color_prd.
*         ENDIF.
       ENDIF.
    END-OF-DEFINITION.
    DEFINE set_color.

      ASSIGN COMPONENT 'COLOR' OF STRUCTURE <fs_line> TO <fv_color>.
      IF <fv_color> IS ASSIGNED.
        CASE &1.
          WHEN 'I'.
          <fv_color> = lt_color_item.
        WHEN 'T'.
          <fv_color> = lt_color_prd.
        when 'P'.
          <fv_color> = lt_color_pnd.
        ENDCASE.
      ENDIF.
    END-OF-DEFINITION.

    ASSIGN mt_data->* TO <ft>.
    ASSIGN ms_data->* TO <fs_line>.

    lv_valor = ms_exibicao-valor. "'VLR_CUSTO_GERENCIAL'

    CASE ms_exibicao-moeda.
      WHEN 'MOEDA_FINAL'.
        lv_moeda = 'VALOR_MOEDA_FINAL'.
      WHEN 'MOEDA_LOCAL'.
        lv_moeda = 'VALOR'.
    ENDCASE.

    "Importado
    DATA lv_tipo TYPE /qaps/s_lista_custo-tipo.
    lv_tipo = 'Importação'.

    DATA(lt_color) = get_color_table( ms_data ).
    REFRESH lt_color.
    DATA(lt_color_item) = get_color_table( ir_data = ms_data iv_producao = 'I' ).
    DATA(lt_color_prd) = get_color_table( ir_data = ms_data iv_producao = 'T' ).
    DATA(lt_color_pnd) = get_color_table( ir_data = ms_data iv_producao = 'P' ).
    DATA(lt_color_err) = get_color_table( ir_data = ms_data iv_producao = 'E' ).

    mv_has_error = abap_false.

    LOOP AT is_data-t_importado INTO DATA(ls_data).

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.

      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        set_ponderado ''.
      ENDIF.

      DATA(ls_id_ponderacao) = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                                  is_lista_custo = ls_data
                                                  iv_producao    = abap_false ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_id>).
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_tipo>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = lv_tipo.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_has_error>).
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      DATA(lt_valores) = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO DATA(ls_valores).
        DATA(ls_fcat) = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_value>).
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO FIELD-SYMBOL(<fv_src>).
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.

    ENDLOOP.

    "Nacional
    LOOP AT is_data-t_nacional INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        set_ponderado ''.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = '' ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Nacional'.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

*    BREAK abap.
    DATA(lt_ponderacao) = is_data-t_ponderacao.
    DELETE lt_ponderacao WHERE items = 1 OR producao = 'X'.

    LOOP AT lt_ponderacao INTO DATA(ls_ponderacao).

      ls_ponderacao-matnr = |{ ls_ponderacao-matnr ALPHA = IN WIDTH = 18 }|.
*      ls_ponderacao-material = |{ ls_ponderacao-material ALPHA = IN WIDTH = 18 }|.

      <fs_line> = CORRESPONDING #( ls_ponderacao ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_ponderacao-id_ponderacao." ls_id_ponderacao-id_ponderacao.
      ENDIF.

*      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
*      IF <fv_tipo> IS ASSIGNED.
*        <fv_tipo> = 'Nacional'.
*      ENDIF.
*
*      IF lines( ls_data-t_erros ) > 0.
*        mv_has_error = abap_true.
*        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
*        IF <fv_tipo> IS ASSIGNED.
*          <fv_has_error> = 'X'.
*        ENDIF.
*      ENDIF.

      lt_valores = ls_ponderacao-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      set_ponderado ''.
      set_color 'P'.

      INSERT <fs_line> INTO TABLE <ft>.

    ENDLOOP.

    "Produção - Conversão
    LOOP AT is_data-t_producao_conversao INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.

      IF ls_data-std_prd_total = 'X'.
*        BREAK c060863.
        CLEAR: ls_data-tipo_origem,ls_data-id_origem,ls_data-cod_origem,ls_data-id_matriz_item,
              ls_data-id_tipo_origem_matriz,ls_data-id_origem_matriz,ls_data-dsc_tipo_ponto,ls_data-codigo,
              ls_data-descricao,ls_data-id_porto,ls_data-cod_porto,ls_data-porto,ls_data-cod_cais,ls_data-cod_trajeto,
              ls_data-desc_trajeto.
*        CLEAR: ls_data-material,ls_data-plant,ls_data-std_prd_pa.
        CLEAR: ls_data-lifnr,ls_data-dsc_lifnr,ls_data-std_prd_cp,ls_data-std_prd_werks,
               ls_data-std_prd_menge,ls_data-std_prd_meins.
      ENDIF.

      <fs_line> = CORRESPONDING #( ls_data ).

*      IF line_exists( lt_ponderacao_full[ cod_grp_planta = ls_data-cod_grp_planta
*                                          werks          = ls_data-werks
*                                          matnr          = ls_data-matnr
*                                          producao       = 'X' ] )
      IF ls_data-ponderacao = 'X'.
        IF lines( ls_data-t_erros ) = 0.
          set_ponderado 'X'.
        ELSE.
          set_ponderado 'E'.
        ENDIF.
        ASSIGN COMPONENT 'CENTRO_ORIGEM' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_centro_origem_std_c>).
        IF <fv_centro_origem_std_c> IS ASSIGNED.
          <fv_centro_origem_std_c> = ls_data-plant.
        ENDIF.
      ELSE.
        set_color 'I'.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = abap_true ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Prod. - Conversão'.
      ENDIF.

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_matnr>).
      IF <fv_matnr> IS ASSIGNED.
        <fv_matnr> = ls_data-std_prd_pa.
      ENDIF.

      ASSIGN COMPONENT 'COMPONENTE' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_comp>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_comp> = ls_data-std_prd_cp.
      ENDIF.

      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_menge>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_menge> = ls_data-std_prd_menge.
      ENDIF.

      ASSIGN COMPONENT 'MEINS' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_meins>).
      IF <fv_tipo> IS ASSIGNED.
        <fv_meins> = ls_data-std_prd_meins.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Produção - Produção
*    BREAK c060863.
    LOOP AT is_data-t_producao_producao INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.

      IF ls_data-std_prd_total = 'X'.
*        BREAK c060863.
        CLEAR: ls_data-tipo_origem,ls_data-id_origem,ls_data-cod_origem,ls_data-id_matriz_item,
              ls_data-id_tipo_origem_matriz,ls_data-id_origem_matriz,ls_data-dsc_tipo_ponto,ls_data-codigo,
              ls_data-descricao,ls_data-id_porto,ls_data-cod_porto,ls_data-porto,ls_data-cod_cais,ls_data-cod_trajeto,
              ls_data-desc_trajeto.
*        CLEAR: ls_data-material,ls_data-plant,ls_data-std_prd_pa.
        CLEAR: ls_data-lifnr,ls_data-dsc_lifnr,ls_data-std_prd_cp,ls_data-std_prd_werks,
               ls_data-std_prd_menge,ls_data-std_prd_meins.
      ENDIF.

      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        IF lines( ls_data-t_erros ) = 0.
          set_ponderado 'X'.
        ELSE.
          set_ponderado 'E'.
        ENDIF..
        ASSIGN COMPONENT 'CENTRO_ORIGEM' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_centro_origem_std>).
        IF <fv_centro_origem_std> IS ASSIGNED.
          <fv_centro_origem_std> = ls_data-plant.
        ENDIF.
      ELSE.
        set_color 'I'.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = abap_true ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Prod. - Produção'.
      ENDIF.

      ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <fs_line> TO <fv_matnr>.
      IF <fv_matnr> IS ASSIGNED.
        <fv_matnr> = |{ ls_data-std_prd_pa ALPHA = IN WIDTH = 18 }|.
      ENDIF.

      ASSIGN COMPONENT 'COMPONENTE' OF STRUCTURE <fs_line> TO <fv_comp>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_comp> = ls_data-std_prd_cp.
      ENDIF.

      ASSIGN COMPONENT 'MENGE' OF STRUCTURE <fs_line> TO <fv_menge>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_menge> = ls_data-std_prd_menge.
      ENDIF.

      ASSIGN COMPONENT 'MEINS' OF STRUCTURE <fs_line> TO <fv_meins>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_meins> = ls_data-std_prd_meins.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.


    "Transf Importação
    LOOP AT is_data-t_transf_importacao INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        set_ponderado ''.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = abap_false ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Importação'.
      ENDIF.

      ASSIGN COMPONENT 'CENTRO_ORIGEM' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fv_centro_origem>).
      IF <fv_centro_origem> IS ASSIGNED.
        <fv_centro_origem> = ls_data-plant.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Transf Nacional
    LOOP AT is_data-t_transf_nacional INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        set_ponderado ''.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = abap_false ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Nacional'.
      ENDIF.

      ASSIGN COMPONENT 'CENTRO_ORIGEM' OF STRUCTURE <fs_line> TO <fv_centro_origem>.
      IF <fv_centro_origem> IS ASSIGNED.
        <fv_centro_origem> = ls_data-plant.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    "Transf Std Produção
    LOOP AT is_data-t_transf_std_producao INTO ls_data.

      ls_data-matnr = |{ ls_data-matnr ALPHA = IN WIDTH = 18 }|.
      ls_data-material = |{ ls_data-material ALPHA = IN WIDTH = 18 }|.
      <fs_line> = CORRESPONDING #( ls_data ).

      IF ls_data-ponderacao = 'X'.
        set_ponderado ''.
      ENDIF.

      ls_id_ponderacao = get_id_ponderacao( it_ponderacao  = is_data-t_ponderacao
                                            is_lista_custo = ls_data
                                            iv_producao    = abap_false ).

      ASSIGN COMPONENT 'ID_PONDERACAO' OF STRUCTURE <fs_line> TO <fv_id>.
      IF <fv_id> IS ASSIGNED.
        <fv_id> = ls_id_ponderacao-id_ponderacao.
      ENDIF.

      ASSIGN COMPONENT 'TIPO' OF STRUCTURE <fs_line> TO <fv_tipo>.
      IF <fv_tipo> IS ASSIGNED.
        <fv_tipo> = 'Transf. - Std Produção'.
      ENDIF.

      IF lines( ls_data-t_erros ) > 0.
        mv_has_error = abap_true.
        ASSIGN COMPONENT 'HAS_ERROR' OF STRUCTURE <fs_line> TO <fv_has_error>.
        IF <fv_tipo> IS ASSIGNED.
          <fv_has_error> = 'X'.
        ENDIF.
      ENDIF.

      lt_valores = ls_data-t_expressao[ fieldname = lv_valor ]-t_valores.

      LOOP AT lt_valores INTO ls_valores.
        ls_fcat = it_fcat[ parameter1 = ls_valores-periodo ].
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <fs_line> TO <fv_value>.
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT lv_moeda OF STRUCTURE ls_valores TO <fv_src>.
        CHECK sy-subrc EQ 0.
        <fv_value> = <fv_src>.
      ENDLOOP.

      INSERT <fs_line> INTO TABLE <ft>.
    ENDLOOP.

    fill_material_data( CHANGING ct_data = <ft> ).
    fill_porto_cais_data( CHANGING ct_data = <ft> ).

  ENDMETHOD.


  METHOD reset.



  ENDMETHOD.


  METHOD SET_DATA.
    FIELD-SYMBOLS <fs> TYPE /qaps/s_retorno_final.
    mr_outtab = ir_outtab.

    ASSIGN mr_outtab->* TO <fs>.


    "get simulação do XML
    ms_periodo = VALUE /qaps/s_periodo_interval( inicial = <fs>-simulacao-periodo_inicial
                                                 final   = <fs>-simulacao-periodo_final ).

    DATA(lt_catalog) = create_dynamic_catalog( ).
    create_dynamic_table( it_fcat = lt_catalog ).
    customize_catalog( CHANGING ct_catalog = lt_catalog ).

    merge_data( is_data = <fs>
                it_fcat = lt_catalog ).

    "Layout
    DATA(ls_layo) = get_layout( ).

    "Sort
    DATA(lt_sort) = get_sort( ).

    "Eventos
    set_events( ).

    "Display ALV
    display_alv( is_layout  = ls_layo
                 it_catalog = lt_catalog
                 it_sort    = lt_sort ).


  ENDMETHOD.


  METHOD set_session_data.

    DATA lr_data TYPE REF TO data.

    ms_exibicao = is_exibicao.
    ms_lista_custo = is_lista_custo.

    REFRESH mt_previous_fcat.

    IF NOT iv_previous_fcat IS INITIAL.
      lr_data = REF #( mt_previous_fcat ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_previous_fcat
                                           CHANGING  cr_data = lr_data ).
    ENDIF.

  ENDMETHOD.


  METHOD toolbar.

    DATA ls_button TYPE stb_button.

*    REFRESH e_object->mt_toolbar.
    DELETE e_object->mt_toolbar WHERE function = '&DETAIL'
                                  OR function = '&&SEP00'
                                  OR function = '&&SEP01'
                                  OR function = '&&SEP02'
                                  OR function = '&MB_SUM'
                                  OR function =  '&MB_SUBTOT'
                                  OR function = '&&SEP05'
                                  OR function = '&PRINT_BACK'
                                  OR function = '&MB_VIEW'
                                  OR function = '&MB_EXPORT'
                                  OR function = '&&SEP06'
                                  OR function = '&GRAPH'
                                  OR function = '&&SEP07'
                                  OR function = '&INFO'.

    CLEAR ls_button.
    ls_button-function  = '&&SEP_QAPS'.
    ls_button-butn_type = cntb_btype_sep.
    APPEND ls_button TO e_object->mt_toolbar.

    " Simulando radio button 1
    CLEAR ls_button.
    ls_button-function  = 'VLR_CUSTO_GERENCIAL'.
    ls_button-text      = 'Custo Gerencial'.
    ls_button-butn_type = cntb_btype_button.
    IF mv_selected_valor = 'VLR_CUSTO_GERENCIAL' OR mv_selected_valor = ''.
      mv_selected_valor = 'VLR_CUSTO_GERENCIAL'.
      ls_button-icon = icon_led_green.
      ls_button-checked   = abap_true.
    ELSE.
      ls_button-icon = icon_led_red.
      ls_button-checked   = abap_false.
    ENDIF.
    APPEND ls_button TO e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = 'VLR_TOTAL_GERENCIAL'.
    ls_button-text      = 'Total Gerencial'.
    ls_button-butn_type = cntb_btype_button.
    IF mv_selected_valor = 'VLR_TOTAL_GERENCIAL'.
      ls_button-icon = icon_led_green.
      ls_button-checked   = abap_true.
    ELSE.
      ls_button-icon = icon_led_red.
      ls_button-checked   = abap_false.
    ENDIF.
    APPEND ls_button TO e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = '&&SEP_QAPS_2'.
    ls_button-butn_type = cntb_btype_sep.
    APPEND ls_button TO e_object->mt_toolbar.

    " Simulando radio button 1
    CLEAR ls_button.
    ls_button-function  = 'MOEDA_LOCAL'.
    ls_button-text      = 'Moeda Local'.
    ls_button-butn_type = cntb_btype_button.
    IF mv_selected_moeda = 'MOEDA_LOCAL'.
      ls_button-icon = icon_led_green.
      ls_button-checked   = abap_true.
    ELSE.
      ls_button-icon = icon_led_red.
      ls_button-checked   = abap_false.
    ENDIF.
    APPEND ls_button TO e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = 'MOEDA_FINAL'.
    ls_button-text      = 'Moeda Final'.
    ls_button-butn_type = cntb_btype_button.
    IF mv_selected_moeda = 'MOEDA_FINAL'  OR mv_selected_moeda = ''.
      mv_selected_moeda = 'MOEDA_FINAL'.
      ls_button-icon = icon_led_green.
      ls_button-checked   = abap_true.
    ELSE.
      ls_button-icon = icon_led_red.
      ls_button-checked   = abap_false.
    ENDIF.
    APPEND ls_button TO e_object->mt_toolbar.

    CHECK mv_has_error = abap_true.

    CLEAR ls_button.
    ls_button-butn_type = cntb_btype_sep.
    APPEND ls_button TO e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = 'LOG'.
    ls_button-text      = 'Log de Erros'.
    ls_button-butn_type = cntb_btype_button.
    ls_button-icon = icon_display_note.
    APPEND ls_button TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD user_command.

    DATA: lt_data TYPE /qaps/t_simulacao,
          lr_data TYPE REF TO data.

    DATA: ls_row TYPE lvc_s_row,
          ls_col TYPE lvc_s_col.

    mo_alv->get_scroll_info_via_id( IMPORTING es_row_info = ls_row
                                              es_col_info = ls_col ).

    mo_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(lt_fcat) ).

    CASE e_ucomm.
      WHEN 'VLR_CUSTO_GERENCIAL' OR 'VLR_TOTAL_GERENCIAL'.
        mv_selected_valor = e_ucomm.
        mo_alv->set_toolbar_interactive( ).
      WHEN 'MOEDA_LOCAL' OR 'MOEDA_FINAL'.
        mv_selected_moeda = e_ucomm.
        mo_alv->set_toolbar_interactive( ).
      when 'LOG'.
*        break c060863.
    ENDCASE.

    DATA(lv_xml_fcat) = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_fcat ) ).

    RAISE EVENT on_user_command
       EXPORTING
         iv_ucomm  = e_ucomm
         iv_source = 'SIMULACAO'
         iv_action = 'C'
         iv_xml_data = VALUE #( )
         iv_addtional_data_1 = CONV #( mv_selected_valor )
         iv_addtional_data_2 = CONV #( mv_selected_moeda )
         iv_addtional_data_3 = CONV #( lv_xml_fcat ).

*    mo_alv->set_frontend_fieldcatalog( lt_fcat ).
    mo_alv->set_scroll_info_via_id( EXPORTING is_row_info = ls_row
                                              is_col_info = ls_col ).

  ENDMETHOD.


  METHOD GET_LINE_PONDERACAO.

    DATA lv_producao TYPE abap_bool.
    data lv_matnr type matnr.

    if iv_producao = ''.
      lv_matnr  = |{ is_lista_custo-matnr ALPHA = in WIDTH = 18 }|.
    else.
      lv_matnr  = |{ is_lista_custo-std_prd_pa ALPHA = in WIDTH = 18 }|.
    endif.

    lv_producao = iv_producao.

    return = VALUE #( it_ponderacao[ cod_grp_planta = is_lista_custo-cod_grp_planta
                                     werks          = is_lista_custo-werks
                                     matnr          = lv_matnr
                                     producao       = lv_producao ] OPTIONAL ).

    IF return IS INITIAL AND lv_producao = abap_false.
      lv_producao = abap_true.
    ELSEIF return IS INITIAL AND lv_producao = abap_true.
      lv_producao = abap_false.
    ELSE.
      RETURN.
    ENDIF.

    return = VALUE #( it_ponderacao[ cod_grp_planta = is_lista_custo-cod_grp_planta
                            werks          = is_lista_custo-werks
                            matnr          = lv_matnr
                            producao       = lv_producao ] OPTIONAL ).

  ENDMETHOD.
ENDCLASS.
