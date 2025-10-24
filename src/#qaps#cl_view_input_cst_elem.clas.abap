class /QAPS/CL_VIEW_INPUT_CST_ELEM definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  events ON_DATA_CHANGED_FINISHED
    exporting
      value(IT_CHANGED_DATA) type /QAPS/T_CHANGED_DATA .

  methods SET_SESSION_DATA
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL
      !IS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR
      !IS_SIMULACAO type /QAPS/S_SIMULACAO .

  methods INITIALIZE
    redefinition .
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
  methods MENU_BUTTON
    redefinition .
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
  methods DATA_CHANGED_FINISHED
    redefinition .
private section.

  data MS_SIMULACAO type /QAPS/S_SIMULACAO .
  data MS_CUSTO_ELEMENTAR type /QAPS/S_CUSTO_ELEMENTAR .
  data MT_CATALOG type LVC_T_FCAT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MT_DATA type ref to DATA .
  data MS_DATA type ref to DATA .

  methods FILL_COMBO_ITEMS
    returning
      value(RETURN) type LVC_T_DRAL .
  methods MERGE_DATA
    importing
      !IT_DATA type /QAPS/T_VAR_INPUT
      !IT_FCAT type LVC_T_FCAT .
  methods CREATE_DYNAMIC_CATALOG
    returning
      value(RETURN) type LVC_T_FCAT .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT .
ENDCLASS.



CLASS /QAPS/CL_VIEW_INPUT_CST_ELEM IMPLEMENTATION.


  METHOD create_dynamic_catalog.

    DATA: lt_fcat       TYPE lvc_t_fcat,
          lt_fcat_comp  TYPE lvc_t_fcat,
          lv_id(2)      TYPE n,
          lv_mes(2)     TYPE n,
          lv_ano(4)     TYPE n,
          lv_periodo(7) TYPE c,
          lv_pos(2)     TYPE n,
          ls_periodo    TYPE /qaps/s_periodo,
          lv_one_month_ok type abap_bool.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_VAR_INPUT'
      CHANGING
        ct_fieldcat      = lt_fcat.

    DATA(ls_periodo_template)    = lt_fcat[ fieldname = 'PERIODO' ].
    DATA(ls_valor_template)      = lt_fcat[ fieldname = 'VALOR' ].
*    DATA(ls_moeda_template)      = lt_fcat[ fieldname = 'MOEDA' ].
    DATA(ls_percentual_template) = lt_fcat[ fieldname = 'PERCENTUAL' ].

    DELETE lt_fcat WHERE fieldname = 'MANDT'
                   OR fieldname = 'PERIODO'
                   OR fieldname = 'VALOR'
                   OR fieldname = 'MOEDA'
                   OR fieldname = 'PERCENTUAL'
*                   OR fieldname = 'ID_VAR_INPUT'
                   OR fieldname = 'ID_SIMULACAO'
                   OR fieldname = 'ID_CUSTO_ELEMENTAR'
                   OR fieldname = 'MATNR'
                   OR fieldname = 'ID_GRUPO_PRODUTO'
                   OR fieldname = 'AGREGADOR'
                   OR fieldname = 'MAT_PLANEJADO'
                   OR fieldname = 'DSC_SIMULACAO'
                   OR fieldname = 'DSC_CUSTO_ELM'
                   OR fieldname = 'MAKTX'
                   OR fieldname = 'MESSAGE_TYPE'
                   OR fieldname = 'MESSAGE'
                   OR fieldname = 'CREATED_BY'
                   OR fieldname = 'CREATED_IN'
                   OR fieldname = 'CREATED_ON'
                   OR fieldname = 'MODIFIED_BY'
                   OR fieldname = 'MODIFIED_IN'
                   OR fieldname = 'MODIFIED_ON'.

    CASE ms_custo_elementar-tipo_variavel.
      WHEN 'G'."Geral
        DELETE lt_fcat WHERE fieldname = 'ID_MODAL'
                   OR fieldname = 'ID_CATEGORIA'
                   OR fieldname = 'ID_PROCESSO'
                   OR FIELDNAME = 'DSC_MODAL'
                   OR FIELDNAME = 'DSC_CATEGORIA'
                   OR FIELDNAME = 'DSC_PROCESSO'
                   OR FIELDNAME = 'COD_TRECHO'.
      WHEN 'C'."Compra
        DELETE lt_fcat WHERE fieldname = 'ID_MODAL'
                   OR fieldname = 'ID_CATEGORIA'
                   OR fieldname = 'ID_PROCESSO'
                   OR FIELDNAME = 'DSC_MODAL'
                   OR FIELDNAME = 'DSC_CATEGORIA'
                   OR FIELDNAME = 'DSC_PROCESSO'
                   OR FIELDNAME = 'COD_TRECHO'.
      WHEN 'L'."Logística
        DELETE lt_fcat WHERE fieldname = 'ID_PROCESSO'
                       OR FIELDNAME = 'DSC_PROCESSO'
                       OR FIELDNAME = 'COD_TRECHO'.
      WHEN 'P'."Produtiva
        DELETE lt_fcat WHERE fieldname = 'ID_MODAL'
                       OR fieldname = 'ID_CATEGORIA'
                       OR fieldname = 'ID_ORIGEM'
                       OR fieldname = 'DSC_ORIGEM'
                       OR fieldname = 'DSC_TIPO_ORIGEM'
                       OR FIELDNAME = 'DSC_MODAL'
                       OR FIELDNAME = 'DSC_CATEGORIA'
                       OR FIELDNAME = 'COD_TRECHO'.
      WHEN 'F'."Frete
        DELETE lt_fcat WHERE fieldname = 'ID_PROCESSO'
                       OR FIELDNAME = 'DSC_PROCESSO'
                       OR FIELDNAME = 'DSC_CATEGORIA'
*                       OR FIELDNAME = 'KEY_INPUT'
*                       OR FIELDNAME = 'DSC_INPUT'
                       OR FIELDNAME = 'DSC_CATEGORIA'.
    ENDCASE.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      <fs_fcat>-col_pos = sy-tabix.
      <fs_fcat>-key = 'X'.
    ENDLOOP.

*    BREAK-POINT.
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

      DATA(ls_per_new)    = ls_periodo_template.
      DATA(ls_val_new)    = ls_valor_template.
*      DATA(ls_moeda_new)  = ls_moeda_template.
      DATA(ls_perc_new)   = ls_percentual_template.

      ls_per_new-reptext = ls_per_new-scrtext_s =
      ls_per_new-scrtext_m = ls_per_new-scrtext_l = lv_periodo.
      ls_per_new-parameter0 = 'PERIODO'.

      ls_val_new-reptext = ls_val_new-scrtext_s
        = ls_val_new-scrtext_m = ls_val_new-scrtext_l = lv_periodo.
      ls_val_new-parameter0 = 'VALOR'.
      ls_val_new-parameter1 = lv_ano && lv_mes.

*      ls_moeda_new-reptext = ls_moeda_new-scrtext_s
*        = ls_moeda_new-scrtext_m = ls_moeda_new-scrtext_l = lv_periodo.
*      ls_moeda_new-parameter0 = 'MOEDA'.

      ls_perc_new-reptext = ls_perc_new-scrtext_s
        = ls_perc_new-scrtext_m = ls_perc_new-scrtext_l = lv_periodo.
      ls_perc_new-parameter0 = 'PERCENTUAL'.
      ls_perc_new-parameter1 = lv_ano && lv_mes.

      ls_per_new-fieldname = ls_per_new-fieldname && `_` && lv_id.
      ls_val_new-fieldname   = ls_val_new-fieldname && `_` && lv_id.
*      ls_moeda_new-fieldname   = ls_moeda_new-fieldname && `_` && lv_id.
      ls_perc_new-fieldname     = ls_perc_new-fieldname && `_` && lv_id.

      CLEAR: ls_per_new-ref_table,
             ls_val_new-ref_table,
*             ls_moeda_new-ref_table,
             ls_perc_new-ref_table.

      ls_per_new-col_pos = lv_pos.
      ls_val_new-col_pos = lv_pos.
*      ls_moeda_new-col_pos = lv_pos.
      ls_perc_new-col_pos = lv_pos.

      APPEND: ls_per_new TO lt_fcat,
              ls_val_new TO lt_fcat,
*              ls_moeda_new TO lt_fcat,
              ls_perc_new TO lt_fcat.

    ENDWHILE.

    IF ms_custo_elementar-tipo_dado = '1'. "Valor Absoluto
      DELETE lt_fcat WHERE parameter0 = 'PERCENTUAL'.
    ELSEIF ms_custo_elementar-tipo_dado = '2'. "Perentual
      DELETE lt_fcat WHERE parameter0 = 'VALOR'." OR parameter0 = 'MOEDA'.
    ENDIF.

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs>).
      IF ( <fs>-parameter0 = 'VALOR' OR <fs>-parameter0 = 'PERCENTUAL' )
        and ms_simulacao-status = 'A'.
        <fs>-edit = 'X'.
      ENDIF.
    ENDLOOP.

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


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = 'DRDN_FIELD'
                      OR fieldname = 'STYLE'
                      OR fieldname = 'COLOR'
                      OR fieldname = 'ORD'
                      OR fieldname = 'MESSAGE'
                      OR fieldname = 'CREATED_BY'
                      OR fieldname = 'CREATED_IN'
                      OR fieldname = 'CREATED_ON'
                      OR fieldname = 'MODIFIED_BY'
                      OR fieldname = 'MODIFIED_IN'
                      OR fieldname = 'MODIFIED_ON'
                      OR fieldname = 'MATNR'
                      OR fieldname = 'AGREGADOR'
                      OR fieldname = 'MAT_PLANEJADO'
                      OR fieldname = 'ID_VAR_INPUT'
                      OR fieldname = 'ID_TP_LISTA'
                      OR fieldname = 'ID_CUSTO_ELEMENTAR'
                      OR fieldname = 'ID_GRUPO_PRODUTO'
                      OR fieldname = 'MOEDA'
                      OR fieldname = 'TIPO_REGRA'
                      OR fieldname = 'TIPO_ORIGEM'
                      OR fieldname = 'TIPO_DESTINO'
                      OR fieldname = 'ID_ORIGEM'
                      OR fieldname = 'ID_DESTINO'
                      OR fieldname = 'ID_PROCESSO'
                      OR fieldname = 'ID_MODAL'
                      OR fieldname = 'ID_CATEGORIA'
                      OR fieldname = 'ID_TRECHO'.

    DELETE ct_catalog WHERE fieldname CS 'PERIODO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'DSC_TIPO_REGRA'.
          <fs>-col_pos = 1.
          <fs>-outputlen = '15'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Tipo de Entrada'.
        WHEN 'ID_ORIGEM'.
          <fs>-outputlen = '10'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Origem'.
        WHEN 'ID_DESTINO'.
          <fs>-outputlen = '10'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Destino'.
        WHEN 'KEY_INPUT'.
          <fs>-col_pos = '2'.
          <fs>-outputlen = '10'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Id'.
        WHEN 'DSC_INPUT'.
          <fs>-col_pos = '3'.
          <fs>-outputlen = '10'.
          <fs>-reptext = <fs>-scrtext_s = <fs>-scrtext_m = <fs>-scrtext_l = 'Descrição'.
        WHEN 'ID_MODAL' OR 'ID_PROCESSO' OR 'ID_CATEGORIA'.
          <fs>-outputlen = '10'.
        WHEN 'DSC_TIPO_ORIGEM' or 'DSC_TIPO_DESTINO'.
*          <fs>-col_pos = 1.
          <fs>-outputlen = '10'.
        WHEN 'DSC_ORIGEM' or 'DSC_DESTINO'.
*          <fs>-col_pos = 1.
          <fs>-outputlen = '10'.
        WHEN 'DSC_CATEGORIA' or 'DSC_MODAL'.
*          <fs>-col_pos = 1.
          <fs>-outputlen = '10'.
        WHEN 'DSC_PROCESSO'.
*          <fs>-col_pos = 1.
          <fs>-outputlen = '14'.
      ENDCASE.

      IF <fs>-fieldname CS 'VALOR'.
        <fs>-outputlen = '10'.
      ELSEIF <fs>-fieldname CS 'MOEDA'.
        <fs>-outputlen = '8'.
      ELSEIF <fs>-fieldname CS 'PERCENTUAL'.
        <fs>-outputlen = '8'.
      ENDIF.

    ENDLOOP.

*    BREAK

  ENDMETHOD.


  METHOD data_changed_finished.

    DATA lv_prefixo TYPE string.
    DATA lv_sufixo TYPE string.
    DATA lv_periodo TYPE string.
    DATA lv_tipo TYPE /qaps/ed_change_type.

    FIELD-SYMBOLS: <ft> TYPE ANY TABLE,
                   <fs> TYPE any.

    DATA lt_changed_data TYPE /qaps/t_changed_data.

    CHECK e_modified = 'X'.

    ASSIGN mt_data->* TO <ft>.

    LOOP AT et_good_cells INTO DATA(ls_good_cells).

      SPLIT ls_good_cells-fieldname AT '_' INTO lv_prefixo lv_sufixo.

      lv_tipo = COND #( WHEN lv_prefixo = 'VALOR' THEN 'V'
                        ELSE 'P' ).

      LOOP AT <ft> ASSIGNING <fs>.
        CHECK sy-tabix = ls_good_cells-row_id.

        lv_periodo = `PERIODO_` && lv_sufixo.
        "Valor
        ASSIGN COMPONENT lv_periodo OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_periodo>).
        ASSIGN COMPONENT 'ID_VAR_INPUT' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_id>).

        CHECK <fv_id> IS ASSIGNED AND <fv_periodo> IS ASSIGNED.

        DATA(ls_changed_data) = VALUE /qaps/s_changed_data(
            id_var_input = <fv_id>
            periodo      = <fv_periodo>
            tipo         = lv_tipo
            valor        = COND #( WHEN lv_tipo = 'V' THEN ls_good_cells-value )
            percentual   = COND #( WHEN lv_tipo = 'P' THEN ls_good_cells-value )
        ).

        APPEND ls_changed_data TO lt_changed_data.

      ENDLOOP.

    ENDLOOP.

    RAISE EVENT on_data_changed_finished
      EXPORTING
        it_changed_data = lt_changed_data.

  ENDMETHOD.


  METHOD display_alv.

    FIELD-SYMBOLS <fs> TYPE ANY TABLE.

    ASSIGN mt_data->* TO <fs>.

    check <fs> is ASSIGNED.

    mo_alv->set_drop_down_table(
      EXPORTING
*        it_drop_down       =     " Dropdown Table
        it_drop_down_alias = fill_combo_items( )    " ALV Control: Dropdown List Boxes
    ).

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
        it_fieldcatalog = it_catalog
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


  METHOD fill_combo_items.

    DATA:
     lt_dd07v TYPE TABLE OF dd07v.

    FIELD-SYMBOLS:
      <ls_dral> TYPE lvc_s_dral,
      <ls_dd07> TYPE dd07v.

* Get values and their short texts for given domain
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name      = '/QAPS/D_TIPO_REGRA'
        langu     = sy-langu
      TABLES
        dd07v_tab = lt_dd07v.

    LOOP AT lt_dd07v ASSIGNING <ls_dd07>.
      APPEND INITIAL LINE TO return ASSIGNING <ls_dral>.
      <ls_dral>-handle    = 1.
      <ls_dral>-int_value = <ls_dd07>-domvalue_l.
      <ls_dral>-value     = <ls_dd07>-ddtext.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_layout.
    return = super->get_layout( ).
*    return-grid_title = 'Valores'.
    return-grid_title = `Valores - [` && ms_custo_elementar-descricao && `]`.
    return-sel_mode = 'A'.
    return-zebra = 'X'.
*    return-stylefname = 'STYLE'.

  ENDMETHOD.


  METHOD initialize.

*    mr_outtab = ir_outtab.
    mv_action = iv_action.

    "Cria instânica
    create_instance( io_container ).

    "Catálogo de Campo
    DATA(lt_catalog) = get_catalog( is_catalog_structure ).

    customize_catalog( CHANGING ct_catalog = lt_catalog ).

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


  METHOD menu_button.


    IF e_ucomm = '&VARIAVEL'.
*      LOOP AT mt_fields INTO DATA(ls_field).
      DO 3 TIMES.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = `teste ` && sy-index"CONV #( ls_field-fieldname )
            text  = `teste ` && sy-index."CONV #( ls_field-reptext ).
      ENDDO.
*      ENDLOOP.
    ENDIF.

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

    DATA(lv_lines) = lines( lt_components ).



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

        DATA(ls_fcat) = it_fcat[ parameter1 = ls_data-periodo ].

        SPLIT ls_fcat-fieldname AT '_' INTO lv_name_prefixo lv_name_sufixo.

        lv_name = ls_fcat-fieldname.
        ASSIGN COMPONENT lv_name_prefixo OF STRUCTURE ls_data TO <fv_src>.
        ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fv_trg>.

        CHECK <fv_src> IS ASSIGNED AND <fv_trg> IS ASSIGNED.

        <fv_trg> = <fv_src>.
        UNASSIGN: <fv_src>, <fv_trg>.

      ENDLOOP.

      "Fill Period
      LOOP AT it_data INTO ls_data
        WHERE id_var_input = ls_var_input-id_var_input.

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


  METHOD reset.

    FIELD-SYMBOLS <fs> TYPE ANY TABLE.

    ASSIGN mt_data->* TO <fs>.

    REFRESH <fs>.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_data.

    FIELD-SYMBOLS <fs> TYPE /qaps/t_var_input.

    "Transpor dados para exibição
    ASSIGN ir_outtab->* TO <fs>.

    DATA(lt_catalog) = create_dynamic_catalog( ).
    create_dynamic_table( it_fcat = lt_catalog ).
    customize_catalog( CHANGING ct_catalog = lt_catalog ).

    merge_data( it_data = <fs>
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
    ms_periodo = is_periodo.
    ms_custo_elementar = is_custo_elementar.
    ms_simulacao = is_simulacao.
  ENDMETHOD.


  METHOD toolbar.

    REFRESH e_object->mt_toolbar.

    check ms_simulacao-status = 'A'.

    APPEND VALUE stb_button(
        function  = '&ADD'
        icon      = icon_insert_row
        quickinfo = 'Criar'
        text      = 'Criar'

    ) TO e_object->mt_toolbar.

*    APPEND VALUE stb_button(
*        function  = '&ADD_COPY'
*        icon      = icon_copy_object
*        quickinfo = 'Criar Simulação com cópia'
*        text      = 'Criar Simulação com cópia'
*
*    ) TO e_object->mt_toolbar.

*    APPEND VALUE stb_button(
*        function  = '&EDIT'
*        icon      = icon_change_text
*        quickinfo = 'Editar Simulação'
*        text      = 'Editar Simulação'
*
*    ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button(
        function  = '&REMOVE'
        icon      = icon_delete_row
        quickinfo = 'Excluir'
        text      = 'Excluir'
    ) TO e_object->mt_toolbar.

*    APPEND VALUE stb_button(
*        function  = '&&SEP00'
*        butn_type = 3
*    ) TO e_object->mt_toolbar.
*
*    APPEND VALUE stb_button(
*        function  = '&ACTIVE'
*        icon      = icon_activate
*        quickinfo = 'Ativar Aprovador'
*        text      = 'Ativar Aprovador'
*    ) TO e_object->mt_toolbar.
*
*    APPEND VALUE stb_button(
*        function  = '&INACTIVE'
*        icon      = icon_deactivate
*        quickinfo = 'Desativar Aprovador'
*        text      = 'Desativar Aprovador'
*    ) TO e_object->mt_toolbar.



  ENDMETHOD.


  METHOD user_command.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    DATA: lv_xml_data TYPE string,
          lv_action type c,
          lt_selected TYPE /qaps/t_var_input_selected.

    CASE e_ucomm.
      WHEN '&REMOVE'.
        mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows)
                                             et_row_no     = DATA(lt_row_no) ).

        ASSIGN mt_data->* TO <ft>.

        LOOP AT <ft> ASSIGNING FIELD-SYMBOL(<fs>).

          CHECK line_exists( lt_row_no[ row_id = sy-tabix ] ).

          ASSIGN COMPONENT 'ID_VAR_INPUT' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fv_id>).

          APPEND VALUE /qaps/s_var_input_selected( id_var_input = <fv_id> ) TO lt_selected.

        ENDLOOP.

        IF lines( lt_selected ) = 0.
          MESSAGE 'Nenhum dado foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_selected ) ).

        lv_action = 'D'.

      WHEN '&ADD'.
        lv_action = 'C'.
    ENDCASE  .

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm  = e_ucomm
        iv_source = 'CUSTO_ELEMENTAR'
        iv_action = lv_action
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
