class /QAPS/CL_VIEW_ITEM_REL_0002 definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods SET_SESSION_DATA
    importing
      !IS_PERIODO type /QAPS/S_PERIODO_INTERVAL
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
  methods MENU_BUTTON
    redefinition .
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MS_EXIBICAO type /QAPS/S_LISTA_CUSTO_EXIBICAO .
  data MS_LISTA_CUSTO type /QAPS/S_LISTA_HEADER .
  data MT_PREVIOUS_FCAT type LVC_T_FCAT .
  data MS_PERIODO type /QAPS/S_PERIODO_INTERVAL .
  data MV_SELECTED_VALOR type UI_FUNC .
  data MV_SELECTED_MOEDA type UI_FUNC .
  data MT_DATA type /QAPS/T_REL_0002 .
  data MS_DATA type /QAPS/S_REL_0002 .
  data MT_CATALOG type LVC_T_FCAT .

  methods GET_COLOR_TABLE
    importing
      !IR_DATA type ref to DATA
      !IV_PRODUCAO type CHAR1 default ''
    returning
      value(RETURN) type LVC_T_SCOL .
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
ENDCLASS.



CLASS /QAPS/CL_VIEW_ITEM_REL_0002 IMPLEMENTATION.


  METHOD CREATE_DYNAMIC_CATALOG.

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
                    OR datatype = 'RAW'.

*    break c060863.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        when 'COD_LISTA_CUSTO'.
          <fs>-outputlen = 6.
        when 'DESCRICAO'.
          <fs>-outputlen = 20.
        when 'PERIODO'.
          <fs>-outputlen = 7.
        when 'WERKS'.
          <fs>-outputlen = 6.
        when 'COD_GRP_PLANTA'.
          <fs>-outputlen = 6.
        when 'MATERIAL'.
          <fs>-outputlen = 12.
        when 'AGREGADOR'.<fs>-outputlen = 12.
        when 'DSC_GRUPO_PRODUTO'.
          <fs>-outputlen = 12.
        when 'CUSTO_GERENCIAL'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Custo Gerencial'.
        when 'MARKUP_VAL'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Markup'.
        when 'MARKUP_PERC'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Markup %'.
        when 'MERCADO_VAL'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Mercado'.
        when 'MERCADO_PERC'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Mercado %'.
        when 'TOTAL_GERENCIAL'.
          <fs>-outputlen = 11.
          <fs>-coltext = 'Total Gerencial'.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD DISPLAY_ALV.

    DATA lt_fcat TYPE lvc_t_fcat.
    FIELD-SYMBOLS <fs> TYPE ANY TABLE.

*    ASSIGN mt_data->* TO <fs>.
*
*    CHECK <fs> IS ASSIGNED.

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
        it_outtab       = mt_data
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


  METHOD FILL_MATERIAL_DATA.

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

    ENDLOOP.

  ENDMETHOD.


  METHOD FILL_PORTO_CAIS_DATA.

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


  METHOD GET_COLOR_TABLE.

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


  METHOD GET_ID_PONDERACAO.

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


  METHOD GET_LAYOUT.
    return = super->get_layout( ).
    return-sel_mode = 'A'.
*    return-no_toolbar = 'X'.

*    DATA(lv_cod_lista_custo) = |{ ms_lista_custo-cod_lista_custo ALPHA = OUT }|.
*    data(lv_descricao) = |{ ms_lista_custo-descricao ALPHA = OUT }|.
*    CONDENSE: lv_cod_lista_custo NO-GAPS,
*              lv_descricao.
*    DATA(lv_title) = |{ lv_cod_lista_custo } - | &&
*                     | { lv_descricao } | &&
*                     | - Status: { ms_lista_custo-dsc_status } |.
*    return-grid_title = lv_title.

  ENDMETHOD.


  METHOD GET_SORT.

*    APPEND VALUE lvc_s_sort(
*        spos       = 1
*        fieldname  = 'ID_PONDERACAO' ) TO return.
*
*    APPEND VALUE lvc_s_sort(
*        spos       = 2
*        fieldname  = 'COD_GRP_PLANTA' ) TO return.
*
*    APPEND VALUE lvc_s_sort(
*        spos       = 3
*        fieldname  = 'WERKS' ) TO return.
*
*    APPEND VALUE lvc_s_sort(
*        spos       = 4
*        fieldname  = 'MATERIAL' ) TO return.
*
*    APPEND VALUE lvc_s_sort(
*        spos       = 5
*        fieldname  = 'PONDERACAO' ) TO return.

  ENDMETHOD.


  METHOD MENU_BUTTON.

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


  METHOD RESET.
*    REFRESH mt_data.
*    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  method SET_DATA.

    FIELD-SYMBOLS <fs> TYPE /qaps/t_rel_0002.
    mr_outtab = ir_outtab.

    ASSIGN mr_outtab->* TO <fs>.
    mt_data = <fs>.

    mo_alv->refresh_table_display(
      EXPORTING
        is_stable      = VALUE lvc_s_stbl( row = 'X' col = 'X' )
        i_soft_refresh = iv_soft_refresh
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  endmethod.


  METHOD SET_SESSION_DATA.

    DATA lr_data TYPE REF TO data.

    ms_periodo = is_periodo.
    ms_exibicao = is_exibicao.
    ms_lista_custo = is_lista_custo.

    REFRESH mt_previous_fcat.

    IF NOT iv_previous_fcat IS INITIAL.
      lr_data = REF #( mt_previous_fcat ).
      /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_previous_fcat
                                           CHANGING  cr_data = lr_data ).
    ENDIF.

  ENDMETHOD.


  METHOD TOOLBAR.

    DATA ls_button TYPE stb_button.

*    REFRESH e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = '&&SEP_QAPS'.
    ls_button-butn_type = cntb_btype_sep.
    APPEND ls_button TO e_object->mt_toolbar.

    " Simulando radio button 1
*    CLEAR ls_button.
*    ls_button-function  = 'VLR_CUSTO_GERENCIAL'.
*    ls_button-text      = 'Custo Gerencial'.
*    ls_button-butn_type = cntb_btype_button.
*    IF mv_selected_valor = 'VLR_CUSTO_GERENCIAL' OR mv_selected_valor = ''.
*      mv_selected_valor = 'VLR_CUSTO_GERENCIAL'.
*      ls_button-icon = icon_led_green.
*      ls_button-checked   = abap_true.
*    ELSE.
*      ls_button-icon = icon_led_red.
*      ls_button-checked   = abap_false.
*    ENDIF.
*    APPEND ls_button TO e_object->mt_toolbar.
*
*    CLEAR ls_button.
*    ls_button-function  = 'VLR_TOTAL_GERENCIAL'.
*    ls_button-text      = 'Total Gerencial'.
*    ls_button-butn_type = cntb_btype_button.
*    IF mv_selected_valor = 'VLR_TOTAL_GERENCIAL'.
*      ls_button-icon = icon_led_green.
*      ls_button-checked   = abap_true.
*    ELSE.
*      ls_button-icon = icon_led_red.
*      ls_button-checked   = abap_false.
*    ENDIF.
*    APPEND ls_button TO e_object->mt_toolbar.
*
*    CLEAR ls_button.
*    ls_button-function  = '&&SEP_QAPS_2'.
*    ls_button-butn_type = cntb_btype_sep.
*    APPEND ls_button TO e_object->mt_toolbar.

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

  ENDMETHOD.


  METHOD USER_COMMAND.

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
ENDCLASS.
