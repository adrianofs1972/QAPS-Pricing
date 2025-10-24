class /QAPS/CL_VIEW_LOGISTICA definition
  public
  inheriting from /QAPS/CL_VIEW_ALV_BASE
  final
  create public .

public section.

  methods REFRESH .

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
  methods HOTSPOT_CLICK
    redefinition .
  methods TOOLBAR
    redefinition .
  methods USER_COMMAND
    redefinition .
private section.

  data MS_SERIALIZABLE type STRING .
  data MT_DATA type /QAPS/T_REGIAO .

  methods CUSTOMIZE_CAT_CENTRO_PORTO
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_TRAJETO
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_TRECHO
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_CAIS
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_FORNECEDOR
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_CLIENTE
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_PORTO
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_PONTO
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_CIDADE
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_CENTROS
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_GRP_CLIENTE
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_GRP_PLANTA
    changing
      !CT_CATALOG type LVC_T_FCAT .
  methods CUSTOMIZE_CAT_REGIAO
    changing
      !CT_CATALOG type LVC_T_FCAT .
ENDCLASS.



CLASS /QAPS/CL_VIEW_LOGISTICA IMPLEMENTATION.


  METHOD customize_catalog.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                       OR fieldname = 'STYLE'
                       OR fieldname = 'COLOR'
                       OR fieldname = 'MESSAGE'
                       OR fieldname = 'CREATED_BY'
                       OR fieldname = 'CREATED_IN'
                       OR fieldname = 'CREATED_ON'
                       OR fieldname = 'MODIFIED_BY'
                       OR fieldname = 'MODIFIED_IN'
                       OR fieldname = 'MODIFIED_ON'.

    CASE mv_source.
      WHEN 'REGIAO'. customize_cat_regiao( CHANGING ct_catalog = ct_catalog ).
      WHEN 'CIDADE'. customize_cat_cidade( CHANGING ct_catalog = ct_catalog ).
      WHEN 'GRP_PLANTA'. customize_cat_grp_planta( CHANGING ct_catalog = ct_catalog ).
      WHEN 'CENTRO'. customize_cat_centros( CHANGING ct_catalog = ct_catalog ).
      WHEN 'PORTO'. customize_cat_porto( CHANGING ct_catalog = ct_catalog ).
      WHEN 'CAIS'. customize_cat_cais( CHANGING ct_catalog = ct_catalog ).
      WHEN 'GRP_CLIENTE'.  customize_cat_grp_cliente( CHANGING ct_catalog = ct_catalog ).
      WHEN 'CLIENTE'.  customize_cat_cliente( CHANGING ct_catalog = ct_catalog ).
      WHEN 'FORNECEDOR'.  customize_cat_fornecedor( CHANGING ct_catalog = ct_catalog ).
      WHEN 'CENTRO_PORTO'.  customize_cat_centro_porto( CHANGING ct_catalog = ct_catalog ).
      WHEN 'TRECHO'.  customize_cat_trecho( CHANGING ct_catalog = ct_catalog ).
      WHEN 'TRAJETO'.  customize_cat_trajeto( CHANGING ct_catalog = ct_catalog ).
    ENDCASE.

  ENDMETHOD.


  METHOD customize_cat_cais.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_CIDADE'
                      OR fieldname = 'ID_PORTO'
                      OR fieldname = 'ID_CAIS'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'COD_CAIS'.
          <fs>-col_pos = 1.
          <fs>-coltext = 'Cód Terminal'.
          <fs>-outputlen = 15.
        WHEN 'CAIS'.
          <fs>-col_pos = 2.
          <fs>-coltext = 'Terminal'.
          <fs>-outputlen = 25.
        WHEN 'COD_PORTO'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 15.
        WHEN 'DSC_PORTO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 25.
        WHEN 'DSC_UF'.
          <fs>-col_pos = 5.
          <fs>-outputlen = 5.
        WHEN 'DSC_CIDADE'.
          <fs>-col_pos = 6.
          <fs>-outputlen = 25.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_centros.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
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
                    OR fieldname = 'ID_GRP_PLANTA'
                    OR fieldname = 'ID_CIDADE'
                    OR fieldname = 'ID_CENTRO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'WERKS'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 8.
          <fs>-just = 'C'.
        WHEN 'DSC_WERKS'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 30.
          <fs>-coltext = 'Descrição Centro'.
        WHEN 'COD_GRP_PLANTA'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 12.
          <fs>-coltext = 'Cód Grp Planta'.
          <fs>-just = 'C'.
        WHEN 'DSC_GRP_PLANTA'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 30.
        WHEN 'DSC_UF'.
          <fs>-col_pos = 5.
          <fs>-outputlen = 5.
        WHEN 'DSC_CIDADE'.
          <fs>-col_pos = 6.
          <fs>-outputlen = 30.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_CENTRO_PORTO.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_PORTO'
                      OR fieldname = 'ID_CAIS'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'WERKS'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 8.
        WHEN 'DSC_WERKS'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 25.
        WHEN 'COD_PORTO'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 8.
        WHEN 'DSC_PORTO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 25.
        WHEN 'ATIVO'.
          <fs>-outputlen = 8.
          <fs>-checkbox = 'X'.
          <fs>-col_pos = 5.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_cidade.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                    OR fieldname = 'STYLE'
                    OR fieldname = 'COLOR'
                    OR fieldname = 'MESSAGE'
                    OR fieldname = 'CREATED_BY'
                    OR fieldname = 'CREATED_IN'
                    OR fieldname = 'CREATED_ON'
                    OR fieldname = 'MODIFIED_BY'
                    OR fieldname = 'MODIFIED_IN'
                    OR fieldname = 'MODIFIED_ON'
                    OR fieldname = 'ID_CIDADE'
                    OR fieldname = 'ID_REGIAO'
                    .

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'CIDADE'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 20.
        WHEN 'UF'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 5.
          <fs>-just = 'C'.
        WHEN 'COD_REGIAO'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 12.
          <fs>-just = 'C'.
        WHEN 'DSC_REGIAO'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 20.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_cliente.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_CIDADE'
                      OR fieldname = 'ID_CLIENTE'
                      OR fieldname = 'ID_GRP_CLIENTE'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'KUNNR'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 15.
        WHEN 'NAME1'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 40.
        WHEN 'DSC_UF'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 5.
        WHEN 'DSC_CIDADE'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 35.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_FORNECEDOR.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_CIDADE'
                      OR fieldname = 'ID_FORNECEDOR'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'LIFNR'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 15.
        WHEN 'NAME1'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 40.
        WHEN 'DSC_UF'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 5.
        WHEN 'DSC_CIDADE'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 40.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_GRP_CLIENTE.

     DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
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
                     OR fieldname = 'ID_GRP_CLIENTE'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'CODIGO'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 15.
          <fs>-just = 'C'.
        WHEN OTHERS.
          <fs>-col_pos = 2.
          <fs>-outputlen = 35.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_GRP_PLANTA.

     DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
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
                     OR fieldname = 'ID_GRP_PLANTA'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'CODIGO'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 15.
          <fs>-just = 'C'.
        WHEN OTHERS.
          <fs>-col_pos = 2.
          <fs>-outputlen = 35.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_ponto.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_PONTO'
                      OR fieldname = 'ID_CIDADE'
                      OR fieldname = 'ID_CAIS'
                      OR fieldname = 'TIPO_PONTO'
                      OR fieldname = 'WERKS'
                      OR fieldname = 'LIFNR'
                      OR fieldname = 'KUNNR'
                      OR fieldname = 'COD_CAIS'
                      OR fieldname = 'DSC_CAIS'
                      OR fieldname = 'DSC_WERKS'
                      OR fieldname = 'DSC_LIFNR'
                      OR fieldname = 'DSC_KUNNR'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'COD_PONTO'.
          <fs>-outputlen = 15.
        WHEN 'DSC_PONTO'.
          <fs>-outputlen = 40.
        WHEN 'DSC_CIDADE'.
          <fs>-outputlen = 30.
        WHEN 'DSC_TIPO_PONTO'.
          <fs>-outputlen = 15.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_porto.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_CIDADE'
                      OR fieldname = 'ID_PORTO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'COD_PORTO'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 15.
        WHEN 'PORTO'.
          <fs>-col_pos = 2.
          <fs>-outputlen = 40.
        WHEN 'DSC_UF'.
          <fs>-col_pos = 3.
          <fs>-outputlen = 5.
        WHEN 'DSC_CIDADE'.
          <fs>-col_pos = 4.
          <fs>-outputlen = 40.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_REGIAO.

     DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
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
                     OR fieldname = 'ID_CATEGORIA'
                     OR fieldname = 'ID_REGIAO'.

    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'CODIGO'.
          <fs>-col_pos = 1.
          <fs>-outputlen = 12.
          <fs>-just = 'C'.
        WHEN OTHERS.
          <fs>-col_pos = 2.
          <fs>-outputlen = '25'.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD CUSTOMIZE_CAT_TRAJETO.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_TRECHO'
                      OR fieldname = 'ID_ORIGEM'
                      OR fieldname = 'ID_DESTINO'
                      OR fieldname = 'ID_MODAL'
                      OR fieldname = 'TIPO_ORIGEM'
                      OR fieldname = 'TIPO_DESTINO'
                      OR fieldname = 'ID_TRAJETO'.
*    BREAK-POINT.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'Código'.
          <fs>-col_pos = 1.
          <fs>-coltext = 'Código'.
          <fs>-outputlen = 5.
          <fs>-hotspot = 'X'.
        WHEN 'DESCRICAO'.
          <fs>-col_pos = 2.
          <fs>-coltext = 'Trajeto'.
          <fs>-outputlen = 20.
          <fs>-hotspot = 'X'.
        WHEN 'DSC_TIPO_ORIGEM'.
          <fs>-col_pos = 3.
          <fs>-coltext = 'Tipo Origem'.
          <fs>-outputlen = 10.
        WHEN 'DSC_ORIGEM'.
          <fs>-col_pos = 5.
          <fs>-coltext = 'Cod Origem'.
          <fs>-outputlen = 10.
        WHEN 'ORIGEM'.
          <fs>-col_pos = 6.
          <fs>-coltext = 'Origem'.
          <fs>-outputlen = 25.
        WHEN 'DSC_TIPO_DESTINO'.
          <fs>-col_pos = 7.
          <fs>-coltext = 'Tipo Destino'.
          <fs>-outputlen = 10.
        WHEN 'DSC_DESTINO'.
          <fs>-col_pos = 8.
          <fs>-coltext = 'Cod Destino'.
          <fs>-outputlen = 10.
        WHEN 'DESTINO'.
          <fs>-col_pos = 9.
          <fs>-coltext = 'Destino'.
          <fs>-outputlen = 25.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD customize_cat_trecho.

    DELETE ct_catalog WHERE fieldname = 'MESSAGE_TYPE'
                      OR fieldname = '/QAPS/YMENG'
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
                      OR fieldname = 'ID_TRECHO'
                      OR fieldname = 'ID_ORIGEM'
                      OR fieldname = 'ID_DESTINO'
                      OR fieldname = 'ID_MODAL'
                      OR fieldname = 'TIPO_ORIGEM'
                      OR fieldname = 'TIPO_DESTINO'
                      OR fieldname = 'ID_MODAL'.
*    BREAK-POINT.
    LOOP AT ct_catalog ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'COD_TRECHO'.
          <fs>-col_pos = 1.
        WHEN 'DSC_TIPO_ORIGEM'.
          <fs>-col_pos = 2.
          <fs>-coltext = 'Tipo Origem'.
          <fs>-outputlen = 10.
        WHEN 'DSC_ORIGEM'.
          <fs>-col_pos = 3.
          <fs>-coltext = 'Cod Origem'.
          <fs>-outputlen = 10.
        WHEN 'ORIGEM'.
          <fs>-col_pos = 4.
          <fs>-coltext = 'Origem'.
          <fs>-outputlen = 25.
        WHEN 'DSC_TIPO_DESTINO'.
          <fs>-col_pos = 5.
          <fs>-coltext = 'Tipo Destino'.
          <fs>-outputlen = 10.
        WHEN 'DSC_DESTINO'.
          <fs>-col_pos = 6.
          <fs>-coltext = 'Cod Destino'.
          <fs>-outputlen = 10.
        WHEN 'DESTINO'.
          <fs>-col_pos = 7.
          <fs>-coltext = 'Destino'.
          <fs>-outputlen = 25.
        WHEN 'DSC_MODAL'.
          <fs>-col_pos = 8.
          <fs>-coltext = 'Modal'.
          <fs>-outputlen = 10.
        WHEN 'TEMPO_DESLOCAMENTO'.
          <fs>-col_pos = 9.
          <fs>-coltext = 'Temp Desloc'.
          <fs>-outputlen = 10.
        WHEN 'DISTANCIA'.
          <fs>-col_pos = 10.
          <fs>-coltext = 'Distância (km)'.
          <fs>-outputlen = 11.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.

    FIELD-SYMBOLS <fs> TYPE ANY TABLE.
    DATA: lo_type TYPE REF TO cl_abap_tabledescr,
          lr_data      TYPE REF TO data.

*    BREAK-POINT.
    DATA(lv_type) = get_content_type( ).
    lo_type ?= cl_abap_tabledescr=>describe_by_name( lv_type ).
    CREATE DATA lr_data TYPE HANDLE lo_type.

    get_content( CHANGING cr_data = lr_data ).

    ASSIGN lr_data->* TO <fs>.

    ms_serializable = /qaps/cl_serialization=>serialize( ir_data = mr_outtab ).

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
        it_outtab       = <fs>"mt_data
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


  METHOD get_layout.
    return = super->get_layout( ).

    CASE mv_source.
      WHEN 'REGIAO'.
        return-grid_title = 'Regiões'.
      WHEN 'CIDADE'.
        return-grid_title = 'Cidades'.
      WHEN 'GRP_PLANTA'.
        return-grid_title = 'Grupo de Plantas'.
      WHEN 'CENTRO'.
        return-grid_title = 'Centros'.
      WHEN 'PORTO'.
        return-grid_title = 'Portos'.
      WHEN 'CAIS'.
        return-grid_title = 'Terminal Portuário'.
*      WHEN 'PONTO'.
*        return-grid_title = 'Pontos'.
      WHEN 'CLIENTE'.
        return-grid_title = 'Clientes'.
      WHEN 'FORNECEDOR'.
        return-grid_title = 'Fornecedores'.
      WHEN 'TRECHO'.
        return-grid_title = 'Trechos'.
      WHEN 'TRAJETO'.
        return-grid_title = 'Trajetos'.
      WHEN 'CENTRO_PORTO'.
        return-grid_title = 'Distribuição Portuária'.
    ENDCASE.

    return-sel_mode = 'A'.
  ENDMETHOD.


  METHOD hotspot_click.

    DATA: lt_trajeto TYPE /qaps/t_trajeto,
          lr_data    TYPE REF TO data.

    CASE mv_source.
      WHEN 'TRAJETO'.
        lr_data = REF #( lt_trajeto ).
        get_content( CHANGING cr_data = lr_data ).
        DATA(ls_data) = lt_trajeto[ e_row_id-index ].
    ENDCASE.

    DATA(lv_xml) = /qaps/cl_serialization=>serialize( ir_data = REF #( ls_data ) ).

    RAISE EVENT on_hotspot_click
      EXPORTING
        iv_source    = mv_source
        is_row_id    =  e_row_id
        is_column_id =  e_column_id
        is_row_no    = es_row_no
        ir_data      = REF #( ls_data )
        iv_xml_data  = lv_xml
    .

  ENDMETHOD.


  method INITIALIZE.

    mr_outtab = ir_outtab.
    mv_action = iv_action.

    "Cria instânica
    create_instance( io_container ).

*    "Catálogo de Campo
*    DATA(lt_catalog) = get_catalog( is_catalog_structure ).
*    customize_catalog( CHANGING ct_catalog = lt_catalog ).
*
*    "Layout
*    DATA(ls_layo) = get_layout( ).
*
*    "Sort
*    DATA(lt_sort) = get_sort( ).
*
*    "Eventos
*    set_events( ).
*
*    "Display ALV
*    display_alv( is_layout  = ls_layo
*                 it_catalog = lt_catalog
*                 it_sort    = lt_sort ).

  endmethod.


  METHOD refresh.

    mo_alv->refresh_table_display( ).

  ENDMETHOD.


  METHOD RESET.
    REFRESH mt_data.
    mo_alv->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_data.

    set_content( ir_outtab ).
    mr_outtab = ir_outtab.
*    mv_action = iv_action.
    mv_source = iv_source.

    set_content( ir_outtab ).

*    "Cria instânica
*    create_instance( io_container ).
    DATA(lv_struct) = get_structure_name( ir_outtab ).

*    "Catálogo de Campo
    DATA(lt_catalog) = get_catalog( lv_struct ).
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


  METHOD toolbar.

*    BREAK c060863.
    CASE mv_source.

      WHEN 'CENTRO_PORTO'.
*        DATA(lt_toolbar) = e_object->mt_toolbar.
*        DELETE lt_toolbar WHERE function <> '&&SEP00'
*                          AND function <> '&SORT_ASC'
*                          AND function <> '&SORT_DSC'
*                          AND function <> '&MB_FILTER'.
        REFRESH e_object->mt_toolbar.
      WHEN OTHERS.
        REFRESH e_object->mt_toolbar.
    ENDCASE.



    DEFINE add_button.
      APPEND VALUE stb_button(
            function  = &1
            icon      = &2
            quickinfo = &3
            text      = &3

        ) TO e_object->mt_toolbar.
    END-OF-DEFINITION.

    CASE mv_source.

      WHEN 'CENTRO_PORTO'.
        add_button '&ACTIVE' icon_activate 'Ativar Distribuição'.
        add_button '&INACTIVE' icon_deactivate 'Inativar Distribuição'.

*        APPEND LINES OF lt_toolbar TO e_object->mt_toolbar.
*      WHEN 'CLIENTE' OR 'FORNECEDOR'.
*        add_button '&ADD' icon_create 'Novo'.
*        add_button '&REMOVE' icon_delete 'Excluir'.
      WHEN OTHERS.
        add_button '&ADD' icon_create 'Novo'.
        add_button '&EDIT' icon_change_text 'Editar'.
        add_button '&REMOVE' icon_delete 'Excluir'.
    ENDCASE.



  ENDMETHOD.


  METHOD user_command.

    DATA: lr_data     TYPE REF TO data,
          lv_xml_data TYPE string,
          lr_index    TYPE RANGE OF i.

    DATA: lt_regiao           TYPE /qaps/t_regiao,
          lt_regiao_out       TYPE /qaps/t_regiao,
          lt_cidade           TYPE /qaps/t_cidade,
          lt_cidade_out       TYPE /qaps/t_cidade,
          lt_grp_planta       TYPE /qaps/t_grp_planta,
          lt_grp_planta_out   TYPE /qaps/t_grp_planta,
          lt_centro           TYPE /qaps/t_centro,
          lt_centro_out       TYPE /qaps/t_centro,
          lt_porto            TYPE /qaps/t_porto,
          lt_porto_out        TYPE /qaps/t_porto,
          lt_cais             TYPE /qaps/t_cais,
          lt_cais_out         TYPE /qaps/t_cais,
          lt_grp_cliente      TYPE /qaps/t_grp_cliente,
          lt_grp_cliente_out  TYPE /qaps/t_grp_cliente,
          lt_cliente          TYPE /qaps/t_cliente,
          lt_cliente_out      TYPE /qaps/t_cliente,
          lt_fornec           TYPE /qaps/t_fornecedor,
          lt_fornec_out       TYPE /qaps/t_fornecedor,
          lt_centro_porto     TYPE /qaps/t_centro_porto,
          lt_centro_porto_out TYPE /qaps/t_centro_porto,
          lt_trajeto          TYPE /qaps/t_trajeto,
          lt_trajeto_out      TYPE /qaps/t_trajeto,
          lt_trecho           TYPE /qaps/t_trecho,
          lt_trecho_out       TYPE /qaps/t_trecho.

    FIELD-SYMBOLS <ft> TYPE ANY TABLE.

    IF e_ucomm = '&EDIT' OR e_ucomm = '&REMOVE' OR e_ucomm = '&ACTIVE'
        OR e_ucomm = '&INACTIVE'.

      mo_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_selected) ).

      IF lines( lt_selected ) = 0.
        MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lines( lt_selected ) > 1 AND e_ucomm = '&EDIT'.
        MESSAGE 'Selecione apenas 1 item para edição' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      lr_index = VALUE #( FOR wa IN lt_selected
                          ( sign = 'I' option = 'EQ' low = wa-index ) ).

      CASE mv_source.
        WHEN 'REGIAO'.
          lr_data = REF #( lt_regiao ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_regiao INTO DATA(ls_regiao).
            CHECK sy-tabix IN lr_index.
            APPEND ls_regiao TO lt_regiao_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_regiao_out ) ).
        WHEN 'CIDADE'.
          lr_data = REF #( lt_cidade ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_cidade INTO DATA(ls_cidade).
            CHECK sy-tabix IN lr_index.
            APPEND ls_cidade TO lt_cidade_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_cidade_out ) ).
        WHEN 'GRP_PLANTA'.
          lr_data = REF #( lt_grp_planta ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_grp_planta INTO DATA(ls_grp_planta).
            CHECK sy-tabix IN lr_index.
            APPEND ls_grp_planta TO lt_grp_planta_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_grp_planta_out ) ).
        WHEN 'CENTRO'.
          lr_data = REF #( lt_centro ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_centro INTO DATA(ls_centro).
            CHECK sy-tabix IN lr_index.
            APPEND ls_centro TO lt_centro_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_centro_out ) ).
        WHEN 'PORTO'.
          lr_data = REF #( lt_porto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_porto INTO DATA(ls_porto).
            CHECK sy-tabix IN lr_index.
            APPEND ls_porto TO lt_porto_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_porto_out ) ).
        WHEN 'CAIS'.
          lr_data = REF #( lt_cais ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_cais INTO DATA(ls_cais).
            CHECK sy-tabix IN lr_index.
            APPEND ls_cais TO lt_cais_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_cais_out ) ).
        WHEN 'GRP_CLIENTE'.
          lr_data = REF #( lt_grp_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_grp_cliente INTO DATA(ls_grp_cliente).
            CHECK sy-tabix IN lr_index.
            APPEND ls_grp_cliente TO lt_grp_cliente_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_grp_cliente_out ) ).
        WHEN 'CLIENTE'.
          lr_data = REF #( lt_cliente ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_cliente INTO DATA(ls_cliente).
            CHECK sy-tabix IN lr_index.
            APPEND ls_cliente TO lt_cliente_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_cliente_out ) ).
        WHEN 'FORNECEDOR'.
          lr_data = REF #( lt_fornec ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_fornec INTO DATA(ls_fornec).
            CHECK sy-tabix IN lr_index.
            APPEND ls_fornec TO lt_fornec_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_fornec_out ) ).
        WHEN 'CENTRO_PORTO'.
          lr_data = REF #( lt_centro_porto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_centro_porto INTO DATA(ls_centro_porto).
            CHECK sy-tabix IN lr_index.
            APPEND ls_centro_porto TO lt_centro_porto_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_centro_porto_out ) ).
        WHEN 'TRECHO'.
          lr_data = REF #( lt_trecho ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_trecho INTO DATA(ls_trecho).
            CHECK sy-tabix IN lr_index.
            APPEND ls_trecho TO lt_trecho_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_trecho_out ) ).
        WHEN 'TRAJETO'.
          lr_data = REF #( lt_trajeto ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = ms_serializable
                                               CHANGING  cr_data = lr_data ).
          LOOP AT lt_trajeto INTO DATA(ls_trajeto).
            CHECK sy-tabix IN lr_index.
            APPEND ls_trajeto TO lt_trajeto_out.
          ENDLOOP.
          lv_xml_data = /qaps/cl_serialization=>serialize( ir_data = REF #( lt_trajeto_out ) ).
      ENDCASE.

    ENDIF.

    RAISE EVENT on_user_command
      EXPORTING
        iv_ucomm  = e_ucomm
        iv_source = mv_source
        iv_action = 'C'
        iv_xml_data = lv_xml_data.

  ENDMETHOD.
ENDCLASS.
