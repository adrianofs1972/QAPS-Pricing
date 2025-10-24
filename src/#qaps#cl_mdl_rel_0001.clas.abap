class /QAPS/CL_MDL_REL_0001 definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods GET_ITEMS
    importing
      !IS_HEADER type /QAPS/S_REL_0001_HEADER
    returning
      value(RETURN) type /QAPS/T_REL_0001 .
  methods VALIDATE
    importing
      !IS_HEADER type /QAPS/S_REL_0001_HEADER
    returning
      value(RETURN) type ABAP_BOOL
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

  methods CALCULATE
    changing
      !CT_DATA type /QAPS/T_REL_0001 .
  methods FILL_AUXILIAR_TABLES .
  methods GET_LISTA_CUSTO
    importing
      !IV_COD_LISTA_CUSTO type /QAPS/ED_COD_LISTA_CUSTO
      !IV_PERIODO type SPMON
    returning
      value(RETURN) type /QAPS/T_LISTA_CUSTO_TAB .
  methods MERGE_DATA
    importing
      !IT_REF type /QAPS/T_LISTA_CUSTO_TAB
      !IT_COMP type /QAPS/T_LISTA_CUSTO_TAB
    returning
      value(RETURN) type /QAPS/T_REL_0001 .
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



CLASS /QAPS/CL_MDL_REL_0001 IMPLEMENTATION.


  METHOD calculate.
*    BREAK c060863.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-valor_var = <fs>-valor_ref - <fs>-valor_comp.
      <fs>-valor_moeda_final_var = <fs>-valor_moeda_final_ref - <fs>-valor_moeda_final_comp.

      if <fs>-valor_comp > 0.
        <fs>-percentual_var = ( ( <fs>-valor_ref / <fs>-valor_comp ) - 1 ) * 100.
      else.
        <fs>-percentual_var = 100.
      endif.

    ENDLOOP.

  ENDMETHOD.


  METHOD FILL_AUXILIAR_TABLES.

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


  METHOD get_items.

    IF NOT is_header-cod_lista_custo_ref IS INITIAL.
      DATA(lista_ref) = get_lista_custo( iv_cod_lista_custo = is_header-cod_lista_custo_ref
                                         iv_periodo = is_header-periodo_ref ).
    ENDIF.

    IF NOT is_header-cod_lista_custo_comp IS INITIAL.
      DATA(lista_comp) = get_lista_custo( iv_cod_lista_custo = is_header-cod_lista_custo_comp
                                         iv_periodo = is_header-periodo_comp ).
    ENDIF.

    return = merge_data( it_ref  = lista_ref
                         it_comp = lista_comp ).

    IF NOT is_header-cod_lista_custo_comp IS INITIAL.
      calculate( CHANGING ct_data = return ).
    ENDIF.

  ENDMETHOD.


  METHOD get_lista_custo.

    DATA: ls_content TYPE /qaps/s_retorno_final,
          lt_data    TYPE /qaps/t_retorno_calculo,
          lr_data    TYPE REF TO data.

    DATA(ls_lista_custo) = NEW /qaps/cl_mdl_lista_custo( )->get_single_lista_custo( iv_cod_lista_custo ).
    DATA(lt_grupo_produto) =  NEW /qaps/cl_mdl_material( )->get_grupo_produto(  ).
    DATA(lt_custo_elm) = NEW /qaps/cl_mdl_custo_elementar( )->get_variavel( ).
    DATA(lt_material) = NEW /qaps/cl_mdl_material( )->get_materiais_all( ir_matnr = VALUE #( ) ).


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

    LOOP AT lt_data INTO DATA(ls_data).

        LOOP AT ls_data-t_expressao INTO DATA(ls_expressao).

          CHECK ls_expressao-tipo_node = 'E' OR ls_expressao-tipo_node = 'C'.

          CASE ls_expressao-tipo_node.
            WHEN 'E'.
              CHECK NOT line_exists( return[ material              = ls_data-material
                                             werks                 = ls_data-werks
                                             cod_grp_planta        = ls_data-cod_grp_planta
                                             id_custo_elementar = ls_expressao-id_custo_elementar ] ).
            WHEN 'C'.
              CHECK NOT line_exists( return[ material           = ls_data-material
                                             werks              = ls_data-werks
                                             cod_grp_planta     = ls_data-cod_grp_planta
                                             id_custo_elementar = ls_expressao-id_calc_node ] ).
          ENDCASE.

          DATA(ls_valores) = ls_expressao-t_valores[ periodo = iv_periodo ].
          DATA(ls_material) = lt_material[ matnr = ls_data-material ].

*        BREAK c060863.
          APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs_return>).
          <fs_return> = VALUE /qaps/s_lista_custo_tab(
              werks                 = ls_data-werks
              cod_grp_planta        = ls_data-cod_grp_planta
              material              = ls_data-material
              agregador             = ls_material-agregador
              id_grupo_produto      = ls_data-id_grupo_produto
              mes                   = ls_lista_custo-created_in+4(2)
              ano                   = ls_lista_custo-created_in(4)
              periodo               = iv_periodo
              tipo_node             = ls_expressao-tipo_node
              cod_lista_custo       = ls_lista_custo-cod_lista_custo
              descricao             = ls_lista_custo-descricao
              status                = ls_lista_custo-status
              taxa                  = ls_content-t_taxa_cambio[ periodo = iv_periodo ]-taxa
              valor                 = ls_valores-valor
              valor_moeda_final     = ls_valores-valor_moeda_final ).

          <fs_return>-dsc_grupo_produto
              = VALUE #( lt_grupo_produto[ id_grupo_produto = ls_material-id_grupo_produto ]-descricao OPTIONAL ).

          CASE ls_expressao-tipo_node.
            WHEN 'E'.
              <fs_return>-id_custo_elementar    = ls_expressao-id_custo_elementar.
              <fs_return>-dsc_custo_elementar   = VALUE #( lt_custo_elm[ id_custo_elementar = ls_expressao-id_custo_elementar ]-descricao OPTIONAL ).
            WHEN 'C'.
              <fs_return>-id_custo_elementar    = ls_expressao-id_calc_node.
              <fs_return>-dsc_custo_elementar   = ls_expressao-descricao.
*
          ENDCASE.

        ENDLOOP.

      ENDLOOP.

    ENDMETHOD.


  METHOD merge_data.

    LOOP AT it_ref INTO DATA(ls_data).

      APPEND INITIAL LINE TO return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-werks                =  ls_data-werks.
      <fs>-cod_grp_planta       =  ls_data-cod_grp_planta     .
      <fs>-material             =  ls_data-material           .
      <fs>-agregador            =  ls_data-agregador  .
      <fs>-mes                  =  ls_data-mes.
      <fs>-ano                  =  ls_data-ano.
      <fs>-tipo_node            =  ls_data-tipo_node   .
      <fs>-id_grupo_produto     =  ls_data-id_grupo_produto   .
      <fs>-dsc_grupo_produto    =  ls_data-dsc_grupo_produto  .
      <fs>-id_custo_elementar   =  ls_data-id_custo_elementar .
      <fs>-dsc_custo_elementar  =  ls_data-dsc_custo_elementar.
      <fs>-periodo_ref          =  ls_data-periodo            .

    ENDLOOP.

    LOOP AT it_comp INTO ls_data.

      CHECK NOT line_exists( return[ werks                =  ls_data-werks
                                     cod_grp_planta       =  ls_data-cod_grp_planta
                                     id_custo_elementar   =  ls_data-id_custo_elementar ] ).

      APPEND INITIAL LINE TO return ASSIGNING <fs>.
      <fs>-werks                =  ls_data-werks.
      <fs>-cod_grp_planta       =  ls_data-cod_grp_planta     .
      <fs>-material             =  ls_data-material           .
      <fs>-agregador            =  ls_data-agregador  .
      <fs>-tipo_node            =  ls_data-tipo_node   .
      <fs>-id_grupo_produto     =  ls_data-id_grupo_produto   .
      <fs>-dsc_grupo_produto    =  ls_data-dsc_grupo_produto  .
      <fs>-id_custo_elementar   =  ls_data-id_custo_elementar .
      <fs>-dsc_custo_elementar  =  ls_data-dsc_custo_elementar.
      <fs>-periodo_comp         =  ls_data-periodo            .

    ENDLOOP.

    SELECT matnr,maktx
      FROM makt
      FOR ALL ENTRIES IN @return
      WHERE matnr = @return-material
      AND spras = @sy-langu
      INTO TABLE @DATA(lt_makt).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs_return>).

      DATA(ls_ref) = VALUE #( it_ref[ werks                =  <fs_return>-werks
                                    cod_grp_planta       =  <fs_return>-cod_grp_planta
                                    id_custo_elementar   =  <fs_return>-id_custo_elementar ] OPTIONAL ).

      DATA(ls_comp) = VALUE #( it_comp[ werks                =  <fs_return>-werks
                                        cod_grp_planta       =  <fs_return>-cod_grp_planta
                                        id_custo_elementar   =  <fs_return>-id_custo_elementar ] OPTIONAL ).

      <fs_return>-maktx                  = VALUE #( lt_makt[ matnr = <fs_return>-material ]-maktx OPTIONAL ).
      <fs_return>-cod_lista_custo_ref    = ls_ref-cod_lista_custo.
      <fs_return>-descricao_ref          = ls_ref-descricao.
      <fs_return>-status_ref             = ls_ref-status.
      <fs_return>-taxa_ref               = ls_ref-taxa.
      <fs_return>-valor_ref              = ls_ref-valor.
      <fs_return>-valor_moeda_final_ref  = ls_ref-valor_moeda_final.

      <fs_return>-cod_lista_custo_comp    = ls_comp-cod_lista_custo.
      <fs_return>-descricao_comp          = ls_comp-descricao.
      <fs_return>-status_comp             = ls_comp-status.
      <fs_return>-taxa_comp               = ls_comp-taxa.
      <fs_return>-valor_comp              = ls_comp-valor.
      <fs_return>-valor_moeda_final_comp  = ls_comp-valor_moeda_final.

      <fs_return>-periodo_ref             = ls_ref-periodo.
      <fs_return>-periodo_comp            = ls_comp-periodo.


    ENDLOOP.

  ENDMETHOD.


  METHOD QUESTION.

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


  METHOD UPDATE_HEADER.

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


  METHOD validate.

    DEFINE trigger_exception.
      RAISE EXCEPTION TYPE /qaps/cx_pricing_error
        EXPORTING
          message  = VALUE #( type = 'E' message = &1 ).
    END-OF-DEFINITION.

    IF is_header-cod_lista_custo_ref IS INITIAL.
      trigger_exception 'Lista Referência é um campo obrigatório'.
    ENDIF.

    IF is_header-periodo_ref IS INITIAL.
      trigger_exception 'Período Referência é um campo obrigatório'.
    ENDIF.

    IF NOT is_header-cod_lista_custo_comp IS INITIAL AND is_header-periodo_comp IS INITIAL.
      trigger_exception 'Período Comparativo é um campo obrigatório'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
