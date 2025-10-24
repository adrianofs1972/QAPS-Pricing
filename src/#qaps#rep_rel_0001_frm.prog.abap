*----------------------------------------------------------------------*
***INCLUDE /QAPS/REP_TIPO_LISTA_FRM.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_0500 .

  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.

  APPEND 'DELETE' TO lt_ucomm.
  APPEND 'EXPORT' TO lt_ucomm.
  APPEND 'IMPORT' TO lt_ucomm.

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING lt_ucomm.

  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_rel_0001( ).
  ENDIF.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GV_LABEL_REF'.
        IF lines( gt_list_periodo_ref ) > 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'GV_LABEL_COMP'.
        IF lines( gt_list_periodo_comp ) > 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'GS_PERIODO_REF-INDEX'.
        IF lines( gt_list_periodo_ref ) > 0.
          screen-required = 1.
          screen-input = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ELSE.
          gs_periodo_ref-index = 0.
          screen-required = 0.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'GS_PERIODO_COMP-INDEX' OR 'GV_LABEL_COMP'.
        IF lines( gt_list_periodo_comp ) > 0.
          screen-required = 1.
          screen-input = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ELSE.
          gs_periodo_comp-index = 0.
          screen-required = 0.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
FORM initialize_1000 .

  SET TITLEBAR '1000'.
  SET PF-STATUS '1000' EXCLUDING 'EXECUTE'.


  IF NOT go_controller IS BOUND.
    go_controller = NEW /qaps/cl_ctrl_rel_0001( ).
  ENDIF.

  go_controller->set_header( is_header = gs_header ).
  go_controller->initialize( io_container = cl_gui_container=>default_screen ).

*  LOOP AT SCREEN.
*
*    IF NOT gs_header-werks IS INITIAL.
*      IF screen-group1 = 'REG'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF screen-group1 = 'WRK'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.


ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  PBO_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pbo_0500 .
  PERFORM initialize_0500.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PAI_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pai_0500 .

  CASE sy-ucomm.
*    WHEN 'NOVO'.
*      PERFORM f_novo.
    WHEN 'EXECUTE'.
      PERFORM submit_data.
  ENDCASE.

ENDFORM.
FORM f_novo.

*  TRY.
*
*      gs_header = go_controller->create_std_producao( ).
*      CALL SCREEN 1000.
*
*    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
*      DATA(ls_message) = lo_excep->get_message( ).
*      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
*  ENDTRY.

ENDFORM.
FORM submit_data.

  TRY.

      IF NOT gs_header IS INITIAL.

        IF NOT gs_header-cod_lista_custo_ref IS INITIAL.
          gs_header-periodo_ref = VALUE #( gt_periodo_ref[ gs_periodo_ref-index ]-periodo OPTIONAL ).
        ENDIF.

        IF NOT gs_header-cod_lista_custo_comp IS INITIAL.
          gs_header-periodo_comp = VALUE #( gt_periodo_comp[ gs_periodo_comp-index ]-periodo OPTIONAL ).
        ENDIF.

        go_controller->validate( gs_header ).
        CALL SCREEN 1000.
      ENDIF.

    CATCH /qaps/cx_pricing_error INTO DATA(lo_excep).
      DATA(ls_message) = lo_excep->get_message( ).
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_werks .

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT werks, name1
  FROM t001w
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'WERKS'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_WERKS'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_matnr.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT /qaps/material~matnr, maktx
  FROM /qaps/material
    INNER JOIN makt
    ON /qaps/material~matnr = makt~matnr
    WHERE makt~spras = @sy-langu
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MATNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_MATNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_cod_regiao.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT codigo,descricao
  FROM /qaps/regiao_prc
    INTO TABLE @DATA(lt_data).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_COD_REGIAO'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_categoria.

  TYPES: BEGIN OF ts_categoria,
           categoria TYPE /qaps/ed_categoria,
           texto     TYPE ddtext,
         END OF ts_categoria.

  DATA lt_data TYPE TABLE OF ts_categoria.
  DATA lt_return_tab TYPE TABLE OF ddshretval.

  DATA(lt_categoria) = /qaps/cl_helper_text=>get_domain_values( iv_domain = '/QAPS/D_CATEGORIA' ).

  lt_data = VALUE #( FOR wa IN lt_categoria
                      ( categoria = wa-domvalue_l texto = wa-ddtext ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'CODIGO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GV_CATEGORIA'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_CODIGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_codigo USING uv_option TYPE c .

  DATA: lt_return_tab TYPE TABLE OF ddshretval,
        lv_field      TYPE help_info-dynprofld.

  SELECT cod_lista_custo,descricao
    FROM /qaps/lista_hdr
    INTO TABLE @DATA(lt_data).

  CASE uv_option.
    WHEN 'R'.
      lv_field = 'COD_LISTA_CUSTO_REF'.
    WHEN 'C'.
      lv_field = 'COD_LISTA_CUSTO_COMP'.
  ENDCASE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'COD_LISTA_CUSTO'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = lv_field
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
FORM f4_periodo USING uv_option TYPE c .

  DATA: lv_spmon TYPE spmon,
        lv_moni  TYPE isellist-month,     "mês (1-12)
        lv_year  TYPE i.     "ano (YYYY)

  "Sugere mês/ano atuais
  lv_year = sy-datum(4).
  lv_moni = sy-datum+4(2).

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month   = lv_moni
*     actual_year    = lv_year
    IMPORTING
      selected_month = lv_moni.
*      selected_year  = lv_year.

  IF sy-subrc = 0 AND lv_moni BETWEEN 1 AND 12 AND lv_year > 0.
    CASE uv_option.
      WHEN 'R'.
        gs_header-periodo_ref = |{ lv_year  }{ lv_moni WIDTH = 2 PAD = '0' }|.
      WHEN 'C'.

        gs_header-periodo_comp = |{ lv_year }{ lv_moni WIDTH = 2 PAD = '0' }|.
    ENDCASE.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_PERIDO_REF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_periodo USING uv_cod_lista_custo TYPE /qaps/ed_cod_lista_custo
                 CHANGING ct_periodo TYPE tt_periodo
                          ct_list    TYPE vrm_values.

  DATA: lv_id(2)      TYPE n,
        lv_mes(2)     TYPE n,
        lv_ano(4)     TYPE n,
        lv_periodo(7) TYPE c,
        lv_pos(2)     TYPE n,
        ls_entry      TYPE /qaps/matriz_dst,
        ls_periodo    TYPE /qaps/s_periodo,
        ls_per        TYPE /qaps/s_periodo_interval.

  REFRESH: ct_periodo,
           ct_list   .

  SELECT SINGLE *
    FROM /qaps/lista_hdr
    WHERE cod_lista_custo = @uv_cod_lista_custo
    INTO @DATA(ls_lista_custo).

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM /qaps/simulacao
    WHERE id_simulacao = @ls_lista_custo-id_simulacao
    INTO @DATA(ls_simulacao).

  DATA(lv_periodo_inicial) = ls_simulacao-periodo_inicial.

  ls_periodo-year = ls_simulacao-periodo_inicial(4).
  ls_periodo-month = ls_simulacao-periodo_inicial+4(2).

  ls_per = VALUE /qaps/s_periodo_interval(
      inicial = ls_simulacao-periodo_inicial
      final   = ls_simulacao-periodo_final    ).

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

    CHECK lv_periodo_inicial <= ls_per-final.

    APPEND VALUE ts_periodo(
        periodo = lv_periodo_inicial
        index   = lv_id ) TO ct_periodo.

    APPEND VALUE vrm_value(
        key  = lv_id
        text = |{ lv_mes }.{ lv_ano }| ) TO ct_list.

  ENDWHILE.

ENDFORM.
