*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  FILL_SOURCE_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_ID_TP_LISTA  text
*----------------------------------------------------------------------*
FORM fill_source_target  USING uv_id TYPE /qaps/ed_tp_lista.

*  DATA: lr_area_exclude TYPE RANGE OF /qaps/ed_id_area,
*        lr_area_include TYPE RANGE OF /qaps/ed_id_area.
*
*  REFRESH: gt_source,
*           gt_target.
*
*  SELECT *
*    FROM /qaps/lista_area
*    WHERE id_tp_lista = @uv_id
*    INTO TABLE @DATA(lt_lista).
*
*  IF lines( lt_lista ) > 0.
*    lr_area_exclude = VALUE #( FOR wa IN lt_lista
*                       ( sign  = 'E' option = 'EQ' low = wa-id_area ) ).
*
*    lr_area_include = VALUE #( FOR wa IN lt_lista
*                                ( sign  = 'I' option = 'EQ' low = wa-id_area ) ).
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_exclude
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_include
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_target.
*
*  ELSE.
*    SELECT *
*      FROM /qaps/area
*      INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*  ENDIF.


ENDFORM.
*FORM fill_list_grp_planta.
*
*
*  SELECT id_grp_planta, codigo, descricao
*              FROM /qaps/grp_planta
*              INTO CORRESPONDING FIELDS OF TABLE @gt_grp_planta.
*
*  SORT gt_grp_planta BY codigo ASCENDING.
*
*  APPEND INITIAL LINE TO gt_list_grp_planta ASSIGNING FIELD-SYMBOL(<fs_vrm>).
*  <fs_vrm>-key  = 0.
*  <fs_vrm>-text = '(Selecionar)'.
*
*  LOOP AT gt_grp_planta ASSIGNING FIELD-SYMBOL(<fs_listbox>).
*    APPEND INITIAL LINE TO gt_list_grp_planta ASSIGNING <fs_vrm>.
*    <fs_listbox>-index = sy-tabix.
*    <fs_vrm>-key  = <fs_listbox>-index.
*    <fs_vrm>-text = <fs_listbox>-codigo && ` - ` && <fs_listbox>-descricao.
*  ENDLOOP.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = 'GS_GRP_PLANTA-INDEX'
*      values = gt_list_grp_planta.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LIST_BOXES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes .

  CHECK gv_loaded IS INITIAL.

  REFRESH: gt_grupo_aps.
*
  PERFORM fill_grupo_aps.

  PERFORM fill_periodo_versao.

  gv_loaded = abap_true.

ENDFORM.
FORM fill_alv.

  IF NOT go_container IS BOUND.

    go_container = NEW cl_gui_custom_container( container_name = 'GO_CONTAINER'     ).

    go_alv = NEW cl_gui_alv_grid( i_parent          = go_container    ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_EFET_PERIODO_VERSAO'
      CHANGING
        ct_fieldcat      = gt_fcat.

    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs>).
      CASE sy-tabix.
        WHEN 1.
          <fs>-convexit = 'PERI'.
        WHEN 2.
*          <fs>-convexit = 'PERI'.
          <fs>-edit = 'X'.
          <fs>-just = 'C'.
        WHEN 3.
          <fs>-edit = 'X'.
          <fs>-outputlen = 10.
          <fs>-just = 'C'.
      ENDCASE.
    ENDLOOP.

    go_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = VALUE lvc_s_layo( zebra        = 'X'
                                                          no_toolbar   = 'X' )    " Layout
      CHANGING
        it_outtab                     = gt_periodo_versao    " Output Table
        it_fieldcatalog               = gt_fcat        ).

  ELSE.

  ENDIF.

ENDFORM.
FORM fill_periodo_versao.
  DATA: lv_id(2)      TYPE n,
        lv_mes(2)     TYPE n,
        lv_ano(4)     TYPE n,
        lv_periodo(7) TYPE c,
        lv_pos(2)     TYPE n,
        ls_periodo    TYPE /qaps/s_periodo.

  DATA ls_interval TYPE /qaps/s_periodo_interval.

  DATA(lv_periodo_inicial) = gs_simulacao-periodo_inicial.

  ls_interval = VALUE /qaps/s_periodo_interval(
      inicial = gs_simulacao-periodo_inicial
      final   = gs_simulacao-periodo_final  ).

  ls_periodo-year = ls_interval-inicial(4).
  ls_periodo-month = ls_interval-inicial+4(2).

  WHILE lv_periodo_inicial <= ls_interval-final.

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
*    break c060863.
    lv_periodo = lv_ano && lv_mes.

    check lv_periodo_inicial <= ls_interval-final.

    APPEND VALUE /qaps/s_efet_periodo_versao(
        periodo      = lv_periodo
        /qaps/period = `01.` && lv_mes && `.` && lv_ano  ) TO gt_periodo_versao.

  ENDWHILE.

ENDFORM.
FORM fill_grupo_aps.

  data lv_index type sy-tabix.

  SELECT /qaps/grkey,/qaps/grdes
    FROM /qaps/zmig0
    INTO CORRESPONDING FIELDS OF TABLE @gt_grupo_aps.

  SORT gt_grupo_aps BY /qaps/grkey.

  APPEND INITIAL LINE TO gt_list_grupo_aps ASSIGNING FIELD-SYMBOL(<fs_vrm>).
  <fs_vrm>-key  = 0.
  <fs_vrm>-text = '(Selecionar)'.

  LOOP AT gt_grupo_aps ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    lv_index = lv_index + 1.
    APPEND INITIAL LINE TO gt_list_grupo_aps ASSIGNING <fs_vrm>.
    <fs_listbox>-index = lv_index.
    <fs_vrm>-key  = lv_index.
    <fs_vrm>-text = <fs_listbox>-/qaps/grkey && ` - ` && <fs_listbox>-/qaps/grdes.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_GRUPO_APS-INDEX'
      values = gt_list_grupo_aps.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  IF gs_grupo_aps-index = 0.
    MESSAGE 'Grupo APS é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  go_alv->check_changed_data( ).
  LOOP AT gt_periodo_versao INTO DATA(ls_periodo_versao).

    CHECK ls_periodo_versao-/qaps/period IS INITIAL
      OR ls_periodo_versao-/qaps/versao IS INITIAL.

    MESSAGE 'Todos os períodos e versões são obrigatórios' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDLOOP.

  LEAVE TO SCREEN 0.

ENDFORM.
