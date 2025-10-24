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

FORM fill_categ_transp CHANGING cv_ok TYPE abap_bool
                                cv_message TYPE bapi_msg.

  SELECT *
    FROM /qaps/categ_trns
    INTO TABLE @gt_categ.

  IF sy-subrc IS INITIAL.
    cv_ok = abap_true.
  ELSE.
    cv_ok = abap_false.
    cv_message = 'Cadastrar categorias de transporte'.
  ENDIF.

ENDFORM.
FORM fill_target USING uv_id TYPE /qaps/id_grupo_produto.


  SELECT *
    FROM /qaps/material
    WHERE id_grupo_produto = @uv_id
    INTO CORRESPONDING FIELDS OF TABLE @gt_target.

  LOOP AT gt_target ASSIGNING FIELD-SYMBOL(<fs>).
    PERFORM get_maktx USING <fs>-matnr CHANGING <fs>-dsc_matnr.
  ENDLOOP.

ENDFORM.
FORM fill_source.

  DATA: lr_user_exclude TYPE RANGE OF matnr.
  DATA lr_mtart TYPE RANGE OF mtart.
  DATA srch_str TYPE string.

  REFRESH: gt_source.

  SELECT matnr
    FROM /qaps/material
    INTO TABLE @DATA(lt_matnr).

  lr_user_exclude = VALUE #( FOR wa IN gt_target
                              ( sign = 'I' option = 'EQ' low = wa-matnr ) ).

  LOOP AT lt_matnr INTO DATA(ls_matnr).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_matnr-matnr ) TO lr_user_exclude.
  ENDLOOP.

  srch_str = `%` && gv_search && `%`.

  lr_mtart = VALUE #( ( sign = 'I' option = 'EQ' low = 'FERT' )
                      ( sign = 'I' option = 'EQ' low = 'HALB' )
                      ( sign = 'I' option = 'EQ' low = 'ROH' )
                      ( sign = 'I' option = 'EQ' low = 'VERP' ) ).

  SELECT DISTINCT matnr , maktx AS dsc_matnr
    FROM /qaps/v_material
    WHERE ( matnr LIKE @srch_str OR maktx LIKE @srch_str )
    AND spras = @sy-langu
    AND mtart IN @lr_mtart
    INTO CORRESPONDING FIELDS OF TABLE @gt_source.

  IF lines( lr_user_exclude ) > 0.
    DELETE gt_source WHERE matnr IN lr_user_exclude.
  ENDIF.

  SORT gt_source BY dsc_matnr ASCENDING.

  go_alv_source->refresh_table_display( ).

ENDFORM.
FORM get_maktx USING uv_matnr TYPE matnr
                  CHANGING cv_maktx TYPE maktx.

  SELECT SINGLE *
    FROM /qaps/v_material
    WHERE matnr = @uv_matnr
    AND spras   = @sy-langu
    INTO @DATA(ls_data).

  cv_maktx = ls_data-maktx.

ENDFORM.
*}   INSERT

*{   INSERT         ECDK9A0F42                                        2
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_objects .

  DATA lt_fcat TYPE lvc_t_fcat.
  DATA: ls_layo_source TYPE lvc_s_layo,
        ls_layo_target TYPE lvc_s_layo.

  CHECK gv_loaded = abap_false.

  go_cont_source = NEW cl_gui_custom_container( container_name = 'GO_SOURCE'
                                                repid = sy-repid
                                                dynnr = sy-dynnr ).
  go_cont_target = NEW cl_gui_custom_container( container_name = 'GO_TARGET'
                                                repid = sy-repid
                                                dynnr = sy-dynnr ).

  go_alv_source = NEW cl_gui_alv_grid( i_parent = go_cont_source ).
  go_alv_target = NEW cl_gui_alv_grid( i_parent = go_cont_target ).

  PERFORM get_catalog CHANGING lt_fcat.

  ls_layo_source-grid_title = 'Materiais Disponíveis'.
  ls_layo_source-no_toolbar = 'X'.
  ls_layo_source-sel_mode = 'A'.

  go_alv_source->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layo_source    " Layout
    CHANGING
      it_outtab                     = gt_source    " Output Table
      it_fieldcatalog               = lt_fcat     ).

  ls_layo_target-grid_title = 'Materiais Vinculados'.
  ls_layo_target-no_toolbar = 'X'.

  go_alv_target->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layo_target
    CHANGING
      it_outtab                     = gt_target    " Output Table
      it_fieldcatalog               = lt_fcat     ).

  gv_loaded = abap_true.

ENDFORM.
FORM get_catalog CHANGING ct_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = '/QAPS/S_MATERIAL'
    CHANGING
      ct_fieldcat      = ct_fcat.

  DELETE ct_fcat WHERE fieldname <> 'MATNR'
                   AND fieldname <> 'DSC_MATNR'.

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fs>).
    CASE <fs>-fieldname.
      WHEN 'DSC_MATNR'.
        <fs>-outputlen = 30.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM move  USING uv_option TYPE sy-ucomm.

  DATA: lt_source_rows TYPE lvc_t_row,
        lt_target_rows TYPE lvc_t_row.

  CASE uv_option.
    WHEN 'RIGHT'.
      PERFORM execute_transfer USING go_alv_source uv_option.
    WHEN 'RIGHT_ALL'.
      APPEND LINES OF gt_source TO gt_target.
      REFRESH gt_source.
    WHEN 'LEFT'.
      PERFORM execute_transfer USING go_alv_target uv_option.
    WHEN 'LEFT_ALL'.
      APPEND LINES OF gt_target TO gt_source.
      REFRESH gt_target.
  ENDCASE.

  go_alv_source->refresh_table_display( ).
  go_alv_target->refresh_table_display( ).

ENDFORM.
FORM execute_transfer USING uo_alv TYPE REF TO cl_gui_alv_grid
                              uv_option TYPE sy-ucomm.
*                        CHANGING ct_transfer TYPE /qaps/t_area_user.

  DATA lt_rows TYPE lvc_t_row.
  DATA lr_index TYPE RANGE OF lvc_index.
  DATA lt_diff TYPE /qaps/t_material.

  uo_alv->get_selected_rows( IMPORTING et_index_rows = lt_rows ).

  lr_index = VALUE #( FOR wa IN lt_rows
                      ( sign = 'I' option = 'EQ' low = wa-index ) ).

  CASE uv_option.
    WHEN 'RIGHT'.
      LOOP AT gt_source INTO DATA(ls_data).
        IF sy-tabix IN lr_index.
          APPEND ls_data TO gt_target.
        ELSE.
          APPEND ls_data TO lt_diff.
        ENDIF.
      ENDLOOP.
      gt_source = lt_diff.
    WHEN 'LEFT'.
      LOOP AT gt_target INTO ls_data.
        IF  sy-tabix IN lr_index.
          APPEND ls_data TO gt_source.
        ELSE.
          APPEND ls_data TO lt_diff.
        ENDIF.
      ENDLOOP.
      gt_target = lt_diff.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_listbox .

  check lines( gt_list ) = 0.
  SELECT id_categoria, descricao
              FROM /qaps/categ_trns
              INTO CORRESPONDING FIELDS OF TABLE @gt_listbox.

  SORT gt_listbox BY id_categoria ASCENDING.

  loop at gt_listbox ASSIGNING FIELD-SYMBOL(<fs_listbox>).
    <fs_listbox>-index = sy-tabix.
    gs_value-key  = <fs_listbox>-index.
    gs_value-text = <fs_listbox>-descricao.
    append gs_value to gt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_LISTBOX-INDEX'
      values = gt_list.

ENDFORM.
