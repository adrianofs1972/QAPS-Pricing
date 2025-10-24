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
FORM fill_target USING uv_id_tp_lista TYPE /qaps/ed_tp_lista
                       uv_id_area TYPE /qaps/ed_id_area.


  SELECT *
    FROM /qaps/area_user
    WHERE id_tp_lista = @uv_id_tp_lista
    AND   id_area = @uv_id_area
    INTO CORRESPONDING FIELDS OF TABLE @gt_target.

  LOOP AT gt_target ASSIGNING FIELD-SYMBOL(<fs>).
    PERFORM get_fullname USING <fs>-uname CHANGING <fs>-fullname.
  ENDLOOP.

ENDFORM.
FORM fill_source.

  DATA: lr_user_exclude TYPE RANGE OF uname.
  DATA srch_str TYPE string.

  REFRESH: gt_source.

  lr_user_exclude = VALUE #( FOR wa IN gt_target
                              ( sign = 'I' option = 'EQ' low = wa-uname ) ).

  srch_str = `%` && gv_search && `%`.

  SELECT DISTINCT bname AS uname, name_text AS fullname
    FROM /qaps/v_users
    WHERE ( bname LIKE @srch_str OR name_text LIKE @srch_str )
    INTO CORRESPONDING FIELDS OF TABLE @gt_source.

  IF lines( lr_user_exclude ) > 0.
    DELETE gt_source WHERE uname IN lr_user_exclude.
  ENDIF.

  SORT gt_source BY fullname ASCENDING.

  go_alv_source->refresh_table_display( ).

ENDFORM.
FORM get_fullname USING uv_uname TYPE xubname
                  CHANGING cv_fullname TYPE ad_namtext.

  DATA: ls_address TYPE bapiaddr3,
        lt_return  TYPE bapiret2_t.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = uv_uname
*     CACHE_RESULTS        = 'X'
    IMPORTING
      address  = ls_address
    TABLES
      return   = lt_return.

  cv_fullname = ls_address-fullname.

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

  IF gv_loaded = abap_false.

    go_cont_source = NEW cl_gui_custom_container( container_name = 'GO_SOURCE'
                                                  repid = sy-repid
                                                  dynnr = sy-dynnr ).
    go_cont_target = NEW cl_gui_custom_container( container_name = 'GO_TARGET'
                                                  repid = sy-repid
                                                  dynnr = sy-dynnr ).

    go_alv_source = NEW cl_gui_alv_grid( i_parent = go_cont_source ).
    go_alv_target = NEW cl_gui_alv_grid( i_parent = go_cont_target ).

    PERFORM get_catalog CHANGING lt_fcat.

    ls_layo_source-grid_title = 'Usuários Disponíveis'.
    ls_layo_source-no_toolbar = 'X'.
    ls_layo_source-sel_mode = 'A'.

    go_alv_source->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo_source    " Layout
      CHANGING
        it_outtab                     = gt_source    " Output Table
        it_fieldcatalog               = lt_fcat     ).

    ls_layo_target-grid_title = 'Usuários Vínculados'.
    ls_layo_target-no_toolbar = 'X'.

    go_alv_target->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layo_target
      CHANGING
        it_outtab                     = gt_target    " Output Table
        it_fieldcatalog               = lt_fcat     ).

    gv_loaded = abap_true.

  ELSE.
    go_alv_source->refresh_table_display( ).
    go_alv_target->refresh_table_display( ).
  ENDIF.

ENDFORM.
FORM get_catalog CHANGING ct_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = '/QAPS/S_AREA_USER'
    CHANGING
      ct_fieldcat      = ct_fcat.

  DELETE ct_fcat WHERE fieldname <> 'UNAME'
                   AND fieldname <> 'FULLNAME'.

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fs>).
    CASE <fs>-fieldname.
      WHEN 'FULLNAME'.
        <fs>-outputlen = 37.
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
  DATA lt_diff TYPE /qaps/t_area_user.

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
