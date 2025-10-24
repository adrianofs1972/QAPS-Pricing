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

  DATA: lr_area_exclude TYPE RANGE OF /qaps/ed_id_area,
        lr_area_include TYPE RANGE OF /qaps/ed_id_area.

  REFRESH: gt_source,
           gt_target.

  SELECT *
    FROM /qaps/lista_area
    WHERE id_tp_lista = @uv_id
    INTO TABLE @DATA(lt_lista).

  IF lines( lt_lista ) > 0.
    lr_area_exclude = VALUE #( FOR wa IN lt_lista
                       ( sign  = 'E' option = 'EQ' low = wa-id_area ) ).

    lr_area_include = VALUE #( FOR wa IN lt_lista
                                ( sign  = 'I' option = 'EQ' low = wa-id_area ) ).

    SELECT *
        FROM /qaps/area
        WHERE id_area IN @lr_area_exclude
        INTO CORRESPONDING FIELDS OF  TABLE @gt_source.

    SELECT *
        FROM /qaps/area
        WHERE id_area IN @lr_area_include
        INTO CORRESPONDING FIELDS OF  TABLE @gt_target.

  ELSE.
    SELECT *
      FROM /qaps/area
      INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
  ENDIF.


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

  ls_layo_source-grid_title = 'Áreas Disponíveis'.
  ls_layo_source-no_toolbar = 'X'.
  ls_layo_source-sel_mode = 'A'.

  go_alv_source->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layo_source    " Layout
    CHANGING
      it_outtab                     = gt_source    " Output Table
      it_fieldcatalog               = lt_fcat     ).

  ls_layo_target-grid_title = 'Áreas Vínculadas'.
  ls_layo_target-no_toolbar = 'X'.

  go_alv_target->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layo_target
    CHANGING
      it_outtab                     = gt_target    " Output Table
      it_fieldcatalog               = lt_fcat     ).

  go_alv_target->refresh_table_display( ).

  gv_loaded = abap_true.

ENDFORM.
FORM get_catalog CHANGING ct_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = '/QAPS/S_AREA'
    CHANGING
      ct_fieldcat      = ct_fcat.

  DELETE ct_fcat WHERE fieldname <> 'DESCRICAO'.


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

  DATA lt_transfer TYPE /qaps/t_area.

  DATA: lt_source_rows TYPE lvc_t_row,
        lt_target_rows TYPE lvc_t_row.

  CASE uv_option.
    WHEN 'RIGHT'.
      PERFORM execute_transfer USING go_alv_source uv_option.
    WHEN 'RIGHT_ALL'.
      append lines of gt_source to gt_target.
      refresh gt_source.
    WHEN 'LEFT'.
      PERFORM execute_transfer USING go_alv_target uv_option.
    WHEN 'LEFT_ALL'.
      append lines of gt_target to gt_source.
      refresh gt_target.
  ENDCASE.

  go_alv_source->refresh_table_display( ).
  go_alv_target->refresh_table_display( ).

ENDFORM.
FORM execute_transfer USING uo_alv TYPE REF TO cl_gui_alv_grid
                              uv_option TYPE sy-ucomm.
*                        CHANGING ct_transfer TYPE /qaps/t_area.

  DATA lt_rows TYPE lvc_t_row.
  DATA lr_index TYPE RANGE OF lvc_index.
  DATA lt_diff TYPE /qaps/t_area.

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
