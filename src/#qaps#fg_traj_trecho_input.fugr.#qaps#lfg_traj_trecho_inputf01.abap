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
FORM fill_target USING  us_trajeto TYPE /qaps/s_trajeto.

  DATA lr_id TYPE RANGE OF /qaps/ed_id_trecho.

  SELECT *
    FROM /qaps/traj_trech
    WHERE id_trajeto = @us_trajeto-id_trajeto
    INTO TABLE @DATA(lt_data).

  lr_id = VALUE #( FOR wa IN lt_data
                   ( sign = 'I' option = 'EQ' low = wa-id_trecho ) ).

  CHECK lines( lr_id ) > 0.

  gt_target = NEW /qaps/cl_mdl_logistica( )->get_trechos( ).
  DELETE gt_target WHERE NOT id_trecho IN lr_id.


ENDFORM.
FORM fill_source USING us_trajeto TYPE /qaps/s_trajeto.

  DATA lr_id TYPE RANGE OF /qaps/ed_id_trecho.

  SELECT *
    FROM /qaps/traj_trech
    WHERE id_trajeto = @us_trajeto-id_trajeto
    INTO TABLE @DATA(lt_data).

  lr_id = VALUE #( FOR wa IN lt_data
                   ( sign = 'I' option = 'EQ' low = wa-id_trecho ) ).

  REFRESH: gt_source.

  gt_source = NEW /qaps/cl_mdl_logistica( )->get_trechos( ).

  IF lines( lr_id ) > 0.
    DELETE gt_source WHERE id_trecho IN lr_id.
  ENDIF.

ENDFORM.

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

  ls_layo_source-grid_title = 'Trechos Disponíveis'.
  ls_layo_source-no_toolbar = 'X'.
  ls_layo_source-sel_mode = 'A'.
  ls_layo_source-zebra = 'X'.

  go_alv_source->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layo_source    " Layout
    CHANGING
      it_outtab                     = gt_source    " Output Table
      it_fieldcatalog               = lt_fcat     ).

  ls_layo_target-grid_title = 'Trechos Vinculados'.
  ls_layo_target-no_toolbar = 'X'.
  ls_layo_target-sel_mode = 'A'.
  ls_layo_target-zebra = 'X'.

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
      i_structure_name = '/QAPS/S_TRECHO'
    CHANGING
      ct_fieldcat      = ct_fcat.

  DELETE ct_fcat WHERE fieldname = 'MESSAGE_TYPE'
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
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fs>).
    CASE <fs>-fieldname.
      WHEN 'DSC_TIPO_ORIGEM'.
        <fs>-col_pos = 1.
        <fs>-coltext = 'Tipo Origem'.
        <fs>-outputlen = 12.
      WHEN 'DSC_ORIGEM'.
        <fs>-col_pos = 2.
        <fs>-coltext = 'Cod Origem'.
        <fs>-outputlen = 10.
      WHEN 'ORIGEM'.
        <fs>-col_pos = 3.
        <fs>-coltext = 'Origem'.
        <fs>-outputlen = 17.
      WHEN 'DSC_TIPO_DESTINO'.
        <fs>-col_pos = 4.
        <fs>-coltext = 'Tipo Destino'.
        <fs>-outputlen = 12.
      WHEN 'DSC_DESTINO'.
        <fs>-col_pos = 5.
        <fs>-coltext = 'Cod Destino'.
        <fs>-outputlen = 10.
      WHEN 'DESTINO'.
        <fs>-col_pos = 6.
        <fs>-coltext = 'Destino'.
        <fs>-outputlen = 17.
      WHEN 'DSC_MODAL'.
        <fs>-col_pos = 6.
        <fs>-coltext = 'Modal'.
        <fs>-outputlen = 10.
      WHEN 'TEMPO_DESLOCAMENTO'.
        <fs>-col_pos = 8.
        <fs>-coltext = 'Temp Desloc'.
        <fs>-outputlen = 10.
      WHEN 'DISTANCIA'.
        <fs>-col_pos = 8.
        <fs>-coltext = 'Distância (km)'.
        <fs>-outputlen = 11.
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
  DATA lt_diff TYPE /qaps/t_trecho.

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
