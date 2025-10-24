FUNCTION /qaps/fm_traj_trecho_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"     VALUE(IS_TRAJETO) TYPE  /QAPS/S_TRAJETO
*"  EXPORTING
*"     VALUE(ET_DATA) TYPE  /QAPS/T_TRAJETO_TRECHO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 30.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 130.
  lv_y2 = lv_y1 + 22.

  gv_tiltle = 'Trechos por Trajeto'.

  REFRESH: gt_source,gt_target.

  PERFORM fill_source USING is_trajeto.
  PERFORM fill_target USING is_trajeto.

*  CLEAR: /qaps/categ_trns,
*         gv_search.

*  REFRESH: gt_source.

  IF go_alv_source IS BOUND.
    go_alv_source->refresh_table_display( ).
  ENDIF.

  IF go_alv_target IS BOUND.
    go_alv_target->refresh_table_display( ).
  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      et_data = CORRESPONDING #( gt_target ).

      LOOP AT et_data ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-id_trajeto = is_trajeto-id_trajeto.
      ENDLOOP.

    WHEN 'CANCEL'.
      REFRESH: et_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
