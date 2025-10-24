FUNCTION /qaps/fm_mat_grupo_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1
*"     REFERENCE(IV_ID_GRUPO_PRODUTO) TYPE  /QAPS/ID_GRUPO_PRODUTO
*"  EXPORTING
*"     REFERENCE(ET_DATA) TYPE  /QAPS/T_MATERIAL
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 118.
  lv_y2 = lv_y1 + 15.

  gv_tiltle = 'Materiais'.

  PERFORM fill_target USING iv_id_grupo_produto.

  CLEAR: /qaps/categ_trns,
         gv_search.

  REFRESH: gt_source.

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

      data(lv_id_categoria) = value #( gt_listbox[ index = gs_listbox-index ]-id_categoria OPTIONAL ).

      LOOP AT et_data ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-id_grupo_produto = iv_id_grupo_produto.
        <fs>-id_categoria = lv_id_categoria.
      ENDLOOP.

    WHEN 'CANCEL'.
      REFRESH: et_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

*  FREE: go_cont_source,
*        go_cont_target,
*        go_alv_source,
*        go_alv_target.

ENDFUNCTION.
