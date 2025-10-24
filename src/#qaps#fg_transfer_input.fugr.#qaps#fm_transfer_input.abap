FUNCTION /qaps/fm_transfer_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_DATA) TYPE  /QAPS/S_PONTO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_PONTO_PREMISSA
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 84.
  lv_y2 = lv_y1 + 3.

  gv_tiltle = 'Origem'.
  CLEAR gv_loaded_edit.

  gv_action = iv_action.

*  IF gv_action = 'E'.
*    gs_data = CORRESPONDING #( is_data ).
*  ELSE.
    CLEAR gs_data.
*  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.

      DATA(lo_ponto) = NEW /qaps/cl_mdl_logistica( ).

      CASE gv_option.
        WHEN 'W'.

          select *
            from /qaps/v_ponto
            where tipo_ponto = 'W'
            into table @data(lt_centro).
          es_data-tipo  = 'W'.
          es_data-id    = lt_centro[ codigo = gs_data-werks ]-id_ponto.
        WHEN 'G'.

          select *
          from /qaps/v_ponto
            where tipo_ponto = 'G'
            into table @data(lt_grp_planta).
          es_data-id   = lt_grp_planta[ codigo = gs_data-cod_grp_planta ]-id_ponto.
      ENDCASE.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
