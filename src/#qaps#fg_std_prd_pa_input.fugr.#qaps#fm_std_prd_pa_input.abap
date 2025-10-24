FUNCTION /qaps/fm_std_prd_pa_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_ITEM) TYPE  /QAPS/S_STD_PRODUCAO_PA OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_STD_PRODUCAO_PA
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 77.
  lv_y2 = lv_y1 + 9.

  gv_tiltle = 'Std de Produção - Produto Acabado'.
  gv_action = iv_action.

  PERFORM fill_tables.

  IF iv_action = 'C'.
    CLEAR gs_data.
  ELSEIF iv_action = 'E'.
    gs_data = is_item.

    gs_data-matnr = |{ gs_data-matnr ALPHA = IN WIDTH = 18 }|.

    IF NOT gs_data-werks IS INITIAL.
      gv_destino_centro = 'X'.
      CLEAR gv_destino_regiao.
      gv_option = 'P'.

      gs_data-werks = gs_data-cod_destino.
      gs_data-dsc_werks = gs_data-dsc_destino.

    ELSEIF NOT gs_data-id_regiao IS INITIAL.
      gv_destino_regiao = 'X'.
      gv_option = 'R'.
      CLEAR gv_destino_centro.

      gs_data-cod_regiao = gs_data-cod_destino.
      gs_data-dsc_regiao = gs_data-dsc_destino.
    ELSEIF NOT gs_data-id_grp_planta IS INITIAL.
      gv_destino_grp_planta = 'X'.
      gv_option = 'G'.
      CLEAR gv_destino_centro.

      gs_data-cod_grp_planta = gs_data-cod_destino.
      gs_data-dsc_grp_planta = gs_data-dsc_destino.

    ENDIF.

  ENDIF.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.

      es_data = CORRESPONDING #( gs_data ).

      IF gv_destino_regiao = 'X'.
        DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
        DATA(ls_regiao) = lo_logistica->get_regiao_by_code( gs_data-cod_regiao ).
        es_data-id_regiao = ls_regiao-id_regiao.
        CLEAR: es_data-werks, es_data-id_grp_planta.
      ELSEIF gv_destino_grp_planta = 'X'.
        lo_logistica = NEW /qaps/cl_mdl_logistica( ).
        DATA(lt_grp_planta) = lo_logistica->get_grp_planta(  ).
        es_data-id_grp_planta = lt_grp_planta[ codigo = gs_data-cod_grp_planta ]-id_grp_planta.""ls_regiao-id_regiao.
        CLEAR: es_data-werks,es_data-id_regiao.
      ELSE.
        CLEAR: es_data-id_regiao, es_data-id_grp_planta..
      ENDIF.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
