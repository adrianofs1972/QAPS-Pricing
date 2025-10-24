FUNCTION /qaps/fm_ponto_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_DATA) TYPE  /QAPS/S_PONTO OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_PONTO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------
*
*  DATA: lv_x1 TYPE i,
*        lv_x2 TYPE i,
*        lv_y1 TYPE i,
*        lv_y2 TYPE i.
*
*  lv_x1 = 45.
*  lv_y1 = 5.
*
*  lv_x2 = lv_x1 + 84.
*  lv_y2 = lv_y1 + 11.
*
*  gv_tiltle = 'Input de Ponto'.
*  CLEAR gv_loaded_edit.
*
*  gv_action = iv_action.
*
*  IF gv_action = 'E'.
*    gs_data = CORRESPONDING #( is_data ).
*  ELSE.
*    CLEAR gs_data.
*  ENDIF.
*
*  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
*                   ENDING AT lv_x2 lv_y2.
*
*  CASE sy-ucomm.
*    WHEN 'OK'.
*      es_data = CORRESPONDING #( gs_data ).
**      es_data-id_cidade = VALUE #( gt_cidade[ index = gs_cidade-index ]-id_cidade OPTIONAL ).
*
*      CASE gv_option.
*        WHEN 'W'.
*          es_data-tipo_ponto     = gv_option.
**          es_data-werks          = gs_data-werks.
*        WHEN 'C'.
*          es_data-tipo_ponto     = gv_option.
**          es_data-kunnr          = gs_data-kunnr.
*        WHEN 'F'.
*          es_data-tipo_ponto     = gv_option.
**          es_data-lifnr          = gs_data-lifnr.
**        WHEN 'P'.
**          es_data-tipo_ponto     = gv_option.
**          es_data-cod_porto      = gs_data-cod_porto.
**          es_data-porto          = gs_data-porto.
*        WHEN 'I'. "Cais Comercial
*
*          DATA(lo_logistica) = NEW /qaps/cl_mdl_logistica( ).
*
*          DATA(lt_cais) = lo_logistica->get_cais( ).
*          es_data-tipo_ponto     = gv_option.
**          es_data-id_cais        = VALUE #( lt_cais[ cod_cais = gs_data-cod_cais ]-id_cais OPTIONAL ).
*      ENDCASE.
*
*    WHEN 'CANCEL'.
*      CLEAR: es_data.
*      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
*
*  ENDCASE.


ENDFUNCTION.
