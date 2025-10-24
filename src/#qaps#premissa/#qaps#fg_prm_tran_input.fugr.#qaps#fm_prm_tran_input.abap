FUNCTION /qaps/fm_prm_tran_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_HEADER) TYPE  /QAPS/S_PREMISSA_HEADER OPTIONAL
*"     VALUE(IS_SIMULACAO) TYPE  /QAPS/S_SIMULACAO
*"  EXPORTING
*"     VALUE(ES_ITEM) TYPE  /QAPS/PREM_ITEM
*"     VALUE(ES_ORIGEM) TYPE  /QAPS/PONTO
*"     VALUE(ES_DESTINO) TYPE  /QAPS/PONTO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 45.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 85.
  lv_y2 = lv_y1 + 15.

  gv_tiltle = 'Input de Transferência'.
  gv_first_time = abap_true.

  gs_header = is_header.
  IF gs_header IS INITIAL.
    gs_header-id_simulacao = is_simulacao-id_simulacao.
    gs_header-dsc_simulacao = is_simulacao-descricao.
  ENDIF.
  gs_simulacao = is_simulacao.

  CLEAR: gv_loaded, gs_data.

  CLEAR: es_item,es_origem.

  IF is_simulacao IS INITIAL.
    es_message = VALUE #( type = 'E' message = 'Selecionar uma simulação' ).
    RETURN.
  ENDIF.

  gs_data-dsc_simulacao = is_simulacao-descricao.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.

      "Dados de tipo de regra
      es_item-tipo_regra         = COND #( WHEN gv_tr_agregador  = 'X' THEN 'AG'
                                           WHEN gv_tr_material  = 'X'THEN 'MA' ).

      CASE es_item-tipo_regra.
        WHEN 'AG'.
          es_item-agregador          = gt_list_ag[ gs_list_ag-index ]-agregador.
        WHEN 'MA'.
          es_item-matnr              = |{ gs_data-matnr ALPHA = IN WIDTH = 18 }|.
      ENDCASE.


      DATA(ls_origem) = VALUE #( gt_tipo_origem[ index =  gs_tipo_origem-index ] OPTIONAL ).

      IF NOT ls_origem IS INITIAL.
        es_origem-tipo_ponto = ls_origem-tipo_ponto.
        es_origem-id_ponto = ls_origem-id_ponto.
      ENDIF.

      DATA(ls_destino) = VALUE #( gt_tipo_destino[ index =  gs_tipo_destino-index ] OPTIONAL ).

      IF NOT ls_destino IS INITIAL.
        es_destino-tipo_ponto = ls_destino-tipo_ponto.
        es_destino-id_ponto = ls_destino-id_ponto.
      ENDIF.

    WHEN 'CANCEL'.
      CLEAR: es_item.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.

ENDFUNCTION.
