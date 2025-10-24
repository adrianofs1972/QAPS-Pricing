FUNCTION /qaps/fm_custo_elem_input .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     VALUE(IS_DATA) TYPE  /QAPS/S_CUSTO_ELEMENTAR OPTIONAL
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_CUSTO_ELEMENTAR
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 73.
  lv_y2 = lv_y1 + 20.

  gv_tiltle = 'Input de Custo Elementar'.
  CLEAR gs_data.

  gv_action = iv_action.

  CASE iv_action.
    WHEN 'C'.
      gv_tipo_dado_valor = 'X'.
      CLEAR gv_tipo_dado_percentual.
      gs_data-importacao = abap_true.
      gs_data-nacional = abap_true.
      gs_data-producao = abap_true.
      gs_data-transferencia = abap_true.

    WHEN 'E'.
      gs_data = CORRESPONDING #( is_data ).
      clear gv_loaded_edit.

  ENDCASE.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.
      IF iv_action = 'E'.
        es_data-id_custo_elementar = is_data-id_custo_elementar.
        es_data-created_by = is_data-created_by.
        es_data-created_in = is_data-created_in.
        es_data-created_on = is_data-created_on.
      ENDIF.
      es_data-descricao = gs_data-descricao.
      IF gv_escopo_tp_lista = abap_true.
        es_data-escopo = 'T'.
        es_data-id_tp_lista = VALUE #( gt_tp_lista[ index  = gs_tp_lista-index ]-id_tp_lista OPTIONAL ).
      ELSE.
        es_data-escopo = 'G'.
        CLEAR es_data-id_tp_lista.
      ENDIF.

      es_data-id_area = VALUE #( gt_area[ index = gs_area-index ]-id_area OPTIONAL ).

      es_data-importacao    = gs_data-importacao.
      es_data-nacional      = gs_data-nacional.
      es_data-producao      = gs_data-producao.
      es_data-transferencia = gs_data-transferencia.

      IF gv_tipo_dado_valor = abap_true.
        es_data-tipo_dado = '1'.
        es_data-moeda = gs_data-moeda.
      ELSEIF gv_tipo_dado_percentual = abap_true.
        es_data-tipo_dado = '2'.
      ENDIF.

      IF gv_origem_dado_input = abap_true.
        es_data-origem_dado = '1'.
      ELSEIF gv_origem_dado_custom = abap_true.
        es_data-origem_dado = '2'.
      ELSEIF gv_origem_dado_tabela = abap_true.
        es_data-origem_dado = '3'.
      ENDIF.

      IF gv_tipo_variavel_geral = abap_true.
        es_data-tipo_variavel = 'G'.
      ELSEIF gv_tipo_variavel_compra = abap_true.
        es_data-tipo_variavel = 'C'.
      ELSEIF gv_tipo_variavel_logistica = abap_true.
        es_data-tipo_variavel = 'L'.
      ELSEIF gv_tipo_variavel_produtiva = abap_true.
        es_data-tipo_variavel = 'P'.
      ELSEIF gv_tipo_variavel_frete = abap_true.
        es_data-tipo_variavel = 'F'.
      ENDIF.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
