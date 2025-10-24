FUNCTION /QAPS/FM_NEW_INPUT .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_ACTION) TYPE  CHAR1 DEFAULT 'C'
*"     REFERENCE(IV_ID_CUSTO_ELEMENTAR) TYPE  /QAPS/ID_CUSTO_ELEMENTAR
*"         OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_DATA) TYPE  /QAPS/S_CUSTO_ELEMENTAR
*"     REFERENCE(ES_MESSAGE) TYPE  BAPIRET2
*"--------------------------------------------------------------------

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 25.
  lv_y1 = 5.

  lv_x2 = lv_x1 + 73.
  lv_y2 = lv_y1 + 18.

  gv_tiltle = 'Input de Custo Elementar'.
*  CLEAR gv_loaded.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.

  CASE sy-ucomm.
    WHEN 'OK'.

*      es_data-descricao = gs_data-descricao.
*      IF gv_escopo_tp_lista = abap_true.
*        es_data-escopo = 'T'.
*        es_data-id_tp_lista = VALUE #( gt_listbox[ index = gs_listbox-index ]-id_tp_lista OPTIONAL ).
*      ELSE.
*        es_data-escopo = 'G'.
*        CLEAR es_data-id_tp_lista.
*      ENDIF.
*
*      es_data-id_area = VALUE #( gt_listarea[ index = gs_listarea-index ]-id_area OPTIONAL ).
*
*      IF gv_tipo_dado_valor = abap_true.
*        es_data-tipo_dado = '1'.
*      ELSEIF gv_tipo_dado_percentual = abap_true.
*        es_data-tipo_dado = '2'.
*      ENDIF.
*
*      IF gv_origem_dado_input = abap_true.
*        es_data-origem_dado = '1'.
*      ELSEIF gv_origem_dado_custom = abap_true.
*        es_data-origem_dado = '2'.
*      ELSEIF gv_origem_dado_tabela = abap_true.
*        es_data-origem_dado = '3'.
*      ENDIF.
*
*      IF gv_tipo_variavel_geral = abap_true.
*        es_data-tipo_variavel = 'G'.
*      ELSEIF gv_tipo_variavel_logistica = abap_true.
*        es_data-tipo_variavel = 'L'.
*      ELSEIF gv_tipo_variavel_produtiva = abap_true.
*        es_data-tipo_variavel = 'P'.
*      ENDIF.

    WHEN 'CANCEL'.
      CLEAR: es_data.
      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).

  ENDCASE.


ENDFUNCTION.
