*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FILL_LIST_BOXES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_list_boxes .

  CHECK gv_loaded IS INITIAL.



  gv_loaded = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRE_FILL_EDICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_fill_edicao .

  CHECK gv_loaded_edit IS INITIAL.

*  gs_uf-index = VALUE #( gt_uf[ uf = gs_cidade-uf ]-index OPTIONAL ).
*  gs_regiao-index = VALUE #( gt_regiao[ id_regiao = gs_cidade-id_regiao ]-index OPTIONAL ).

  gv_loaded_edit = abap_true.

ENDFORM.
FORM fill_origem.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.

  REFRESH gt_list_tipo_origem.

  IF gv_origem_geral = abap_true.
    CLEAR lv_tipo.
  ELSEIF gv_origem_regiao = abap_true.
    lv_tipo = 'R'.
  ELSEIF gv_origem_cidade = abap_true.
    lv_tipo = 'C'.
  ELSEIF gv_origem_grp_planta = abap_true.
    lv_tipo = 'G'.
  ELSEIF gv_origem_centro = abap_true.
    lv_tipo = 'W'.
  ELSEIF gv_origem_cliente = abap_true.
    lv_tipo = 'K'.
  ELSEIF gv_origem_fornec = abap_true.
    lv_tipo = 'F'.
  ELSEIF gv_origem_porto = abap_true.
    lv_tipo = 'P'.
  ELSEIF gv_origem_term_port = abap_true.
    lv_tipo = 'I'.
  ENDIF.

  SELECT *
    FROM /qaps/v_ponto
    WHERE tipo_ponto = @lv_tipo
    INTO CORRESPONDING FIELDS OF TABLE @gt_tipo_origem.

  APPEND INITIAL LINE TO gt_list_tipo_origem ASSIGNING FIELD-SYMBOL(<fs_value_origem>).
  <fs_value_origem>-key = 0.
  <fs_value_origem>-text = '(Selecionar)'.

  LOOP AT gt_tipo_origem ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_tipo_origem ASSIGNING <fs_value_origem>.

    <fs_value_origem>-key = <fs>-index.
    DATA(lv_codigo) = |{ <fs>-codigo ALPHA = OUT }|.
    CONDENSE lv_codigo.
    lv_text =  |{ lv_codigo } - { <fs>-descricao }|.
    <fs_value_origem>-text = lv_text.

  ENDLOOP.

  SORT: gt_list_tipo_origem BY text.

  gs_tipo_origem-index = 0.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TIPO_ORIGEM-INDEX'
      values = gt_list_tipo_origem.



ENDFORM.
FORM fill_destino.

  DATA lv_text TYPE string.
  DATA lv_tipo TYPE /qaps/ponto-tipo_ponto.

  REFRESH gt_list_tipo_destino.

  IF gv_destino_geral = abap_true.
    CLEAR lv_tipo.
  ELSEIF gv_destino_regiao = abap_true.
    lv_tipo = 'R'.
  ELSEIF gv_destino_cidade = abap_true.
    lv_tipo = 'C'.
  ELSEIF gv_destino_grp_planta = abap_true.
    lv_tipo = 'G'.
  ELSEIF gv_destino_centro = abap_true.
    lv_tipo = 'W'.
  ELSEIF gv_destino_cliente = abap_true.
    lv_tipo = 'K'.
  ELSEIF gv_destino_fornec = abap_true.
    lv_tipo = 'F'.
  ELSEIF gv_destino_porto = abap_true.
    lv_tipo = 'P'.
  ELSEIF gv_destino_term_port = abap_true.
    lv_tipo = 'I'.
  ENDIF.

  SELECT *
    FROM /qaps/v_ponto
    WHERE tipo_ponto = @lv_tipo
    INTO CORRESPONDING FIELDS OF TABLE @gt_tipo_destino.

*  gt_list_dsp = gt_list_orp.

  APPEND INITIAL LINE TO gt_list_tipo_destino ASSIGNING FIELD-SYMBOL(<fs_value_destino>).
  <fs_value_destino>-key = 0.
  <fs_value_destino>-text = '(Selecionar)'.

  LOOP AT gt_tipo_destino ASSIGNING FIELD-SYMBOL(<fs>).

    <fs>-index = sy-tabix.

    APPEND INITIAL LINE TO gt_list_tipo_destino ASSIGNING <fs_value_destino>.

    <fs_value_destino>-key = <fs>-index.
    DATA(lv_codigo) = |{ <fs>-codigo ALPHA = OUT }|.
    CONDENSE lv_codigo.
    lv_text =  |{ lv_codigo } - { <fs>-descricao }|.
    <fs_value_destino>-text = lv_text.

  ENDLOOP.

  SORT: gt_list_tipo_destino BY text.

  gs_tipo_destino-index = 0.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'GS_TIPO_DESTINO-INDEX'
      values = gt_list_tipo_destino.



ENDFORM.
FORM f_ok .

  IF gs_data-codigo IS INITIAL.
    MESSAGE 'Código é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gs_data-descricao IS INITIAL.
    MESSAGE 'Descrição é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_origem_geral = abap_false AND gs_tipo_origem-index = 0.
    MESSAGE 'Origem é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_destino_geral = abap_false AND gs_tipo_destino-index = 0.
    MESSAGE 'Destino é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.
FORM set_origem .

  PERFORM fill_origem.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_DESTINO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_destino .

  PERFORM fill_destino.

ENDFORM.
