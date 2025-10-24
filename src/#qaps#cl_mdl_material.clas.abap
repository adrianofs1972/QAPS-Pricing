class /QAPS/CL_MDL_MATERIAL definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods GET_MATERIAIS_ALL
    importing
      !IR_MATNR type RANGES_MATNR
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_MATERIAIS_BY_MATERIAL
    importing
      !IV_MATNR type MATNR
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_MATERIAIS_BY_MAT_PLANEJADO
    importing
      !IV_MAT_PLANEJADO type /QAPS/MAT_PLANEJADO
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_MATERIAIS_BY_AGREGADOR
    importing
      !IV_AGREGADOR type /QAPS/AGREGADOR
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_MATERIAIS_BY_GRP_PRODUTO
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_MATERIAIS
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO
    returning
      value(RETURN) type /QAPS/T_MATERIAL .
  methods GET_GRUPO_PRODUTO
    importing
      !IV_ID_GRUPO_PRODUTO type /QAPS/ID_GRUPO_PRODUTO optional
    returning
      value(RETURN) type /QAPS/T_GRUPO_PRODUTO .
  methods CREATE_MATERIAIS
    importing
      value(IV_ID_GRUPO_PRODUTO) type /QAPS/ID_GRUPO_PRODUTO
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_GRUPO_PRODUTO
    importing
      value(IS_DATA) type /QAPS/S_GRUPO_PRODUTO
    returning
      value(RETURN) type /QAPS/ID_GRUPO_PRODUTO .
  methods UPDATE_TIPO_LISTA
    importing
      !IS_DATA type /QAPS/S_TP_LISTA .
  methods DESVINCULAR_CATEG_TRANSPORTE
    importing
      !IT_DATA type /QAPS/T_MATERIAL
    returning
      value(RETURN) type ABAP_BOOL .
  methods VINCULAR_CATEG_TRANSPORTE
    importing
      !IT_DATA type /QAPS/T_MATERIAL
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_MATERIAIS
    importing
      !IT_DATA type /QAPS/T_MATERIAL
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_GRUPO_PRODUTO
    importing
      !IS_DATA type /QAPS/S_GRUPO_PRODUTO
    returning
      value(RETURN) type ABAP_BOOL .
protected section.
private section.

  data MT_GRUPO_PRODUTO type /QAPS/T_GRUPO_PRODUTO .
  data MT_MATERIAL type /QAPS/T_MATERIAL .

  methods GET_CATEG_TRANS_DESCRIPTION
    importing
      !IV_ID_CATEGORIA type /QAPS/ID_CATEGORIA
    returning
      value(RETURN) type /QAPS/ED_DSC_CATEGORIA .
  methods PREENCHER_MASTER_DATA
    changing
      !CS_DATA type /QAPS/MATERIAL .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_NEXT_NUMBER
    returning
      value(RETURN) type /QAPS/ID_GRUPO_PRODUTO .
ENDCLASS.



CLASS /QAPS/CL_MDL_MATERIAL IMPLEMENTATION.


  METHOD create_grupo_produto.

    DATA lr_data TYPE REF TO data.
    DATA: ls_data    TYPE /qaps/s_grupo_produto,
          ls_entry   TYPE /qaps/grp_prod,
          ls_message TYPE bapiret2.

    CALL FUNCTION '/QAPS/FM_GRP_PRODUTO_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        is_data    = is_data
        iv_action  = 'C'
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_grupo_produto = get_next_number( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/grp_prod FROM ls_entry.

    IF sy-subrc IS INITIAL.
      return = ls_entry-id_grupo_produto.
    ELSE.
      MESSAGE 'Grupo de Produtos já existe' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.


  ENDMETHOD.


  METHOD create_materiais.

    DATA lr_data TYPE REF TO data.
    DATA: lt_data     TYPE /qaps/t_material,
          lt_inactive TYPE TABLE OF /qaps/material,
          lt_new      TYPE TABLE OF /qaps/material,
          ls_entry    TYPE /qaps/material,
          ls_message  TYPE bapiret2.

    IF iv_id_grupo_produto IS INITIAL.
      MESSAGE 'Nenhum Grupo de Produto selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_MAT_GRUPO_INPUT'
      EXPORTING
        iv_id_grupo_produto = iv_id_grupo_produto
        iv_action           = 'C'
      IMPORTING
        et_data             = lt_data
        es_message          = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    IF lines( lt_data ) > 0.

      "Áreas que existiam e form retiradas, não serão deletadas, mas sim desativadas
      SELECT *
        FROM /qaps/material
        WHERE id_grupo_produto = @iv_id_grupo_produto
        INTO TABLE @DATA(lt_material).

      LOOP AT lt_material INTO DATA(ls_material).

        CHECK not line_exists( lt_data[ matnr = ls_material-matnr ] ).
        ls_entry = CORRESPONDING #( ls_material ).
        ls_entry-id_grupo_produto = iv_id_grupo_produto.
        APPEND ls_entry TO lt_inactive.

      ENDLOOP.

      LOOP AT lt_data INTO DATA(ls_data).

        CHECK NOT line_exists( lt_material[ matnr = ls_data-matnr ] ).
        ls_entry = CORRESPONDING #( ls_data ).
        ls_entry-id_grupo_produto = iv_id_grupo_produto.
        APPEND ls_entry TO lt_new.

      ENDLOOP.

    ENDIF.

    IF lines( lt_inactive ) > 0.
      MESSAGE 'Temos que ver o comportamento' TYPE 'I'.
    ENDIF.
*    LOOP AT lt_inactive REFERENCE INTO lr_data.
*      preencher_dados_controle( CHANGING cr_data = lr_data ).
*    ENDLOOP.
*    MODIFY /qaps/lista_aprv FROM TABLE lt_inactive.
*    DELETE lt_inactive FROM TABLE /qaps/material.

    LOOP AT lt_new ASSIGNING FIELD-SYMBOL(<fs_new>).
      preencher_master_data( CHANGING cs_data = <fs_new> ).
    ENDLOOP.

    LOOP AT lt_new REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/material FROM TABLE lt_new.

    return = abap_true.

  ENDMETHOD.


  METHOD DELETE_GRUPO_PRODUTO.

    DATA lv_message TYPE string.

    lv_message = text-m01.

    IF question( lv_message ) = abap_true.
      DELETE FROM /qaps/material WHERE id_grupo_produto = is_data-id_grupo_produto.
      DELETE FROM /qaps/grp_prod WHERE id_grupo_produto = is_data-id_grupo_produto.
      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_materiais.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    if lines( it_data ) = 0.
      message 'Nenhum item foi selecionado' type 'S' DISPLAY LIKE 'E'.
      return.
    endif.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/material WHERE id_grupo_produto = ls_data-id_grupo_produto
                                   AND   matnr = ls_data-matnr.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD desvincular_categ_transporte.

    DATA: lv_message TYPE string,
          ls_message TYPE bapiret2.
    DATA lv_id_categoria TYPE /qaps/id_categoria.
    DATA lr_data TYPE REF TO data.

    lv_message = TEXT-m05.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.

      SELECT *
        FROM /qaps/material
        FOR ALL ENTRIES IN @it_data
        WHERE matnr = @it_data-matnr
        INTO TABLE @DATA(lt_material).

      LOOP AT lt_material ASSIGNING  FIELD-SYMBOL(<fs_material>).

        CLEAR <fs_material>-id_categoria.

        lr_data = REF #( <fs_material> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

      ENDLOOP.

      MODIFY /qaps/material FROM TABLE lt_material.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD get_categ_trans_description.

    SELECT SINGLE descricao
      FROM /qaps/categ_trns
      WHERE id_categoria = @iv_id_categoria
      INTO @return.

  ENDMETHOD.


  METHOD get_grupo_produto.

    IF lines( mt_grupo_produto ) = 0.

      SELECT *
        FROM /qaps/grp_prod
        INTO CORRESPONDING FIELDS OF TABLE mt_grupo_produto.

    ENDIF.

    return = mt_grupo_produto.

    IF NOT iv_id_grupo_produto IS INITIAL.
      DELETE return WHERE id_grupo_produto <> iv_id_grupo_produto.
    ENDIF.

    SORT return BY descricao DESCENDING.
    return = mt_grupo_produto.

  ENDMETHOD.


  METHOD get_materiais.

    SELECT *
      FROM /qaps/material
      WHERE id_grupo_produto = @iv_id_grupo_produto
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
      <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
      CHECK NOT <fs>-id_categoria IS INITIAL.
      <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
    ENDLOOP.

    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD get_materiais_all.

    IF lines( mt_material ) = 0.

      SELECT *
        FROM /qaps/material
        WHERE matnr IN @ir_matnr
        INTO CORRESPONDING FIELDS OF TABLE @mt_material.

      LOOP AT mt_material ASSIGNING FIELD-SYMBOL(<fs>).
        <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
        <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
        CHECK NOT <fs>-id_categoria IS INITIAL.
        <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
      ENDLOOP.

    ENDIF.

    return = mt_material.
    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD get_materiais_by_agregador.

    SELECT *
      FROM /qaps/material
      WHERE agregador = @iv_agregador
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
      <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
      CHECK NOT <fs>-id_categoria IS INITIAL.
      <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
    ENDLOOP.

    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD GET_MATERIAIS_BY_GRP_PRODUTO.

    SELECT *
      FROM /qaps/material
      WHERE id_grupo_produto = @iv_id_grupo_produto
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
      <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
      CHECK NOT <fs>-id_categoria IS INITIAL.
      <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
    ENDLOOP.

    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD get_materiais_by_material.

    SELECT *
      FROM /qaps/material
      WHERE matnr = @iv_matnr
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
      <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
      CHECK NOT <fs>-id_categoria IS INITIAL.
      <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
    ENDLOOP.

    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD get_materiais_by_mat_planejado.

    SELECT *
      FROM /qaps/material
      WHERE mat_planejado = @iv_mat_planejado
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-dsc_matnr = get_material_description( <fs>-matnr ).
      <fs>-dsc_mat_planejado = get_material_description( <fs>-mat_planejado ).
      CHECK NOT <fs>-id_categoria IS INITIAL.
      <fs>-dsc_categoria = get_categ_trans_description( <fs>-id_categoria ).
    ENDLOOP.

    SORT return BY matnr ASCENDING.

  ENDMETHOD.


  METHOD get_next_number.

    return = cl_system_uuid=>create_uuid_x16_static( ).
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr = '01'
*        object      = '/QAPS/GRPP'
**       QUANTITY    = '1'
**       SUBOBJECT   = ' '
**       TOYEAR      = '0000'
**       IGNORE_BUFFER                 = ' '
*      IMPORTING
*        number      = return
**       QUANTITY    =
**       RETURNCODE  =
**     EXCEPTIONS
**       INTERVAL_NOT_FOUND            = 1
**       NUMBER_RANGE_NOT_INTERN       = 2
**       OBJECT_NOT_FOUND              = 3
**       QUANTITY_IS_0                 = 4
**       QUANTITY_IS_NOT_1             = 5
**       INTERVAL_OVERFLOW             = 6
**       BUFFER_OVERFLOW               = 7
**       OTHERS      = 8
*      .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


  ENDMETHOD.


  METHOD preencher_master_data.

    DATA: lr_atnam  TYPE RANGE OF atnam,
          lv_object TYPE ausp-objek.

    DATA:lt_class      TYPE wrf_class_tty,
         lt_objectdata TYPE rihclobjdat_tab.

    lv_object = cs_data-matnr.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
*        class              = 'CHEMIC_PHYS_CHARAC'
        classtext          = 'X'
        classtype          = 'Z02'
*       CLINT              = 0
*       FEATURES           = 'X'
*       LANGUAGE           = SY-LANGU
        object             = lv_object
*       OBJECTTABLE        = ' '
*       KEY_DATE           = SY-DATUM
*       INITIAL_CHARACT    = 'X'
*       NO_VALUE_DESCRIPT  =
*       CHANGE_SERVICE_CLF = 'X'
*       INHERITED_CHAR     = ' '
*       CHANGE_NUMBER      = ' '
      TABLES
        t_class            = lt_class
        t_objectdata       = lt_objectdata
*       I_SEL_CHARACTERISTIC       =
*       T_NO_AUTH_CHARACT  =
      EXCEPTIONS
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    BREAK-POINT.

    IF lines( lt_objectdata ) > 0.
      cs_data-agregador = VALUE #( lt_objectdata[ smbez = 'AGREGADOR' ]-ausp1 OPTIONAL ).
      cs_data-classif_perigosa = VALUE #( lt_objectdata[ smbez = 'CLASSIFICACAO PERIGOSA' ]-ausp1 OPTIONAL ).

    ENDIF.

    SELECT *
      FROM pgmi
      WHERE nrmit = @cs_data-matnr
      INTO TABLE @DATA(lt_pgmi).

    IF lines( lt_pgmi ) > 0.
      cs_data-mat_planejado = lt_pgmi[ 1 ]-prgrp.
    ENDIF.

  ENDMETHOD.


  METHOD QUESTION.

    DATA lv_answer TYPE c.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR      = ' '
*       DIAGNOSE_OBJECT             = ' '
        text_question = iv_message
*       TEXT_BUTTON_1 = 'Ja'(001)
*       ICON_BUTTON_1 = ' '
*       TEXT_BUTTON_2 = 'Nein'(002)
*       ICON_BUTTON_2 = ' '
*       DEFAULT_BUTTON              = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN  = 25
*       START_ROW     = 6
*       POPUP_TYPE    =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer        = lv_answer
*     TABLES
*       PARAMETER     =
*     EXCEPTIONS
*       TEXT_NOT_FOUND              = 1
*       OTHERS        = 2
      .

    IF lv_answer = '1'.
      return = abap_true.
    ELSE.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  method UPDATE_TIPO_LISTA.
  endmethod.


  METHOD vincular_categ_transporte.

    DATA: lv_message TYPE string,
          ls_message TYPE bapiret2.
    DATA lv_id_categoria TYPE /qaps/id_categoria.
    DATA lr_data TYPE REF TO data.
    DATA ls_entry TYPE /qaps/material.

    lv_message = TEXT-m04.

    if lines( it_data ) = 0.
      message 'Nenhum item foi selecionado' type 'S' DISPLAY LIKE 'E'.
      return.
    endif.

    IF question( lv_message ) = abap_true.

      CALL FUNCTION '/QAPS/FM_MAT_CAT_TRANSP_INPUT'
        IMPORTING
          ev_id_categoria = lv_id_categoria
          es_message      = ls_message.

      IF ls_message-type = 'E'.
        MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE 'E'.
        return = abap_false.
      ENDIF.

      select *
        from /qaps/material
        FOR ALL ENTRIES IN @it_data
        where matnr = @it_data-matnr
        into table @data(lt_material).

      LOOP AT lt_material ASSIGNING  FIELD-SYMBOL(<fs_material>).

        <fs_material>-id_categoria = lv_id_categoria.

        lr_data = REF #( <fs_material> ).
        preencher_dados_controle( CHANGING cr_data = lr_data ).

      ENDLOOP.

      MODIFY /qaps/material FROM TABLE lt_material.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
