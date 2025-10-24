class /QAPS/CL_MDL_CUSTO_ELEMENTAR definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods GET_VARIAVEIS_BY_TIPO_LISTA
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_PARENT type /QAPS/ED_ID_CALC_NODE optional
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods GET_VARIAVEL_BY_ID
    importing
      !IV_ID_CUSTO_ELEMENTAR type /QAPS/ID_CUSTO_ELEMENTAR
    returning
      value(RETURN) type /QAPS/S_CUSTO_ELEMENTAR .
  methods GET_VARIAVEL
    returning
      value(RETURN) type /QAPS/T_CUSTO_ELEMENTAR .
  methods DELETE_VARIAVEL
    importing
      !IT_DATA type /QAPS/T_CUSTO_ELEMENTAR
    returning
      value(RETURN) type ABAP_BOOL .
  methods EDIT_VARIAVEL
    importing
      value(IT_DATA) type /QAPS/T_CUSTO_ELEMENTAR
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_VARIAVEL
    importing
      !IS_DATA type /QAPS/S_CUSTO_ELEMENTAR optional
    returning
      value(RETURN) type ABAP_BOOL .
protected section.
private section.

  methods PREENCHER_MASTER_DATA
    changing
      !CS_DATA type /QAPS/MATERIAL .
  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
ENDCLASS.



CLASS /QAPS/CL_MDL_CUSTO_ELEMENTAR IMPLEMENTATION.


  METHOD create_variavel.

    DATA lr_data TYPE REF TO data.
    DATA: ls_data    TYPE /qaps/s_custo_elementar,
          ls_entry   TYPE /qaps/custo_elm,
          ls_message TYPE bapiret2.


    IF is_data IS INITIAL.

      CALL FUNCTION '/QAPS/FM_CUSTO_ELEM_INPUT'
        EXPORTING
*         iv_id_grupo_produto = iv_id_grupo_produto
          iv_action  = 'C'
        IMPORTING
          es_data    = ls_data
          es_message = ls_message.

      IF ls_message-type = 'E'.
        MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
        RETURN.
      ENDIF.

    ELSE.
      ls_data = CORRESPONDING #( is_data ).
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).
    ls_entry-id_custo_elementar = cl_system_uuid=>create_uuid_x16_static( ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/custo_elm FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD delete_variavel.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/custo_elm WHERE id_custo_elementar = ls_data-id_custo_elementar.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD edit_variavel.

    DATA lr_data TYPE REF TO data.
    DATA: ls_data    TYPE /qaps/s_custo_elementar,
          ls_entry   TYPE /qaps/custo_elm,
          ls_message TYPE bapiret2.

    ls_data = it_data[ 1 ].

    CALL FUNCTION '/QAPS/FM_CUSTO_ELEM_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        is_data    = ls_data
        iv_action  = 'E'
      IMPORTING
        es_data    = ls_data
        es_message = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    ls_entry = CORRESPONDING #( ls_data ).

    lr_data = REF #( ls_entry ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).
    MODIFY /qaps/custo_elm FROM ls_entry.

    return = abap_true.

  ENDMETHOD.


  METHOD get_variaveis_by_tipo_lista.

    DATA lr_custo_elem TYPE RANGE OF /qaps/id_custo_elementar.

    SELECT *
      FROM /qaps/custo_elm
      WHERE ( escopo = 'T' AND id_tp_lista = @iv_id_tp_lista )
      OR    escopo = 'G'
      INTO CORRESPONDING FIELDS OF TABLE @return.

    IF not iv_id_parent IS INITIAL.
      DATA(lo_custo_calc) = NEW /qaps/cl_mdl_input_custo_calc( ).
      DATA(lt_child_nodes) = lo_custo_calc->get_child_nodes( iv_id_tp_lista    =  iv_id_tp_lista
                                                          iv_id_parent_node =  iv_id_parent ).

      delete lt_child_nodes where tipo_node <> 'E'.

      lr_custo_elem = VALUE #( FOR wa IN lt_child_nodes
                               ( sign = 'I' option = 'EQ' low = wa-id_custo_elementar ) ).

      IF lines( lr_custo_elem ) > 0.
        DELETE return WHERE id_custo_elementar IN lr_custo_elem.
      ENDIF.

    ENDIF.

    CHECK lines( return ) > 0.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).

    SELECT *
      FROM /qaps/area
      FOR ALL ENTRIES IN @return
      WHERE id_area = @return-id_area
      INTO TABLE @DATA(lt_area).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      <fs>-icon = icon_green_light.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao.
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_area[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_variavel.

    SELECT *
      FROM /qaps/custo_elm
      INTO CORRESPONDING FIELDS OF TABLE @return.

    CHECK lines( return ) > 0.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).

    SELECT *
      FROM /qaps/area
      FOR ALL ENTRIES IN @return
      WHERE id_area = @return-id_area
      INTO TABLE @DATA(lt_area).

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).

      "Avaliar utilizações - para impossibiltar mudanças
      <fs>-icon = icon_green_light.

      IF NOT <fs>-id_tp_lista IS INITIAL.
        <fs>-dsc_tp_lista = value #( lt_tp_lista[ id_tp_lista = <fs>-id_tp_lista ]-descricao OPTIONAL ).
      ENDIF.

      <fs>-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                            iv_value  = CONV #( <fs>-escopo ) ).

      <fs>-dsc_area = VALUE #( lt_area[ id_area = <fs>-id_area ]-descricao OPTIONAL ).

      <fs>-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                            iv_value  = CONV #( <fs>-tipo_dado ) ).

      <fs>-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                            iv_value  = CONV #( <fs>-tipo_variavel  ) ).

      <fs>-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                            iv_value  = CONV #( <fs>-origem_dado  ) ).

    ENDLOOP.

    SORT return BY descricao ASCENDING.

  ENDMETHOD.


  METHOD get_variavel_by_id.

    SELECT SINGLE *
      FROM /qaps/custo_elm
      WHERE id_custo_elementar = @iv_id_custo_elementar
      INTO CORRESPONDING FIELDS OF @return.

    CHECK sy-subrc IS INITIAL.

    DATA(lo_model_tp_lista) = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
    DATA(lt_tp_lista) = lo_model_tp_lista->get_tipo_lista( ).

    SELECT *
      FROM /qaps/area
      WHERE id_area = @return-id_area
      INTO TABLE @DATA(lt_area).

    "Avaliar utilizações - para impossibiltar mudanças
    return-icon = icon_green_light.

    IF NOT return-id_tp_lista IS INITIAL.
      return-dsc_tp_lista = VALUE #( lt_tp_lista[ id_tp_lista = return-id_tp_lista ]-descricao OPTIONAL ).
    ENDIF.

    return-dsc_escopo = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ESCOPO'
                                                                          iv_value  = CONV #( return-escopo ) ).

    return-dsc_area = VALUE #( lt_area[ id_area = return-id_area ]-descricao OPTIONAL ).

    return-dsc_tipo_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_DADO'
                                                                          iv_value  = CONV #( return-tipo_dado ) ).

    return-dsc_tipo_variavel = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_TIPO_VARIAVEL'
                                                                          iv_value  = CONV #( return-tipo_variavel  ) ).

    return-dsc_origem_dado = /qaps/cl_helper_text=>get_domain_text( EXPORTING iv_domain = '/QAPS/D_ORIGEM_DADO'
                                                                              iv_value =  CONV #( return-origem_dado  ) ).


  ENDMETHOD.


  METHOD PREENCHER_MASTER_DATA.

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
ENDCLASS.
