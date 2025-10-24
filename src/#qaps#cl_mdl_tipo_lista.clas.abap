class /QAPS/CL_MDL_TIPO_LISTA definition
  public
  inheriting from /QAPS/CL_MODEL_BASE
  final
  create public .

public section.

  methods GET_AREA_USERS
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_AREA type /QAPS/ED_ID_AREA
    returning
      value(RETURN) type /QAPS/T_AREA_USER .
  methods GET_USUARIOS_POR_AREA
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_AREA type /QAPS/ED_ID_AREA
    returning
      value(RETURN) type /QAPS/T_AREA_USER .
  methods GET_AREAS
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IV_ID_AREA type /QAPS/ED_ID_AREA optional
    returning
      value(RETURN) type /QAPS/T_AREA .
  methods GET_APROVADORES
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type /QAPS/T_LISTA_APROV .
  methods GET_TIPO_LISTA
    importing
      !IV_ID_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
    returning
      value(RETURN) type /QAPS/T_TP_LISTA .
  methods CREATE_APROVADORES
    importing
      value(IV_ID_TP_LISTA) type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_AREA_USER
    importing
      value(IV_ID_TP_LISTA) type /QAPS/ED_TP_LISTA
      value(IV_ID_AREA) type /QAPS/ED_ID_AREA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_AREAS
    importing
      value(IV_ID_TP_LISTA) type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods CREATE_TIPO_LISTA
    importing
      value(IS_DATA) type /QAPS/S_TP_LISTA
    returning
      value(RETURN) type /QAPS/ED_TP_LISTA .
  methods UPDATE_TIPO_LISTA
    importing
      !IS_DATA type /QAPS/S_TP_LISTA .
  methods CHANGE_STATUS_APROVADORES
    importing
      !IT_DATA type /QAPS/T_LISTA_APROV
      !IV_ACTION type UI_FUNC
    returning
      value(RETURN) type ABAP_BOOL .
  methods CHANGE_STATUS_AREA_USER
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IT_DATA type /QAPS/T_AREA_USER
      !IV_ACTION type UI_FUNC
    returning
      value(RETURN) type ABAP_BOOL .
  methods CHANGE_STATUS_AREAS
    importing
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
      !IT_DATA type /QAPS/T_AREA
      !IV_ACTION type UI_FUNC
    returning
      value(RETURN) type ABAP_BOOL .
  methods CHANGE_STATUS_TIPO_LISTA
    importing
      !IS_DATA type /QAPS/S_TP_LISTA
      !IV_ACTION type UI_FUNC
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_AREA_USER
    importing
      !IT_DATA type /QAPS/T_AREA_USER
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_AREAS
    importing
      !IT_DATA type /QAPS/T_AREA
      !IV_ID_TP_LISTA type /QAPS/ED_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_APROVADORES
    importing
      !IT_DATA type /QAPS/T_LISTA_APROV
    returning
      value(RETURN) type ABAP_BOOL .
  methods DELETE_TIPO_LISTA
    importing
      !IS_DATA type /QAPS/S_TP_LISTA
    returning
      value(RETURN) type ABAP_BOOL .
protected section.
private section.

  methods QUESTION
    importing
      !IV_MESSAGE type STRING
    returning
      value(RETURN) type ABAP_BOOL .
  methods GET_NEXT_TIPO_LISTA
    returning
      value(RETURN) type /QAPS/ED_TP_LISTA .
ENDCLASS.



CLASS /QAPS/CL_MDL_TIPO_LISTA IMPLEMENTATION.


  METHOD change_status_aprovadores.

    DATA lv_message TYPE string.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE iv_action.
      WHEN '&ACTIVE'.
        lv_message = TEXT-m02.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO DATA(ls_data).
            UPDATE /qaps/lista_aprv SET ativo = 'X' WHERE id_tp_lista = ls_data-id_tp_lista
                                                  AND uname = ls_data-uname.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi ativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
      WHEN '&INACTIVE'.
        lv_message = TEXT-m03.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO ls_data.
            UPDATE /qaps/lista_aprv SET ativo = '' WHERE id_tp_lista = ls_data-id_tp_lista
                                                  AND uname = ls_data-uname.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi inativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD change_status_areas.

    DATA lv_message TYPE string.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE iv_action.
      WHEN '&ACTIVE'.
        lv_message = TEXT-m02.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO DATA(ls_data).
            UPDATE /qaps/lista_area SET ativo = 'X' WHERE id_tp_lista = iv_id_tp_lista
                                                    AND id_area = ls_data-id_area.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi ativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
      WHEN '&INACTIVE'.
        lv_message = TEXT-m03.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO ls_data.
            UPDATE /qaps/lista_area SET ativo = '' WHERE id_tp_lista = iv_id_tp_lista
                                                    AND id_area = ls_data-id_area.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi inativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD change_status_area_user.

    DATA lv_message TYPE string.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE iv_action.
      WHEN '&ACTIVE'.
        lv_message = TEXT-m02.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO DATA(ls_data).
            UPDATE /qaps/area_user SET ativo = 'X' WHERE id_tp_lista = iv_id_tp_lista
                                                   AND id_area = ls_data-id_area
                                                   AND uname = ls_data-uname.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi ativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
      WHEN '&INACTIVE'.
        lv_message = TEXT-m03.
        IF question( lv_message ) = abap_true.
          LOOP AT it_data INTO ls_data.
            UPDATE /qaps/area_user SET ativo = '' WHERE id_tp_lista = iv_id_tp_lista
                                                  AND id_area = ls_data-id_area
                                                  AND uname = ls_data-uname.
          ENDLOOP.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi inativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD change_status_tipo_lista.

    DATA lv_message TYPE string.

    IF is_data is INITIAL.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE iv_action.
      WHEN 'ACTIVE'.
        lv_message = TEXT-m02.
        IF question( lv_message ) = abap_true.
          UPDATE /qaps/tp_lista SET ativo = 'X' WHERE id_tp_lista = is_data-id_tp_lista.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi ativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
      WHEN 'INACTIVE'.
        lv_message = TEXT-m03.
        IF question( lv_message ) = abap_true.
          UPDATE /qaps/tp_lista SET ativo = '' WHERE id_tp_lista = is_data-id_tp_lista.
          COMMIT WORK AND WAIT.
          MESSAGE 'O Item selecionado foi inativado com sucesso' TYPE 'S'.
          return = abap_true.
        ELSE.
          MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
          return = abap_false.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD create_aprovadores.

    DATA lr_data TYPE REF TO data.
    DATA: lt_data        TYPE /qaps/t_lista_aprov,
          lt_inactive    TYPE TABLE OF /qaps/lista_aprv,
          lt_new         TYPE TABLE OF /qaps/lista_aprv,
          ls_lista_aprov TYPE /qaps/lista_aprv,
          ls_message     TYPE bapiret2.

    IF iv_id_tp_lista IS INITIAL.
      MESSAGE 'Nenhum tipo de Lista selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_APROVADORES_INPUT'
      EXPORTING
        iv_id_tp_lista = iv_id_tp_lista
        iv_action      = 'C'
      IMPORTING
        et_data        = lt_data
        es_message     = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    IF lines( lt_data ) > 0.

      "Áreas que existiam e form retiradas, não serão deletadas, mas sim desativadas
      SELECT *
        FROM /qaps/lista_aprv
        WHERE id_tp_lista = @iv_id_tp_lista
        INTO TABLE @DATA(lt_aprov).

      LOOP AT lt_aprov INTO DATA(ls_aprov).

        CHECK NOT line_exists( lt_data[ uname = ls_aprov-uname ] ).
        ls_lista_aprov = CORRESPONDING #( ls_aprov ).
        ls_lista_aprov-id_tp_lista = iv_id_tp_lista.
        CLEAR ls_lista_aprov-ativo.
        APPEND ls_lista_aprov TO lt_inactive.

      ENDLOOP.

      LOOP AT lt_data INTO DATA(ls_data).

        CHECK NOT line_exists( lt_aprov[ uname = ls_data-uname ] ).
        ls_lista_aprov = CORRESPONDING #( ls_data ).
        ls_lista_aprov-id_tp_lista = iv_id_tp_lista.
        ls_lista_aprov-ativo = 'X'.
        APPEND ls_lista_aprov TO lt_new.

      ENDLOOP.

    ENDIF.

    LOOP AT lt_inactive REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/lista_aprv FROM TABLE lt_inactive.

    LOOP AT lt_new REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/lista_aprv FROM TABLE lt_new.

    return = abap_true.

  ENDMETHOD.


  METHOD create_areas.

    DATA lr_data TYPE REF TO data.
    DATA: lt_data       TYPE /qaps/t_area,
          lt_inactive   TYPE TABLE OF /qaps/lista_area,
          lt_new        TYPE TABLE OF /qaps/lista_area,
          ls_lista_area TYPE /qaps/lista_area,
          ls_message    TYPE bapiret2.

    if iv_id_tp_lista is INITIAL.
      message 'Nenhum tipo de Lista selecionado' type 'S' DISPLAY LIKE 'E'.
      return.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_AREA_INPUT'
      EXPORTING
        iv_id_tp_lista = iv_id_tp_lista
        iv_action      = 'C'
      IMPORTING
        et_data        = lt_data
        es_message     = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    IF lines( lt_data ) > 0.

      "Áreas que existiam e form retirdas, não serão deletadas, mas sim desativadas
      SELECT *
        FROM /qaps/lista_area
        WHERE id_tp_lista = @iv_id_tp_lista
        INTO TABLE @DATA(lt_area).

      LOOP AT lt_area INTO DATA(ls_area).

        CHECK NOT line_exists( lt_data[ id_area = ls_area-id_area ] ).
        ls_lista_area = CORRESPONDING #( ls_area ).
        ls_lista_area-id_tp_lista = iv_id_tp_lista.
        CLEAR ls_lista_area-ativo.
        APPEND ls_lista_area TO lt_inactive.

      ENDLOOP.

      LOOP AT lt_data INTO data(ls_data).

        CHECK NOT line_exists( lt_area[ id_area = ls_data-id_area ] ).
        ls_lista_area = CORRESPONDING #( ls_data ).
        ls_lista_area-id_tp_lista = iv_id_tp_lista.
        ls_lista_area-ativo = 'X'.
        APPEND ls_lista_area TO lt_new.

      ENDLOOP.

    ENDIF.

    loop at lt_inactive REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/lista_area FROM TABLE lt_inactive.

    loop at lt_new REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    endloop.
    MODIFY /qaps/lista_area FROM TABLE lt_new.

    return = abap_true.

  ENDMETHOD.


  METHOD create_area_user.

    DATA lr_data TYPE REF TO data.
    DATA: lt_data     TYPE /qaps/t_area_user,
          lt_inactive TYPE TABLE OF /qaps/area_user,
          lt_new      TYPE TABLE OF /qaps/area_user,
          ls_entry    TYPE /qaps/area_user,
          ls_message  TYPE bapiret2.

    IF iv_id_tp_lista IS INITIAL.
      MESSAGE 'Nenhum Tipo de Lista selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF iv_id_area IS INITIAL.
      MESSAGE 'Nenhuma Área selecionada' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION '/QAPS/FM_AREA_USER_INPUT'
      DESTINATION 'NONE'
      EXPORTING
        iv_id_tp_lista = iv_id_tp_lista
        iv_id_area     = iv_id_area
        iv_action      = 'C'
      IMPORTING
        et_data        = lt_data
        es_message     = ls_message.

    IF ls_message-type = 'E'.
      MESSAGE ls_message-message TYPE 'S' DISPLAY LIKE ls_message-type.
      RETURN.
    ENDIF.

    IF lines( lt_data ) > 0.

      "Áreas que existiam e form retirdas, não serão deletadas, mas sim desativadas
      SELECT *
        FROM /qaps/area_user
        WHERE id_tp_lista = @iv_id_tp_lista
        AND   id_area     = @iv_id_area
        INTO TABLE @DATA(lt_area_user).

      LOOP AT lt_area_user INTO DATA(ls_area_user).

        CHECK NOT line_exists( lt_data[ uname = ls_area_user-uname ] ).
        ls_entry = CORRESPONDING #( ls_area_user ).
        ls_entry-id_tp_lista = iv_id_tp_lista.
        ls_entry-id_area = iv_id_area.
        CLEAR ls_entry-ativo.
        APPEND ls_entry TO lt_inactive.

      ENDLOOP.

      LOOP AT lt_data INTO DATA(ls_data).

        CHECK NOT line_exists( lt_area_user[ uname = ls_data-uname ] ).
        ls_entry = CORRESPONDING #( ls_data ).
        ls_entry-id_tp_lista = iv_id_tp_lista.
        ls_entry-id_area = iv_id_area.
        ls_entry-ativo = 'X'.
        APPEND ls_entry TO lt_new.

      ENDLOOP.

    ENDIF.

    LOOP AT lt_inactive REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/area_user FROM TABLE lt_inactive.

    LOOP AT lt_new REFERENCE INTO lr_data.
      preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.
    MODIFY /qaps/area_user FROM TABLE lt_new.

    return = abap_true.

  ENDMETHOD.


  METHOD create_tipo_lista.

    DATA lr_data TYPE REF TO data.
    DATA: ls_data     TYPE /qaps/s_tp_lista,
          ls_tp_lista TYPE /qaps/tp_lista,
          ls_message  TYPE bapiret2.

    CALL FUNCTION '/QAPS/FM_TIPO_LISTA_INPUT'
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

    ls_tp_lista = CORRESPONDING #( ls_data ).
    ls_tp_lista-id_tp_lista = get_next_tipo_lista( ).

    lr_data = REF #( ls_tp_lista ).
    preencher_dados_controle( CHANGING cr_data = lr_data ).

    MODIFY /qaps/tp_lista FROM ls_tp_lista.

    return = ls_tp_lista-id_tp_lista.


  ENDMETHOD.


  METHOD delete_aprovadores.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.
      LOOP AT it_data INTO DATA(ls_data).
        DELETE FROM /qaps/lista_aprv WHERE id_tp_lista = ls_data-id_tp_lista
                                       AND uname = ls_data-uname.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_areas.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF iv_id_tp_lista IS INITIAL.
        MESSAGE 'Nenhum tipo de Lista selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ELSEIF lines( it_data ) = 0.
        MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    IF question( lv_message ) = abap_true.

      LOOP AT it_data INTO DATA(ls_data).

        DELETE FROM /qaps/area_user WHERE id_tp_lista = iv_id_tp_lista
                                    AND   id_area = ls_data-id_area.

        DELETE FROM /qaps/lista_area WHERE id_tp_lista = iv_id_tp_lista
                               AND   id_area = ls_data-id_area.
      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD DELETE_AREA_USER.

    DATA lv_message TYPE string.

    lv_message = TEXT-m01.

    IF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF iv_id_tp_lista IS INITIAL.
        MESSAGE 'Nenhum tipo de Lista selecionado' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ELSEIF lines( it_data ) = 0.
      MESSAGE 'Nenhum item foi selecionado' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF question( lv_message ) = abap_true.

      LOOP AT it_data INTO DATA(ls_data).

        DELETE FROM /qaps/area_user WHERE id_tp_lista = iv_id_tp_lista
                                    AND   id_area = ls_data-id_area
                                    and   uname   = ls_data-uname.

      ENDLOOP.

      COMMIT WORK AND WAIT.

      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD delete_tipo_lista.

    DATA lv_message TYPE string.

    lv_message = text-m01.

    IF question( lv_message ) = abap_true.
      DELETE FROM /qaps/area_user WHERE id_tp_lista = is_data-id_tp_lista.
      DELETE FROM /qaps/lista_area WHERE id_tp_lista = is_data-id_tp_lista.
      DELETE FROM /qaps/lista_aprv WHERE id_tp_lista = is_data-id_tp_lista.
      DELETE FROM /qaps/lista_area WHERE id_tp_lista = is_data-id_tp_lista.
      DELETE FROM /qaps/tp_lista WHERE id_tp_lista = is_data-id_tp_lista.
      return = abap_true.
    ELSE.
      MESSAGE 'Operação cancelada pelo usuário' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD get_aprovadores.

    SELECT *
      FROM /qaps/lista_aprv
      WHERE id_tp_lista = @iv_id_tp_lista
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-fullname = get_user_fullname( <fs>-uname ).
    ENDLOOP.

    SORT return BY uname ASCENDING.

  ENDMETHOD.


  METHOD get_areas.

    IF iv_id_area IS INITIAL.
      SELECT *
        FROM /qaps/v_ls_area
        WHERE id_tp_lista = @iv_id_tp_lista
        INTO CORRESPONDING FIELDS OF TABLE @return.
    ELSE.
      SELECT *
       FROM /qaps/v_ls_area
       WHERE id_tp_lista = @iv_id_tp_lista
       AND   id_area     = @iv_id_area
       INTO CORRESPONDING FIELDS OF TABLE @return.
    ENDIF.

    SORT return BY id_area ASCENDING.

  ENDMETHOD.


  METHOD get_area_users.

    SELECT *
      FROM /qaps/area_user
      WHERE id_tp_lista = @iv_id_tp_lista
      AND   id_area     = @iv_id_area
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-fullname = get_user_fullname( <fs>-uname ).
    ENDLOOP.

    SORT return BY uname ASCENDING.

  ENDMETHOD.


  METHOD get_next_tipo_lista.

    return = cl_system_uuid=>create_uuid_x16_static( ).
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr = '01'
*        object      = '/QAPS/TPLS'
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


  METHOD get_tipo_lista.

    IF iv_id_tipo_lista IS INITIAL.

      SELECT *
        FROM /qaps/tp_lista
        INTO CORRESPONDING FIELDS OF TABLE return.

    ELSE.

      SELECT *
        FROM /qaps/tp_lista
        WHERE id_tp_lista = @iv_id_tipo_lista
        INTO CORRESPONDING FIELDS OF TABLE @return.

    ENDIF.

    SORT return BY descricao DESCENDING.

  ENDMETHOD.


  METHOD get_usuarios_por_area.

    SELECT *
      FROM /qaps/area_user
      WHERE id_tp_lista = @iv_id_tp_lista
      AND   id_area     = @iv_id_area
      INTO CORRESPONDING FIELDS OF TABLE @return.

    LOOP AT return ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-fullname = get_user_fullname( <fs>-uname ).
    ENDLOOP.

    SORT return BY uname ASCENDING.

  ENDMETHOD.


  METHOD question.

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
ENDCLASS.
