*&---------------------------------------------------------------------*
*& Report /QAPS/FILL_COD_TRECHO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /qaps/fill_cod_trecho.


START-OF-SELECTION.

  DATA lt_trecho TYPE TABLE OF /qaps/trecho.

  SELECT *
    FROM /qaps/trecho
    INTO TABLE @DATA(lt_data).

  LOOP AT lt_data INTO DATA(ls_data).

    CHECK ls_data-cod_trecho IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = '/QAPS/TRCH'
*       QUANTITY    = '1'
*       SUBOBJECT   = ' '
*       TOYEAR      = '0000'
*       IGNORE_BUFFER                 = ' '
      IMPORTING
        number      = ls_data-cod_trecho
*       QUANTITY    =
*       RETURNCODE  =
*     EXCEPTIONS
*       INTERVAL_NOT_FOUND            = 1
*       NUMBER_RANGE_NOT_INTERN       = 2
*       OBJECT_NOT_FOUND              = 3
*       QUANTITY_IS_0                 = 4
*       QUANTITY_IS_NOT_1             = 5
*       INTERVAL_OVERFLOW             = 6
*       BUFFER_OVERFLOW               = 7
*       OTHERS      = 8
      .

    APPEND ls_data TO lt_trecho.

  ENDLOOP.

  IF lines( lt_trecho ) > 0.
    MODIFY /qaps/trecho FROM TABLE lt_trecho.
    message 'Atualização efetuada com sucesso' type 'S'.
  ELSE.
    message 'Não há dados para execução' type ''.
  ENDIF.
