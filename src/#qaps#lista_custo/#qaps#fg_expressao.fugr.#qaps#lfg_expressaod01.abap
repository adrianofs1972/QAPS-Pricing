*&---------------------------------------------------------------------*
*&  Include           /QAPS/LFG_EXPRESSAOD01
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
CLASS lcl_expr_eval DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_program_code IMPORTING iv_program    TYPE programm
                                 is_data       TYPE /qaps/s_expressao
                                 it_parameters TYPE /qaps/t_calc_parameter
                       RETURNING VALUE(return) TYPE /qaps/t_program_code.

ENDCLASS.

CLASS lcl_expr_eval IMPLEMENTATION.
  METHOD get_program_code.

    DATA: prog TYPE string,
*          return  TYPE STANDARD returnLE OF string,
          mess TYPE string.

    APPEND `PROGRAM ` && iv_program && `.`                        TO return.

    APPEND `FREE MEMORY ID 'QAPS_VALOR'.`                        TO return.

*    CASE is_data-tipo_dado.
*      WHEN '1'.
    APPEND `DATA ` && is_data-fieldname && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO return.
*      WHEN '2'.
*        APPEND `DATA ` && is_data-fieldname && ` TYPE /QAPS/PERCENTUAL.` TO return.
*    ENDCASE.

    "Variáveis
    LOOP AT it_parameters INTO DATA(ls_parameters).
      CASE ls_parameters-tipo_dado.
        WHEN '1'.
          APPEND `DATA ` && ls_parameters-fieldname && ` TYPE /QAPS/VALOR_ABSOLUTO.` TO return.
        WHEN '2'.
          APPEND `DATA ` && ls_parameters-fieldname && ` TYPE /QAPS/PERCENTUAL.` TO return.
      ENDCASE.
    ENDLOOP.

    LOOP AT it_parameters INTO ls_parameters.
      CASE ls_parameters-tipo_dado.
        WHEN '1'.
          APPEND ls_parameters-fieldname && ` = '` && ls_parameters-valor && `'.` TO return.
        WHEN '2'.
*          BREAK-POINT.
          APPEND ls_parameters-fieldname && ` = '` && ls_parameters-percentual && `'.` TO return.
          APPEND ls_parameters-fieldname && ` = ` && ls_parameters-fieldname && ` / 100.` TO return.
      ENDCASE.
    ENDLOOP.

    APPEND is_data-fieldname && ` = ` && is_data-expressao && `.` TO return.
*    APPEND `WRITE ` && is_data-fieldname && `.` TO return.
    APPEND `EXPORT LV_RESULT = ` && is_data-fieldname && ` TO MEMORY ID 'QAPS_VALOR' .` TO return.

  ENDMETHOD.
ENDCLASS.
*}   INSERT
