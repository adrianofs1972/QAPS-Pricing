FUNCTION /QAPS/FM_GRP_PRODUTO_INPUT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  /QAPS/S_GRUPO_PRODUTO
*"     VALUE(IV_ACTION) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(ES_DATA) TYPE  /QAPS/S_GRUPO_PRODUTO
*"     VALUE(ES_MESSAGE) TYPE  BAPIRET2
*"----------------------------------------------------------------------
*{   INSERT         ECDK9A0F42                                        1

  DATA: lv_x1 TYPE i,
        lv_x2 TYPE i,
        lv_y1 TYPE i,
        lv_y2 TYPE i.

  lv_x1 = 5.
  lv_y1 = 5.

  lv_x2 = 80.
  lv_y2 = 5.

  CALL SCREEN 1000 STARTING AT lv_x1 lv_y1
                   ENDING AT lv_x2 lv_y2.


  CASE sy-ucomm.
    WHEN 'OK'.
      es_data = CORRESPONDING #( gs_data ).
    WHEN 'CANCEL'.
      CLEAR: gs_data,
             es_data.

      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
    when others.
      CLEAR: gs_data,
             es_data.

      es_message = VALUE #( type = 'E' message = 'Operação cancelada pelo usuário' ).
  ENDCASE.

*}   INSERT
ENDFUNCTION.
