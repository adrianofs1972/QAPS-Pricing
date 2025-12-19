*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Form  FILL_SOURCE_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_ID_TP_LISTA  text
*----------------------------------------------------------------------*
FORM fill_source_target  USING uv_id TYPE /qaps/ed_tp_lista.

*  DATA: lr_area_exclude TYPE RANGE OF /qaps/ed_id_area,
*        lr_area_include TYPE RANGE OF /qaps/ed_id_area.
*
*  REFRESH: gt_source,
*           gt_target.
*
*  SELECT *
*    FROM /qaps/lista_area
*    WHERE id_tp_lista = @uv_id
*    INTO TABLE @DATA(lt_lista).
*
*  IF lines( lt_lista ) > 0.
*    lr_area_exclude = VALUE #( FOR wa IN lt_lista
*                       ( sign  = 'E' option = 'EQ' low = wa-id_area ) ).
*
*    lr_area_include = VALUE #( FOR wa IN lt_lista
*                                ( sign  = 'I' option = 'EQ' low = wa-id_area ) ).
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_exclude
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*
*    SELECT *
*        FROM /qaps/area
*        WHERE id_area IN @lr_area_include
*        INTO CORRESPONDING FIELDS OF  TABLE @gt_target.
*
*  ELSE.
*    SELECT *
*      FROM /qaps/area
*      INTO CORRESPONDING FIELDS OF  TABLE @gt_source.
*  ENDIF.


ENDFORM.
