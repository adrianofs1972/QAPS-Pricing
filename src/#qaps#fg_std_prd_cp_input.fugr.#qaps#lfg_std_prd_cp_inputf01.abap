*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
FORM f4_matnr.

  DATA lt_return_tab TYPE TABLE OF ddshretval.

  SELECT /qaps/material~matnr, maktx
  FROM /qaps/material
  INNER JOIN makt
  ON /qaps/material~matnr = makt~matnr
  WHERE makt~spras = @sy-langu
  AND  NOT /qaps/material~matnr IN @gr_not_allowed
  INTO TABLE @DATA(lt_data).

  SORT lt_data BY matnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MATNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-COMPONENTE'
      value_org   = 'S'
    TABLES
      value_tab   = lt_data
      return_tab  = lt_return_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ok .

  DATA: lv_matnr TYPE matnr.

  IF gv_action = 'C'.
    lv_matnr = |{ gs_data-componente ALPHA = IN WIDTH = 18 }|.

    SELECT SINGLE *
      FROM mara
      WHERE matnr = @lv_matnr
      INTO @DATA(ls_mara).

    IF sy-subrc NE 0.
      MESSAGE 'Material não existe' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lv_matnr IN gr_not_allowed.
      MESSAGE 'Material não permitido' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_NOT_ALLOWED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IS_PA  text
*----------------------------------------------------------------------*
FORM fill_not_allowed  USING us_pa TYPE /qaps/s_std_producao_pa.

  DATA: lv_pa          TYPE matnr.

  REFRESH gr_not_allowed.

  lv_pa = |{ gs_pa-matnr ALPHA = IN WIDTH = 18 }|.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_pa ) TO gr_not_allowed.

  SELECT 'I' AS sign, 'EQ' AS option, componente AS low
      FROM /qaps/std_prd_cp
      WHERE id_std_producao = @gs_pa-id_std_producao
      AND   id_std_prod_pa  = @gs_pa-id_std_prod_pa
      APPENDING TABLE @gr_not_allowed.

  LOOP AT gr_not_allowed ASSIGNING FIELD-SYMBOL(<fs>).
    <fs>-low = |{ <fs>-low ALPHA = IN WIDTH = 18 }|.
  ENDLOOP.


ENDFORM.
