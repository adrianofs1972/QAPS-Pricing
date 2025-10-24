*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTI01.
*----------------------------------------------------------------------*

*{   INSERT         ECDK9A0F42                                        1
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
MODULE user_command_1000 INPUT.
*  break c060863.
  CASE sy-ucomm.
    WHEN 'TIPO_REGRA'.
      PERFORM set_tipo_regra.
    WHEN 'ORIGEM'.
      PERFORM set_origem.
    WHEN 'FN_ORIGEM'.
      PERFORM fill_list_agregador.
    WHEN 'DESTINO'.
      PERFORM set_destino.
    WHEN 'OK'.
      PERFORM f_ok.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_matnr INPUT.

  DATA: lr_matnr TYPE RANGE OF matnr,
        lv_where type string.

  IF gs_tipo_origem-index = 0.
    MESSAGE 'Selecionar origem' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  data(ls_data) = gt_tipo_origem[ gs_tipo_origem-index ].

*  break c060863.

  IF gv_origem_grp_planta = 'X'.
    lv_where = ` id_grp_planta = '` && ls_data-id_externo && `'`.
  ELSEIF gv_origem_centro = 'X'.
    lv_where = ` id_centro = '` && ls_data-id_externo && `'`.
  ENDIF.

  SELECT /qaps/prem_item~matnr
    FROM /qaps/prem_item
    INNER JOIN /qaps/prem_hdr
    ON /qaps/prem_item~id_premissa = /qaps/prem_hdr~id_premissa
    WHERE tipo_regra = 'MA'
    AND (lv_where)
    and   /qaps/prem_hdr~id_simulacao  = @gs_simulacao-id_simulacao
    INTO TABLE @DATA(lt_item).

  CHECK sy-subrc IS INITIAL.

  lr_matnr = VALUE #( FOR wa IN lt_item
                      ( sign = 'I'
                        option = 'EQ'
                        low = |{ wa-matnr ALPHA = IN WIDTH = 18 }| ) ).

  SELECT /qaps/material~matnr, maktx
  FROM /qaps/material
    INNER JOIN makt
    ON /qaps/material~matnr = makt~matnr
    WHERE makt~spras = @sy-langu
    AND /qaps/material~matnr IN @lr_matnr
    INTO TABLE @DATA(lt_matnr).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MATNR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'GS_DATA-MATNR'
      value_org   = 'S'
    TABLES
      value_tab   = lt_matnr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FILL_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fill_description INPUT.

  DATA lv_matnr TYPE matnr.

  IF NOT gs_data-matnr IS INITIAL.
    TRY.

        lv_matnr = |{ gs_data-matnr ALPHA = IN WIDTH  = 18 }|.
        gs_data-maktx = /qaps/cl_helper_text=>get_material_text( lv_matnr ).
      CATCH /qaps/cx_general.
        CLEAR: gs_data-matnr,
               gs_data-maktx.
        MESSAGE 'Material não existe' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ELSE.
    CLEAR gs_data-maktx.
  ENDIF.

ENDMODULE.
