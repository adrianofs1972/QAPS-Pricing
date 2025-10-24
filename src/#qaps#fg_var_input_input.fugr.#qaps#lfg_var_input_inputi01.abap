**----------------------------------------------------------------------*
****INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTI01.
**----------------------------------------------------------------------*
*
**{   INSERT         ECDK9A0F42                                        1
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_1000  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE exit_command INPUT.
*  LEAVE TO SCREEN 0.
*ENDMODULE.
*MODULE user_command_1000 INPUT.
*  CASE sy-ucomm.
*    WHEN 'TIPO_REGRA'.
*      PERFORM set_tipo_regra.
*    WHEN 'ORIGEM'.
*      PERFORM set_origem.
*    WHEN 'DESTINO'.
*      PERFORM set_destino.
*    WHEN 'OK' OR 'CANCEL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  F4_MATNR  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE f4_matnr INPUT.
**  BREAK-POINT.
*
*  SELECT /qaps/material~matnr, maktx
*  FROM /qaps/material
*    INNER JOIN makt
*    ON /qaps/material~matnr = makt~matnr
*    WHERE makt~spras = @sy-langu
*    INTO TABLE @DATA(lt_matnr).
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield    = 'MATNR'
*      dynpprog    = sy-repid
*      dynpnr      = sy-dynnr
*      dynprofield = 'GS_DATA-MATNR'
*      value_org   = 'S'
*    TABLES
*      value_tab   = lt_matnr.
*
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  FILL_DESCRIPTION  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE fill_description INPUT.
*
**  DATA lv_matnr TYPE matnr.
**
**  IF NOT gs_data-matnr IS INITIAL.
**    TRY.
**
**        lv_matnr = |{ gs_data-matnr ALPHA = IN WIDTH  = 18 }|.
**        gs_data-maktx = /qaps/cl_helper_text=>get_material_text( lv_matnr ).
**      CATCH /qaps/cx_general.
**        CLEAR: gs_data-matnr,
**               gs_data-maktx.
**        MESSAGE 'Material não existe' TYPE 'S' DISPLAY LIKE 'E'.
**    ENDTRY.
**      ELSE.
**        CLEAR gs_data-maktx.
**      ENDIF.
*
*ENDMODULE.
