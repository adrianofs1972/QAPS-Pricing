*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFM_CST_PRCF01.
*----------------------------------------------------------------------*
FORM frm_numbering.

  /qaps/v_cst_prc-id_processo = cl_system_uuid=>create_uuid_x16_static( ).

ENDFORM.
FORM frm_dados_controle.
  DATA lr_data TYPE REF TO data.

  lr_data = REF #( /qaps/v_cst_prc ).
  /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  HIDE_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hide_fields OUTPUT.
  ASSIGN tctrl_/qaps/v_cst_prc-cols[ 1 ] TO FIELD-SYMBOL(<fs>).

  CHECK <fs> IS ASSIGNED.
  <fs>-invisible = 1.

ENDMODULE.
