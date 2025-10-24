*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFM_FNT_CMBF01.
*----------------------------------------------------------------------*
FORM frm_numbering.

  /qaps/v_fnt_cmb-id_fonte = cl_system_uuid=>create_uuid_x16_static( ).

ENDFORM.
FORM frm_dados_controle.
  DATA lr_data TYPE REF TO data.

  lr_data = REF #( /qaps/v_fnt_cmb ).
  /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  HIDE_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hide_fields OUTPUT.
  ASSIGN tctrl_/qaps/v_fnt_cmb-cols[ 1 ] TO FIELD-SYMBOL(<fs>).

  CHECK <fs> IS ASSIGNED.
  <fs>-invisible = 1.

ENDMODULE.
