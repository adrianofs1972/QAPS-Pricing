*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFM_AREAF01.
*----------------------------------------------------------------------*
FORM frm_numbering.

  /qaps/v_area-id_area = cl_system_uuid=>create_uuid_x16_static( ).

ENDFORM.
FORM frm_dados_controle.
  DATA lr_data TYPE REF TO data.

  lr_data = REF #( /qaps/v_area ).
  /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  HIDE_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hide_fields OUTPUT.
  ASSIGN tctrl_/qaps/v_area-cols[ 1 ] TO FIELD-SYMBOL(<fs>).

  CHECK <fs> IS ASSIGNED.
  <fs>-invisible = 1.

ENDMODULE.
