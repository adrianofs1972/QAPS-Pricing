*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFM_CATEG_TF01.
*----------------------------------------------------------------------*
FORM frm_numbering.

  /qaps/v_categ_tr-id_categoria = cl_system_uuid=>create_uuid_x16_static( ).

ENDFORM.
FORM frm_dados_controle.
  data lr_data type ref to data.

  lr_data = ref #( /qaps/v_categ_tr ).
  /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  HIDE_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hide_fields OUTPUT.
  ASSIGN tctrl_/qaps/v_categ_tr-cols[ 1 ] TO FIELD-SYMBOL(<fs>).

  CHECK <fs> IS ASSIGNED.
  <fs>-invisible = 1.
ENDMODULE.
