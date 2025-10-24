*----------------------------------------------------------------------*
***INCLUDE /QAPS/LFG_TIPO_LISTA_INPUTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FILL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_alv .

  DATA lt_fcat TYPE lvc_t_fcat.
  DATA ls_layo TYPE lvc_s_layo.

  IF NOT go_alv IS BOUND.
    go_alv = NEW cl_gui_alv_grid( i_parent          = cl_gui_container=>default_screen ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = '/QAPS/S_CUSTO_ELEMENTAR'
      CHANGING
        ct_fieldcat      = lt_fcat.

    DELETE lt_fcat WHERE fieldname = 'ICON'
                        OR fieldname = 'MANDT'
                        OR fieldname = 'ID_CUSTO_ELEMENTAR'
                        OR fieldname = 'ID_TP_LISTA'
                        OR fieldname = 'ESCOPO'
                        OR fieldname = 'ID_AREA'
                        OR fieldname = 'TIPO_DADO'
                        OR fieldname = 'DSC_ORIGEM_DADO'
                        OR fieldname = 'TIPO_VARIAVEL'
                        OR fieldname = 'ORIGEM_DADO'
                        OR fieldname = 'CREATED_BY'
                        OR fieldname = 'CREATED_IN'
                        OR fieldname = 'CREATED_ON'
                        OR fieldname = 'MODIFIED_BY'
                        OR fieldname = 'MODIFIED_IN'
                        OR fieldname = 'MODIFIED_ON'
                        OR fieldname = 'UTILIZACOES'
                        OR fieldname = 'MESSAGE_TYPE'
                        OR fieldname = 'MESSAGE'.


    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs>).
      CASE <fs>-fieldname.
        WHEN 'DESCRICAO'.
          <fs>-hotspot = 'X'.
          <fs>-outputlen = '15'.
        WHEN 'MOEDA'.
          <fs>-outputlen = '7'.
        WHEN 'DSC_ESCOPO'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_TP_LISTA'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_AREA'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_TIPO_DADO'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_TIPO_VARIAVEL'.
          <fs>-outputlen = '15'.
        WHEN 'DSC_ORIGEM_DADO'.
          <fs>-outputlen = '15'.
      ENDCASE.
    ENDLOOP.

    ls_layo-zebra = 'X'.
    ls_layo-sel_mode = 'A'.
    ls_layo-no_toolbar = 'X'.

    go_event = NEW lcl_event( ).
    SET HANDLER go_event->on_hotspot_click FOR go_alv.

    go_alv->set_table_for_first_display(
      EXPORTING
*        i_buffer_active               =     " Buffering Active
*        i_bypassing_buffer            =     " Switch Off Buffer
*        i_consistency_check           =     " Starting Consistency Check for Interface Error Recognition
*        i_structure_name              =     " Internal Output Table Structure Name
*        is_variant                    =     " Layout
*        i_save                        =     " Save Layout
*        i_default                     = 'X'    " Default Display Variant
        is_layout                     = ls_layo    " Layout
*        is_print                      =     " Print Control
*        it_special_groups             =     " Field Groups
*        it_toolbar_excluding          =     " Excluded Toolbar Standard Functions
*        it_hyperlink                  =     " Hyperlinks
*        it_alv_graphics               =     " Table of Structure DTC_S_TC
*        it_except_qinfo               =     " Table for Exception Quickinfo
*        ir_salv_adapter               =     " Interface ALV Adapter
      CHANGING
        it_outtab                     = gt_data    " Output Table
        it_fieldcatalog               = lt_fcat    " Field Catalog
*        it_sort                       =     " Sort Criteria
*        it_filter                     =     " Filter Criteria
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        others                        = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    go_alv->refresh_table_display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data USING us_parent TYPE /qaps/s_calculation_node.

  REFRESH gt_data.
  DATA(lo_custo) = NEW /qaps/cl_mdl_custo_elementar( iv_action = 'C' ).
  gt_data = lo_custo->get_variaveis_by_tipo_lista( iv_id_tp_lista = us_parent-id_tp_lista
                                                   iv_id_parent   = us_parent-id_calc_node ).

ENDFORM.
