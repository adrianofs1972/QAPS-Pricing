class /QAPS/CL_CTRL_TIPO_LISTA definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods INITIALIZE
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER .
protected section.
private section.

  types:
    BEGIN OF ts_container,
      tipo_lista     TYPE REF TO cl_gui_container,
      tipo_lista_hdr TYPE REF TO cl_gui_container,
      lista_aprov    TYPE REF TO cl_gui_container,
      lista_area     TYPE REF TO cl_gui_container,
      area_user      TYPE REF TO cl_gui_container,
    END OF ts_container .
  types:
    BEGIN OF ts_update_controls,
           tipo_lista     TYPE abap_bool,
           tipo_lista_hdr TYPE abap_bool,
           lista_aprov    TYPE abap_bool,
           lista_area     TYPE abap_bool,
           area_user      TYPE abap_bool,
         END OF ts_update_controls .

  data MV_ID_TP_LISTA type /QAPS/ED_TP_LISTA .
  data MV_ID_AREA type /QAPS/ED_ID_AREA .
  data MO_MODEL type ref to /QAPS/CL_MDL_TIPO_LISTA .
  data MT_TIPO_LISTA type /QAPS/T_TP_LISTA .
  data MV_INITIALIZED type ABAP_BOOL .
  data MO_SPLITTER_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_SPLITTER_CHILD type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_TIPO_LISTA_HDR type ref to /QAPS/CL_VIEW_TIPO_LISTA_HDR .
  data MO_VIEW_LISTA_AREA type ref to /QAPS/CL_VIEW_LISTA_AREA .
  data MO_VIEW_AREA_USER type ref to /QAPS/CL_VIEW_AREA_USER .
  data MO_VIEW_LISTA_APROV type ref to /QAPS/CL_VIEW_LISTA_APROV .
  data MT_LISTA_AREA type /QAPS/T_AREA .
  data MT_AREA_USER type /QAPS/T_AREA_USER .
  data MT_LISTA_APROV type /QAPS/T_LISTA_APROV .
  data MO_BAR type ref to CL_GUI_CONTAINER_BAR_2 .
  data MO_SPLITTER_AREA type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_VIEW_TIPO_LISTA type ref to /QAPS/CL_VIEW_SIMP_TIPO_LISTA .

  methods ON_HOTSPOT_CLICK
    for event ON_HOTSPOT_CLICK of /QAPS/CL_VIEW_ALV_BASE
    importing
      !IV_SOURCE
      !IS_ROW_ID
      !IS_COLUMN_ID
      !IS_ROW_NO
      !IR_DATA .
  methods ON_USER_COMMAND_CHANGE_STATUS
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_REMOVE
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1
      !IV_XML_DATA type STRING .
  methods ON_USER_COMMAND_CREATE
    importing
      !IV_UCOMM type SYUCOMM
      !IV_SOURCE type CHAR20
      !IV_ACTION type CHAR1 .
  methods ON_USER_COMMAND
    for event ON_USER_COMMAND of /QAPS/CL_VIEW_ALV_BASE
    importing
      !IV_UCOMM
      !IV_SOURCE
      !IV_ACTION
      !IV_XML_DATA .
  methods UPDATE_AREA_USER
    importing
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA
      !IR_DATA type ref to DATA .
  methods UPDATE_LISTA_AREA
    importing
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA .
  methods ON_NODE_DOUBLE_CLICK
    for event ON_NODE_DOUBLE_CLICK of /QAPS/CL_VIEW_SIMPLE_TREE_BASE
    importing
      !IV_NODE_KEY
      !IV_XML_DATA .
  methods UPDATE_APROVADORES
    importing
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE_TIPO_LISTA_HEADER
    importing
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods UPDATE_TIPO_LISTA
    importing
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA optional .
  methods REFRESH
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
      !IV_ID_AREA type /QAPS/ED_ID_AREA optional
      !IR_DATA type ref to DATA optional .
  methods UPDATE
    importing
      !IS_CONTROLS type TS_UPDATE_CONTROLS
      !IV_TIPO_LISTA type /QAPS/ED_TP_LISTA optional
      !IV_ID_AREA type /QAPS/ED_ID_AREA optional
      !IR_DATA type ref to DATA optional .
  methods ON_FUNCTION_SELECTED
    for event ON_FUNCTION_SELECTED of /QAPS/CL_VIEW_SIMPLE_TREE_BASE
    importing
      !IV_FUNCTION
      !IV_XML_DATA .
  methods CREATE_MAIN_CONTAINER
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER
    returning
      value(RETURN) type ref to CL_GUI_SPLITTER_CONTAINER .
  methods INITIALIZE_COMPONENTES
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  methods GET_CONTAINERS
    importing
      !IO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER
    returning
      value(RETURN) type /QAPS/CL_CTRL_TIPO_LISTA=>TS_CONTAINER .
ENDCLASS.



CLASS /QAPS/CL_CTRL_TIPO_LISTA IMPLEMENTATION.


  METHOD constructor.
    mo_model = NEW /qaps/cl_mdl_tipo_lista( iv_action = 'C' ).
  ENDMETHOD.


  METHOD create_main_container.

    mo_splitter_parent = NEW cl_gui_splitter_container(
*        link_dynnr              =
*        link_repid              =
*        shellstyle              =
*        left                    =
*        top                     =
*        width                   =
*        height                  =
*        metric                  = CNTL_METRIC_DYNPRO
*        align                   = 15
           parent                  = io_container
           rows                    = 1
           columns                 = 2
*        no_autodef_progid_dynnr =
*        name                    =
       ).


    mo_splitter_parent->set_column_width( id = 1 width = 25 ).
    mo_splitter_parent->set_border(
      EXPORTING
        border            = ''    " Draw Frame (gfw_true); Do Not Draw Frame (gfw_false)
*      EXCEPTIONS
*        cntl_error        = 1
*        cntl_system_error = 2
*        others            = 3
    ).


    return = mo_splitter_parent.


  ENDMETHOD.


  METHOD get_containers.

    "treview
    return-tipo_lista = io_splitter->get_container( row       = 1    " Row
                                                    column    = 1     ).

    data(lo_parent_child) = io_splitter->get_container( row       = 1    " Row
                                                        column    = 2     ).

    mo_splitter_child = new cl_gui_splitter_container(
        parent                  = lo_parent_child
        rows                    = 2
        columns                 = 1 ).

    mo_splitter_child->set_row_height( id = 1 height = 15 ).

    "Html Header
    return-tipo_lista_hdr = mo_splitter_child->get_container( row       = 1    " Row
                                                              column    = 1     ).

    data(lo_bar_container) = mo_splitter_child->get_container( row       = 2    " Row
                                                               column    = 1     ).

    mo_bar = new cl_gui_container_bar_2(
*        active_id                    =
        captions                     = value sbptcaptns( ( caption  = 'Aprovadores' ) ( caption  = 'Área/Usuários' ) )
        parent                       = lo_bar_container
    ).

    "Aprovadores
    return-lista_aprov = mo_bar->get_container( id = 1 ).

    data(lo_container_area) = mo_bar->get_container( id = 2 ).

    mo_splitter_area = new cl_gui_splitter_container( parent                  = lo_container_area
                                                      rows                    = 2
                                                      columns                 = 1 ).

    return-lista_area = mo_splitter_area->get_container( row       = 1    " Row
                                                         column    = 1     ).

    return-area_user = mo_splitter_area->get_container( row       = 2    " Row
                                                        column    = 1     ).


  ENDMETHOD.


  METHOD initialize.

    CHECK mv_initialized = abap_false.

    DATA(lo_splitter) = create_main_container( io_container  ).
    initialize_componentes( lo_splitter ).

    mv_initialized = abap_true.

    update( EXPORTING is_controls   = value #( tipo_lista     = abap_true
                                               tipo_lista_hdr = abap_false
                                               lista_aprov    = abap_false
                                               lista_area     = abap_false
                                               area_user      = abap_false
                                              ) ).

  ENDMETHOD.


  METHOD initialize_componentes.

    DATA(ls_containers) = get_containers( io_splitter ).

    mo_view_tipo_lista = NEW /qaps/cl_view_simp_tipo_lista( ).
    SET HANDLER: on_function_selected FOR mo_view_tipo_lista,
                 on_node_double_click FOR mo_view_tipo_lista.
    mo_view_tipo_lista->initialize( io_container = ls_containers-tipo_lista
                                           iv_root_text = 'Tipo de Lista'
                                           iv_toolbar = abap_true ).

    "Html - Tipo Lista
    mo_view_tipo_lista_hdr = NEW /qaps/cl_view_tipo_lista_hdr( ).
    mo_view_tipo_lista_hdr->initialize( ls_containers-tipo_lista_hdr ).


    "Aprovadores
    mo_view_lista_aprov = NEW /qaps/cl_view_lista_aprov( ).
    SET HANDLER on_user_command FOR mo_view_lista_aprov.
    mo_view_lista_aprov->initialize( ir_outtab            = REF #( mt_lista_aprov )
                                    io_container         = ls_containers-lista_aprov
                                    is_catalog_structure = '/QAPS/S_LISTA_APROV'
                                    iv_action = 'C' ).

    "Área
    mo_view_lista_area = NEW /qaps/cl_view_lista_area( ).
    SET HANDLER: on_user_command FOR mo_view_lista_area,
                 on_hotspot_click FOR mo_view_lista_area.
    mo_view_lista_area->initialize( ir_outtab            = REF #( mt_lista_area )
                                    io_container         = ls_containers-lista_area
                                    is_catalog_structure = '/QAPS/S_AREA'
                                    iv_action = 'C' ).

    "Usuário x Área
    mo_view_area_user = NEW /qaps/cl_view_area_user( ).
    SET HANDLER on_user_command FOR mo_view_area_user.
    mo_view_area_user->initialize( ir_outtab            = REF #( mt_area_user )
                                    io_container         = ls_containers-area_user
                                    is_catalog_structure = '/QAPS/S_AREA_USER'
                                    iv_action = 'C' ).


  ENDMETHOD.


  METHOD on_function_selected.

    DATA ls_data TYPE /qaps/s_tp_lista.
    DATA lr_data TYPE REF TO data.

    CASE iv_function.
      WHEN 'INSERT'.
        DATA(lv_id_tp_lista) = mo_model->create_tipo_lista( is_data = VALUE #( ) ).
        IF NOT lv_id_tp_lista IS INITIAL.
          update(
            EXPORTING
              is_controls   = VALUE #( tipo_lista = abap_true )
              iv_tipo_lista = lv_id_tp_lista ).
        ENDIF.
      WHEN 'REMOVE'.

        lr_data =  REF #( ls_data ).
        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                             CHANGING  cr_data = lr_data ).

        DATA(lv_return) = mo_model->delete_tipo_lista( ls_data ).

        IF lv_return = abap_true.
          update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true ) ).
          refresh( EXPORTING is_controls   = VALUE #( tipo_lista_hdr = abap_true
                                                      lista_aprov = abap_true
                                                      lista_area = abap_true
                                                      area_user = abap_true ) ).
        ENDIF.

      WHEN 'ACTIVE' OR 'INACTIVE'.

        lr_data =  REF #( ls_data ).
        /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                             CHANGING  cr_data = lr_data ).

        lv_return = mo_model->change_status_tipo_lista( is_data = ls_data
                                                        iv_action = iv_function ).

        IF lv_return = abap_true.
          update( EXPORTING is_controls   = VALUE #( tipo_lista = abap_true
                                                     tipo_lista_hdr = abap_true )
                            iv_tipo_lista = ls_data-id_tp_lista ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD on_hotspot_click.

    FIELD-SYMBOLS <fs_area> TYPE /qaps/s_area.

    CASE iv_source.
      WHEN 'LISTA_AREA'.

        ASSIGN ir_data->* TO <fs_area>.

        update( EXPORTING is_controls   = VALUE #( area_user      = abap_true )
                          iv_tipo_lista = mv_id_tp_lista
*                          iv_id_area    = mv_id_area
                          ir_data       = ir_data ).

        mv_id_area = <fs_area>-id_area.

    ENDCASE.

  ENDMETHOD.


  METHOD on_node_double_click.

    DATA: ls_data TYPE /qaps/s_tp_lista,
          lr_data TYPE REF TO data.

    lr_data = REF #( ls_data ).

    /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  =  iv_xml_data
                                         CHANGING  cr_data = lr_data ).

    mv_id_tp_lista = ls_data-id_tp_lista.

    update( EXPORTING is_controls   = VALUE #( tipo_lista     = abap_false
                                               tipo_lista_hdr = abap_true
                                               lista_aprov    = abap_true
                                               lista_area     = abap_true
                                               area_user      = abap_false
                                              )
                      iv_tipo_lista = ls_data-id_tp_lista ).

    mo_view_area_user->reset( ).


    CLEAR mv_id_area.
  ENDMETHOD.


  METHOD on_user_command.

    CASE iv_ucomm.
      WHEN '&ADD'.
        on_user_command_create( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action ).
      WHEN '&REMOVE'.
        on_user_command_remove( EXPORTING iv_ucomm  = iv_ucomm
                                          iv_source = iv_source
                                          iv_action = iv_action
                                          iv_xml_data = iv_xml_data ).
       WHEN '&ACTIVE' OR '&INACTIVE'.
        on_user_command_change_status( EXPORTING iv_ucomm  = iv_ucomm
                                                 iv_source = iv_source
                                                 iv_action = iv_action
                                                 iv_xml_data = iv_xml_data ).
    ENDCASE.

  ENDMETHOD.


  METHOD on_user_command_change_status.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'LISTA_APROV'.
        DATA lt_aprov TYPE /qaps/t_lista_aprov.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_aprov ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->change_status_aprovadores( it_data = lt_aprov
                                                               iv_action = iv_ucomm ).
        ls_update_controls-lista_aprov = abap_true.
      WHEN 'LISTA_AREA'.
        DATA lt_area TYPE /qaps/t_area.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_area ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->change_status_areas( iv_id_tp_lista = mv_id_tp_lista
                                                   it_data = lt_area
                                                   iv_action = iv_ucomm ).
        ls_update_controls-lista_area = abap_true.
      WHEN 'AREA_USER'.
        DATA lt_area_user TYPE /qaps/t_area_user.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_area_user ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->change_status_area_user( iv_id_tp_lista = mv_id_tp_lista
                                                       it_data = lt_area_user
                                                       iv_action = iv_ucomm ).
        IF lv_return = abap_true.
          DATA(lt_area_ref) = mo_model->get_areas( iv_id_tp_lista = mv_id_tp_lista
                                         iv_id_area     = mv_id_area ).

          lr_data =  REF #( lt_area_ref[ 1 ] ).
          ls_update_controls-area_user = abap_true.
        ENDIF.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_tipo_lista = mv_id_tp_lista    " QAPS: Id Tipo de Lista Pricing
        iv_id_area    = mv_id_area    " QAPS:Id Área
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD ON_USER_COMMAND_CREATE.

    DATA ls_update_controls TYPE ts_update_controls.

    CASE iv_source.
      WHEN 'LISTA_AREA'.
        DATA(lv_return) = mo_model->create_areas( mv_id_tp_lista ).
        ls_update_controls-lista_area = abap_true.
      WHEN 'LISTA_APROV'.
        lv_return = mo_model->create_aprovadores( mv_id_tp_lista ).
        ls_update_controls-lista_aprov = abap_true.
      WHEN 'AREA_USER'.
        lv_return = mo_model->create_area_user( iv_id_tp_lista =  mv_id_tp_lista
                                    iv_id_area = mv_id_area ).
        IF lv_return = abap_true.
          DATA(lt_area) = mo_model->get_areas( iv_id_tp_lista = mv_id_tp_lista
                                               iv_id_area = mv_id_area ).

          DATA(lr_data) = REF #( lt_area[ 1 ] ).
          ls_update_controls-area_user = abap_true.
        ENDIF.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_tipo_lista = mv_id_tp_lista    " QAPS: Id Tipo de Lista Pricing
        iv_id_area    = mv_id_area    " QAPS:Id Área
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD on_user_command_remove.

    DATA: ls_update_controls TYPE ts_update_controls,
          lr_data            TYPE REF TO data.

    CASE iv_source.
      WHEN 'LISTA_APROV'.
        DATA lt_aprov TYPE /qaps/t_lista_aprov.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_aprov ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        DATA(lv_return) = mo_model->delete_aprovadores( lt_aprov ).
        ls_update_controls-lista_aprov = abap_true.
      WHEN 'LISTA_AREA'.
        DATA lt_area TYPE /qaps/t_area.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_area ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_areas( iv_id_tp_lista = mv_id_tp_lista
                                            it_data = lt_area ).
        ls_update_controls-lista_area = abap_true.
      WHEN 'AREA_USER'.
        DATA lt_area_user TYPE /qaps/t_area_user.
        IF NOT iv_xml_data IS INITIAL.
          lr_data = REF #( lt_area_user ).
          /qaps/cl_serialization=>deserialize( EXPORTING iv_xml  = iv_xml_data
                                               CHANGING  cr_data = lr_data ).
        ENDIF.
        lv_return = mo_model->delete_area_user( iv_id_tp_lista = mv_id_tp_lista
                                                it_data = lt_area_user ).

        IF lv_return = abap_true.
          DATA(lt_area_ref) = mo_model->get_areas( iv_id_tp_lista = mv_id_tp_lista
                                         iv_id_area     = mv_id_area ).

          lr_data =  REF #( lt_area_ref[ 1 ] ).
          ls_update_controls-area_user = abap_true.
        ENDIF.
    ENDCASE.

    CHECK lv_return = abap_true.

    update(
      EXPORTING
        is_controls   = ls_update_controls
        iv_tipo_lista = mv_id_tp_lista    " QAPS: Id Tipo de Lista Pricing
        iv_id_area    = mv_id_area    " QAPS:Id Área
        ir_data       = lr_data
    ).

  ENDMETHOD.


  METHOD refresh.

    IF is_controls-tipo_lista = abap_true.
*      update_tipo_lista( iv_tipo_lista = iv_tipo_lista ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
*      update_tipo_lista_header( iv_tipo_lista = iv_tipo_lista ).
      mo_view_tipo_lista_hdr->refresh( ).
    ENDIF.

    IF is_controls-lista_aprov = abap_true.
      mo_view_lista_aprov->reset( ).
    ENDIF.

    IF is_controls-lista_area = abap_true.
      mo_view_lista_area->reset( ).
    ENDIF.

    IF is_controls-area_user = abap_true.
      mo_view_area_user->reset( ).
    ENDIF.

  ENDMETHOD.


  METHOD update.

    IF is_controls-tipo_lista = abap_true.
      update_tipo_lista( iv_tipo_lista = iv_tipo_lista ).
    ENDIF.

    IF is_controls-tipo_lista_hdr = abap_true.
      update_tipo_lista_header( iv_tipo_lista = iv_tipo_lista ).
    ENDIF.

    IF is_controls-lista_aprov = abap_true.
      update_aprovadores( iv_tipo_lista = iv_tipo_lista ).
    ENDIF.

    IF is_controls-lista_area = abap_true.
      update_lista_area( iv_tipo_lista = iv_tipo_lista ).
    ENDIF.

    IF is_controls-area_user = abap_true.
      update_area_user( iv_tipo_lista = iv_tipo_lista
                        ir_data = ir_data ).
    ENDIF.

  ENDMETHOD.


  METHOD update_aprovadores.

    DATA(lt_data) = mo_model->get_aprovadores( iv_tipo_lista ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_lista_aprov->set_data( lr_data ).

  ENDMETHOD.


  METHOD update_area_user.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_area.

    ASSIGN ir_data->* TO <fs>.

    DATA(lt_data) = mo_model->get_area_users(
                    iv_id_tp_lista = iv_tipo_lista
                    iv_id_area     = <fs>-id_area ).

    DATA(lr_data) = REF #( lt_data ).
    mo_view_area_user->set_data( ir_outtab = lr_data
                                 ir_parent = ir_data
                                 iv_soft_refresh = abap_false ).

  ENDMETHOD.


  METHOD update_lista_area.

    DATA(lt_data) = mo_model->get_areas( iv_tipo_lista ).
    DATA(lr_data) = REF #( lt_data ).
    mo_view_lista_area->set_data( ir_outtab = lr_data
                                  iv_soft_refresh = abap_false ).

  ENDMETHOD.


  METHOD update_tipo_lista.

    DATA(lt_data) = mo_model->get_tipo_lista( ).
    DATA(lr_data) = REF #( lt_data ).
    MO_VIEW_TIPO_LISTA->update( lr_data ).

  ENDMETHOD.


  METHOD update_tipo_lista_header.

    DATA(lt_data) = mo_model->get_tipo_lista( iv_tipo_lista ).

    IF lines( lt_data ) > 0.
      DATA(lr_data) = REF #( lt_data[ 1 ] ).
    ENDIF.

    mo_view_tipo_lista_hdr->update( lr_data ).

  ENDMETHOD.
ENDCLASS.
