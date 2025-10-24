class /QAPS/CL_VIEW_INPUT_VARIABLE definition
  public
  inheriting from /QAPS/CL_VIEW_DD_DOCUMENT_BASE
  final
  create public .

public section.

  events ON_SUBMIT_CHANGE_MODALIDADE
    exporting
      value(IV_ACTION) type CHAR10
      value(IS_DATA) type /QAPS/S_CALCULATION_NODE .
  events ON_SUBMIT_EXPRESSAO
    exporting
      value(IV_ACTION) type CHAR10
      value(IS_DATA) type /QAPS/S_CALCULATION_NODE
      value(IT_PARAMETERS) type /QAPS/T_CALCULATION_NODE .

  methods SET_CHILD_NODES
    importing
      !IT_CHILD_NODES type /QAPS/T_CALCULATION_NODE .

  methods UPDATE
    redefinition .
protected section.

  methods BUTTON_CLICKED
    redefinition .
  methods ENTERED
    redefinition .
  methods LINK_CLICKED
    redefinition .
  methods INPUT_ENTERED
    redefinition .
PRIVATE SECTION.

  TYPES:
    BEGIN OF ts_column_width,
      width TYPE sdydo_value,
      head  TYPE sdydo_text_element,
    END OF ts_column_width .
  TYPES:
    tt_column_width TYPE TABLE OF ts_column_width .

  DATA ms_calc_node TYPE /qaps/s_calculation_node .
  DATA mo_table_cabecalho TYPE REF TO cl_dd_table_element .
  DATA mo_expressao TYPE REF TO cl_dd_input_element .
  DATA mt_child_nodes TYPE /qaps/t_calculation_node .
  DATA mo_chk_importacao TYPE REF TO cl_dd_input_element .

  METHODS add_script .
  METHODS on_check
        FOR EVENT entered OF cl_dd_input_element
    IMPORTING
        !sender .
  METHODS add_parameters
    IMPORTING
      !is_data   TYPE /qaps/s_calculation_node
      !io_parent TYPE REF TO cl_dd_form_area .
  METHODS add_variable_data
    IMPORTING
      !is_data   TYPE /qaps/s_calculation_node
      !io_parent TYPE REF TO cl_dd_form_area .
  METHODS add_expressao
    IMPORTING
      !io_form TYPE REF TO cl_dd_form_area .
  METHODS add_modalidade
    IMPORTING
      !io_form   TYPE REF TO cl_dd_form_area
      !iv_source TYPE /qaps/ed_tipo_node .
  METHODS add_table
    IMPORTING
      !io_parent        TYPE REF TO cl_dd_area
      !iv_columns       TYPE i
      !it_columns_width TYPE tt_column_width OPTIONAL
      !iv_with_heading  TYPE sdydo_flag DEFAULT ''
    EXPORTING
      !et_columns       TYPE /qaps/t_dd_columns
      !eo_table         TYPE REF TO cl_dd_table_element .
  METHODS update_elementar
    IMPORTING
      !is_data TYPE /qaps/s_calculation_node .
  METHODS update_calculada
    IMPORTING
      !is_data TYPE /qaps/s_calculation_node .
ENDCLASS.



CLASS /QAPS/CL_VIEW_INPUT_VARIABLE IMPLEMENTATION.


  METHOD add_expressao.

    io_form->new_line( repeat = 1 ).

    io_form->add_text( EXPORTING text = 'Expressão' ).

    io_form->new_line( ).
    io_form->add_input_element_v2(
      EXPORTING
        value         = CONV #( ms_calc_node-expressao )
*        name          =     " Name (You Can Use Any Name You Choose)
        size          = 135    " Length of Input Field
        maxlength     = 150   " Maximum Permitted Text Entry Length
      IMPORTING
        input_element = mo_expressao   ).

    SET HANDLER entered FOR mo_expressao.


    io_form->new_line( ).
    io_form->line_with_layout( EXPORTING start  = 'X'  ).
    io_form->add_button_with_style(
      EXPORTING
        label    = 'Verificar'    " Button Text
        sap_icon = 'ICON_CHECK'    " Name of SAP ICon from Table ICON (ICON_TREE, ICON_...)
        style = 'padding-left:5px;padding-top:0px;margin-top:0px;'
        name = 'CHECK'
      IMPORTING
        button   =  DATA(lo_button_verificar)   " Button Element
    ).

    SET HANDLER button_clicked FOR lo_button_verificar.
    io_form->add_gap( EXPORTING width  = 4 ).

    io_form->add_button_with_style(
      EXPORTING
        label    = 'Salvar'    " Button Text
        sap_icon = 'ICON_SYSTEM_SAVE'    " Name of SAP ICon from Table ICON (ICON_TREE, ICON_...)
        style = 'padding-left:5px;padding-top:0px;margin-top:0px;'
        name = 'SAVE'
      IMPORTING
        button   =  DATA(lo_button_save)   " Button Element
    ).

    SET HANDLER button_clicked FOR lo_button_save.

    io_form->line_with_layout( EXPORTING end  = 'X'  ).

  ENDMETHOD.


  METHOD add_modalidade.

    DATA: lv_importacao    TYPE abap_bool,
          lv_nacional      TYPE abap_bool,
          lv_producao      TYPE abap_bool,
          lv_transferencia TYPE abap_bool.

    DATA lv_disabled TYPE abap_bool.

    CASE iv_source.
      WHEN 'E'.
        lv_disabled = abap_true.
        lv_importacao   = CONV #( ms_calc_node-importacao_elementar ).
        lv_nacional     = CONV #( ms_calc_node-nacional_elementar ).
        lv_producao     = CONV #( ms_calc_node-producao_elementar ).
        lv_transferencia = CONV #( ms_calc_node-transferencia_elementar ).
      WHEN 'C'.
        lv_importacao   = CONV #( ms_calc_node-importacao_calculada ).
        lv_nacional     = CONV #( ms_calc_node-nacional_calculada ).
        lv_producao     = CONV #( ms_calc_node-producao_calculada ).
        lv_transferencia = CONV #( ms_calc_node-transferencia_calculada ).
    ENDCASE.


    " Modalidade
    io_form->line_with_layout( EXPORTING start  = 'X'  ).
    io_form->add_text( EXPORTING text = 'Modalidade' ).

    io_form->add_gap( EXPORTING width      =  5 ).
    io_form->add_checkbox_element( EXPORTING value      = 'Importação'
                                             name = 'IMP'
                                             checked    = lv_importacao
                                             a11y_label    = 'Importação'
                                             disabled = lv_disabled
                                        IMPORTING
                                          input_element = mo_chk_importacao ).


    io_form->add_gap( EXPORTING width      =  5 ).
    io_form->add_checkbox_element( EXPORTING value  = 'Nacional'
                                      name = 'NAC'
                                      checked = lv_nacional
                                      a11y_label    = 'Nacional'
                                      disabled = lv_disabled
                                    IMPORTING
                                      input_element = DATA(lo_chk_nacional) ).




    io_form->add_gap( EXPORTING width      =  5 ).
    io_form->add_checkbox_element( EXPORTING value  = 'Produção'
                                             name = 'PROD'
                                             checked = lv_producao
                                             a11y_label    = 'Produção'
                                             disabled = lv_disabled
                                    IMPORTING
                                      input_element = DATA(lo_chk_producao) ).



    io_form->add_gap( EXPORTING width      =  5 ).
    io_form->add_checkbox_element( EXPORTING value  = 'Transferência'
                                             name = 'TRAN'
                                             checked = lv_transferencia
                                             a11y_label    = 'Transferência'
                                             disabled = lv_disabled
                                    IMPORTING
                                      input_element = DATA(lo_chk_transferencia) ).
    IF iv_source = 'C'.
      SET HANDLER on_check FOR mo_chk_importacao.
      SET HANDLER on_check FOR lo_chk_nacional.
      SET HANDLER on_check FOR lo_chk_producao.
      SET HANDLER on_check FOR lo_chk_transferencia.
    ENDIF.

    io_form->line_with_layout( EXPORTING end  = 'X'  ).

  ENDMETHOD.


  METHOD add_parameters.

    DATA lt_width TYPE tt_column_width.
*    data lv_url type string.
    DATA lv_tipo TYPE sdydo_text_element.

    CHECK lines( mt_child_nodes ) > 0.

    io_parent->new_line( repeat = 1 ).
    io_parent->add_text( EXPORTING text = 'Variáveis' ).
    io_parent->new_line( repeat = 1 ).

    "Tabela 1
    APPEND VALUE ts_column_width( width = '8%' head = '' ) TO lt_width.
    APPEND VALUE ts_column_width( width = '15%' head = 'Variável' ) TO lt_width.
    APPEND VALUE ts_column_width( width = '50%' head = 'Descrição' ) TO lt_width.
    APPEND VALUE ts_column_width( width = '15%' head = 'Tipo de Variável' ) TO lt_width.
    APPEND VALUE ts_column_width( width = '20%' head = 'Tipo de Dado') TO lt_width.

    add_table( EXPORTING io_parent  =  io_parent
                         iv_columns = 5
                         it_columns_width = lt_width
                         iv_with_heading = 'X'
                IMPORTING et_columns = DATA(lt_columns)
                          eo_table = DATA(lo_table) ).


    "TÍTITULOS
*    lt_columns[ 1 ]-column->add_text( EXPORTING text = 'X' ).
*    lt_columns[ 2 ]-column->add_text( EXPORTING text = 'Variável' ).
*    lt_columns[ 3 ]-column->add_text( EXPORTING text = 'Descrição' ).
*    lt_columns[ 4 ]-column->add_text( EXPORTING text = 'Tipo de Variável' ).
*    lt_columns[ 5 ]-column->add_text( EXPORTING text = 'Tipo de Dado' ).

    LOOP AT mt_child_nodes INTO DATA(ls_child_nodes).

      IF sy-tabix > 1.
        lo_table->new_row( ).
      ENDIF.

*      lv_url = `javascript:add('` && ls_child_nodes-fieldname && `');`.

      lt_columns[ 1 ]-column->add_link( EXPORTING name      = CONV #( ls_child_nodes-fieldname )
                                                  text      = 'Add'
                                        IMPORTING link      = DATA(lo_link) ).

      SET HANDLER link_clicked FOR lo_link.

      lt_columns[ 2 ]-column->add_text( EXPORTING text = CONV #( ls_child_nodes-fieldname ) ).

      CASE ls_child_nodes-tipo_node.
        WHEN 'C'.
          lt_columns[ 3 ]-column->add_text( EXPORTING text = CONV #( ls_child_nodes-descricao ) ).
          lt_columns[ 4 ]-column->add_text( EXPORTING text = 'Calculada' ).
        WHEN 'E'.
          lt_columns[ 3 ]-column->add_text( EXPORTING text = CONV #( ls_child_nodes-dsc_custo_elementar ) ).
          lt_columns[ 4 ]-column->add_text( EXPORTING text = 'Elementar' ).
      ENDCASE.

      lv_tipo = COND #( WHEN ls_child_nodes-tipo_dado = '1' THEN 'Valor'
                        WHEN ls_child_nodes-tipo_dado = '2' THEN 'Percentual' ).

      lt_columns[ 5 ]-column->add_text( EXPORTING text = lv_tipo ).
    ENDLOOP.



  ENDMETHOD.


  METHOD add_script.

    DATA lv_script TYPE string.

    DATA(lv_id) = mo_expressao->id.

    lv_script = `<script>`.
*    lv_script = lv_script && ` const input = document.getElementsByName("A1F3I7");`.
*    lv_script = lv_script && ` var ar = input[0].getAttributeNames();`.
*    lv_script = lv_script && ` alert(ar);`.
*    //lv_script = lv_script && ` input[0].addEventListener("change", updateValue);`.

    lv_script = lv_script && ` function showMessage()`.
    lv_script = lv_script && `{`.
    lv_script = lv_script && `  alert('OK');`.
    lv_script = lv_script && `}`.

    lv_script = lv_script && ` function submitForm(form,field)`.
    lv_script = lv_script && `{`.
    lv_script = lv_script && `  fname = form.name;`.
    lv_script = lv_script && `  form.sapbu_cl.value = field.id + 'sapENTER';` .
    lv_script = lv_script && `  form.sapse_cl.value = " ";`.
    lv_script = lv_script && `  form.sapin_cl.value = " ";`.
    lv_script = lv_script && `  document[fname].submit();`.
    lv_script = lv_script && `}`.
    lv_script = lv_script && `</script>`.

    mo_document->set_script( lv_script ).

  ENDMETHOD.


  METHOD add_table.

    DATA: lv_width TYPE sdydo_value.

    io_parent->add_table(
               EXPORTING no_of_columns               = iv_columns
                         cell_background_transparent = space
                         with_heading                = iv_with_heading
                         with_a11y_marks             = ''
*                         a11y_label                  = str
                         width                       = '75%'
*                         border                      = '0'

                IMPORTING table = eo_table ).

    DO iv_columns TIMES.

      APPEND INITIAL LINE TO et_columns ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-index = sy-index.

      IF lines( it_columns_width ) > 0.

        DATA(ls_columns_width) = VALUE #( it_columns_width[ <fs>-index ] OPTIONAL ).

        IF NOT ls_columns_width IS INITIAL.
          lv_width = ls_columns_width-width.
        ELSE.
          lv_width = '30%'.
        ENDIF.
        IF NOT ls_columns_width-head IS INITIAL.
          eo_table->add_column( EXPORTING width   = lv_width
                                        bg_color = cl_dd_area=>list_background_int
                                        heading = ls_columns_width-head
                              IMPORTING column  = <fs>-column ).
        ELSE.
          eo_table->add_column( EXPORTING width   = lv_width
                                        bg_color = cl_dd_area=>list_background_int
                              IMPORTING column  = <fs>-column ).
        ENDIF.

      ELSE.
        lv_width = '30%'.

        eo_table->add_column( EXPORTING width   = lv_width
                                      bg_color = cl_dd_area=>list_background_int
                            IMPORTING column  = <fs>-column ).
      ENDIF.



      eo_table->set_column_style( EXPORTING col_no  = conv #( <fs>-index )
                                            sap_color = cl_dd_area=>list_background_int ).

    ENDDO.

  ENDMETHOD.


  METHOD add_variable_data.

    DATA lv_tipo TYPE sdydo_text_element.

    "Tabela 1
    add_table( EXPORTING io_parent  =  io_parent
                         iv_columns = 2
                         iv_with_heading = 'X'
                         it_columns_width = value #( ( width = '50%' head = 'Campo' )
                                                     ( width = '50%' head = 'Valor' ) )
                IMPORTING et_columns = DATA(lt_columns)
                          eo_table   = mo_table_cabecalho ).

    lt_columns[ 1 ]-column->add_text( EXPORTING text = 'Descrição' ).
    CASE is_data-tipo_node.
      WHEN 'C'.
        lt_columns[ 2 ]-column->add_text( EXPORTING text = CONV #( is_data-descricao ) ).
      WHEN 'E'.
        lt_columns[ 2 ]-column->add_text( EXPORTING text = CONV #( is_data-dsc_custo_elementar ) ).
    ENDCASE.

    mo_table_cabecalho->new_row( ).
    lt_columns[ 1 ]-column->add_text( EXPORTING text = 'Field Name' ).
    lt_columns[ 2 ]-column->add_text( EXPORTING text = CONV #( is_data-fieldname ) ).

    mo_table_cabecalho->new_row( ).
    lt_columns[ 1 ]-column->add_text( EXPORTING text = 'Tipo' ).

    lv_tipo = COND #( WHEN is_data-tipo_dado = '1' THEN 'Valor'
                      WHEN is_data-tipo_dado = '2' THEN 'Percentual' ).
    lt_columns[ 2 ]-column->add_text( EXPORTING text = lv_tipo ).

  ENDMETHOD.


  METHOD button_clicked.

    DATA(lv_expressao) = mo_expressao->value.

    ms_calc_node-expressao = lv_expressao.

    RAISE EVENT on_submit_expressao
      EXPORTING
        iv_action     = CONV #( sender->name )
        is_data       = ms_calc_node
        it_parameters = mt_child_nodes.

  ENDMETHOD.


  METHOD entered.

    DATA: lv_inicial TYPE string,
          lv_final   TYPE string.

    lv_inicial = ms_calc_node-expressao.
    lv_final = sender->value.

    CONDENSE: lv_inicial,
              lv_final.

    CHECK lv_inicial <> lv_final.
    ms_calc_node-expressao = sender->value.
    update_calculada( ms_calc_node ).

  ENDMETHOD.


  METHOD input_entered.

    BREAK-POINT.

  ENDMETHOD.


  METHOD link_clicked.

*    DATA(lv_value) = mo_expressao->value.

*    CONDENSE lv_value.

    ms_calc_node-expressao = ms_calc_node-expressao && ` ` && sender->name .

    update_calculada( ms_calc_node ).


  ENDMETHOD.


  METHOD on_check.

    DATA lv_name TYPE lvc_fname.

    CASE sender->name.
      WHEN 'IMP'.  lv_name = 'IMPORTACAO_CALCULADA'.
      WHEN 'NAC'.  lv_name = 'NACIONAL_CALCULADA'.
      WHEN 'PROD'. lv_name = 'PRODUCAO_CALCULADA'.
      WHEN 'TRAN'. lv_name = 'TRANSFERENCIA_CALCULADA'.
    ENDCASE.

    ASSIGN COMPONENT lv_name OF STRUCTURE ms_calc_node TO FIELD-SYMBOL(<fv_field>).

    IF <fv_field> = abap_true.
      <fv_field> = abap_false.
    ELSE.
      <fv_field> = abap_true.
    ENDIF.

    RAISE EVENT on_submit_change_modalidade
      EXPORTING
        iv_action = CONV #( sender->name )
        is_data   = ms_calc_node.
  ENDMETHOD.


  METHOD set_child_nodes.
    REFRESH mt_child_nodes.
    mt_child_nodes = it_child_nodes.
  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS <fs> TYPE /qaps/s_calculation_node.

    ASSIGN ir_data->* TO <fs>.

    ms_calc_node = <fs>.

    CASE <fs>-tipo_node.
      WHEN 'C'.
        update_calculada( <fs> ).
      WHEN 'E'.
        update_elementar( <fs> ).
    ENDCASE.

  ENDMETHOD.


  METHOD update_calculada.

    mo_document->initialize_document( ).


    set_title( iv_title = 'Variável Calculada'
               iv_icon  = cond #( when is_data-price_field = 'X' then 'ICON_GRAPHICS'
                                  else 'ICON_CALCULATION' ) ).

    mo_document->add_form( IMPORTING formarea = DATA(lo_form)  ).

    "Dados da Variável
    add_variable_data( is_data   = is_data
                       io_parent = lo_form ).

    "Modalidade
    add_modalidade( io_form = lo_form
                    iv_source = 'C' ).

    "Variáveis filhas
    add_parameters( is_data   = is_data
                    io_parent = lo_form ).

    "Expressao
    add_expressao( lo_form ).

    "Script
    add_script( ).

    mo_document->merge_document( ).
    display_document( ).


  ENDMETHOD.


  METHOD update_elementar.

    mo_document->initialize_document( ).

    set_title( iv_title = 'Variável Elementar'
               iv_icon = 'ICON_SYM_REAL_SERVER' ).

    mo_document->add_form( IMPORTING formarea = DATA(lo_form)  ).

    add_variable_data( is_data   = is_data    " QAPS: Calcuation Nodes
                       io_parent = lo_form ).

    "Modalidade
    add_modalidade( io_form = lo_form
                    iv_source = 'E' ).

    mo_document->merge_document( ).
    display_document( ).


  ENDMETHOD.
ENDCLASS.
