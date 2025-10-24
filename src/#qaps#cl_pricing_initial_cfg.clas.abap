class /QAPS/CL_PRICING_INITIAL_CFG definition
  public
  abstract
  final
  create public .

public section.

  class-methods CREATE_INITIAL_CONFIG .
protected section.
private section.

  class-methods CREATE_AREA_CONTROLADORIA
    returning
      value(RETURN) type /QAPS/ED_ID_AREA .
  class-methods CREATE_VARIAVEIS_ELEMENTARES
    importing
      !IV_ID_AREA type /QAPS/ED_ID_AREA .
ENDCLASS.



CLASS /QAPS/CL_PRICING_INITIAL_CFG IMPLEMENTATION.


  METHOD create_area_controladoria.

    DATA lr_data TYPE REF TO data.

    SELECT SINGLE *
      FROM /qaps/area
      WHERE id_intial_cfg = '0001'
      INTO @DATA(ls_data).

    IF sy-subrc = 0.
      return = ls_data-id_area.
    ELSE.
      DATA(ls_area) = VALUE /qaps/area(
          id_area       = cl_system_uuid=>create_uuid_x16_static( )
          descricao     = 'Controladoria'
          ativo         = 'X'
          id_intial_cfg = '001' ).

      lr_data = REF #( ls_area ).
      /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).
      MODIFY /qaps/area FROM ls_area.

      return = ls_area-id_area.

    ENDIF.

  ENDMETHOD.


  METHOD create_initial_config.

    DATA(lv_id_controladoria) = create_area_controladoria( ).

    create_variaveis_elementares( lv_id_controladoria ).

  ENDMETHOD.


  METHOD create_variaveis_elementares.

    DATA: lt_init TYPE TABLE OF /qaps/custo_elm,
          lr_data type ref to data.

    SELECT *
      FROM /qaps/custo_elm
      WHERE id_intial_cfg <> ''
      INTO TABLE @DATA(lt_config).

    DEFINE add_item.
      APPEND VALUE /qaps/custo_elm(
          id_custo_elementar = cl_system_uuid=>create_uuid_x16_static( )
          descricao          = &1
          escopo             = 'G'
          id_area            = iv_id_area
          tipo_dado          = &2
          tipo_variavel      = 'G'
          origem_dado        = '1'
          moeda              = &3
          importacao         = 'X'
          nacional           = 'X'
          producao           = 'X'
          transferencia      = 'X'
          id_intial_cfg      = &4 ) TO lt_init.
    END-OF-DEFINITION.

    IF NOT line_exists( lt_config[ id_intial_cfg = '0001' ] ).
      add_item 'Markup $' '1' 'USD' '0001'.
    ENDIF.

    IF NOT line_exists( lt_config[ id_intial_cfg = '0002' ] ).
      add_item 'Markup %' '2' '' '0002'.
    ENDIF.

    IF NOT line_exists( lt_config[ id_intial_cfg = '0003' ] ).
      add_item 'Mercado $' '1' 'USD' '0003'.
    ENDIF.

    IF NOT line_exists( lt_config[ id_intial_cfg = '0004' ] ).
      add_item 'Mercado %' '2' '' '0004'.
    ENDIF.

    CHECK lines( lt_init ) > 0.

    LOOP AT lt_init REFERENCE INTO lr_data.
      /qaps/cl_helper_data=>preencher_dados_controle( CHANGING cr_data = lr_data ).
    ENDLOOP.

    MODIFY /qaps/custo_elm FROM TABLE lt_init.

  ENDMETHOD.
ENDCLASS.
