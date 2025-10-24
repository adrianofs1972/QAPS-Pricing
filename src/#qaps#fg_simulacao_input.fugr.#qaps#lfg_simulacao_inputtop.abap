FUNCTION-POOL /qaps/fg_simulacao_input.    "MESSAGE-ID ..

TYPES: BEGIN OF ts_tipo_lista,
         id_tp_lista TYPE /qaps/tp_lista-id_tp_lista,
         descricao   TYPE /qaps/tp_lista-descricao,
         index       TYPE i,
       END OF ts_tipo_lista.

"Área
DATA: gv_loaded TYPE abap_bool,
      gv_search TYPE char30,
      gv_tiltle TYPE char30,
      gv_action type c.

DATA: gv_name    TYPE vrm_id,
      gt_list    TYPE vrm_values,
      gt_listbox TYPE TABLE OF ts_tipo_lista,
      gs_listbox TYPE ts_tipo_lista,
      gs_value   LIKE LINE OF gt_list.

DATA gs_data TYPE /qaps/s_simulacao.

"Escopo variável
DATA: gv_escopo_global   TYPE abap_bool,
      gv_escopo_tp_lista TYPE abap_bool.

"tipo dado
DATA: gv_tipo_dado_valor      TYPE abap_bool,
      gv_tipo_dado_percentual TYPE abap_bool.

DATA: gv_origem_dado_input  TYPE abap_bool,
      gv_origem_dado_tabela TYPE abap_bool,
      gv_origem_dado_custom TYPE abap_bool.

DATA: gv_tipo_variavel_geral     TYPE abap_bool,
      gv_tipo_variavel_logistica TYPE abap_bool,
      gv_tipo_variavel_produtiva TYPE abap_bool.

DATA gv_required TYPE abap_bool.

TYPE-POOLS: vrm.

TABLES: tfacd,                         " Fabrikkalender
        thoci,                         " Feiertagskalender
        t247.                          " Monatsbezeichnungen

DATA: BEGIN OF month_tab OCCURS 12.
    INCLUDE STRUCTURE t247.
DATA: END   OF month_tab.

DATA: BEGIN OF year_tab OCCURS 30,
        year LIKE tfacd-vjahr.
DATA: END   OF year_tab.

DATA: p100_txt_month LIKE t247-ltx,
      p100_month     LIKE t247-mnr,
      p100_year      LIKE tfacd-vjahr,
      p100_ok_code   LIKE sy-xcode.

DATA: g_number_of_month LIKE sy-tabix,
      g_number_of_year  LIKE sy-tabix,
      g_factory_vjahr   LIKE tfacd-vjahr,
      g_factory_bjahr   LIKE tfacd-bjahr,
      g_holiday_vjahr   LIKE thoci-vjahr,
      g_holiday_bjahr   LIKE thoci-bjahr,
      g_p100_year       LIKE tfacd-vjahr,
*      g_p100_month      like t247-mnr,
      g_p100_month      LIKE ical_info-month_no,
      g_p100_txt_month  LIKE t247-ltx,
      g_calendar        TYPE c,              " H = Holiday, F = Factory
      g_facid           LIKE tfacd-ident,    " Fabikkalender-ID
      g_hocid           LIKE thoci-ident.    " Feiertagskalender-ID

DATA: help_values(30)     TYPE c VALUE 'HELP_VALUES_GET_NO_DD_NAME    ',
      help_values_get(30) TYPE c VALUE 'HELP_VALUES_GET_WITH_VALUE    ',
      dynp_update(30)     TYPE c VALUE 'DYNP_VALUES_UPDATE            ',
      dynp_read(30)       TYPE c VALUE 'DYNP_VALUES_READ              '.

* Globale Variable für F4-Hilfe in COMBO-Box
DATA: it_f4_drop TYPE vrm_values,
      id         TYPE vrm_id.

DATA: gs_periodo_inicial TYPE /qaps/s_periodo,
      gs_periodo_final   TYPE /qaps/s_periodo..
