PROGRAM /qaps/rep_tx_cambio_top.

DATA: go_controller TYPE REF TO /qaps/cl_ctrl_taxa_cambio.

DATA: gv_waers   TYPE /qaps/s_taxa_cambio-moeda_local,
      gs_header  TYPE tcurt,
      gs_periodo TYPE /qaps/s_periodo_interval.

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
