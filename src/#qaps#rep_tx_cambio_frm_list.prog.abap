*&---------------------------------------------------------------------*
*&  Include           /QAPS/REP_TX_CAMBIO_FRM_LIST
*&---------------------------------------------------------------------*
FORM listbox_month CHANGING p_month.

  DATA: h_fields         LIKE help_value OCCURS 0 WITH HEADER LINE,
        h_select_values  LIKE help_vtab  OCCURS 0 WITH HEADER LINE,
        h_t002t_tab      LIKE t002t      OCCURS 0 WITH HEADER LINE,
        h_value_tab(100) OCCURS 0 WITH HEADER LINE,
        h_index          TYPE i.

  h_fields-tabname    = 'T247'.
  h_fields-fieldname  = 'MNR'.
  h_fields-selectflag = 'X'.
  APPEND h_fields.

  h_fields-tabname    = 'T247'.
  h_fields-fieldname  = 'LTX'.
  h_fields-selectflag = ' '.
  APPEND h_fields.

  LOOP AT month_tab.
    h_value_tab = month_tab-mnr.
    APPEND h_value_tab.
    h_value_tab = month_tab-ltx.
    APPEND h_value_tab.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      fieldname     = 'MNR'
      tabname       = 'T247'
      no_conversion = ' '
    IMPORTING
      select_value  = p_month
    TABLES
      fields        = h_fields
      select_values = h_select_values
      valuetab      = h_value_tab.

ENDFORM.
FORM listbox_year CHANGING p_year.

  DATA: h_fields         LIKE help_value OCCURS 0 WITH HEADER LINE,
        h_select_values  LIKE help_vtab  OCCURS 0 WITH HEADER LINE,
        h_t002t_tab      LIKE t002t      OCCURS 0 WITH HEADER LINE,
        h_value_tab(100) OCCURS 0 WITH HEADER LINE.


  PERFORM fill_year_tab.

  h_fields-tabname    = 'TFACD'.
  h_fields-fieldname  = 'VJAHR'.
  h_fields-selectflag = 'X'.
  APPEND h_fields.

  LOOP AT year_tab.
    h_value_tab = year_tab-year.
    APPEND h_value_tab.
  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      fieldname     = 'VJAHR'
      tabname       = 'TFACD'
      no_conversion = ' '
    IMPORTING
      select_value  = p_year
    TABLES
      fields        = h_fields
      select_values = h_select_values
      valuetab      = h_value_tab.

ENDFORM.
FORM fill_year_tab.

  DATA: h_year    LIKE tfacd-vjahr.

  h_year = g_p100_year - 1.

  IF h_year < 0001.
    h_year = 0001.
  ENDIF.

  DO 3 TIMES.
    LOOP AT year_tab WHERE year = h_year.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      year_tab-year = h_year.
      APPEND year_tab.
    ENDIF.
    IF h_year = 9999.
      EXIT.
    ELSE.
      h_year = h_year + 1.
    ENDIF.
  ENDDO.

  DESCRIBE TABLE year_tab LINES g_number_of_year.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_P100_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FACTORY_CALENDAR  text
*      -->P_HOLIDAY_CALENDAR  text
*      -->P_ACTUAL_MONTH  text
*      -->P_LANGUAGE  text
*----------------------------------------------------------------------*
FORM fill_p100_fields USING factory_calendar
                            holiday_calendar
                            actual_month
                            language.

  DATA: h_month        LIKE t247-mnr,
        h_year         LIKE tfacd-vjahr,
        h_actual_month LIKE isellist-month.


  CLEAR: g_calendar,
         g_facid,
         g_hocid,
         g_factory_vjahr,
         g_factory_bjahr,
         g_holiday_vjahr,
         g_holiday_bjahr,
         g_p100_month,
         g_p100_year.

  g_facid       = factory_calendar.
  g_hocid       = holiday_calendar.

  h_actual_month = actual_month.
  h_month       = h_actual_month+4(2).
  h_year        = h_actual_month(4).

  IF factory_calendar <> space.
    SELECT SINGLE * FROM tfacd WHERE ident = factory_calendar.

    IF sy-subrc = 0.
      g_calendar = 'F'.
      g_factory_vjahr = tfacd-vjahr.
      g_factory_bjahr = tfacd-bjahr.
    ELSE.
*      message e350 with factory_calendar
*              raising factory_calendar_not_found.
    ENDIF.

  ELSEIF holiday_calendar <> space.

    SELECT SINGLE * FROM thoci WHERE ident = holiday_calendar.

    IF sy-subrc = 0.
      g_calendar = 'H'.
      g_holiday_vjahr = thoci-vjahr.
      g_holiday_bjahr = thoci-bjahr.
    ELSE.
*      message e350 with holiday_calendar
*              raising holiday_calendar_not_found.
    ENDIF.

  ENDIF.

  p100_year   = h_year.
  g_p100_year = h_year.

  PERFORM fill_month_tab USING language.
  PERFORM fill_year_tab.

  LOOP AT month_tab WHERE mnr = h_month.
    p100_month       = month_tab-mnr.
    p100_txt_month   = month_tab-ltx.
    g_p100_month     = month_tab-mnr.
    g_p100_txt_month = month_tab-ltx.
    EXIT.
  ENDLOOP.

  IF sy-subrc <> 0.
*    message e361 with h_month raising month_not_found.
  ENDIF.

ENDFORM.                               " FILL_P100_FIELDS
FORM fill_month_tab USING language.

  IF g_number_of_month = 0.

    SELECT * FROM t247 INTO TABLE month_tab WHERE spras = language.

    DESCRIBE TABLE month_tab LINES g_number_of_month.

  ENDIF.

ENDFORM.
