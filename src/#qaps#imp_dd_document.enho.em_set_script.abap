METHOD set_script .

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = iv_script
      i_tabline_length = 255
    TABLES
      et_table         = mt_script.

ENDMETHOD.
