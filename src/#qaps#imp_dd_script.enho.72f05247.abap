"Name: \TY:CL_DD_DOCUMENT\ME:MERGE_DOCUMENT\SE:END\EI
ENHANCEMENT 0 /QAPS/IMP_DD_SCRIPT.
  check lines( mt_script ) > 0.
  append LINES OF mt_script to html_table.
ENDENHANCEMENT.
