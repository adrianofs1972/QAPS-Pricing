*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/QAPS/FG_AREAS
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/QAPS/FG_AREAS     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
