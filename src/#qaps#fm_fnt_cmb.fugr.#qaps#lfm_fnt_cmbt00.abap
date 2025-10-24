*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /QAPS/V_FNT_CMB.................................*
TABLES: /QAPS/V_FNT_CMB, */QAPS/V_FNT_CMB. "view work areas
CONTROLS: TCTRL_/QAPS/V_FNT_CMB
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/QAPS/V_FNT_CMB. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/QAPS/V_FNT_CMB.
* Table for entries selected to show on screen
DATA: BEGIN OF /QAPS/V_FNT_CMB_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_FNT_CMB.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_FNT_CMB_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /QAPS/V_FNT_CMB_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_FNT_CMB.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_FNT_CMB_TOTAL.

*.........table declarations:.................................*
TABLES: /QAPS/FONTE_CMB                .
