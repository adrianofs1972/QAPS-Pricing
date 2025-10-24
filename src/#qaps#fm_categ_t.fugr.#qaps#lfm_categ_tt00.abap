*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /QAPS/V_CATEG_TR................................*
TABLES: /QAPS/V_CATEG_TR, */QAPS/V_CATEG_TR. "view work areas
CONTROLS: TCTRL_/QAPS/V_CATEG_TR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/QAPS/V_CATEG_TR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/QAPS/V_CATEG_TR.
* Table for entries selected to show on screen
DATA: BEGIN OF /QAPS/V_CATEG_TR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_CATEG_TR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_CATEG_TR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /QAPS/V_CATEG_TR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_CATEG_TR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_CATEG_TR_TOTAL.

*.........table declarations:.................................*
TABLES: /QAPS/CATEG_TRNS               .
