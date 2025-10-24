*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /QAPS/V_CST_PRC.................................*
TABLES: /QAPS/V_CST_PRC, */QAPS/V_CST_PRC. "view work areas
CONTROLS: TCTRL_/QAPS/V_CST_PRC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/QAPS/V_CST_PRC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/QAPS/V_CST_PRC.
* Table for entries selected to show on screen
DATA: BEGIN OF /QAPS/V_CST_PRC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_CST_PRC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_CST_PRC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /QAPS/V_CST_PRC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_CST_PRC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_CST_PRC_TOTAL.

*.........table declarations:.................................*
TABLES: /QAPS/CUSTO_PRC                .
