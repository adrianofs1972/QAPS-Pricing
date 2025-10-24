*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /QAPS/V_AREA....................................*
TABLES: /QAPS/V_AREA, */QAPS/V_AREA. "view work areas
CONTROLS: TCTRL_/QAPS/V_AREA
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/QAPS/V_AREA. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/QAPS/V_AREA.
* Table for entries selected to show on screen
DATA: BEGIN OF /QAPS/V_AREA_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_AREA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_AREA_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /QAPS/V_AREA_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /QAPS/V_AREA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /QAPS/V_AREA_TOTAL.

*.........table declarations:.................................*
TABLES: /QAPS/AREA                     .
