*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /QAPS/AREA......................................*
DATA:  BEGIN OF STATUS_/QAPS/AREA                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/QAPS/AREA                    .
CONTROLS: TCTRL_/QAPS/AREA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */QAPS/AREA                    .
TABLES: /QAPS/AREA                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
