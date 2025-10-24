*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE /QAPS/LFM_AREATOP.                 " Global Declarations
  INCLUDE /QAPS/LFM_AREAUXX.                 " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE /QAPS/LFM_AREAF...                 " Subroutines
* INCLUDE /QAPS/LFM_AREAO...                 " PBO-Modules
* INCLUDE /QAPS/LFM_AREAI...                 " PAI-Modules
* INCLUDE /QAPS/LFM_AREAE...                 " Events
* INCLUDE /QAPS/LFM_AREAP...                 " Local class implement.
* INCLUDE /QAPS/LFM_AREAT99.                 " ABAP Unit tests
  INCLUDE /QAPS/LFM_AREAF00                       . " subprograms
  INCLUDE /QAPS/LFM_AREAI00                       . " PAI modules
  INCLUDE LSVIMFXX                                . " subprograms
  INCLUDE LSVIMOXX                                . " PBO modules
  INCLUDE LSVIMIXX                                . " PAI modules
*{   INSERT         ECDK9A0F42                                        1
*
INCLUDE /qaps/lfm_areaf01.
*}   INSERT
