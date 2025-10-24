FUNCTION-POOL /QAPS/FORMULACAO_V2C MESSAGE-ID /QAPS/ZMI_MSG.

*----- TABLES

TABLES:
  /qaps/zmi00,
  /qaps/zmi60,
  /qaps/zmicig0,
  /qaps/zmidfcig,
  /qaps/zmidfrs.

TYPES:
  BEGIN OF ty_fc,                      " Function Code
    okcod TYPE sy-ucomm,
  END OF ty_fc,
  BEGIN OF ty_pc,                      " Propr/Caracts
    flgut  TYPE /qaps/zmicig1-/qaps/flgut,
    comnr  TYPE /qaps/zmicig1-/qaps/comnr,
    coktx  TYPE makt-maktx,
    wgmeng TYPE /qaps/zmicig1-/qaps/wgmeng,
    wemeng TYPE /qaps/zmicig1-/qaps/wemeng,
    wlmeng TYPE /qaps/zmicig1-/qaps/wlmeng,
    wymeng TYPE /qaps/zmicig1-/qaps/wymeng,
    gemng  TYPE /qaps/zmicig1-/qaps/wgmeng,
    eqmng  TYPE /qaps/zmicig1-/qaps/wemeng,
    lemng  TYPE /qaps/zmicig1-/qaps/wlmeng,
  END OF ty_pc,

  BEGIN OF ty_rs,
    chkey    TYPE /qaps/zmi01-/qaps/chkey,
    chdes    TYPE /qaps/zmitc-/qaps/chdes,
    ref      TYPE /qaps/zmi01-/qaps/gmeng,
    gmeng    TYPE /qaps/zmi01-/qaps/gmeng,
    emeng    TYPE /qaps/zmi01-/qaps/emeng,
    lmeng    TYPE /qaps/zmi01-/qaps/lmeng,
    ymeng    TYPE /qaps/zmi01-/qaps/ymeng,
    meuni    TYPE /qaps/zmitc-/qaps/meuni,
    lmark    TYPE char1,
    ord(3)   TYPE c,
    gmeng_bk TYPE /qaps/zmi01-/qaps/gmeng,
    changed  TYPE abap_bool,
  END OF ty_rs.

TYPES:
  BEGIN OF ty_re,
    comnr    TYPE  /qaps/zmic1-/qaps/comnr,
    coktx    TYPE  makt-maktx,
    cmeng    TYPE  /qaps/zmicig1-/qaps/cmeng,
    cmeng_ng TYPE  /qaps/zmicig1-/qaps/cmeng,
    cmuni    TYPE  mara-meins,
    stprs    TYPE  mbew-stprs,
    verpr    TYPE  mbew-verpr,
    vjbwh    TYPE  mbew-vjbwh,
    bwph1    TYPE  mbew-bwph1,

  END OF ty_re.

TYPES:
  BEGIN OF ty_ng,
    chkey  TYPE /qaps/zmitc-/qaps/chkey,
    chdes  TYPE /qaps/zmidfng-/qaps/chdes,
    cmeng  TYPE /qaps/zmidfng-/qaps/cmeng,
    cmuni  TYPE /qaps/zmidfng-/qaps/cmuni,
    flbeg  TYPE /qaps/zmidfng-/qaps/flbeg,
    lmeng  TYPE /qaps/zmidfcig-/qaps/cmeng,
    flend  TYPE /qaps/zmidfng-/qaps/flend,
    gmeng  TYPE /qaps/zmidfcig-/qaps/cmeng,
    ymeng  TYPE /qaps/zmidfcig-/qaps/cmeng,
    ord(3) TYPE c,
  END OF ty_ng.

TYPES: BEGIN OF ty_re_hist,
         combi(10) TYPE c,
         costt     TYPE /qaps/zmidffic-/qaps/costt,
         comnr     TYPE  /qaps/zmic1-/qaps/comnr,
         coktx     TYPE  makt-maktx,
         cmeng     TYPE  /qaps/zmicig1-/qaps/cmeng,
         cmeng_ng  TYPE  /qaps/zmicig1-/qaps/cmeng,
         cmuni     TYPE  mara-meins,
         stprs     TYPE  mbew-stprs,
         verpr     TYPE  mbew-verpr,
         vjbwh     TYPE  mbew-vjbwh,
         bwph1     TYPE  mbew-bwph1,
       END OF ty_re_hist.

TYPES:
  BEGIN OF ty_dif,
    chkey TYPE /qaps/zmitc-/qaps/chkey,
    xdif  TYPE /qaps/zmirm0-/qaps/gemng,
  END OF ty_dif,

  BEGIN OF ty_caract,
    chkey TYPE /qaps/zmitc-/qaps/chkey,
  END OF ty_caract.

DATA it_re_hist TYPE TABLE OF ty_re_hist.
DATA: it_ng    TYPE TABLE OF ty_ng WITH KEY chkey ord.
DATA: it_dif   TYPE TABLE OF ty_dif.

*--------------
*-- TYPES
*--------------

TYPES:
  BEGIN OF ty_ma,
    line(6) TYPE n,
    col(6)  TYPE n,
    valor   TYPE f,
  END OF ty_ma,

  BEGIN OF ty_vb,
    chave(6) TYPE n,
    valor    TYPE f,
  END OF ty_vb,

  BEGIN OF ty_vc,
    chave(6) TYPE n,
    valor    TYPE sy-tabix,
  END OF ty_vc,

  BEGIN OF ty_comp,
    comnr TYPE mara-matnr,
  END OF ty_comp,

  BEGIN OF ty_min,
    comnr TYPE mara-matnr,
    ind   TYPE sy-tabix,
    flag  TYPE char1,
    combi TYPE /qaps/zmicig0-/qaps/cignr,
  END OF ty_min,

  BEGIN OF ty_desmarcar,
    comnr TYPE mara-matnr,
  END OF ty_desmarcar,

  BEGIN OF ty_rest_comp,
    matnr       TYPE /qaps/zmic1-matnr,
    werks       TYPE /qaps/zmic1-werks,
    /qaps/grkey TYPE /qaps/zmic1-/qaps/grkey,
    /qaps/comnr TYPE /qaps/zmic1-/qaps/comnr,
    /qaps/gemng TYPE /qaps/zmic1-/qaps/gemng,
    /qaps/eqmng TYPE /qaps/zmic1-/qaps/eqmng,
    /qaps/lemng TYPE /qaps/zmic1-/qaps/lemng,
    auto        TYPE abap_bool,
    /qaps/chkey TYPE /qaps/zmitc-/qaps/chkey,
    cond_min    TYPE abap_bool,
    forced      TYPE abap_bool,
  END OF ty_rest_comp,
  tt_rest_comp TYPE TABLE OF ty_rest_comp,
  BEGIN OF ty_prop_comp,
    matnr       TYPE /qaps/zmi10-matnr,
    werks       TYPE /qaps/zmi10-werks,
    /qaps/grkey TYPE /qaps/zmi10-/qaps/grkey,
    /qaps/rmeng TYPE /qaps/zmi10-/qaps/rmeng,
    /qaps/rmuni TYPE /qaps/zmi10-/qaps/rmuni,
    /qaps/chkey TYPE /qaps/zmi11-/qaps/chkey,
    /qaps/cmeng TYPE /qaps/zmi11-/qaps/cmeng,
    /qaps/cmuni TYPE /qaps/zmi11-/qaps/cmuni,
  END OF ty_prop_comp,

  BEGIN OF ty_cust_matnr,
    matnr    TYPE mara-matnr,
    bwkey    TYPE mbew-bwkey,
    verpr    TYPE mbew-verpr,
    stprs    TYPE mbew-stprs,
    peinh    TYPE mbew-peinh,
    vjbwh    TYPE mbew-vjbwh,
    bwph1    TYPE mbew-bwph1,
    lcost    TYPE /qaps/zmilc1-/qaps/lcost,
    fcost    TYPE /qaps/zmilc1-/qaps/fcost,
    contab   TYPE /qaps/zmilc1-/qaps/contab,
    reposi   TYPE /qaps/zmilc1-/qaps/reposi,
    gerenc   TYPE /qaps/zmilc1-/qaps/gerenc,
    priori   TYPE /qaps/zmilc1-/qaps/priori,
    gerxprio TYPE /qaps/zmilc1-/qaps/gerxprio,
    totmkp   TYPE /qaps/zmilc1-/qaps/totmkp,
    totajmkt TYPE /qaps/zmilc1-/qaps/totajmkt,
    mcost    TYPE /qaps/zmilc1-/qaps/mcost,
  END OF ty_cust_matnr,

  BEGIN OF ty_prop_carac,
    matnr TYPE /qaps/zmi10-matnr,
    werks TYPE /qaps/zmi10-werks,
    grkey TYPE /qaps/zmi10-/qaps/grkey,
    rmeng TYPE /qaps/zmi10-/qaps/rmeng,
    rmuni TYPE /qaps/zmi10-/qaps/rmuni,
    chkey TYPE /qaps/zmi11-/qaps/chkey,
    cmeng TYPE /qaps/zmi11-/qaps/cmeng,
  END OF ty_prop_carac,

  BEGIN OF ty_combi,
    combi(10) TYPE c,
    comnr(18) TYPE c,
    costt     TYPE /qaps/zmidfcig-/qaps/costt,
    msg(100)  TYPE c,
    it_re       TYPE STANDARD TABLE OF ty_re WITH KEY comnr,
  END OF ty_combi,

  BEGIN OF ty_rest_mist,
    matnr       TYPE /qaps/zmirm0-matnr,
    /qaps/mixnr TYPE /qaps/zmirm0-/qaps/mixnr,
    /qaps/comnr TYPE /qaps/zmirm1-/qaps/comnr,
    /qaps/rmeng TYPE /qaps/zmirm1-/qaps/rmeng,
    /qaps/gemng TYPE /qaps/zmirm0-/qaps/gemng,
    /qaps/eqmng TYPE /qaps/zmirm0-/qaps/eqmng,
    /qaps/lemng TYPE /qaps/zmirm0-/qaps/lemng,
  END OF ty_rest_mist,

  BEGIN OF ty_zmimi,
    mandt       TYPE /qaps/zmimi-mandt,
    werks       TYPE /qaps/zmimi-werks,
    matnr       TYPE /qaps/zmimi-matnr,
    /qaps/rmeng TYPE /qaps/zmimi-/qaps/rmeng,
    /qaps/rmuni TYPE /qaps/zmimi-/qaps/rmuni,
  END OF ty_zmimi.


TYPES: BEGIN OF ty_zmicomp,
         matnr       TYPE /qaps/zmic1-matnr,
         werks       TYPE /qaps/zmic1-werks,
         /qaps/grkey TYPE /qaps/zmic1-/qaps/grkey,
         /qaps/comnr TYPE /qaps/zmic1-/qaps/comnr,
         /qaps/gemng TYPE /qaps/zmic1-/qaps/gemng,
         /qaps/eqmng TYPE /qaps/zmic1-/qaps/eqmng,
         /qaps/lemng TYPE /qaps/zmic1-/qaps/lemng,
       END OF ty_zmicomp.

TYPES: BEGIN OF ts_inicial,
         zdiv        TYPE i,
         reformulate TYPE i,
         combi(10)   TYPE c,
         comnr(18)   TYPE c,
         costt       TYPE /qaps/zmidfcig-/qaps/costt,
         msg(100)    TYPE c,
         it_re       TYPE STANDARD TABLE OF ty_re WITH KEY comnr,
       END OF ts_inicial.

DATA gt_initial TYPE TABLE OF ts_inicial..

DATA: it_zmicomp TYPE TABLE OF ty_zmicomp,
      wa_zmicomp TYPE ty_zmicomp.
*-------------------------------
*--- INTERNAL TABLES E WORKAREA
*-------------------------------
DATA:
  it_ma            TYPE HASHED TABLE OF ty_ma WITH UNIQUE KEY line col,
  it_vb            TYPE TABLE OF ty_vb WITH KEY chave,
  it_vc            TYPE TABLE OF ty_vc WITH KEY chave,
  it_comp          TYPE TABLE OF ty_comp,
  it_combi_final   TYPE TABLE OF ty_combi,
  it_combi_finalx  TYPE STANDARD TABLE OF ty_combi INITIAL SIZE 0 WITH HEADER LINE,
  it_combi_fim     TYPE TABLE OF ty_combi,
  it_min           TYPE TABLE OF ty_min,
  it_min2          TYPE TABLE OF ty_min,
  it_min_bk        TYPE TABLE OF ty_min,
  it_desmarcar     TYPE TABLE OF ty_desmarcar,
  it_pc            TYPE TABLE OF ty_pc WITH KEY comnr,
  it_pc_bk         TYPE TABLE OF ty_pc WITH KEY comnr,
  it_pc_0100       TYPE TABLE OF ty_pc WITH KEY comnr,
  it_pc_aux        TYPE TABLE OF ty_pc WITH KEY comnr,
  it_rs            TYPE TABLE OF ty_rs,
  it_rs_bk         TYPE TABLE OF ty_rs,
  it_incompat1     TYPE TABLE OF /qaps/zmi61,
  it_incompat2     TYPE TABLE OF /qaps/zmi61,
  it_prop_carac    TYPE TABLE OF ty_prop_carac,
  it_re            TYPE TABLE OF ty_re WITH KEY comnr,
  it_re_bk         TYPE TABLE OF ty_re WITH KEY comnr,
  it_re_re         TYPE TABLE OF ty_re WITH KEY comnr,
  it_rest_comp     TYPE TABLE OF ty_rest_comp,
  it_restcomp      TYPE TABLE OF /qaps/zmic1,
  it_rest_comp_ini TYPE TABLE OF ty_rest_comp,
  it_rest_comp_2   TYPE TABLE OF ty_rest_comp,
  it_rest_comp_bk  TYPE TABLE OF ty_rest_comp,
  it_rest_verif    TYPE TABLE OF ty_rest_comp,
  it_rest_mist     TYPE SORTED TABLE OF ty_rest_mist
                       WITH NON-UNIQUE KEY matnr /qaps/mixnr,
  it_zmicig0       TYPE TABLE OF /qaps/zmicig0 WITH KEY matnr werks,
  it_zmicig0_bk    TYPE TABLE OF /qaps/zmicig0 WITH KEY matnr werks,
  it_zmicig0_min   TYPE TABLE OF /qaps/zmicig0 WITH KEY matnr werks,
  it_zmicig0_aux   TYPE TABLE OF /qaps/zmicig0 WITH KEY matnr werks,
  it_zmic1         TYPE TABLE OF /qaps/zmic1,
  it_zmicig1       TYPE TABLE OF /qaps/zmicig1 WITH KEY /qaps/cignr,
  it_zmicig1_bk    TYPE TABLE OF /qaps/zmicig1 WITH KEY /qaps/cignr,
  it_zmicig1_min   TYPE TABLE OF /qaps/zmicig1 WITH KEY /qaps/cignr,
  it_zmicig1_aux   TYPE TABLE OF /qaps/zmicig1 WITH KEY /qaps/cignr,
  it_zmicig2       TYPE TABLE OF /qaps/zmicig2
                      WITH KEY matnr werks /qaps/grkey /qaps/cignr,
  it_zmicig2_bk    TYPE TABLE OF /qaps/zmicig2
                      WITH KEY matnr werks /qaps/grkey /qaps/cignr,
  it_zmicig2_min   TYPE TABLE OF /qaps/zmicig2
                      WITH KEY matnr werks /qaps/grkey /qaps/cignr,
  it_zmicig2_aux   TYPE TABLE OF /qaps/zmicig2
                      WITH KEY matnr werks /qaps/grkey /qaps/cignr,
  it_zmi01         TYPE TABLE OF /qaps/zmi01 WITH KEY matnr werks,
  it_zmi01_bk      TYPE TABLE OF /qaps/zmi01 WITH KEY matnr werks,
  it_zmi11_aux     TYPE TABLE OF /qaps/zmi11 WITH KEY matnr werks,
  it_zmi60         TYPE TABLE OF /qaps/zmi60 WITH KEY matnr werks /qaps/mixin,
  it_zmi60_bk      TYPE TABLE OF /qaps/zmi60 WITH KEY matnr werks /qaps/mixin,
  it_zmi61         TYPE TABLE OF /qaps/zmi61 WITH KEY matnr werks /qaps/mixin,
  it_zmi61_bk      TYPE TABLE OF /qaps/zmi61 WITH KEY matnr werks /qaps/mixin,
  it_zmimi         TYPE TABLE OF /qaps/zmimi WITH KEY werks matnr,
  it_zmimi_aux     TYPE TABLE OF /qaps/zmimi WITH KEY werks matnr.

DATA: it_zmic1_comp TYPE TABLE OF /qaps/zmic1,
      wa_zmic1_comp TYPE /qaps/zmic1.

DATA: it_ajuste TYPE TABLE OF /qaps/zmiajuste.

*  WORKAREA
DATA:
  wa_ct_comp(6)    TYPE n,
  wa_ct_rest_le(6) TYPE n,
  wa_ct_rest_eq(6) TYPE n,
  wa_ct_rest_ge(6) TYPE n,
  wa_ma            TYPE ty_ma,
  wa_vb            TYPE ty_vb,
  wa_vc            TYPE ty_vc,
  wa_pc            TYPE ty_pc,
  wa_pc_bk         TYPE ty_pc,
  wa_re            TYPE ty_re,
  wa_re_bk         TYPE ty_re,
  wa_re_re         TYPE ty_re,
  wa_comp          TYPE ty_comp,
  wa_min           TYPE ty_min,
  wa_min_bk        TYPE ty_min,
  wa_desmarcar     TYPE ty_desmarcar,
  wa_combi_final   TYPE ty_combi,
  wa_combi_finalx  TYPE ty_combi,
  wa_flag_min      TYPE sy-tabix,
  wa_rest_mist     TYPE ty_rest_mist,
  wa_prop_carac    TYPE ty_prop_carac,
  wa_cust_matnr    TYPE ty_cust_matnr,
  wa_msg0(100)     TYPE c,
  wa_msg15(100)    TYPE c,
  wa_zmicig0       TYPE /qaps/zmicig0,
  wa_zmicig1       TYPE /qaps/zmicig1,
  wa_zmicig2       TYPE /qaps/zmicig2,
  wa_zmi01         TYPE /qaps/zmi01,
  wa_zmi60         TYPE /qaps/zmi60,
  wa_zmi61         TYPE /qaps/zmi61,
  wa_zmimi         TYPE /qaps/zmimi.



********************
*  Variáveis       *
********************
DATA:
  lines    TYPE sy-tabix,
  subrc    TYPE sy-subrc,
  xrmeng   TYPE /qaps/zmirm1-/qaps/rmeng,
  tabix    TYPE sy-tabix,
  a        TYPE sy-tabix,
  b        TYPE sy-tabix,
  c        TYPE sy-tabix,
  d(6)     TYPE n,
  cp       TYPE sy-tabix,
  i        TYPE ty_vb-valor,
  j        TYPE ty_vb-valor,
  k        TYPE ty_vb-valor,
  lp       TYPE sy-tabix,
  n        TYPE ty_ma-valor,
  nv       TYPE sy-tabix,
  p        TYPE ty_vb-valor,
  pv       TYPE ty_vb-valor,
  r1       TYPE sy-tabix,
  r2       TYPE sy-tabix,
  r3       TYPE sy-tabix,
  tp       TYPE sy-tabix,
  tp_n     TYPE p,
  tp_custo TYPE p,
  x        TYPE sy-tabix,
  y        TYPE sy-tabix,
  z        TYPE sy-tabix.

DATA:
  aux1             TYPE /qaps/zmicig1-/qaps/comnr,
  aux2             TYPE /qaps/zmicig1-/qaps/comnr,
  aux3             TYPE /qaps/zmicig1-/qaps/comnr,
  aux4             TYPE /qaps/zmicig1-/qaps/comnr,
  aux5             TYPE /qaps/zmicig1-/qaps/comnr,
  aux6             TYPE /qaps/zmicig1-/qaps/comnr,
  aux7             TYPE /qaps/zmicig1-/qaps/comnr,
  custox1          TYPE /qaps/zmidfcig-/qaps/costt,
  custox2          TYPE /qaps/zmidfcig-/qaps/costt,
  custoxt          TYPE /qaps/zmidfcig-/qaps/costt,
  exp1             TYPE f  VALUE ' 1E38 ',
  ind(3)           TYPE c,
  ind_re(3)        TYPE c,
  ind_min(3)       TYPE c,
  ind_combi        TYPE sy-tabix,
  var1(6)          TYPE p,
  varj(6)          TYPE p,
  vark(6)          TYPE p,
  valork           TYPE i,
  valor_0001       TYPE f  VALUE  ' -.0001',
  valor_0001p      TYPE f  VALUE  ' .0001 ',
  valor1           TYPE f,
  valor2           TYPE f,
  valor2t          TYPE f,
  valor3           TYPE f,
  valori           TYPE i,
  valor1000n       TYPE f VALUE -1000,
  valorx(16)       TYPE p DECIMALS 10,
  xcombi(10)       TYPE c,
  xcombi_atual     TYPE /qaps/zmicig1-/qaps/cignr,
  xcombi_aux       TYPE /qaps/zmi61-/qaps/mixin,
  xcombi_min       TYPE /qaps/zmicig1-/qaps/cignr,
  xcombi_n(10)     TYPE n,
  xcombi_n2(10)    TYPE n,
  xcond_min        TYPE c,
  xcond_min0       TYPE c,
  xcond_min15      TYPE c,
  xchkey           TYPE /qaps/zmitc-/qaps/chkey,
  xmixnr           TYPE /qaps/zmirm0-/qaps/mixnr,
  xmixin           TYPE /qaps/zmi61-/qaps/mixin,
  xcomnr           TYPE /qaps/zmic1-/qaps/comnr,
  xcomnr_min       TYPE /qaps/zmic1-/qaps/comnr,
  xcod_min         TYPE c,
  xcmeng           TYPE /qaps/zmicig1-/qaps/cmeng,
  xcmeng_min       TYPE /qaps/zmicig1-/qaps/cmeng,
  xlemng           TYPE /qaps/zmirm0-/qaps/lemng,
  xeqmng           TYPE /qaps/zmirm0-/qaps/eqmng,
  xgemng           TYPE /qaps/zmirm0-/qaps/gemng,
  xstprs           TYPE mbew-stprs,
  xverpr           TYPE mbew-verpr,
  xfixcost         TYPE /qaps/zmidfcig-/qaps/costt,
  xcontab          TYPE /qaps/zmiquo0-/qaps/contab,
  xreposi          TYPE /qaps/zmiquo0-/qaps/reposi,
  xgerenc          TYPE /qaps/zmiquo0-/qaps/gerenc,
  xpriori          TYPE /qaps/zmiquo0-/qaps/priori,
  xgerxprio        TYPE /qaps/zmicig0-/qaps/gerxprio,
  xtotmkp          TYPE /qaps/zmilc1-/qaps/totmkp,
  xtotajmkt        TYPE /qaps/zmilc1-/qaps/totajmkt,
  xmcost           TYPE /qaps/zmicig0-/qaps/gerxprio,
  zcontab          TYPE /qaps/zmicig0-/qaps/gerxprio,
  zreposi          TYPE /qaps/zmicig0-/qaps/gerxprio,
  zgerenc          TYPE /qaps/zmicig0-/qaps/gerxprio,
  zgerxprio        TYPE /qaps/zmicig0-/qaps/gerxprio,
  ztotmkp          TYPE /qaps/zmilc1-/qaps/totmkp,
  ztotajmkt        TYPE /qaps/zmilc1-/qaps/totajmkt,
  zmcost           TYPE /qaps/zmicig0-/qaps/gerxprio,
  zcomnr           TYPE /qaps/zmic1-/qaps/comnr,
  ze               TYPE /qaps/zmidfcig-/qaps/costt,
  zdiv             TYPE i,
  zdiv_reformulate TYPE i,
  zajustou         TYPE c,
  zdeusolucao      TYPE c,
  zsemsolucao      TYPE c,
  lversao          TYPE /qaps/zmiqual01-/qaps/versao,
  gv_matrix        TYPE abap_bool VALUE abap_false,
  gt_rest_linha    TYPE TABLE OF /qaps/zmatr_rest,
  vl_gemng         TYPE p DECIMALS 6.

DATA: v_numqual     TYPE /qaps/zmi10-/qaps/numqual.
* ---------------------------------------------------------------------*
* Constantes                                                           *
* ---------------------------------------------------------------------*
CONSTANTS:
  c_s(01)  TYPE c VALUE 'S',
  c_n(01)  TYPE c VALUE 'N',
  c_p(01)  TYPE c VALUE 'P',
  c_k(01)  TYPE c VALUE 'K',
  c_b(01)  TYPE c VALUE 'B',
  c_zn(02) TYPE c VALUE 'ZN',
  c_ca(02) TYPE c VALUE 'CA',
  c_x(01)  TYPE c VALUE 'X'.

* ---------------------------------------------------------------------*
* Range                                                                *
* ---------------------------------------------------------------------*
RANGES: r_matnr FOR mara-matnr.

DATA: gv_reformulate   TYPE i,
      gv_key_increment TYPE /qaps/zmitc-/qaps/chkey.

CONSTANTS gc_reformulate  TYPE i VALUE 10.

TYPES: BEGIN OF ts_snapshot,
         zdiv        TYPE i,
         valido      TYPE flag,
         zmidfcig    TYPE /qaps/zmidfcig,
         combi_final TYPE ty_combi,
         t_re        LIKE it_re,
         t_ng        LIKE it_ng,
         t_pc_0100   LIKE it_pc_0100,
         t_rest_comp LIKE it_rest_comp,
         t_zmicig0   LIKE it_zmicig0,
         t_zmicig1   LIKE it_zmicig1,
         t_zmicig2   LIKE it_zmicig2,
         t_matrix    LIKE it_ma,
         path        TYPE string,
         reformulate TYPE i,
         matrix_html TYPE string,
       END OF ts_snapshot.

DATA: gt_snapshot    TYPE TABLE OF ts_snapshot.
