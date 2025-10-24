class /QAPS/CL_FORMULACAO_PROCESS_V2 definition
  public
  final
  create public .

public section.

  methods FORMULAR
    importing
      value(IS_HEADER) type /QAPS/S_FORMULACAO
      value(IT_RS_PARAM) type /QAPS/T_RS
      value(IT_PC_PARAM) type /QAPS/T_PC
      !IV_PRIO_VS_GERENCIAL type ABAP_BOOL default ABAP_TRUE
      !IV_GERENCIAL type ABAP_BOOL default ABAP_FALSE
      !IV_REPOSICAO type ABAP_BOOL default ABAP_FALSE
      !IV_CONTABIL type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_RE type /QAPS/T_RE
      !ET_NG type /QAPS/T_NG
      !ET_HEADER type /QAPS/T_ZMIFIC0
      !ET_ITEMS type /QAPS/T_ZMIFIC1
      !ET_NIVEIS_GARANTIA type /QAPS/T_ZMIFIC2
      !EV_MESSAGE type STRING
      !EV_TYPE type BAPI_MTYPE
      !EV_NUMBER type SYMSGNO .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      /qaps/zmi00    TYPE /qaps/zmi00,
      /qaps/zmi60    TYPE /qaps/zmi60,
      /qaps/zmicig0  TYPE /qaps/zmicig0,
      /qaps/zmidfcig TYPE /qaps/zmidfcig,
      /qaps/zmidfrs  TYPE /qaps/zmidfrs.

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
        it_re     TYPE STANDARD TABLE OF ty_re WITH KEY comnr,
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


    DATA it_re_hist TYPE TABLE OF ty_re_hist.
    DATA: it_ng    TYPE TABLE OF ty_ng WITH KEY chkey ord.
    DATA: it_dif   TYPE TABLE OF ty_dif.
    DATA gt_initial TYPE TABLE OF ts_inicial..

    DATA: it_zmicomp TYPE TABLE OF ty_zmicomp,
          wa_zmicomp TYPE ty_zmicomp.
    DATA:
      it_ma            TYPE HASHED TABLE OF ty_ma WITH UNIQUE KEY line col,
      it_vb            TYPE TABLE OF ty_vb WITH KEY chave,
      it_vc            TYPE TABLE OF ty_vc WITH KEY chave,
      it_comp          TYPE TABLE OF ty_comp,
      it_combi_final   TYPE TABLE OF ty_combi,
      it_combi_finalx  TYPE STANDARD TABLE OF ty_combi, " INITIAL SIZE 0 WITH HEADER LINE,
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
      i        TYPE sy-tabix,
      j        TYPE sy-tabix,
      k        TYPE sy-tabix,
      lp       TYPE sy-tabix,
      n        TYPE sy-tabix,
      nv       TYPE sy-tabix,
      p        TYPE sy-tabix,
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
      valor1           TYPE /qaps/zmirm1-/qaps/rmeng,
      valor2           TYPE /qaps/zmirm1-/qaps/rmeng,
      valor2t          TYPE f,
      valor3           TYPE /qaps/zmirm1-/qaps/rmeng,
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
    CONSTANTS:
      c_s(01)  TYPE c VALUE 'S',
      c_n(01)  TYPE c VALUE 'N',
      c_p(01)  TYPE c VALUE 'P',
      c_k(01)  TYPE c VALUE 'K',
      c_b(01)  TYPE c VALUE 'B',
      c_zn(02) TYPE c VALUE 'ZN',
      c_ca(02) TYPE c VALUE 'CA',
      c_x(01)  TYPE c VALUE 'X'.

    DATA r_matnr TYPE RANGE OF  mara-matnr.

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

    METHODS: ajustar_limites_rest_comp  CHANGING ct_data TYPE tt_rest_comp,
      ajusta_margem_seguranca_2 ,
      clear_reformulate ,
      clear_workarea ,
      combinations ,
      condicao_minimo_nivel01 ,
      condicao_minimo_nivel02 ,
      contingencia CHANGING cv_has_result TYPE abap_bool
                   RAISING  /qaps/cx_formulation_error
                            /qaps/cx_div_no_result
                            /qaps/cx_general,
      del_comd_min ,
      execute  RAISING /qaps/cx_general /qaps/cx_formulation_error /qaps/cx_div_no_result,
      exec_simulation RAISING /qaps/cx_general,
      exec_simulation_old RAISING /qaps/cx_general,
      fill_components ,
      fill_matrix_a  IMPORTING VALUE(p_nrlin) TYPE sy-tabix VALUE(p_nrcol) TYPE sy-tabix,
      fill_restricao_item ,
      fill_version ,
      finalize_simulation  RAISING /qaps/cx_general,
      first_formulation ,
      formulation ,
      f_reformulate_v1  IMPORTING lo_excep TYPE REF TO /qaps/cx_formulation_error,
      f_reformulate_v2 ,
      guarantee_levels ,
      guarantee_levels_min ,
      incompatibilidade ,
      load_cuts ,
      load_eq_restrictions ,
      load_ge_restrictions ,
      load_internal_tables ,
      load_le_restrictions ,
      load_matrix CHANGING cv_has_result TYPE abap_bool
                  RAISING  /qaps/cx_formulation_error
                           /qaps/cx_div_no_result
                           /qaps/cx_general,
      load_results  RAISING /qaps/cx_div_no_result /qaps/cx_general,
      load_results_2 RAISING /qaps/cx_div_no_result /qaps/cx_general,
      modifies_matrix ,
      ordena_niveis_garantia ,
      process_combi_1 ,
      process_combi_12 ,
      process_combi_123 ,
      process_combi_1234 ,
      process_combi_12345 ,
      process_combi_123456 ,
      process_combi_1235 ,
      process_combi_1236 ,
      process_combi_1237 ,
      process_combi_124 ,
      process_combi_125 ,
      process_combi_126 ,
      process_combi_127 ,
      process_combi_13 ,
      process_combi_134 ,
      process_combi_1345 ,
      process_combi_13456 ,
      process_combi_134567 ,
      process_combi_1346 ,
      process_combi_1347 ,
      process_combi_135 ,
      process_combi_136 ,
      process_combi_137 ,
      process_combi_14 ,
      process_combi_1456,
      process_combi_14567,
      process_combi_1457 ,
      process_combi_15 ,
      process_combi_1567,
      process_combi_16 ,
      process_combi_17 ,
      process_combi_2 ,
      process_combi_23 ,
      process_combi_234 ,
      process_combi_2345 ,
      process_combi_23456 ,
      process_combi_234567 ,
      process_combi_2346 ,
      process_combi_2347 ,
      process_combi_235 ,
      process_combi_2356 ,
      process_combi_2357 ,
      process_combi_236 ,
      process_combi_237 ,
      process_combi_24 ,
      process_combi_2456,
      process_combi_24567,
      process_combi_2457 ,
      process_combi_25 ,
      process_combi_26 ,
      process_combi_27 ,
      process_combi_3 ,
      process_combi_34 ,
      process_combi_345 ,
      process_combi_3456 ,
      process_combi_34567 ,
      process_combi_3457 ,
      process_combi_346 ,
      process_combi_347 ,
      process_combi_35 ,
      process_combi_36 ,
      process_combi_37 ,
      process_combi_4 ,
      process_combi_45 ,
      process_combi_456 ,
      process_combi_4567 ,
      process_combi_457 ,
      process_combi_46 ,
      process_combi_47 ,
      process_combi_5 ,
      process_combi_56 ,
      process_combi_567 ,
      process_combi_57 ,
      process_combi_6 ,
      process_combi_67 ,
      process_combi_7 ,
      process_it_min ,
      process_it_min_15 ,
      product_formulate RAISING /qaps/cx_formulation_error
                                /qaps/cx_div_no_result
                                /qaps/cx_general,
      regra_1  IMPORTING us_header TYPE  /qaps/s_formulacao
               RAISING   /qaps/cx_general,
      regra_2 IMPORTING us_header TYPE  /qaps/s_formulacao
              RAISING   /qaps/cx_general,
      resultado CHANGING cv_has_result TYPE abap_bool
                RAISING  /qaps/cx_formulation_error
                         /qaps/cx_div_no_result
                         /qaps/cx_general,
      resultado_3  CHANGING cv_has_result TYPE abap_bool
                   RAISING  /qaps/cx_formulation_error
                            /qaps/cx_div_no_result
                            /qaps/cx_general,
      resultado_4 CHANGING cv_has_result TYPE abap_bool
                  RAISING  /qaps/cx_formulation_error
                           /qaps/cx_div_no_result
                           /qaps/cx_general,
      save_snapshot,
      save_soluctions ,
      save_soluctions_min ,
      set_matrix_a  IMPORTING VALUE(p_line) TYPE sy-tabix VALUE(p_col) TYPE sy-tabix VALUE(p_valor) TYPE /qaps/zmi10-/qaps/rmeng,
      stage_1000 ,
      stage_1000_min ,
      stage_1080 ,
      stage_2000 ,
      stage_2180 ,
      stage_3000 ,
      stage_5050 ,
      stage_5130 ,
      stage_590 ,
      stage_650 ,
      step_2160 ,
      step_3060 ,
      step_4000 ,
      step_500 ,
      step_5010 ,
      step_510a530 ,
      test_it_min15 ,
      update_restricao ,
      validacoes_preliminares IMPORTING us_header TYPE  /qaps/s_formulacao
                              RAISING   /qaps/cx_general,
      validar_rest_comp  CHANGING ct_data TYPE tt_rest_comp,
      validar_snapshot CHANGING cv_valido TYPE abap_bool,
      verificar_condicoes ,
      verifica_cond_minimo ,
      verifica_niveis_garantia ,
      verifica_niveis_garantia_2 ,
      verifica_niveis_garantia_3 ,
      verif_incomp_e_formula ,
      zf_ajusta_garantia ,
      zf_calcula_restricao,
      zf_gravar_re_hist  IMPORTING p_wa_combi_total TYPE ty_combi,
      zf_refaz_ate_imcompatib RAISING /qaps/cx_div_no_result,
      zf_verifica_flags ,
      zf_verifica_rest_minimos  RAISING /qaps/cx_div_no_result /qaps/cx_general,
      zf_verifica_tudo ,
      zf_ver_comp_min  RAISING /qaps/cx_general.


ENDCLASS.



CLASS /QAPS/CL_FORMULACAO_PROCESS_V2 IMPLEMENTATION.


  METHOD AJUSTAR_LIMITES_REST_COMP.

    DATA: lt_gmeng TYPE tt_rest_comp,
          lt_lmeng TYPE tt_rest_comp.

    "Quando houver mais de 1 limite superior ou inferior para o mesmo componente
    "utilizar o mais restritivo

    DATA(lt_gemng_aux) = ct_data.
    DATA(lt_lmeng_aux) = ct_data.

    DELETE lt_gemng_aux WHERE /qaps/gemng IS INITIAL.
    DELETE lt_lmeng_aux WHERE /qaps/lemng IS INITIAL.

    DELETE ct_data WHERE NOT /qaps/gemng IS INITIAL
                      OR NOT /qaps/lemng IS INITIAL.

    SORT lt_gemng_aux BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng DESCENDING.
    LOOP AT lt_gemng_aux ASSIGNING FIELD-SYMBOL(<fs_data>).

      IF NOT line_exists( lt_gmeng[ matnr         = <fs_data>-matnr
                                    werks         = <fs_data>-werks
                                    /qaps/grkey   = <fs_data>-/qaps/grkey
                                    /qaps/comnr   = <fs_data>-/qaps/comnr ] ).

        APPEND <fs_data> TO lt_gmeng.
      ENDIF.

    ENDLOOP.

    SORT lt_lmeng_aux BY matnr werks /qaps/grkey /qaps/comnr /qaps/lemng ASCENDING.
    LOOP AT lt_lmeng_aux ASSIGNING <fs_data>.

      IF NOT line_exists( lt_lmeng[ matnr         = <fs_data>-matnr
                                    werks         = <fs_data>-werks
                                    /qaps/grkey   = <fs_data>-/qaps/grkey
                                    /qaps/comnr   = <fs_data>-/qaps/comnr ] ).

        APPEND <fs_data> TO lt_lmeng.
      ENDIF.

    ENDLOOP.

    APPEND LINES OF lt_gmeng TO ct_data.
    APPEND LINES OF lt_lmeng TO ct_data.

    SORT ct_data BY matnr werks /qaps/grkey /qaps/comnr.


  ENDMETHOD.


  METHOD AJUSTA_MARGEM_SEGURANCA_2 .

    DATA wa_rs            TYPE ty_rs.
    DATA wa_dif   TYPE ty_dif.
    DATA: l_tabix  TYPE sy-tabix.

    SELECT * FROM /qaps/zmiajuste
      INTO TABLE it_ajuste
        WHERE /qaps/chkey IN ('C_CA', 'C_ZN', 'C_B', 'C_S').

    READ TABLE it_dif INTO wa_dif INDEX 1.

    SORT it_rs BY chkey.

    LOOP AT it_zmi01 INTO wa_zmi01.
      l_tabix = sy-tabix.
      READ TABLE it_rs INTO wa_rs WITH KEY chkey = wa_zmi01-/qaps/chkey
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        IF wa_zmi01-/qaps/gmeng = 0 AND wa_rs-gmeng <> 0.
          wa_zmi01-/qaps/gmeng = wa_rs-gmeng.
          wa_zmi01-/qaps/emeng = wa_rs-emeng.
          wa_zmi01-/qaps/lmeng = wa_rs-lmeng.
          wa_zmi01-/qaps/ymeng = wa_rs-ymeng.
        ENDIF.
      ENDIF.
      IF  wa_zmi01-/qaps/chkey EQ wa_dif-chkey.
        IF   wa_zmi01-/qaps/gmeng GE 0  AND wa_zmi01-/qaps/gmeng LE 5.
          IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
            wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
          ELSE.
            wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
          ENDIF.
          MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
          zajustou = c_x.
          EXIT.
        ELSE.
          IF   wa_zmi01-/qaps/gmeng GT 5  AND wa_zmi01-/qaps/gmeng LE 10.
            IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
              wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
            ELSE.
              wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
            ENDIF.
            MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
            zajustou = c_x.
            EXIT.
          ELSE.
            IF   wa_zmi01-/qaps/gmeng GT 10  AND wa_zmi01-/qaps/gmeng LE 15.
              IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
              ELSE.
                wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
              ENDIF.
              MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
              zajustou = c_x.
              EXIT.
            ELSE.
              IF   wa_zmi01-/qaps/gmeng GT 15  AND wa_zmi01-/qaps/gmeng LE 20.
                IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                  wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
                ELSE.
                  wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
                ENDIF.
                MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
                zajustou = c_x.
                EXIT.
              ELSE.
                IF   wa_zmi01-/qaps/gmeng GT 20  AND wa_zmi01-/qaps/gmeng LE 25.
                  IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                    wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
                  ELSE.
                    wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
                  ENDIF.
                  MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
                  zajustou = c_x.
                  EXIT.
                ELSE.
                  IF   wa_zmi01-/qaps/gmeng GT 25  AND wa_zmi01-/qaps/gmeng LE 30.
                    IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                      wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
                    ELSE.
                      wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
                    ENDIF.
                    MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
                    zajustou = c_x.
                    EXIT.
                  ELSE.
                    IF   wa_zmi01-/qaps/gmeng GT 30  AND wa_zmi01-/qaps/gmeng LE 35.
                      IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                        wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
                      ELSE.
                        wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
                      ENDIF.
                      MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
                      zajustou = c_x.
                      EXIT.
                    ELSE.
                      IF   wa_zmi01-/qaps/gmeng GT 35.
                        IF  wa_dif-chkey = c_b OR wa_dif-chkey = c_zn.
                          wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 5 / 1000 ).
                        ELSE.
                          wa_zmi01-/qaps/gmeng = wa_zmi01-/qaps/gmeng + ( 2 / 1000 ).
                        ENDIF.
                        MODIFY it_zmi01 FROM wa_zmi01 INDEX l_tabix.
                        zajustou = c_x.
                        EXIT.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    " AJUSTA_MARGEM_SEGURANCA_2


  METHOD CLEAR_REFORMULATE .

    REFRESH: it_re_hist,
              it_ng,
              it_dif,
              it_combi_final,
              it_combi_fim,
              it_pc_bk,
              it_pc_0100,
              it_pc_aux,
              it_rs_bk,
              it_re_re,
              it_rest_comp_ini,
              it_restcomp,
              it_rest_comp_2,
              it_zmicig0,
              it_zmicig0_bk,
              it_zmicig0_min,
              it_zmicig1,
              it_zmicig1_bk,
              it_zmicig1_min,
              it_zmicig2,
              it_zmicig2_bk,
              it_zmicig2_min,
              it_zmi61,
              it_zmi60_bk,
              it_zmimi,
              it_zmimi_aux,
              it_zmic1_comp,
              it_zmic1_comp.

  ENDMETHOD.


  METHOD CLEAR_WORKAREA.

    REFRESH:
      it_ma,
      it_vb,
      it_vc,
      it_comp,
      it_incompat1,
      it_incompat2,
      it_prop_carac,
      it_re,
      it_re_bk,
      it_rest_comp,
      it_rest_mist,
      it_ng,
      it_re.

    CLEAR:
        wa_ct_comp,
        wa_ct_rest_le,
        wa_ct_rest_eq,
        wa_ct_rest_ge,
        wa_ma,
        wa_vb,
        wa_vc,
        wa_pc,
        wa_re,
        wa_re_bk,
        wa_comp,
        wa_rest_mist,
        wa_prop_carac,
        wa_cust_matnr,
        wa_combi_final-msg,
        wa_zmicig0,
        wa_zmi01,
        wa_zmi60.

    CLEAR:
          lines,
          xrmeng,
          tabix,
          a,
          b,
          c,
          d,
          cp,
          i,
          j ,
          k,
          lp,
          n,
          nv,
          p,
          pv,
          r1,
          r2,
          r3,
          tp,
          tp_n,
          tp_custo,
          x,
          y,
          z.

    CLEAR:
          ind,
          var1,
          varj,
          vark,
          valork,
          valor1,
          valor2,
          valor2t,
          valor3,
          ze,
          custox1,
          custox2,
          custoxt,
          xchkey,
          xmixnr,
          xcomnr,
          xcmeng,
          xlemng,
          xeqmng,
          xgemng,
          xstprs,
          xverpr,
          zcomnr,
          xcontab,
          zcontab,
          xgerenc,
          zgerenc,
          xreposi,
          zreposi,
          xgerxprio,
          zgerxprio,
          xtotmkp,
          ztotmkp,
          xtotajmkt,
          ztotajmkt,
          xmcost,
          zmcost.

    CLEAR: zsemsolucao.

  ENDMETHOD.                    " CLEAR_WORKAREA


  METHOD COMBINATIONS.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    LOOP AT it_re INTO wa_re.
      wa_comp-comnr = wa_re-comnr.
      APPEND wa_comp TO it_comp.
      wa_pc-comnr = wa_re-comnr.
      wa_pc-coktx = wa_re-coktx.
      APPEND wa_pc  TO it_pc_bk.
    ENDLOOP.

    CALL METHOD zf_calcula_restricao.

    SELECT *
     FROM /qaps/zmic1
     WHERE matnr = @/qaps/zmidfcig-matnr AND
           werks = @/qaps/zmidfcig-werks AND
           /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
     INTO TABLE @DATA(lt_zmic1).

    LOOP AT lt_zmic1 INTO DATA(ls_zmic1).
      CHECK NOT line_exists( it_pc[ comnr = ls_zmic1-/qaps/comnr ] ).
      APPEND INITIAL LINE TO it_rest_comp ASSIGNING FIELD-SYMBOL(<fs_rest_comp>).
      <fs_rest_comp> = CORRESPONDING #( ls_zmic1 ).
    ENDLOOP.

    IF NOT it_zmicomp[] IS INITIAL.
      DELETE it_zmicomp WHERE /qaps/gemng = 0 AND /qaps/eqmng = 0 AND /qaps/lemng = 0.
      SORT it_zmicomp BY matnr werks /qaps/grkey /qaps/comnr.
      DELETE ADJACENT DUPLICATES FROM it_zmicomp
                      COMPARING  matnr werks /qaps/grkey /qaps/comnr.

      LOOP AT it_zmicomp INTO wa_zmicomp.
        IF NOT line_exists( it_rest_comp[ /qaps/comnr = wa_zmicomp-/qaps/comnr ] ).
          APPEND INITIAL LINE TO it_rest_comp ASSIGNING <fs_rest_comp>.
          IF line_exists( it_pc[ comnr = wa_zmicomp-/qaps/comnr ] ).
            DATA(ls_pc) = it_pc[ comnr = wa_zmicomp-/qaps/comnr ].

            <fs_rest_comp>-matnr = wa_zmicomp-matnr.
            <fs_rest_comp>-werks = wa_zmicomp-werks.
            <fs_rest_comp>-/qaps/grkey = wa_zmicomp-/qaps/grkey.
            <fs_rest_comp>-/qaps/comnr = ls_pc-comnr.
            <fs_rest_comp>-/qaps/gemng = ls_pc-gemng.
            <fs_rest_comp>-/qaps/eqmng = ls_pc-eqmng.
            <fs_rest_comp>-/qaps/lemng = ls_pc-lemng.

          ELSE.
            MOVE-CORRESPONDING wa_zmicomp TO wa_rest_comp.
            APPEND wa_rest_comp TO  it_rest_comp.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT it_rest_comp INTO wa_rest_comp.
      READ TABLE it_comp WITH KEY comnr = wa_rest_comp-/qaps/comnr
                                                   TRANSPORTING NO FIELDS
  .
      IF sy-subrc <> 0.
        DELETE it_rest_comp.
      ENDIF.

    ENDLOOP.

    LOOP AT it_comp INTO wa_comp.

      REFRESH: it_zmi61_bk.

      SELECT * INTO TABLE it_zmi61_bk FROM /qaps/zmi61
         WHERE   matnr = wa_comp-comnr  AND
                 werks = /qaps/zmidfcig-werks.

      LOOP AT it_zmi61_bk INTO  wa_zmi61.
        APPEND wa_zmi61 TO it_zmi61.
      ENDLOOP.

    ENDLOOP.

    DATA l_tabix_61  TYPE sy-tabix.

    LOOP AT it_zmi61  INTO  wa_zmi61.
      l_tabix_61  = sy-tabix.
      READ TABLE it_comp INTO wa_comp WITH KEY comnr =  wa_zmi61-/qaps/comnr.
      IF sy-subrc NE 0.
        DELETE it_zmi61_bk INDEX   l_tabix_61.
      ENDIF.
    ENDLOOP.


    LOOP AT it_rest_comp INTO wa_rest_comp.
      READ TABLE it_zmi61 INTO wa_zmi61
                              WITH KEY matnr = wa_rest_comp-/qaps/comnr
                                       werks = wa_rest_comp-werks.

      IF sy-subrc = 0.

        xmixin = wa_zmi61-/qaps/mixin.

        LOOP AT it_zmi61 INTO wa_zmi61 WHERE /qaps/mixin = xmixin.

          DELETE it_comp WHERE comnr = wa_zmi61-/qaps/comnr.
          DELETE it_re WHERE comnr = wa_zmi61-/qaps/comnr.
          DELETE it_pc WHERE comnr = wa_zmi61-/qaps/comnr.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

    LOOP AT it_rest_comp INTO wa_rest_comp.

      LOOP AT it_zmi61 INTO wa_zmi61
               WHERE matnr =  wa_rest_comp-/qaps/comnr.

        wa_zmi60-matnr = wa_zmi61-matnr.
        wa_zmi60-werks = wa_zmi61-werks.
        wa_zmi60-/qaps/mixin = wa_zmi61-/qaps/mixin.

        APPEND wa_zmi60 TO it_zmi60.

      ENDLOOP.

    ENDLOOP.

    SORT  it_zmi60 BY  matnr werks /qaps/mixin.

    DELETE ADJACENT DUPLICATES FROM it_zmi60
                       COMPARING matnr werks /qaps/mixin.

    DESCRIBE TABLE it_zmi60 LINES lines.

    IF lines <> 0.

      LOOP AT it_zmi61 INTO wa_zmi61.

        SELECT SINGLE * INTO  /qaps/zmi60
        FROM /qaps/zmi60
        WHERE matnr = wa_zmi61-matnr AND
              werks = /qaps/zmidfcig-werks AND
              /qaps/mixin = wa_zmi61-/qaps/mixin.

        wa_zmi60-matnr = /qaps/zmi60-matnr.
        wa_zmi60-werks = /qaps/zmi60-werks.
        wa_zmi60-/qaps/mixin = /qaps/zmi60-/qaps/mixin.

        APPEND wa_zmi60 TO it_zmi60.

      ENDLOOP.

      SORT it_zmi60 BY matnr werks /qaps/mixin.

      DELETE ADJACENT DUPLICATES FROM it_zmi60
                 COMPARING matnr werks /qaps/mixin.

    ENDIF.

  ENDMETHOD.                    " COMBINATIONS


  METHOD CONDICAO_MINIMO_NIVEL01 .

    DATA  l_tabix TYPE sy-tabix.

    CLEAR it_desmarcar[].
    REFRESH it_desmarcar.

    CALL METHOD update_restricao.

    LOOP AT it_zmicig1 INTO wa_zmicig1 WHERE /qaps/cignr =  xcombi_min.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = wa_zmicig1-/qaps/comnr.
      IF  sy-subrc = 0.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.
        IF sy-subrc = 0.
          IF wa_zmicig1-/qaps/cmeng GE wa_zmimi-/qaps/rmeng.
            LOOP AT it_zmi61 INTO wa_zmi61 WHERE matnr = wa_zmicig1-/qaps/comnr.
              wa_desmarcar-comnr = wa_zmi61-/qaps/comnr.
              APPEND wa_desmarcar TO it_desmarcar.
            ENDLOOP.
          ENDIF.
        ELSE.
          LOOP AT it_zmi61 INTO wa_zmi61 WHERE matnr = wa_zmicig1-/qaps/comnr.
            wa_desmarcar-comnr = wa_zmi61-/qaps/comnr.
            APPEND wa_desmarcar TO it_desmarcar.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.


    SORT it_min BY comnr.
    DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.
    DESCRIBE TABLE it_min LINES lines.
*
    DELETE it_zmicig0 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig1 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig2 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig0_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig1_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig2_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_combi_final WHERE combi =  xcombi_min.


    IF lines = 1.

      READ TABLE it_min INTO wa_min INDEX 1.
      aux1 = wa_min-comnr.

      CLEAR: xcond_min, ind_re.
      xcombi = xcombi + 11100.

      CLEAR: wa_combi_final-msg, wa_msg0.
      xcond_min0 = ' '.
      xcombi =  wa_combi_finalx-combi + 20.
      CALL METHOD process_it_min.
      CLEAR:  zajustou..
      wa_msg0 = wa_combi_final-msg.

      CLEAR: wa_combi_final-msg, wa_msg15.
      xcond_min15 = ' '.
      xcombi = wa_combi_finalx-combi + 200.
      CALL METHOD process_it_min_15.
      CLEAR:  zajustou.
      wa_msg15 = wa_combi_final-msg.

      LOOP AT it_min INTO wa_min.
        APPEND wa_min TO it_min_bk.
      ENDLOOP.

    ELSE.


      IF lines = 2.

        CLEAR xcombi_aux.
        CLEAR: xcond_min.

        xcombi = xcombi  + 10 .
        CLEAR: wa_combi_final-msg.
        it_pc_bk = it_pc.
        CALL METHOD process_it_min.
        CLEAR:  zajustou..

        xcombi = xcombi + 10.
        CLEAR: wa_combi_final-msg.
        CALL METHOD process_it_min_15.
        CLEAR:  zajustou..

        READ TABLE it_min INTO wa_min INDEX 1.
        aux1 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 2.
        aux2 = wa_min-comnr.

        CALL METHOD update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100.
        CALL METHOD process_combi_1.
        CLEAR:  zajustou..

        CALL METHOD update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 1111.
        CALL METHOD process_combi_2.
        CLEAR:  zajustou..

        CALL METHOD update_restricao.

        LOOP AT it_min INTO wa_min.
          APPEND wa_min TO it_min_bk.
        ENDLOOP.

      ELSE.


        IF lines = 3.
          CLEAR xcombi_aux.
          CLEAR: xcond_min.
          xcombi = xcombi + 1000.
          CLEAR: wa_combi_final-msg.
          CALL METHOD process_it_min.
          CLEAR:  zajustou..

          CLEAR: wa_combi_final-msg.
          xcombi =  xcombi + 888  + 1033.
          CALL METHOD process_it_min_15.
          CLEAR:  zajustou..

          READ TABLE it_min INTO wa_min INDEX 1.
          aux1 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 2.
          aux2 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 3.
          aux3 = wa_min-comnr.

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30001  + 1033.
          CALL METHOD process_combi_1.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30002  + 1033.
          CALL METHOD process_combi_2.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30003  + 1033.
          CALL METHOD process_combi_3.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 200012  + 1033.
          CALL METHOD process_combi_12.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 100013  + 1033.
          CALL METHOD process_combi_13.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 100023  + 1033.
          CALL METHOD process_combi_23.
          CLEAR:  zajustou..

          LOOP AT it_min INTO wa_min.
            APPEND wa_min TO it_min_bk.
          ENDLOOP.

        ELSE.


          IF lines EQ 4.
            CLEAR xcombi_aux.
            CLEAR: xcond_min.
            xcombi = 10000.

            CLEAR: wa_combi_final-msg.
            CALL METHOD process_it_min.

            CLEAR: wa_combi_final-msg.
            xcombi = 88888.
            CALL METHOD process_it_min_15.
            READ TABLE it_min INTO wa_min INDEX 1.
            aux1 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 2.
            aux2 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 3.
            aux3 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 4.
            aux4 = wa_min-comnr.

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100001.
            CALL METHOD process_combi_1.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100002.
            CALL METHOD process_combi_2.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100003.
            CALL METHOD process_combi_3.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100004.
            CALL METHOD process_combi_4.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100012.
            CALL METHOD process_combi_12.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100013.
            CALL METHOD process_combi_13.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100014.
            CALL METHOD process_combi_14.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100023.
            CALL METHOD process_combi_23.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100024.
            CALL METHOD process_combi_24.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100034.
            CALL METHOD process_combi_34.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000123.
            CALL METHOD process_combi_123.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000124.
            CALL METHOD process_combi_124.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000134.
            CALL METHOD process_combi_134.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000234.
            CALL METHOD process_combi_234.
            CLEAR:  zajustou..

            LOOP AT it_min INTO wa_min.
              APPEND wa_min TO it_min_bk.
            ENDLOOP.

          ELSE.


            IF lines EQ 5.
              CLEAR xcombi_aux.

              CLEAR: xcond_min.
              xcombi = 100000.
              CLEAR: wa_combi_final-msg.
              CALL METHOD process_it_min.
              CLEAR:  zajustou..

              CLEAR: wa_combi_final-msg.
              xcombi = 88888.
              CALL METHOD process_it_min_15.
              CLEAR:  zajustou..

              READ TABLE it_min INTO wa_min INDEX 1.
              aux1 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 2.
              aux2 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 3.
              aux3 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 4.
              aux4 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 5.
              aux5 = wa_min-comnr.

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001.
              CALL METHOD process_combi_1.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002.
              CALL METHOD process_combi_2.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003.
              CALL METHOD process_combi_3.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000004.
              CALL METHOD process_combi_4.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000005.
              CALL METHOD process_combi_5.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000012.
              CALL METHOD process_combi_12.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000013.
              CALL METHOD process_combi_13.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000014.
              CALL METHOD process_combi_14.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000015.
              CALL METHOD process_combi_15.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000023.
              CALL METHOD process_combi_23.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000024.
              CALL METHOD process_combi_24.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000025.
              CALL METHOD process_combi_25.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000034.
              CALL METHOD process_combi_34.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000035.
              CALL METHOD process_combi_35.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000123.
              CALL METHOD process_combi_123.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000124.
              CALL METHOD process_combi_124.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000125.
              CALL METHOD process_combi_125.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000134.
              CALL METHOD process_combi_134.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000135.
              CALL METHOD process_combi_135.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000234.
              CALL METHOD process_combi_234.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000235.
              CALL METHOD process_combi_235.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000345.
              CALL METHOD process_combi_345.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001234.
              CALL METHOD process_combi_1234.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001235.
              CALL METHOD process_combi_1235.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002345.
              CALL METHOD process_combi_2345.
              CLEAR:  zajustou..

              LOOP AT it_min INTO wa_min.
                APPEND wa_min TO it_min_bk.
              ENDLOOP.

            ELSE.


              IF lines EQ 6.

                CLEAR: xcond_min.
                xcombi = 100000.
                CLEAR: wa_combi_final-msg.
                CALL METHOD process_it_min.
                CLEAR:  zajustou..

                CLEAR: wa_combi_final-msg.
                xcombi = 88888.
                CALL METHOD process_it_min_15.
                CLEAR:  zajustou..

                READ TABLE it_min INTO wa_min INDEX 1.
                aux1 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 2.
                aux2 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 3.
                aux3 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 4.
                aux4 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 5.
                aux5 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 6.
                aux6 = wa_min-comnr.

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001.
                CALL METHOD process_combi_1.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002.
                CALL METHOD process_combi_2.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003.
                CALL METHOD process_combi_3.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000004.
                CALL METHOD process_combi_4.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000005.
                CALL METHOD process_combi_5.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000006.
                CALL METHOD process_combi_6.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000012.
                CALL METHOD process_combi_12.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000013.
                CALL METHOD process_combi_13.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000014.
                CALL METHOD process_combi_14.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000016.
                CALL METHOD process_combi_16.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000023.
                CALL METHOD process_combi_23.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000024.
                CALL METHOD process_combi_24.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000025.
                CALL METHOD process_combi_25.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000026.
                CALL METHOD process_combi_26.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000034.
                CALL METHOD process_combi_34.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000035.
                CALL METHOD process_combi_35.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000036.
                CALL METHOD process_combi_36.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000123.
                CALL METHOD process_combi_123.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000124.
                CALL METHOD process_combi_124.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000125.
                CALL METHOD process_combi_125.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000126.
                CALL METHOD process_combi_126.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000134.
                CALL METHOD process_combi_134.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000135.
                CALL METHOD process_combi_135.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000136.
                CALL METHOD process_combi_136.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000234.
                CALL METHOD process_combi_234.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000235.
                CALL METHOD process_combi_235.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000236.
                CALL METHOD process_combi_236.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000345.
                CALL METHOD process_combi_345.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000346.
                CALL METHOD process_combi_346.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001234.
                CALL METHOD process_combi_1234.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001235.
                CALL METHOD process_combi_1235.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001236.
                CALL METHOD process_combi_1236.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002345.
                CALL METHOD process_combi_2345.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002346.
                CALL METHOD process_combi_2346.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002356.
                CALL METHOD process_combi_2356.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003456.
                CALL METHOD process_combi_3456.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                LOOP AT it_min INTO wa_min.
                  APPEND wa_min TO it_min_bk.
                ENDLOOP.

              ELSE.


                IF lines EQ 7.

                  CLEAR: xcond_min.
                  xcombi = 100000.
                  CLEAR: wa_combi_final-msg.
                  CALL METHOD process_it_min.
                  CLEAR:  zajustou..

                  CLEAR: wa_combi_final-msg.
                  xcombi = 88888.
                  CALL METHOD process_it_min_15.
                  CLEAR:  zajustou..

                  READ TABLE it_min INTO wa_min INDEX 1.
                  aux1 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 2.
                  aux2 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 3.
                  aux3 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 4.
                  aux4 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 5.
                  aux5 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 6.
                  aux6 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 7.
                  aux7 = wa_min-comnr.

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000001.
                  CALL METHOD process_combi_1.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000002.
                  CALL METHOD process_combi_2.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000003.
                  CALL METHOD process_combi_3.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000004.
                  CALL METHOD process_combi_4.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000005.
                  CALL METHOD process_combi_5.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000006.
                  CALL METHOD process_combi_6.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000007.
                  CALL METHOD process_combi_7.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000012.
                  CALL METHOD process_combi_12.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000013.
                  CALL METHOD process_combi_13.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000014.
                  CALL METHOD process_combi_14.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000016.
                  CALL METHOD process_combi_16.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000017.
                  CALL METHOD process_combi_17.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000023.
                  CALL METHOD process_combi_23.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000024.
                  CALL METHOD process_combi_24.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000025.
                  CALL METHOD process_combi_25.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000026.
                  CALL METHOD process_combi_26.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000027.
                  CALL METHOD process_combi_27.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000034.
                  CALL METHOD process_combi_34.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000035.
                  CALL METHOD process_combi_35.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000036.
                  CALL METHOD process_combi_36.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000037.
                  CALL METHOD process_combi_37.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000045.
                  CALL METHOD process_combi_45.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000046.
                  CALL METHOD process_combi_46.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000047.
                  CALL METHOD process_combi_47.
                  CLEAR:  zajustou.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000056.
                  CALL METHOD process_combi_56.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000057.
                  CALL METHOD process_combi_57.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000067.
                  CALL METHOD process_combi_67.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000123.
                  CALL METHOD process_combi_123.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000124.
                  CALL METHOD process_combi_124.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000125.
                  CALL METHOD process_combi_125.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000126.
                  CALL METHOD process_combi_126.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000127.
                  CALL METHOD process_combi_127.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000134.
                  CALL METHOD process_combi_134.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000135.
                  CALL METHOD process_combi_135.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000136.
                  CALL METHOD process_combi_136.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000137.
                  CALL METHOD process_combi_137.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000234.
                  CALL METHOD process_combi_234.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000235.
                  CALL METHOD process_combi_235.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000236.
                  CALL METHOD process_combi_236.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000237.
                  CALL METHOD process_combi_237.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000345.
                  CALL METHOD process_combi_345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000346.
                  CALL METHOD process_combi_346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000347.
                  CALL METHOD process_combi_347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000456.
                  CALL METHOD process_combi_456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000457.
                  CALL METHOD process_combi_457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000567.
                  CALL METHOD process_combi_567.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000001234.
                  CALL METHOD process_combi_1234.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001235.
                  CALL METHOD process_combi_1235.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001236.
                  CALL METHOD process_combi_1236.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001237.
                  CALL METHOD process_combi_1237.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001345.
                  CALL METHOD process_combi_1345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001347.
                  CALL METHOD process_combi_1346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001347.
                  CALL METHOD process_combi_1347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001456.
                  CALL METHOD process_combi_1456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001457.
                  CALL METHOD process_combi_1457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001567.
                  CALL METHOD process_combi_1567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002345.
                  CALL METHOD process_combi_2345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002346.
                  CALL METHOD process_combi_2346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002347.
                  CALL METHOD process_combi_2347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002356.
                  CALL METHOD process_combi_2356.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002357.
                  CALL METHOD process_combi_2357.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002456.
                  CALL METHOD process_combi_2456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002457.
                  CALL METHOD process_combi_2457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10003456.
                  CALL METHOD process_combi_3456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10003457.
                  CALL METHOD process_combi_3457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10004567.
                  CALL METHOD process_combi_4567.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10012345.
                  CALL METHOD process_combi_12345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10013456.
                  CALL METHOD process_combi_13456.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10014567.
                  CALL METHOD process_combi_14567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10023456.
                  CALL METHOD process_combi_23456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10024567.
                  CALL METHOD process_combi_24567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10034567.
                  CALL METHOD process_combi_34567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100123456.
                  CALL METHOD process_combi_123456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100134567.
                  CALL METHOD process_combi_134567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100234567.
                  CALL METHOD process_combi_234567.
                  CLEAR:  zajustou..

                  LOOP AT it_min INTO wa_min.
                    APPEND wa_min TO it_min_bk.
                  ENDLOOP.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.
    LOOP AT it_combi_final INTO wa_combi_final.
      l_tabix = sy-tabix.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = wa_combi_final-combi.
      IF sy-subrc NE 0.
        DELETE it_combi_final INDEX l_tabix.
      ENDIF.
    ENDLOOP.
    DELETE it_combi_final WHERE costt = 0.
    SORT it_combi_final BY costt combi.
    DELETE  ADJACENT DUPLICATES FROM it_combi_final COMPARING costt.
    READ TABLE it_combi_final INTO wa_combi_final  INDEX 1.
    xcombi = wa_combi_final-combi.

  ENDMETHOD.                    " CONDICAO_MINIMO_NIVEL01


  METHOD CONDICAO_MINIMO_NIVEL02 .

    DATA l_tabix TYPE sy-tabix.

    CLEAR it_desmarcar[].
    REFRESH it_desmarcar.

    CALL METHOD update_restricao.

    LOOP AT it_zmicig1 INTO wa_zmicig1 WHERE /qaps/cignr =  xcombi_min.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = wa_zmicig1-/qaps/comnr.
      IF  sy-subrc = 0.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.
        IF sy-subrc = 0.
          IF wa_zmicig1-/qaps/cmeng GE wa_zmimi-/qaps/rmeng.
            LOOP AT it_zmi61 INTO wa_zmi61 WHERE matnr = wa_zmicig1-/qaps/comnr.
              wa_desmarcar-comnr = wa_zmi61-/qaps/comnr.
              APPEND wa_desmarcar TO it_desmarcar.
            ENDLOOP.
          ENDIF.
        ELSE.
          LOOP AT it_zmi61 INTO wa_zmi61 WHERE matnr = wa_zmicig1-/qaps/comnr.
            wa_desmarcar-comnr = wa_zmi61-/qaps/comnr.
            APPEND wa_desmarcar TO it_desmarcar.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE it_zmicig0 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig1 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig2 WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig0_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig1_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_zmicig2_min WHERE /qaps/cignr =  xcombi_min.
    DELETE it_combi_final WHERE combi =  xcombi_min.

    SORT it_min BY comnr.
    DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.
    DESCRIBE TABLE it_min LINES lines.

    IF lines = 0.
      EXIT.
    ELSE.
      LOOP AT it_min_bk INTO wa_min_bk.
        CLEAR: xcmeng_min.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min_bk-comnr.
        IF sy-subrc = 0.
          READ TABLE it_min INTO wa_min WITH KEY comnr = wa_min_bk-comnr.
          IF sy-subrc NE 0.
            xcond_min = 'X'.
            xcmeng_min = wa_zmimi-/qaps/rmeng.
            wa_min-combi = wa_zmicig1-/qaps/cignr.
            wa_min-comnr = wa_min_bk-comnr.
            wa_min-ind = wa_min-ind + 1.
            APPEND wa_min TO it_min.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
*
    SORT it_min BY comnr.
    DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.
    DESCRIBE TABLE it_min LINES lines.

    IF lines = 1.

      READ TABLE it_min INTO wa_min INDEX 1.
      aux1 = wa_min-comnr.

      CLEAR: xcond_min, ind_re.
      xcombi = xcombi + 11100.

      CLEAR: wa_combi_final-msg, wa_msg0.
      xcond_min0 = ' '.
      xcombi =  wa_combi_finalx-combi + 20.
      CALL METHOD process_it_min.
      CLEAR:  zajustou..
      wa_msg0 = wa_combi_final-msg.

      CLEAR: wa_combi_final-msg, wa_msg15.
      xcond_min15 = ' '.
      xcombi = wa_combi_finalx-combi + 200.
      CALL METHOD process_it_min_15.
      CLEAR:  zajustou.
      wa_msg15 = wa_combi_final-msg.

      LOOP AT it_min INTO wa_min.
        APPEND wa_min TO it_min_bk.
      ENDLOOP.

    ELSE.


      IF lines = 2.

        CLEAR xcombi_aux.
        CLEAR: xcond_min.

        xcombi = xcombi  + 10 .
        CLEAR: wa_combi_final-msg.
        it_pc_bk = it_pc.
        CALL METHOD process_it_min.
        CLEAR:  zajustou..

        xcombi = xcombi + 10.
        CLEAR: wa_combi_final-msg.
        CALL METHOD process_it_min_15.
        CLEAR:  zajustou..

        READ TABLE it_min INTO wa_min INDEX 1.
        aux1 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 2.
        aux2 = wa_min-comnr.

        CALL METHOD update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100.
        CALL METHOD process_combi_1.
        CLEAR:  zajustou..

        CALL METHOD update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 1111.
        CALL METHOD process_combi_2.
        CLEAR:  zajustou..

        CALL METHOD update_restricao.

        LOOP AT it_min INTO wa_min.
          APPEND wa_min TO it_min_bk.
        ENDLOOP.

      ELSE.


        IF lines = 3.
          CLEAR xcombi_aux.
          CLEAR: xcond_min.
          xcombi = xcombi + 1000.
          CLEAR: wa_combi_final-msg.
          CALL METHOD process_it_min.
          CLEAR:  zajustou..

          CLEAR: wa_combi_final-msg.
          xcombi =  xcombi + 888  + 1033.
          CALL METHOD process_it_min_15.
          CLEAR:  zajustou..

          READ TABLE it_min INTO wa_min INDEX 1.
          aux1 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 2.
          aux2 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 3.
          aux3 = wa_min-comnr.

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30001  + 1033.
          CALL METHOD process_combi_1.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30002  + 1033.
          CALL METHOD process_combi_2.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 30003  + 1033.
          CALL METHOD process_combi_3.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 200012  + 1033.
          CALL METHOD process_combi_12.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 100013  + 1033.
          CALL METHOD process_combi_13.
          CLEAR:  zajustou..

          CALL METHOD update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = xcombi + 100023  + 1033.
          CALL METHOD process_combi_23.
          CLEAR:  zajustou..

          LOOP AT it_min INTO wa_min.
            APPEND wa_min TO it_min_bk.
          ENDLOOP.

        ELSE.


          IF lines EQ 4.
            CLEAR xcombi_aux.
            CLEAR: xcond_min.
            xcombi = 10000.

            CLEAR: wa_combi_final-msg.
            CALL METHOD process_it_min.

            CLEAR: wa_combi_final-msg.
            xcombi = 88888.
            CALL METHOD process_it_min_15.
            READ TABLE it_min INTO wa_min INDEX 1.
            aux1 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 2.
            aux2 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 3.
            aux3 = wa_min-comnr.

            READ TABLE it_min INTO wa_min INDEX 4.
            aux4 = wa_min-comnr.

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100001.
            CALL METHOD process_combi_1.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100002.
            CALL METHOD process_combi_2.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100003.
            CALL METHOD process_combi_3.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100004.
            CALL METHOD process_combi_4.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100012.
            CALL METHOD process_combi_12.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100013.
            CALL METHOD process_combi_13.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100014.
            CALL METHOD process_combi_14.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100023.
            CALL METHOD process_combi_23.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100024.
            CALL METHOD process_combi_24.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100034.
            CALL METHOD process_combi_34.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000123.
            CALL METHOD process_combi_123.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000124.
            CALL METHOD process_combi_124.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000134.
            CALL METHOD process_combi_134.
            CLEAR:  zajustou..

            CALL METHOD update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000234.
            CALL METHOD process_combi_234.
            CLEAR:  zajustou..

            LOOP AT it_min INTO wa_min.
              APPEND wa_min TO it_min_bk.
            ENDLOOP.

          ELSE.


            IF lines EQ 5.
              CLEAR xcombi_aux.

              CLEAR: xcond_min.
              xcombi = 100000.
              CLEAR: wa_combi_final-msg.
              CALL METHOD process_it_min.
              CLEAR:  zajustou..

              CLEAR: wa_combi_final-msg.
              xcombi = 88888.
              CALL METHOD process_it_min_15.
              CLEAR:  zajustou..

              READ TABLE it_min INTO wa_min INDEX 1.
              aux1 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 2.
              aux2 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 3.
              aux3 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 4.
              aux4 = wa_min-comnr.

              READ TABLE it_min INTO wa_min INDEX 5.
              aux5 = wa_min-comnr.

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001.
              CALL METHOD process_combi_1.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002.
              CALL METHOD process_combi_2.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003.
              CALL METHOD process_combi_3.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000004.
              CALL METHOD process_combi_4.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000005.
              CALL METHOD process_combi_5.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000012.
              CALL METHOD process_combi_12.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000013.
              CALL METHOD process_combi_13.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000014.
              CALL METHOD process_combi_14.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000015.
              CALL METHOD process_combi_15.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000023.
              CALL METHOD process_combi_23.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000024.
              CALL METHOD process_combi_24.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000025.
              CALL METHOD process_combi_25.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000034.
              CALL METHOD process_combi_34.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000035.
              CALL METHOD process_combi_35.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000123.
              CALL METHOD process_combi_123.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000124.
              CALL METHOD process_combi_124.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000125.
              CALL METHOD process_combi_125.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000134.
              CALL METHOD process_combi_134.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000135.
              CALL METHOD process_combi_135.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000234.
              CALL METHOD process_combi_234.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000235.
              CALL METHOD process_combi_235.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000345.
              CALL METHOD process_combi_345.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001234.
              CALL METHOD process_combi_1234.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001235.
              CALL METHOD process_combi_1235.
              CLEAR:  zajustou..

              CALL METHOD update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002345.
              CALL METHOD process_combi_2345.
              CLEAR:  zajustou..

              LOOP AT it_min INTO wa_min.
                APPEND wa_min TO it_min_bk.
              ENDLOOP.

            ELSE.


              IF lines EQ 6.

                CLEAR: xcond_min.
                xcombi = 100000.
                CLEAR: wa_combi_final-msg.
                CALL METHOD process_it_min.
                CLEAR:  zajustou..

                CLEAR: wa_combi_final-msg.
                xcombi = 88888.
                CALL METHOD process_it_min_15.
                CLEAR:  zajustou..

                READ TABLE it_min INTO wa_min INDEX 1.
                aux1 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 2.
                aux2 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 3.
                aux3 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 4.
                aux4 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 5.
                aux5 = wa_min-comnr.

                READ TABLE it_min INTO wa_min INDEX 6.
                aux6 = wa_min-comnr.

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001.
                CALL METHOD process_combi_1.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002.
                CALL METHOD process_combi_2.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003.
                CALL METHOD process_combi_3.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000004.
                CALL METHOD process_combi_4.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000005.
                CALL METHOD process_combi_5.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000006.
                CALL METHOD process_combi_6.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000012.
                CALL METHOD process_combi_12.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000013.
                CALL METHOD process_combi_13.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000014.
                CALL METHOD process_combi_14.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000016.
                CALL METHOD process_combi_16.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000023.
                CALL METHOD process_combi_23.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000024.
                CALL METHOD process_combi_24.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000025.
                CALL METHOD process_combi_25.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000026.
                CALL METHOD process_combi_26.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000034.
                CALL METHOD process_combi_34.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000035.
                CALL METHOD process_combi_35.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000036.
                CALL METHOD process_combi_36.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000123.
                CALL METHOD process_combi_123.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000124.
                CALL METHOD process_combi_124.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000125.
                CALL METHOD process_combi_125.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000126.
                CALL METHOD process_combi_126.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000134.
                CALL METHOD process_combi_134.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000135.
                CALL METHOD process_combi_135.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000136.
                CALL METHOD process_combi_136.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000234.
                CALL METHOD process_combi_234.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000235.
                CALL METHOD process_combi_235.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000236.
                CALL METHOD process_combi_236.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000345.
                CALL METHOD process_combi_345.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000346.
                CALL METHOD process_combi_346.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001234.
                CALL METHOD process_combi_1234.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001235.
                CALL METHOD process_combi_1235.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001236.
                CALL METHOD process_combi_1236.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002345.
                CALL METHOD process_combi_2345.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002346.
                CALL METHOD process_combi_2346.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002356.
                CALL METHOD process_combi_2356.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003456.
                CALL METHOD process_combi_3456.
                CLEAR:  zajustou..

                CALL METHOD update_restricao.

                LOOP AT it_min INTO wa_min.
                  APPEND wa_min TO it_min_bk.
                ENDLOOP.

              ELSE.


                IF lines EQ 7.

                  CLEAR: xcond_min.
                  xcombi = 100000.
                  CLEAR: wa_combi_final-msg.
                  CALL METHOD process_it_min.
                  CLEAR:  zajustou..

                  CLEAR: wa_combi_final-msg.
                  xcombi = 88888.
                  CALL METHOD process_it_min_15.
                  CLEAR:  zajustou..

                  READ TABLE it_min INTO wa_min INDEX 1.
                  aux1 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 2.
                  aux2 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 3.
                  aux3 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 4.
                  aux4 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 5.
                  aux5 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 6.
                  aux6 = wa_min-comnr.

                  READ TABLE it_min INTO wa_min INDEX 7.
                  aux7 = wa_min-comnr.

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000001.
                  CALL METHOD process_combi_1.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000002.
                  CALL METHOD process_combi_2.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000003.
                  CALL METHOD process_combi_3.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000004.
                  CALL METHOD process_combi_4.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000005.
                  CALL METHOD process_combi_5.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000006.
                  CALL METHOD process_combi_6.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000007.
                  CALL METHOD process_combi_7.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000012.
                  CALL METHOD process_combi_12.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000013.
                  CALL METHOD process_combi_13.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000014.
                  CALL METHOD process_combi_14.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000016.
                  CALL METHOD process_combi_16.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000017.
                  CALL METHOD process_combi_17.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000023.
                  CALL METHOD process_combi_23.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000024.
                  CALL METHOD process_combi_24.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000025.
                  CALL METHOD process_combi_25.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000026.
                  CALL METHOD process_combi_26.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000027.
                  CALL METHOD process_combi_27.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000034.
                  CALL METHOD process_combi_34.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000035.
                  CALL METHOD process_combi_35.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000036.
                  CALL METHOD process_combi_36.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000037.
                  CALL METHOD process_combi_37.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000045.
                  CALL METHOD process_combi_45.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000046.
                  CALL METHOD process_combi_46.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000047.
                  CALL METHOD process_combi_47.
                  CLEAR:  zajustou.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10000056.
                  CALL METHOD process_combi_56.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000057.
                  CALL METHOD process_combi_57.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000067.
                  CALL METHOD process_combi_67.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000123.
                  CALL METHOD process_combi_123.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000124.
                  CALL METHOD process_combi_124.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000125.
                  CALL METHOD process_combi_125.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000126.
                  CALL METHOD process_combi_126.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000127.
                  CALL METHOD process_combi_127.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000134.
                  CALL METHOD process_combi_134.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000135.
                  CALL METHOD process_combi_135.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000136.
                  CALL METHOD process_combi_136.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000137.
                  CALL METHOD process_combi_137.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000234.
                  CALL METHOD process_combi_234.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000235.
                  CALL METHOD process_combi_235.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000236.
                  CALL METHOD process_combi_236.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000237.
                  CALL METHOD process_combi_237.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000345.
                  CALL METHOD process_combi_345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000346.
                  CALL METHOD process_combi_346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000347.
                  CALL METHOD process_combi_347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000456.
                  CALL METHOD process_combi_456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000457.
                  CALL METHOD process_combi_457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100000567.
                  CALL METHOD process_combi_567.
                  CLEAR:  zajustou..


                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 1000001234.
                  CALL METHOD process_combi_1234.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001235.
                  CALL METHOD process_combi_1235.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001236.
                  CALL METHOD process_combi_1236.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001237.
                  CALL METHOD process_combi_1237.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001345.
                  CALL METHOD process_combi_1345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001347.
                  CALL METHOD process_combi_1346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001347.
                  CALL METHOD process_combi_1347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001456.
                  CALL METHOD process_combi_1456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001457.
                  CALL METHOD process_combi_1457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10001567.
                  CALL METHOD process_combi_1567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002345.
                  CALL METHOD process_combi_2345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002346.
                  CALL METHOD process_combi_2346.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002347.
                  CALL METHOD process_combi_2347.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002356.
                  CALL METHOD process_combi_2356.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002357.
                  CALL METHOD process_combi_2357.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002456.
                  CALL METHOD process_combi_2456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10002457.
                  CALL METHOD process_combi_2457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10003456.
                  CALL METHOD process_combi_3456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10003457.
                  CALL METHOD process_combi_3457.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10004567.
                  CALL METHOD process_combi_4567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10012345.
                  CALL METHOD process_combi_12345.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10013456.
                  CALL METHOD process_combi_13456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10014567.
                  CALL METHOD process_combi_14567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10023456.
                  CALL METHOD process_combi_23456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10024567.
                  CALL METHOD process_combi_24567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 10034567.
                  CALL METHOD process_combi_34567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100123456.
                  CALL METHOD process_combi_123456.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100134567.
                  CALL METHOD process_combi_134567.
                  CLEAR:  zajustou..

                  CALL METHOD update_restricao.

                  CLEAR: wa_combi_final-msg.
                  xcombi = 100234567.
                  CALL METHOD process_combi_234567.
                  CLEAR:  zajustou..

                  LOOP AT it_min INTO wa_min.
                    APPEND wa_min TO it_min_bk.
                  ENDLOOP.

                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

    DELETE it_combi_final WHERE costt = 0.
    SORT it_combi_final BY costt combi.


    LOOP AT it_combi_final INTO wa_combi_final.
      l_tabix = sy-tabix.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = wa_combi_final-combi.
      IF sy-subrc NE 0.
        DELETE it_combi_final INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    DELETE it_combi_final WHERE costt = 0.
    SORT it_combi_final BY costt combi.
    DELETE  ADJACENT DUPLICATES FROM it_combi_final COMPARING costt.

    READ TABLE it_combi_final INTO wa_combi_final  INDEX 1.
    xcombi = wa_combi_final-combi.
    CLEAR  it_min[].

    DATA l_tabix_combi TYPE sy-tabix.

    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.

    LOOP AT it_combi_final INTO wa_combi_final.
      l_tabix_combi = sy-tabix.
      READ TABLE it_combi_finalx INTO wa_combi_finalx WITH KEY combi = wa_combi_final-combi.
      xcombi =  wa_combi_final-combi.
      xcombi_min =  wa_combi_final-combi.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
      IF  sy-subrc = 0.
        CLEAR  it_min[].
        CALL METHOD verifica_cond_minimo.
        IF  NOT it_min[] IS INITIAL.
          LOOP AT it_min INTO wa_min.
            READ TABLE  it_min_bk INTO wa_min_bk WITH KEY comnr = wa_min-comnr.
            IF sy-subrc = 0.
              DELETE it_combi_final  INDEX l_tabix_combi.
              DELETE it_combi_finalx WHERE combi = wa_combi_final-combi.
              DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig0_min WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig1_min WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig2_min WHERE /qaps/cignr = xcombi.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE it_combi_final WHERE costt = 0.
    SORT it_combi_final BY costt combi.
    DELETE  ADJACENT DUPLICATES FROM it_combi_final COMPARING costt.


    READ TABLE it_combi_final INTO wa_combi_final  INDEX 1.
    xcombi = wa_combi_final-combi.
    CLEAR  it_min[].

    CALL METHOD verificar_condicoes.

  ENDMETHOD.                    " CONDICAO_MINIMO_NIVEL02


  METHOD CONTINGENCIA .

    it_combi_finalx[] =  it_combi_final[].
    READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
    xcombi =  wa_combi_finalx-combi.
    xcombi_min =  wa_combi_finalx-combi.
    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

    IF  sy-subrc = 0.

      REFRESH it_min.

      LOOP AT it_pc INTO DATA(ls_pc) WHERE flgut = 'X'.
        APPEND VALUE ty_min(
            comnr = ls_pc-comnr
            ind   = sy-tabix
            combi = wa_combi_final-combi ) TO it_min.
      ENDLOOP.

      IF  it_min[] IS INITIAL.
        CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

        IF cv_has_result = abap_true.
          RETURN.
        ENDIF.
      ENDIF.

      CALL METHOD condicao_minimo_nivel02.

    ELSE.
      CALL METHOD del_comd_min.
      DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
    ENDIF.

    SORT it_min  BY comnr.
    DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

    SORT it_min_bk  BY comnr.
    DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.
  ENDMETHOD.


  METHOD DEL_COMD_MIN.

    LOOP AT it_combi_final INTO wa_combi_final.
      IF wa_combi_final-msg+0(13) <> 'SOLUÇÃO ÒTIMA'.
        DELETE it_zmicig0_min WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_zmicig1_min WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_zmicig2_min WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_zmicig0 WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_zmicig1 WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_zmicig2 WHERE /qaps/cignr = wa_combi_final-combi.
        DELETE it_combi_final INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    " DEL_COMD_MIN


  METHOD EXECUTE.

    LOOP AT it_pc INTO wa_pc WHERE flgut = 'X'.
      wa_re-comnr = wa_pc-comnr.
      wa_re-coktx = wa_pc-coktx.
      APPEND wa_re TO it_re.
    ENDLOOP.
    IF sy-subrc <> 0.
      DATA lv_message_material TYPE string.
      MESSAGE e017(/qaps/zmi) INTO lv_message_material.
      RAISE EXCEPTION TYPE /qaps/cx_general
        EXPORTING
          message = VALUE bapiret2( type = 'E'
                                    message = lv_message_material ).
    ENDIF.
    it_rs_bk[] = it_rs[].
    it_pc_0100[] = it_pc[].
    CALL METHOD zf_ver_comp_min.
    CALL METHOD product_formulate.

  ENDMETHOD.


  METHOD EXEC_SIMULATION .

    TYPES: BEGIN OF ts_div,
             zdiv TYPE i,
           END OF ts_div.

    DATA lv_valid TYPE c.
    DATA lo_excep TYPE REF TO /qaps/cx_formulation_error.
    FIELD-SYMBOLS <fs_rs> TYPE ty_rs.
    DATA lv_div_valido TYPE abap_bool.

    DATA lt_div TYPE TABLE OF ts_div.

    REFRESH gt_initial.

    lt_div = VALUE #(
                        ( zdiv = 1000 ) ).

    DATA(lv_times) = lines( lt_div ).

    DO lv_times TIMES.

      zdiv = lt_div[ sy-index ]-zdiv.

      CLEAR: lv_valid,
             gv_reformulate..

      WHILE lv_valid = ''.

        TRY.

            CALL METHOD execute.

            lv_valid = 'X'.

            CALL METHOD clear_workarea.
            CALL METHOD clear_reformulate.

          CATCH /qaps/cx_div_no_result.

            IF gv_reformulate >= 10.
              lv_valid = 'X'.
              CALL METHOD clear_workarea.
              CALL METHOD clear_reformulate.
            ELSE.
              CALL METHOD f_reformulate_v2.
            ENDIF.

          CATCH /qaps/cx_formulation_error INTO lo_excep.

            CALL METHOD f_reformulate_v1 EXPORTING lo_excep = lo_excep.
            CHECK gv_reformulate >= 10.
            lv_valid = 'X'.

        ENDTRY.

      ENDWHILE.

    ENDDO.


    CALL METHOD finalize_simulation.

  ENDMETHOD.


  METHOD EXEC_SIMULATION_OLD .

    TYPES: BEGIN OF ts_div,
             zdiv TYPE i,
           END OF ts_div.

    DATA lv_valid TYPE c.
    DATA lo_excep TYPE REF TO /qaps/cx_formulation_error.
    FIELD-SYMBOLS <fs_rs> TYPE ty_rs.
    DATA lv_div_valido TYPE abap_bool.
    DATA lt_div TYPE TABLE OF ts_div.

    lt_div = VALUE #( ( zdiv = 1 )
                      ( zdiv = 2 )
                      ( zdiv = 5 )
                      ( zdiv = 10 )
                      ( zdiv = 12 )
                      ( zdiv = 15 )
                      ( zdiv = 25 )
                      ( zdiv = 30 )
                      ( zdiv = 50 )
                      ( zdiv = 100 )
                      ( zdiv = 1000 ) ).

    WHILE lv_valid = ''.

      TRY.

          DATA(lv_times) = lines( lt_div ).

          DO lv_times TIMES.

            zdiv = lt_div[ sy-index ]-zdiv.

            TRY.

                LOOP AT it_pc INTO wa_pc WHERE flgut = 'X'.
                  wa_re-comnr = wa_pc-comnr.
                  wa_re-coktx = wa_pc-coktx.
                  APPEND wa_re TO it_re.
                ENDLOOP.

                IF sy-subrc <> 0.
                  DATA lv_message_material TYPE string.
                  MESSAGE e017(/qaps/zmi) INTO lv_message_material.
                  RAISE EXCEPTION TYPE /qaps/cx_general
                    EXPORTING
                      message = VALUE bapiret2( type = 'E'
                                                message = lv_message_material ).
                ENDIF.
                it_rs_bk[] = it_rs[].
                it_pc_0100[] = it_pc[].
                CALL METHOD zf_ver_comp_min.
                CALL METHOD product_formulate.

                CALL METHOD clear_workarea.
                CALL METHOD clear_reformulate.

              CATCH /qaps/cx_div_no_result.
                CALL METHOD clear_workarea.
                CALL METHOD clear_reformulate.
            ENDTRY.

          ENDDO.

          lv_valid = 'X'.

          IF lines( gt_snapshot ) > 0.
            CALL METHOD validar_snapshot CHANGING cv_valido = lv_div_valido.
            IF lv_div_valido = abap_true.
              CALL METHOD fill_version.
              RETURN.
            ELSE.
              zsemsolucao = abap_true.
              DATA lv_message TYPE string.
              MESSAGE e133(/qaps/zmi) INTO lv_message.
              RAISE EXCEPTION TYPE /qaps/cx_general
                EXPORTING
                  message = VALUE bapiret2( type = 'E'
                                            message = lv_message ).
            ENDIF.
          ELSE.
            zsemsolucao = abap_true.
            MESSAGE e133(/qaps/zmi) INTO lv_message.
            RAISE EXCEPTION TYPE /qaps/cx_general
              EXPORTING
                message = VALUE bapiret2( type = 'E'
                                          message = lv_message ).
          ENDIF.

        CATCH /qaps/cx_formulation_error INTO lo_excep.

          gv_reformulate = gv_reformulate + 1.
          gv_key_increment = lo_excep->get_increment( ).
          zdiv_reformulate = lo_excep->get_previous_div( ).

          LOOP AT it_rs ASSIGNING <fs_rs>.
            <fs_rs>-gmeng_bk = <fs_rs>-gmeng.
            CHECK <fs_rs>-chkey = gv_key_increment.
            <fs_rs>-gmeng = <fs_rs>-gmeng + ( '0.005' * gv_reformulate ).
          ENDLOOP.

          REFRESH it_rest_comp_2.
          CLEAR zajustou.
          CALL METHOD clear_workarea.

          CHECK gv_reformulate >= 10.
          EXIT.

      ENDTRY.

    ENDWHILE.

  ENDMETHOD.


  METHOD FILL_COMPONENTS.

    SELECT *
        FROM /qaps/zmic1
        WHERE matnr = @/qaps/zmidfcig-matnr
        AND werks = @/qaps/zmidfcig-werks
        AND /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
        INTO TABLE @DATA(lt_zmic1).

    IF lines( it_pc[] ) = 0.

      SELECT *
        FROM /qaps/zmirm1
        WHERE matnr = @/qaps/zmidfcig-matnr
        AND werks = @/qaps/zmidfcig-werks
        AND /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
        INTO TABLE @DATA(lt_zmirm1).

      LOOP AT lt_zmirm1 INTO DATA(ls_zmirm1).
        APPEND INITIAL LINE TO it_pc ASSIGNING FIELD-SYMBOL(<fs_pc>).
        <fs_pc>-flgut = 'X'.
        <fs_pc>-comnr = ls_zmirm1-/qaps/comnr.

        TRY.
            DATA(ls_zmic1) = lt_zmic1[ /qaps/comnr = ls_zmirm1-/qaps/comnr ].
            <fs_pc>-wgmeng = ls_zmic1-/qaps/gemng.
            <fs_pc>-wemeng = ls_zmic1-/qaps/eqmng.
            <fs_pc>-wlmeng = ls_zmic1-/qaps/lemng.

            <fs_pc>-gemng = ls_zmic1-/qaps/gemng.
            <fs_pc>-eqmng = ls_zmic1-/qaps/eqmng.
            <fs_pc>-lemng = ls_zmic1-/qaps/lemng.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDLOOP.

    ELSE.

      LOOP AT it_pc ASSIGNING <fs_pc>.
        <fs_pc>-gemng = <fs_pc>-wgmeng.
        <fs_pc>-eqmng = <fs_pc>-wemeng.
        <fs_pc>-lemng = <fs_pc>-wlmeng.
      ENDLOOP.

    ENDIF.

    REFRESH it_zmicomp.

    LOOP AT lt_zmic1 INTO ls_zmic1.
      CLEAR wa_zmicomp.
      wa_zmicomp-/qaps/comnr = ls_zmic1-/qaps/comnr.
      wa_zmicomp-matnr       = ls_zmic1-matnr.
      wa_zmicomp-werks       = ls_zmic1-werks.
      wa_zmicomp-/qaps/grkey = ls_zmic1-/qaps/grkey.
      wa_zmicomp-/qaps/gemng = ls_zmic1-/qaps/gemng.
      wa_zmicomp-/qaps/eqmng = ls_zmic1-/qaps/eqmng.
      wa_zmicomp-/qaps/lemng = ls_zmic1-/qaps/lemng.
      APPEND wa_zmicomp TO it_zmicomp.
    ENDLOOP.

    LOOP AT it_pc INTO DATA(ls_pc).


      ASSIGN it_zmicomp[ /qaps/comnr = ls_pc-comnr ] TO FIELD-SYMBOL(<fs_zmicomp>).
      IF <fs_zmicomp> IS ASSIGNED.
        <fs_zmicomp>-/qaps/gemng = ls_pc-gemng.
        <fs_zmicomp>-/qaps/eqmng = ls_pc-eqmng.
        <fs_zmicomp>-/qaps/lemng = ls_pc-lemng.
        UNASSIGN <fs_zmicomp>.
      ELSE.
        APPEND INITIAL LINE TO it_zmicomp ASSIGNING <fs_zmicomp>.
        <fs_zmicomp>-/qaps/comnr = ls_pc-comnr.
        <fs_zmicomp>-matnr       = /qaps/zmidfcig-matnr.
        <fs_zmicomp>-werks       = /qaps/zmidfcig-werks.
        <fs_zmicomp>-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        <fs_zmicomp>-/qaps/gemng = ls_pc-gemng.
        <fs_zmicomp>-/qaps/eqmng = ls_pc-eqmng.
        <fs_zmicomp>-/qaps/lemng = ls_pc-lemng.
        UNASSIGN <fs_zmicomp>.
      ENDIF.

    ENDLOOP.

    DELETE it_zmicomp WHERE /qaps/gemng IS INITIAL
                        AND /qaps/eqmng IS INITIAL
                        AND /qaps/lemng IS INITIAL.


  ENDMETHOD.


  METHOD FILL_MATRIX_A.

    DATA:
      l_ctcol TYPE sy-tabix,
      l_ctlin TYPE sy-tabix.

    l_ctlin = 1.

    WHILE l_ctlin <= p_nrlin.

      l_ctcol = 1.

      WHILE l_ctcol <= p_nrcol.

        wa_ma-line  = l_ctlin.
        wa_ma-col = l_ctcol.

        INSERT wa_ma INTO TABLE it_ma.

        l_ctcol = l_ctcol + 1.

      ENDWHILE.

      l_ctlin = l_ctlin + 1.

    ENDWHILE.

  ENDMETHOD.                               " FILL_MATRIX_A


  METHOD FILL_RESTRICAO_ITEM.

    REFRESH it_zmi01.

    SELECT *
     FROM /qaps/zmi01
     INTO TABLE it_zmi01
     WHERE matnr = /qaps/zmidfcig-matnr
     AND werks = /qaps/zmidfcig-werks
     AND /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

    IF lines( it_rs ) > 0.

      LOOP AT it_zmi01 ASSIGNING FIELD-SYMBOL(<fs>).

        CLEAR: <fs>-/qaps/gmeng,
               <fs>-/qaps/emeng,
               <fs>-/qaps/lmeng,
               <fs>-/qaps/ymeng.

        TRY.
            DATA(ls_rs) = it_rs[ chkey = <fs>-/qaps/chkey ].
            <fs>-/qaps/gmeng = ls_rs-gmeng.
            <fs>-/qaps/emeng = ls_rs-emeng.
            <fs>-/qaps/lmeng = ls_rs-lmeng.
            <fs>-/qaps/ymeng = ls_rs-gmeng.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDLOOP.

    ELSE.

      SELECT *
        FROM /qaps/zmic1
        WHERE matnr = @/qaps/zmidfcig-matnr
        AND werks = @/qaps/zmidfcig-werks
        AND /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
        INTO TABLE @DATA(lt_zmic1).

      LOOP AT it_zmi01 INTO DATA(ls_zmi01).
        APPEND INITIAL LINE TO it_rs ASSIGNING FIELD-SYMBOL(<fs_rs>).

        <fs_rs>-chkey = ls_zmi01-/qaps/chkey.
        <fs_rs>-gmeng = ls_zmi01-/qaps/gmeng.
        <fs_rs>-emeng = ls_zmi01-/qaps/emeng.
        <fs_rs>-lmeng = ls_zmi01-/qaps/lmeng.
        <fs_rs>-ymeng = ls_zmi01-/qaps/gmeng.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD FILL_VERSION.

    DATA ls_version TYPE ts_snapshot.

    LOOP AT gt_snapshot INTO DATA(ls_snapshot).

      CHECK ls_snapshot-valido = 'X'.

      IF ls_version IS INITIAL
        OR ls_version-combi_final-costt > ls_snapshot-combi_final-costt.
        ls_version = ls_snapshot.
      ENDIF.

    ENDLOOP.

    "Preencher dados
    /qaps/zmidfcig = ls_version-zmidfcig.
    it_re = ls_version-t_re.
    it_ng = ls_version-t_ng.
    it_pc_0100 = ls_version-t_pc_0100.
    it_zmicig0 = ls_version-t_zmicig0.
    it_zmicig1 = ls_version-t_zmicig1.
    it_zmicig2 = ls_version-t_zmicig2.

  ENDMETHOD.


  METHOD FINALIZE_SIMULATION.

    DATA lv_valid TYPE c.
    DATA lv_div_valido TYPE abap_bool.

    lv_valid = 'X'.

    IF lines( gt_snapshot ) > 0.
      CALL METHOD validar_snapshot CHANGING cv_valido = lv_div_valido.
      IF lv_div_valido = abap_true.
        CALL METHOD fill_version.
        RETURN.
      ELSE.
        zsemsolucao = abap_true.
        DATA lv_message TYPE string.
        MESSAGE e133(/qaps/zmi) INTO lv_message.
        RAISE EXCEPTION TYPE /qaps/cx_general
          EXPORTING
            message = VALUE bapiret2( type = 'E'
                                      message = lv_message ).
      ENDIF.
    ELSE.
      zsemsolucao = abap_true.
      MESSAGE e133(/qaps/zmi) INTO lv_message.
      RAISE EXCEPTION TYPE /qaps/cx_general
        EXPORTING
          message = VALUE bapiret2( type = 'E'
                                    message = lv_message ).
    ENDIF.


  ENDMETHOD.


  METHOD FIRST_FORMULATION .

    DATA lt_matrixaa TYPE TABLE OF /qaps/matrixaa.

    REFRESH: it_pc_bk.
    it_pc_bk = it_pc.

    xcombi = '0000999995'.

    wa_combi_final-combi  = xcombi.
    APPEND wa_combi_final TO it_combi_final.
    CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.

    CALL METHOD load_internal_tables.

    tp = -1.
    nv = wa_ct_comp.
    r1 = wa_ct_rest_le.
    r2 = wa_ct_rest_eq.
    r3 = wa_ct_rest_ge.
    x  = r1 + r2 + r3.
    a  = nv + 1.
    y  = x  + r3 + a.
    b  = y  - 1.
    c  = x  + 1.
    z  = a  - 1.



    CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
    CALL METHOD load_le_restrictions.
    CALL METHOD load_eq_restrictions.
    CALL METHOD load_ge_restrictions.
    CALL METHOD load_cuts.

    IF gv_matrix = abap_true. " 'X'.
      DELETE FROM /qaps/matrixaa.
      DELETE FROM /qaps/zmatrixaa.
      DELETE FROM /qaps/zmatr_rest.
      COMMIT WORK AND WAIT.
      lt_matrixaa = VALUE #( FOR wa IN it_ma
                             ( line = wa-line col = wa-col value = wa-valor ) ).

      MODIFY /qaps/matrixaa FROM TABLE lt_matrixaa.
      MODIFY /qaps/zmatrixaa FROM TABLE it_ma.
      MODIFY /qaps/zmatr_rest FROM TABLE gt_rest_linha.
      COMMIT WORK.
      MESSAGE 'Matrix foi gerada' TYPE 'S'.
      gv_matrix = abap_false.
      LEAVE PROGRAM.
    ENDIF.

    CALL METHOD stage_590.
    CALL METHOD modifies_matrix.

    DO.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_1000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_2000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_3000.
      ENDIF.

      IF NOT wa_combi_final-msg IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

    BREAK c060863.

  ENDMETHOD.


  METHOD FORMULAR.

    DATA: lv_valid   TYPE c,
          lv_subrc   TYPE sy-subrc,
          lv_id      TYPE i,
          ls_message TYPE  /qaps/s_general_error.

    DATA lo_excep TYPE REF TO /qaps/cx_formulation_error.
    FIELD-SYMBOLS <fs_rs> TYPE /qaps/s_rs. "ty_rs.

    clear_workarea(  ).

    REFRESH: et_re,
             et_ng,
             et_header,
             et_items,
             et_niveis_garantia.

    CLEAR: ev_message,
           ev_type,
           ev_number.

*  opert = 'I'.

    it_rs[] = CORRESPONDING #( it_rs_param ).
    it_pc[] = CORRESPONDING #( it_pc_param ).

    /qaps/zmidfcig-matnr = is_header-matnr.
    /qaps/zmidfcig-werks = is_header-werks.
    /qaps/zmidfcig-/qaps/grkey = is_header-grkey.
    /qaps/zmidfcig-/qaps/period = is_header-period.
    /qaps/zmidfcig-/qaps/versao = is_header-versao.
*  /qaps/zmidfcig-/qaps/dt_entrega = is_header-dt_entrega.
    /qaps/zmidfcig-/qaps/rmeng = '1.000'.

    IF iv_prio_vs_gerencial = abap_true.
      /qaps/zmidfcig-/qaps/flpxg = 'X'.
    ELSEIF iv_gerencial = abap_true.
      /qaps/zmidfcig-/qaps/flger = 'X'.
    ELSEIF iv_reposicao = abap_true.
      /qaps/zmidfcig-/qaps/flrep = 'X'.
    ELSEIF iv_contabil = abap_true.
      /qaps/zmidfcig-/qaps/flcont = 'X'.
    ELSE.
      ev_type = 'E'.
      ev_message = 'Selecionar o tipo de opção'.
      RETURN.
    ENDIF.

    SELECT * FROM /qaps/zmi01
     INTO TABLE it_zmi01
     WHERE matnr = is_header-matnr
       AND werks = is_header-werks
       AND /qaps/grkey = is_header-grkey.

    fill_restricao_item(  ).
    fill_components(  ).

    TRY.

        validacoes_preliminares( EXPORTING us_header =  is_header ).

        exec_simulation(  ).

        et_re     = CORRESPONDING #( it_re[] ).
        et_ng     = CORRESPONDING #( it_ng[] ).

        LOOP AT et_re ASSIGNING FIELD-SYMBOL(<fs_re>).
          <fs_re>-valor = <fs_re>-cmeng * <fs_re>-verpr.
          <fs_re>-cmuni = 'TO'.
        ENDLOOP.

        SORT et_ng BY chkey.

        lv_id = 4.

        LOOP AT et_ng ASSIGNING FIELD-SYMBOL(<fs_ng>).

          TRY.
              <fs_ng>-teor_nominal = it_rs_param[ chkey = <fs_ng>-chkey ]-ref.

              IF <fs_ng>-teor_nominal IS INITIAL.
                <fs_ng>-teor_nominal = floor( <fs_ng>-gmeng ).
              ENDIF.


            CATCH cx_sy_itab_line_not_found.

          ENDTRY.

          CASE <fs_ng>-chkey.
            WHEN 'N'.
              <fs_ng>-ord = '1'.
            WHEN 'P'.
              <fs_ng>-ord = '2'.
            WHEN 'K'.
              <fs_ng>-ord = '3'.
            WHEN OTHERS.
              <fs_ng>-ord = lv_id.
              lv_id = lv_id + 1.
          ENDCASE.
        ENDLOOP.

        SORT et_ng BY ord ASCENDING.

        et_header = CORRESPONDING #( it_zmicig0 ).
        et_items  = CORRESPONDING #( it_zmicig1 ).

        LOOP AT et_items ASSIGNING FIELD-SYMBOL(<fs_items>).
          <fs_items>-/qaps/cmuni = 'TO'.
        ENDLOOP.

        et_niveis_garantia = CORRESPONDING #( it_zmicig2 ).

        LOOP AT it_zmicig2 ASSIGNING FIELD-SYMBOL(<fs_cig2>).

          TRY.
              <fs_cig2>-teor_nominal = it_rs_param[ chkey = <fs_cig2>-/qaps/chkey ]-ref.
            CATCH cx_sy_itab_line_not_found.

          ENDTRY.

        ENDLOOP.

        ls_message = VALUE /qaps/s_general_error(
            type    = 'S'
            number  = '000'
            message = 'Formulação executada com sucesso' ).

      CATCH /qaps/cx_formulation_error INTO DATA(lo_formulation_error).
        ls_message-message = lo_formulation_error->get_text( ).
        ls_message-type = 'E'.
      CATCH /qaps/cx_div_no_result INTO DATA(lo_div_no_result).
        ls_message-message = lo_div_no_result->get_message( ).
        ls_message-type = 'E'.
      CATCH /qaps/cx_general INTO DATA(lo_general).
        DATA(ls_bapiret2) = lo_general->get_message( ).
        ls_message-message = ls_bapiret2-message.
        ls_message-type = ls_bapiret2-type.
    ENDTRY.

    IF ls_message-type <> 'S'.
*    REFRESH: et_re,
*             et_ng.
*    CLEAR es_resultado.
    ENDIF.

    ev_type = ls_message-type.
    ev_number = ls_message-number.
    ev_message = ls_message-message.

  ENDMETHOD.


  METHOD FORMULATION.

    CALL METHOD load_internal_tables.

    tp = -1.
    nv = wa_ct_comp.
    r1 = wa_ct_rest_le.
    r2 = wa_ct_rest_eq.
    r3 = wa_ct_rest_ge.
    x  = r1 + r2 + r3.
    a  = nv + 1.
    y  = x  + r3 + a.
    b  = y  - 1.
    c  = x  + 1.
    z  = a  - 1.

    CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.

    CALL METHOD load_le_restrictions.

    CALL METHOD load_eq_restrictions.

    CALL METHOD load_ge_restrictions.

    CALL METHOD load_cuts.

    CALL METHOD stage_590.
*
    CALL METHOD modifies_matrix.

    DO.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_1000_min.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_2000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_3000.
      ENDIF.

      IF NOT wa_combi_final-msg IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.                    " FORMULATION


  METHOD F_REFORMULATE_V1.

    gv_reformulate = gv_reformulate + 1.
    gv_key_increment = lo_excep->get_increment( ).
    zdiv_reformulate = lo_excep->get_previous_div( ).

    LOOP AT it_rs ASSIGNING FIELD-SYMBOL(<fs_rs>).
      <fs_rs>-gmeng_bk = <fs_rs>-gmeng.
      CHECK <fs_rs>-chkey = gv_key_increment.
      IF gv_reformulate < 5.
        <fs_rs>-gmeng = <fs_rs>-gmeng + ( '0.005' * gv_reformulate ).
      ELSE.
        <fs_rs>-gmeng = <fs_rs>-gmeng + ( '0.010' * gv_reformulate ).
      ENDIF.
      <fs_rs>-ymeng = <fs_rs>-gmeng.
    ENDLOOP.


    REFRESH it_rest_comp_2.
    CLEAR zajustou.
    CALL METHOD clear_workarea.

    REFRESH: it_re,it_pc_bk.

  ENDMETHOD.


  METHOD F_REFORMULATE_V2.

    gv_reformulate = gv_reformulate + 1.

    LOOP AT it_rs ASSIGNING FIELD-SYMBOL(<fs_rs>).
      <fs_rs>-gmeng_bk = <fs_rs>-gmeng.
      CHECK <fs_rs>-chkey = gv_key_increment.
      IF gv_reformulate < 5.
        <fs_rs>-gmeng = <fs_rs>-gmeng + ( '0.005' * gv_reformulate ).
      ELSE.
        <fs_rs>-gmeng = <fs_rs>-gmeng + ( '0.010' * gv_reformulate ).
      ENDIF.
      <fs_rs>-ymeng = <fs_rs>-gmeng.
    ENDLOOP.


    REFRESH it_rest_comp_2.
    CLEAR zajustou.
    CALL METHOD clear_workarea.

    REFRESH: it_re,it_pc_bk.

  ENDMETHOD.


  METHOD GUARANTEE_LEVELS.

    DATA wa_rs            TYPE ty_rs.
    DATA wa_ng    TYPE ty_ng.

    DATA:
      l_cmeng TYPE /qaps/zmidfcig-/qaps/cmeng.

    REFRESH: it_ng.

    SELECT /qaps/chkey
      FROM /qaps/zmig1
      INTO TABLE it_ng
      WHERE /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

    LOOP AT it_ng INTO wa_ng.

      CLEAR l_cmeng.

      SELECT SINGLE /qaps/chdes /qaps/meuni
        INTO (wa_ng-chdes, wa_ng-cmuni)
        FROM /qaps/zmitc
        WHERE /qaps/chkey = wa_ng-chkey.

      LOOP AT it_re INTO wa_re.

        LOOP AT it_prop_carac INTO wa_prop_carac
          WHERE matnr = wa_re-comnr AND chkey = wa_ng-chkey.

          l_cmeng =  l_cmeng  + ( wa_prop_carac-cmeng * wa_re-cmeng ).

        ENDLOOP.

      ENDLOOP.

      wa_ng-cmeng = ( l_cmeng / /qaps/zmidfcig-/qaps/rmeng ).

      CLEAR: wa_rs.
      READ TABLE it_rs INTO wa_rs WITH KEY chkey = wa_ng-chkey.

      IF sy-subrc = 0.

        CLEAR:  wa_ng-gmeng, wa_ng-lmeng.
        IF NOT wa_rs-emeng IS INITIAL.

          wa_ng-flbeg = '='.
          wa_ng-gmeng = wa_rs-emeng.

        ELSE.

          IF NOT wa_rs-gmeng IS INITIAL.
            wa_ng-flbeg = '>='.
            wa_ng-gmeng = wa_rs-ymeng.
          ENDIF.

          IF NOT wa_rs-lmeng IS INITIAL.
            wa_ng-flend = '<='.
            wa_ng-lmeng = wa_rs-lmeng.
          ENDIF.

        ENDIF.

      ENDIF.

      MODIFY it_ng FROM wa_ng.

    ENDLOOP.
*
    ind = 3.

    LOOP AT it_ng INTO wa_ng.

      IF wa_ng-chkey = 'N'.
        wa_ng-ord = 1.
      ELSE.
        IF wa_ng-chkey = 'P'.
          wa_ng-ord = 2.
        ELSE.
          IF wa_ng-chkey = 'K'.
            wa_ng-ord = 3.
          ELSE.
            ind = ind + 1.
            wa_ng-ord = ind.
          ENDIF.
        ENDIF.
      ENDIF.

      MODIFY it_ng FROM wa_ng.

    ENDLOOP.

    SORT it_ng BY ord.

  ENDMETHOD.                               " GUARANTEE_LEVELS


  METHOD GUARANTEE_LEVELS_MIN.

    DATA wa_rs            TYPE ty_rs.
    DATA wa_ng    TYPE ty_ng.
    DATA: l_cmeng(8) TYPE p DECIMALS 6.

    REFRESH it_ng.

    SELECT /qaps/chkey FROM /qaps/zmig1
    INTO TABLE it_ng
    WHERE /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
*
    LOOP AT it_ng INTO wa_ng.

      CLEAR l_cmeng.

      SELECT SINGLE /qaps/chdes /qaps/meuni
      INTO (wa_ng-chdes, wa_ng-cmuni)
      FROM /qaps/zmitc
      WHERE /qaps/chkey = wa_ng-chkey.

      LOOP AT it_re INTO wa_re.

        LOOP AT it_prop_carac INTO wa_prop_carac
          WHERE matnr = wa_re-comnr AND chkey = wa_ng-chkey.

          l_cmeng =  l_cmeng  + ( wa_prop_carac-cmeng * wa_re-cmeng ).

        ENDLOOP.

      ENDLOOP.

      wa_ng-cmeng = ( l_cmeng / /qaps/zmidfcig-/qaps/rmeng ).

      wa_ng-cmeng = ( l_cmeng / /qaps/zmidfcig-/qaps/rmeng ).

      CLEAR: wa_rs.
      READ TABLE it_rs INTO wa_rs WITH KEY chkey = wa_ng-chkey.

      IF sy-subrc = 0.

        CLEAR:  wa_ng-gmeng, wa_ng-lmeng.
        IF NOT wa_rs-emeng IS INITIAL.

          wa_ng-flbeg = '='.
          wa_ng-gmeng = wa_rs-emeng.

        ELSE.

          IF NOT wa_rs-gmeng IS INITIAL.
            wa_ng-flbeg = '>='.
            wa_ng-gmeng = wa_rs-ymeng.
          ENDIF.

          IF NOT wa_rs-lmeng IS INITIAL.
            wa_ng-flend = '<='.
            wa_ng-lmeng = wa_rs-lmeng.
          ENDIF.

        ENDIF.

      ENDIF.

      MODIFY it_ng FROM wa_ng.

    ENDLOOP.

    SORT it_ng BY chkey.
    DELETE ADJACENT DUPLICATES FROM it_ng COMPARING chkey.

    ind = 3.

    LOOP AT it_ng INTO wa_ng.

      IF wa_ng-chkey = 'N'.
        wa_ng-ord = 1.
      ELSE.
        IF wa_ng-chkey = 'P'.
          wa_ng-ord = 2.
        ELSE.
          IF wa_ng-chkey = 'K'.
            wa_ng-ord = 3.
          ELSE.
            ind = ind + 1.
            wa_ng-ord = ind.
          ENDIF.
        ENDIF.
      ENDIF.

      MODIFY it_ng FROM wa_ng.

    ENDLOOP.

    SORT it_ng BY ord.

  ENDMETHOD.                               " GUARANTEE_LEVELS_MIN


  METHOD INCOMPATIBILIDADE .

    LOOP AT it_zmi60 INTO wa_zmi60.
      CLEAR: wa_combi_final-costt.
      wa_combi_final-combi  = wa_zmi60-/qaps/mixin.
      APPEND wa_combi_final TO it_combi_final.
      CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
    ENDLOOP.

    LOOP AT it_zmi60 INTO wa_zmi60.
      it_pc_bk = it_pc.

      LOOP AT it_zmi61 INTO wa_zmi61 WHERE /qaps/mixin = wa_zmi60-/qaps/mixin.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr = wa_zmi61-/qaps/comnr.
          wa_pc-flgut = ' '.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.

      xcombi = wa_zmi60-/qaps/mixin.
      xcombi_atual = xcombi.

      CALL METHOD load_internal_tables.

      tp = -1.
      nv = wa_ct_comp.
      r1 = wa_ct_rest_le.
      r2 = wa_ct_rest_eq.
      r3 = wa_ct_rest_ge.
      x  = r1 + r2 + r3.
      a  = nv + 1.
      y  = x  + r3 + a.
      b  = y  - 1.
      c  = x  + 1.
      z  = a  - 1.

      CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
      CALL METHOD load_le_restrictions.
      CALL METHOD load_eq_restrictions.
      CALL METHOD load_ge_restrictions.
      CALL METHOD load_cuts.
      CALL METHOD stage_590.
      CALL METHOD modifies_matrix.

      DO.

        IF wa_combi_final-msg IS INITIAL.
          CALL METHOD stage_1000.
        ENDIF.

        IF wa_combi_final-msg IS INITIAL.
          CALL METHOD stage_2000.
        ENDIF.

        IF wa_combi_final-msg IS INITIAL.
          CALL METHOD stage_3000.
        ENDIF.

        IF NOT wa_combi_final-msg IS INITIAL.
          EXIT.
        ENDIF.

      ENDDO.

      IF NOT it_ng IS INITIAL  AND
        wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
        IF NOT it_dif[]  IS INITIAL.
          DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
          DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
          DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
          DELETE it_combi_final WHERE combi = xcombi.
          CALL METHOD clear_workarea.

          xcombi_atual = xcombi.
          CALL METHOD load_internal_tables.

          tp = -1.
          nv = wa_ct_comp.
          r1 = wa_ct_rest_le.
          r2 = wa_ct_rest_eq.
          r3 = wa_ct_rest_ge.
          x  = r1 + r2 + r3.
          a  = nv + 1.
          y  = x  + r3 + a.
          b  = y  - 1.
          c  = x  + 1.
          z  = a  - 1.

          CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
          CALL METHOD load_le_restrictions.
          CALL METHOD load_eq_restrictions.
          CALL METHOD load_ge_restrictions.
          CALL METHOD load_cuts.
          CALL METHOD stage_590.
          CALL METHOD modifies_matrix.

          DO.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_1000.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_2000.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_3000.
            ENDIF.

            IF NOT wa_combi_final-msg IS INITIAL.
              EXIT.
            ENDIF.

          ENDDO.

          IF NOT it_ng IS INITIAL  AND
            wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
            IF NOT it_dif[]  IS INITIAL.
              DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
              DELETE it_combi_final WHERE combi = xcombi.
              CALL METHOD clear_workarea.

              xcombi_atual = xcombi.
              CALL METHOD load_internal_tables.

              tp = -1.
              nv = wa_ct_comp.
              r1 = wa_ct_rest_le.
              r2 = wa_ct_rest_eq.
              r3 = wa_ct_rest_ge.
              x  = r1 + r2 + r3.
              a  = nv + 1.
              y  = x  + r3 + a.
              b  = y  - 1.
              c  = x  + 1.
              z  = a  - 1.

              CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
              CALL METHOD load_le_restrictions.
              CALL METHOD load_eq_restrictions.
              CALL METHOD load_ge_restrictions.
              CALL METHOD load_cuts.
              CALL METHOD stage_590.
              CALL METHOD modifies_matrix.

              DO.

                IF wa_combi_final-msg IS INITIAL.
                  CALL METHOD stage_1000.
                ENDIF.

                IF wa_combi_final-msg IS INITIAL.
                  CALL METHOD stage_2000.
                ENDIF.

                IF wa_combi_final-msg IS INITIAL.
                  CALL METHOD stage_3000.
                ENDIF.

                IF NOT wa_combi_final-msg IS INITIAL.
                  EXIT.
                ENDIF.

              ENDDO.

              IF NOT it_ng IS INITIAL  AND
                wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
                IF NOT it_dif[]  IS INITIAL.
                  DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
                  DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
                  DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
                  DELETE it_combi_final WHERE combi = xcombi.
                  CALL METHOD clear_workarea.

                  xcombi_atual = xcombi.
                  CALL METHOD load_internal_tables.

                  tp = -1.
                  nv = wa_ct_comp.
                  r1 = wa_ct_rest_le.
                  r2 = wa_ct_rest_eq.
                  r3 = wa_ct_rest_ge.
                  x  = r1 + r2 + r3.
                  a  = nv + 1.
                  y  = x  + r3 + a.
                  b  = y  - 1.
                  c  = x  + 1.
                  z  = a  - 1.

                  CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
                  CALL METHOD load_le_restrictions.
                  CALL METHOD load_eq_restrictions.
                  CALL METHOD load_ge_restrictions.
                  CALL METHOD load_cuts.
                  CALL METHOD stage_590.
                  CALL METHOD modifies_matrix.

                  DO.

                    IF wa_combi_final-msg IS INITIAL.
                      CALL METHOD stage_1000.
                    ENDIF.

                    IF wa_combi_final-msg IS INITIAL.
                      CALL METHOD stage_2000.
                    ENDIF.

                    IF wa_combi_final-msg IS INITIAL.
                      CALL METHOD stage_3000.
                    ENDIF.

                    IF NOT wa_combi_final-msg IS INITIAL.
                      EXIT.
                    ENDIF.

                  ENDDO.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

      CALL METHOD clear_workarea.

    ENDLOOP.

  ENDMETHOD.


  METHOD LOAD_CUTS.

    k = 0.
    n = n + 1.

    SELECT SINGLE /qaps/descrv
                INTO /qaps/zmidfcig-/qaps/descrv
                FROM /qaps/zmilc0
                WHERE werks  = /qaps/zmidfcig-werks AND
                      /qaps/grkey = /qaps/zmidfcig-/qaps/grkey   AND
                      /qaps/period = /qaps/zmidfcig-/qaps/period AND
                      /qaps/versao = /qaps/zmidfcig-/qaps/versao.

    IF NOT /qaps/zmidfcig-/qaps/fllt1e8 IS INITIAL.
      LOOP AT it_comp INTO wa_comp.
        SELECT SINGLE matnr bwkey verpr stprs peinh vjbwh bwph1
               INTO (wa_cust_matnr-matnr,
                      wa_cust_matnr-bwkey,
                      wa_cust_matnr-verpr,
                      wa_cust_matnr-stprs,
                      wa_cust_matnr-peinh,
                      wa_cust_matnr-vjbwh,
                      wa_cust_matnr-bwph1)
               FROM mbew
               WHERE matnr = wa_comp-comnr AND
                     bwkey = /qaps/zmidfcig-werks.

        IF sy-subrc <> 0.
          MESSAGE e183(/qaps/zmi) WITH wa_comp-comnr.
        ENDIF.

        k      = k + 1.
        tp_n   = tp * -1.
        CLEAR: valor1.
        valorx = wa_cust_matnr-vjbwh / zdiv.
        valor1 = ( valorx * tp_n ).
        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = valor1.
      ENDLOOP.

    ELSE.


      LOOP AT it_comp INTO wa_comp.
        SELECT SINGLE matnr werks
         /qaps/contab /qaps/reposi /qaps/gerenc
         /qaps/priori /qaps/gerxprio /qaps/mcost
               INTO (wa_cust_matnr-matnr,
                      wa_cust_matnr-bwkey,
                      wa_cust_matnr-contab,
                      wa_cust_matnr-reposi,
                      wa_cust_matnr-gerenc,
                      wa_cust_matnr-priori,
                      wa_cust_matnr-gerxprio,
                      wa_cust_matnr-mcost)
                FROM /qaps/zmilc1
               WHERE matnr  = wa_comp-comnr                     AND
                     werks  = /qaps/zmidfcig-werks              AND
                     /qaps/grkey = /qaps/zmidfcig-/qaps/grkey   AND
                     /qaps/period = /qaps/zmidfcig-/qaps/period AND
                     /qaps/versao = /qaps/zmidfcig-/qaps/versao.

        IF sy-subrc <> 0.
          MESSAGE e183(/qaps/zmi) WITH wa_comp-comnr.
        ENDIF.

        k      = k + 1.
        tp_n   = tp * -1.
        CLEAR: valor1.

        IF NOT /qaps/zmidfcig-/qaps/flcont IS INITIAL.
          valorx = wa_cust_matnr-contab   / zdiv.
          valorx = trunc( valorx ).
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flger  IS INITIAL.
          valorx = wa_cust_matnr-gerenc   / zdiv.
          valorx = trunc( valorx ).
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flrep IS INITIAL.
          valorx = wa_cust_matnr-reposi   / zdiv.
          valorx = trunc( valorx ).
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flpxg  IS INITIAL.
          valorx = wa_cust_matnr-gerxprio   / zdiv.
          valorx = trunc( valorx ).
        ENDIF.

        valor1 = ( valorx * tp_n ).
        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = valor1.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                               " LOAD_CUTS


  METHOD LOAD_EQ_RESTRICTIONS.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR:
      k, xmixnr.

    CLEAR xcomnr.

    LOOP AT it_rest_mist INTO wa_rest_mist.

      IF NOT wa_rest_mist-/qaps/eqmng IS INITIAL.

        IF xmixnr <> wa_rest_mist-/qaps/mixnr.

          n      = n + 1.
          xmixnr = wa_rest_mist-/qaps/mixnr.
          xrmeng = /qaps/zmidfcig-/qaps/rmeng.
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        READ TABLE it_comp INTO wa_comp
              WITH KEY comnr = wa_rest_mist-/qaps/comnr.

        IF sy-subrc = 0.
          k = sy-tabix.
          xrmeng = wa_rest_mist-/qaps/rmeng.
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.
        ENDIF.

      ELSE.

        IF  wa_rest_mist-/qaps/lemng IS INITIAL AND
            wa_rest_mist-/qaps/eqmng IS INITIAL AND
            wa_rest_mist-/qaps/gemng IS INITIAL.
          IF xmixnr <> wa_rest_mist-/qaps/mixnr.
            n      = n + 1.
            xmixnr = wa_rest_mist-/qaps/mixnr.

            IF  wa_rest_mist-/qaps/lemng IS INITIAL AND
                wa_rest_mist-/qaps/eqmng IS INITIAL AND
                wa_rest_mist-/qaps/gemng IS INITIAL.
              CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = 0.
            ELSE.
              xrmeng = /qaps/zmidfcig-/qaps/rmeng.
              CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    k = 0.

    LOOP AT it_rest_comp INTO wa_rest_comp.

      IF NOT wa_rest_comp-/qaps/eqmng IS INITIAL.

        n = n + 1.

        IF xcomnr <> wa_rest_comp-/qaps/comnr.

          xrmeng = ( wa_rest_comp-/qaps/eqmng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

          xcomnr = wa_rest_comp-/qaps/comnr.

        ENDIF.

        READ TABLE it_prop_carac INTO wa_prop_carac
              WITH KEY matnr = wa_rest_comp-/qaps/comnr.

        k = sy-tabix.

        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = 1.

      ENDIF.

    ENDLOOP.

    k = 1.

    CLEAR xchkey.

    LOOP AT it_zmi01 INTO wa_zmi01.

      IF NOT wa_zmi01-/qaps/emeng IS INITIAL.

        IF xchkey <> wa_zmi01-/qaps/chkey.

          n      = n + 1.
          k      = 1.
          xchkey = wa_zmi01-/qaps/chkey.

          xrmeng = ( wa_zmi01-/qaps/emeng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        LOOP AT it_prop_carac INTO wa_prop_carac
          WHERE chkey = wa_zmi01-/qaps/chkey.

          IF k <> a.
            xrmeng = wa_prop_carac-cmeng.
            CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.
          ENDIF.

          k = k + 1.

        ENDLOOP.

        k = 1.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                               " LOAD_EQ_RESTRICTIONS


  METHOD LOAD_GE_RESTRICTIONS.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR:
      k, xmixnr.

    CLEAR xcomnr.

    LOOP AT it_rest_mist INTO wa_rest_mist.

      IF NOT wa_rest_mist-/qaps/gemng IS INITIAL.

        IF xmixnr <> wa_rest_mist-/qaps/mixnr.

          n      = n + 1.
          xmixnr = wa_rest_mist-/qaps/mixnr.

          xrmeng =  ( wa_rest_mist-/qaps/gemng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        READ TABLE it_comp INTO wa_comp
              WITH KEY comnr = wa_rest_mist-/qaps/comnr.

        IF sy-subrc = 0.
          k = sy-tabix.
          xrmeng = wa_rest_mist-/qaps/rmeng.
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.
        ENDIF.

      ENDIF.

    ENDLOOP.

    k = 0.

    LOOP AT it_rest_comp INTO wa_rest_comp.

      IF NOT wa_rest_comp-/qaps/gemng IS INITIAL.

        n = n + 1.

        IF xcomnr <> wa_rest_comp-/qaps/comnr.

          xrmeng = ( wa_rest_comp-/qaps/gemng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

          xcomnr = wa_rest_comp-/qaps/comnr.

        ENDIF.

        READ TABLE it_prop_carac INTO wa_prop_carac
              WITH KEY matnr = wa_rest_comp-/qaps/comnr.

        k = sy-tabix.

        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = 1.

      ENDIF.

    ENDLOOP.

    k = 1.

    CLEAR xchkey.

    LOOP AT it_zmi01 INTO wa_zmi01.

      IF NOT wa_zmi01-/qaps/gmeng IS INITIAL.

        IF xchkey <> wa_zmi01-/qaps/chkey.

          n      = n + 1.
          k      = 1.
          xchkey = wa_zmi01-/qaps/chkey.

          xrmeng =  ( wa_zmi01-/qaps/gmeng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        LOOP AT it_prop_carac INTO wa_prop_carac
          WHERE chkey = wa_zmi01-/qaps/chkey.

          IF k <> a.
            xrmeng = wa_prop_carac-cmeng.
            CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.
          ENDIF.

          k = k + 1.

        ENDLOOP.

        k = 1.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                               " LOAD_GE_RESTRICTIONS


  METHOD LOAD_INTERNAL_TABLES.

    DATA wa_rs            TYPE ty_rs.
    DATA wa_rest_comp     TYPE ty_rest_comp.
    REFRESH: it_re.
    CLEAR: wa_re.

    LOOP AT it_pc_bk INTO wa_pc WHERE flgut = 'X'.
      wa_re-comnr = wa_pc-comnr.
      wa_re-coktx = wa_pc-coktx.
      APPEND wa_re TO it_re.
      APPEND wa_re TO it_re_bk.
    ENDLOOP.

    REFRESH: it_comp.
    CLEAR: wa_comp.

    LOOP AT it_re INTO wa_re.
      wa_ct_comp    =  wa_ct_comp + 1.
      wa_comp-comnr =  wa_re-comnr.
      APPEND wa_comp TO it_comp.
    ENDLOOP.

    IF   zajustou NE c_x.

      REFRESH it_zmi01.

      /qaps/zmi00-mandt = sy-mandt.
      /qaps/zmi00-matnr = /qaps/zmidfrs-matnr.
      /qaps/zmi00-werks = /qaps/zmidfrs-werks.
      /qaps/zmi00-/qaps/grkey = /qaps/zmidfrs-/qaps/grkey.
      /qaps/zmi00-/qaps/rmeng = /qaps/zmidfrs-/qaps/rmeng.
      /qaps/zmi00-/qaps/rmuni = /qaps/zmidfrs-/qaps/rmuni.

      IF NOT gv_reformulate IS INITIAL.
        BREAK c060863.
      ENDIF.

      LOOP AT it_rs INTO wa_rs.

        wa_zmi01-mandt = sy-mandt.
        wa_zmi01-matnr = /qaps/zmidfrs-matnr.
        wa_zmi01-werks = /qaps/zmidfrs-werks.
        wa_zmi01-/qaps/grkey = /qaps/zmidfrs-/qaps/grkey.
        wa_zmi01-/qaps/chkey = wa_rs-chkey.
        wa_zmi01-/qaps/gmuni = wa_rs-meuni.
        wa_zmi01-/qaps/emeng = wa_rs-emeng.
        wa_zmi01-/qaps/gmeng = wa_rs-gmeng.
        wa_zmi01-/qaps/lmeng = wa_rs-lmeng.
        wa_zmi01-/qaps/ymeng = wa_rs-ymeng.

        APPEND wa_zmi01 TO it_zmi01.

      ENDLOOP.

    ENDIF.



    LOOP AT it_zmi01 INTO wa_zmi01.

      IF  NOT wa_zmi01-/qaps/gmeng IS INITIAL.
        wa_ct_rest_ge = wa_ct_rest_ge + 1.
      ENDIF.

      IF  NOT wa_zmi01-/qaps/emeng IS INITIAL.
        wa_ct_rest_eq = wa_ct_rest_eq + 1.
      ENDIF.

      IF  NOT wa_zmi01-/qaps/lmeng IS INITIAL.
        wa_ct_rest_le = wa_ct_rest_le + 1.
      ENDIF.

    ENDLOOP.

    SELECT *
     FROM /qaps/zmic1
     WHERE matnr = @/qaps/zmidfcig-matnr AND
           werks = @/qaps/zmidfcig-werks AND
           /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
     INTO TABLE @DATA(lt_zmic1).

    LOOP AT lt_zmic1 INTO DATA(ls_zmic1).
      CHECK NOT line_exists( it_pc[ comnr = ls_zmic1-/qaps/comnr ] ).
      APPEND INITIAL LINE TO it_rest_comp ASSIGNING FIELD-SYMBOL(<fs_rest_comp>).
      <fs_rest_comp> = CORRESPONDING #( ls_zmic1 ).
    ENDLOOP.

    CALL METHOD zf_calcula_restricao.


    IF NOT it_zmicomp[] IS INITIAL.
      DELETE it_zmicomp WHERE /qaps/gemng = 0 AND /qaps/eqmng = 0 AND /qaps/lemng = 0.
      SORT it_zmicomp BY matnr werks /qaps/grkey /qaps/comnr.
      DELETE ADJACENT DUPLICATES FROM it_zmicomp
                      COMPARING  matnr werks /qaps/grkey /qaps/comnr.

      LOOP AT it_zmicomp INTO wa_zmicomp.
        IF NOT line_exists( it_rest_comp[ /qaps/comnr = wa_zmicomp-/qaps/comnr ] ).
          APPEND INITIAL LINE TO it_rest_comp ASSIGNING <fs_rest_comp>.
          IF line_exists( it_pc[ comnr = wa_zmicomp-/qaps/comnr ] ).
            DATA(ls_pc) = it_pc[ comnr = wa_zmicomp-/qaps/comnr ].

            <fs_rest_comp>-matnr = wa_zmicomp-matnr.
            <fs_rest_comp>-werks = wa_zmicomp-werks.
            <fs_rest_comp>-/qaps/grkey = wa_zmicomp-/qaps/grkey.
            <fs_rest_comp>-/qaps/comnr = ls_pc-comnr.
            <fs_rest_comp>-/qaps/gemng = ls_pc-gemng.
            <fs_rest_comp>-/qaps/eqmng = ls_pc-eqmng.
            <fs_rest_comp>-/qaps/lemng = ls_pc-lemng.
            CLEAR <fs_rest_comp>-auto.

          ELSE.
            MOVE-CORRESPONDING wa_zmicomp TO wa_rest_comp.
            CLEAR wa_rest_comp-auto.
            APPEND wa_rest_comp TO  it_rest_comp.
          ENDIF.
        ELSE.

          ASSIGN it_rest_comp[ /qaps/comnr = wa_zmicomp-/qaps/comnr ] TO <fs_rest_comp>.

          IF NOT <fs_rest_comp>-/qaps/gemng IS INITIAL AND <fs_rest_comp>-/qaps/gemng < wa_zmicomp-/qaps/gemng.
            <fs_rest_comp>-/qaps/gemng = wa_zmicomp-/qaps/gemng.
            CLEAR:<fs_rest_comp>-auto,
                  <fs_rest_comp>-/qaps/chkey.
          ELSEIF <fs_rest_comp>-/qaps/gemng IS INITIAL AND <fs_rest_comp>-/qaps/gemng < wa_zmicomp-/qaps/gemng.
            APPEND INITIAL LINE TO it_rest_comp ASSIGNING FIELD-SYMBOL(<fs_rest_comp_new>).
            <fs_rest_comp_new> = CORRESPONDING #( <fs_rest_comp> ).
            CLEAR: <fs_rest_comp_new>-/qaps/gemng,
                   <fs_rest_comp_new>-/qaps/eqmng,
                   <fs_rest_comp_new>-/qaps/lemng,
                   <fs_rest_comp_new>-auto,
                   <fs_rest_comp_new>-/qaps/chkey.
            <fs_rest_comp_new>-/qaps/gemng = wa_zmicomp-/qaps/gemng.
            CLEAR <fs_rest_comp_new>-auto.
            UNASSIGN <fs_rest_comp_new>.
          ENDIF.

          IF NOT <fs_rest_comp>-/qaps/eqmng IS INITIAL AND <fs_rest_comp>-/qaps/eqmng <> wa_zmicomp-/qaps/eqmng.
            <fs_rest_comp>-/qaps/eqmng = wa_zmicomp-/qaps/eqmng.
          ELSEIF <fs_rest_comp>-/qaps/eqmng IS INITIAL AND <fs_rest_comp>-/qaps/eqmng <> wa_zmicomp-/qaps/eqmng.
            APPEND INITIAL LINE TO it_rest_comp ASSIGNING <fs_rest_comp_new>.
            <fs_rest_comp_new> = CORRESPONDING #( <fs_rest_comp> ).
            CLEAR: <fs_rest_comp_new>-/qaps/gemng,
                   <fs_rest_comp_new>-/qaps/eqmng,
                   <fs_rest_comp_new>-/qaps/lemng,
                   <fs_rest_comp_new>-auto,
                   <fs_rest_comp_new>-/qaps/chkey.
            <fs_rest_comp_new>-/qaps/eqmng = wa_zmicomp-/qaps/eqmng.
            UNASSIGN <fs_rest_comp_new>.
          ENDIF.

          IF NOT <fs_rest_comp>-/qaps/lemng IS INITIAL AND <fs_rest_comp>-/qaps/lemng  > wa_zmicomp-/qaps/lemng .
            <fs_rest_comp>-/qaps/lemng = wa_zmicomp-/qaps/lemng.
          ELSEIF <fs_rest_comp>-/qaps/lemng IS INITIAL AND <fs_rest_comp>-/qaps/lemng < wa_zmicomp-/qaps/lemng.
            APPEND INITIAL LINE TO it_rest_comp ASSIGNING <fs_rest_comp_new>.
            <fs_rest_comp_new> = CORRESPONDING #( <fs_rest_comp> ).
            CLEAR: <fs_rest_comp_new>-/qaps/gemng,
                   <fs_rest_comp_new>-/qaps/eqmng,
                   <fs_rest_comp_new>-/qaps/lemng,
                   <fs_rest_comp_new>-auto,
                   <fs_rest_comp_new>-/qaps/chkey.
            <fs_rest_comp_new>-/qaps/lemng = wa_zmicomp-/qaps/lemng.
            UNASSIGN <fs_rest_comp_new>.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

    CALL METHOD ajustar_limites_rest_comp CHANGING ct_data = it_rest_comp.

    it_rest_comp_bk[] = it_rest_comp[].
    it_rest_verif[]   = it_rest_comp[].

    LOOP AT it_rest_comp INTO wa_rest_comp.
      READ TABLE it_comp INTO wa_comp WITH KEY comnr = wa_rest_comp-/qaps/comnr.

      IF sy-subrc <> 0.
        DELETE it_rest_comp.
      ENDIF.

    ENDLOOP.


    IF NOT it_rest_comp[] IS INITIAL.
      APPEND LINES OF it_rest_comp TO it_rest_comp_2.
    ENDIF.

    IF NOT it_rest_comp_2[] IS INITIAL.
      it_rest_comp[] = it_rest_comp_2[].
    ENDIF.

    DELETE it_rest_comp WHERE /qaps/gemng = 0 AND /qaps/eqmng = 0 AND /qaps/lemng = 0.

    CLEAR: wa_min-comnr.
    LOOP AT it_rest_comp INTO wa_rest_comp.
      READ TABLE it_comp WITH KEY comnr = wa_rest_comp-/qaps/comnr
                                  TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE it_rest_comp.
      ENDIF.
    ENDLOOP.

    SORT it_rest_comp BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng /qaps/eqmng /qaps/lemng.
    DELETE ADJACENT DUPLICATES FROM it_rest_comp
    COMPARING matnr werks /qaps/grkey /qaps/comnr /qaps/gemng /qaps/eqmng /qaps/lemng.

    LOOP AT it_rest_comp INTO wa_rest_comp.

      IF  NOT wa_rest_comp-/qaps/gemng IS INITIAL.
        wa_ct_rest_ge = wa_ct_rest_ge + 1.
      ENDIF.

      IF  NOT wa_rest_comp-/qaps/eqmng IS INITIAL.
        wa_ct_rest_eq = wa_ct_rest_eq + 1.
      ENDIF.

      IF  NOT wa_rest_comp-/qaps/lemng IS INITIAL.
        wa_ct_rest_le = wa_ct_rest_le + 1.
      ENDIF.

    ENDLOOP.

    SELECT matnr /qaps/mixnr /qaps/gemng /qaps/eqmng /qaps/lemng
      INTO CORRESPONDING FIELDS OF TABLE it_rest_mist
      FROM /qaps/zmirm0
      WHERE matnr = /qaps/zmidfcig-matnr
       AND werks = /qaps/zmidfcig-werks
       AND /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

    LOOP AT it_rest_mist INTO wa_rest_mist.

      IF  NOT wa_rest_mist-/qaps/gemng IS INITIAL.
        wa_ct_rest_ge = wa_ct_rest_ge + 1.
      ENDIF.

      IF  NOT wa_rest_mist-/qaps/eqmng IS INITIAL.
        wa_ct_rest_eq = wa_ct_rest_eq + 1.
      ENDIF.

      IF  NOT wa_rest_mist-/qaps/lemng IS INITIAL.
        wa_ct_rest_le = wa_ct_rest_le + 1.
      ENDIF.

    ENDLOOP.

    REFRESH it_rest_mist.

    SELECT a~matnr a~/qaps/mixnr a~/qaps/comnr a~/qaps/rmeng
           b~/qaps/gemng b~/qaps/eqmng b~/qaps/lemng
      INTO TABLE it_rest_mist
      FROM /qaps/zmirm1 AS a JOIN /qaps/zmirm0 AS b
        ON b~matnr = a~matnr
       AND b~werks = a~werks
       AND b~/qaps/grkey = a~/qaps/grkey
       AND b~/qaps/mixnr = a~/qaps/mixnr
     WHERE a~matnr = /qaps/zmidfcig-matnr
       AND a~werks = /qaps/zmidfcig-werks
       AND a~/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

    IF sy-subrc <> 0.
      MESSAGE e198(/qaps/zmi).
    ENDIF.

    LOOP AT it_rest_mist INTO wa_rest_mist.
      tabix = sy-tabix.

      READ TABLE it_comp INTO wa_comp
                 WITH KEY comnr = wa_rest_mist-/qaps/comnr.

      IF sy-subrc <> 0.
        DELETE it_rest_mist INDEX tabix.
      ENDIF.

    ENDLOOP.

    REFRESH it_prop_carac.
    SELECT *
      FROM /qaps/v_comp
      FOR ALL ENTRIES IN @it_comp
      WHERE matnr = @it_comp-comnr
      AND werks = @/qaps/zmidfcig-werks
      AND /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
      INTO TABLE @DATA(lt_v_comp).

    LOOP AT lt_v_comp INTO DATA(ls_v_comp).

      APPEND VALUE ty_prop_carac(
          matnr = ls_v_comp-matnr
          werks = ls_v_comp-werks
          grkey = ls_v_comp-/qaps/grkey
          rmeng = ls_v_comp-/qaps/rmeng
          rmuni = ls_v_comp-/qaps/rmuni
          chkey = ls_v_comp-/qaps/chkey
          cmeng = ls_v_comp-/qaps/cmeng ) TO it_prop_carac.
    ENDLOOP.

*
*
*


    SORT it_prop_carac BY chkey matnr.

    IF it_rest_comp_ini[] IS INITIAL.
      it_rest_comp_ini[] = it_rest_comp[].
    ENDIF.

  ENDMETHOD.                               " LOAD_INTERNAL_TABLES


  METHOD LOAD_LE_RESTRICTIONS.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR:
       k, xmixnr.

    CLEAR xcomnr.

    LOOP AT it_rest_mist INTO wa_rest_mist.

      IF NOT wa_rest_mist-/qaps/lemng IS INITIAL.

        IF xmixnr <> wa_rest_mist-/qaps/mixnr.

          n      = n + 1.
          xmixnr = wa_rest_mist-/qaps/mixnr.
          xrmeng = wa_rest_mist-/qaps/lemng * /qaps/zmidfcig-/qaps/rmeng.
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        READ TABLE it_comp INTO wa_comp
          WITH KEY comnr = wa_rest_mist-/qaps/comnr.

        IF sy-subrc = 0.
          k = sy-tabix.
          xrmeng = wa_rest_mist-/qaps/rmeng.
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.

        ENDIF.

      ENDIF.

    ENDLOOP.

    k = 0.

    LOOP AT it_rest_comp INTO wa_rest_comp.

      IF NOT wa_rest_comp-/qaps/lemng IS INITIAL.

        n = n + 1.

        IF xcomnr <> wa_rest_comp-/qaps/comnr.
          xrmeng = ( wa_rest_comp-/qaps/lemng * /qaps/zmidfcig-/qaps/rmeng ).

          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

          xcomnr = wa_rest_comp-/qaps/comnr.
        ENDIF.

        READ TABLE it_prop_carac INTO wa_prop_carac
              WITH KEY matnr = wa_rest_comp-/qaps/comnr.

        k = sy-tabix.

        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = 1.

      ENDIF.

    ENDLOOP.

    k = 1.

    CLEAR xchkey.

    LOOP AT it_zmi01 INTO wa_zmi01.

      IF NOT wa_zmi01-/qaps/lmeng IS INITIAL.

        IF xchkey <> wa_zmi01-/qaps/chkey.

          n      = n + 1.
          k      = 1.
          xchkey = wa_zmi01-/qaps/chkey.

          xrmeng = ( wa_zmi01-/qaps/lmeng * /qaps/zmidfcig-/qaps/rmeng ).
          CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = xrmeng.

        ENDIF.

        LOOP AT it_prop_carac INTO wa_prop_carac
          WHERE chkey = wa_zmi01-/qaps/chkey.

          IF k <> a.
            xrmeng = wa_prop_carac-cmeng.
            CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = xrmeng.
          ENDIF.

          k = k + 1.

        ENDLOOP.

        k = 1.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                               " LOAD_LE_RESTRICTIONS


  METHOD LOAD_MATRIX.

    DATA: wa_zmi60_bk      TYPE /qaps/zmi60,
          wa_rest_comp     TYPE ty_rest_comp,
          lv_message_error TYPE string.

    CALL METHOD first_formulation.

    IF NOT it_ng IS INITIAL  AND
      wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
      IF NOT it_dif[]  IS INITIAL.
        DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
        DELETE it_combi_final WHERE combi = xcombi.
      ENDIF.
    ENDIF.


    DELETE it_combi_final WHERE costt = 0.

    IF lines( it_combi_final ) = 0.
      MESSAGE e133(/qaps/zmi) INTO lv_message_error.
      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
        EXPORTING
          message = lv_message_error.
    ENDIF.

    DATA(ls_combi) = it_combi_final[ 1 ].
    APPEND VALUE ts_inicial(
        zdiv           = zdiv
        reformulate    = gv_reformulate
        combi       = ls_combi-combi
        comnr       = ls_combi-comnr
        costt       = ls_combi-costt
        msg         = ls_combi-msg
        it_re          = it_re[] ) TO gt_initial.

    LOOP AT it_combi_final INTO wa_combi_final.
      IF  wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
        wa_combi_final-costt = 0.
        MODIFY it_combi_final FROM wa_combi_final.
      ELSE.
        REFRESH it_zmi61.
      ENDIF.
    ENDLOOP.


    CLEAR : xcombi.
    CALL METHOD clear_workarea.

    REFRESH: it_zmi60.

    IF NOT it_zmi61[] IS INITIAL.
      SELECT * INTO TABLE it_zmi60
        FROM /qaps/zmi60
        FOR ALL ENTRIES IN it_zmi61
        WHERE matnr = it_zmi61-matnr AND
              werks = it_zmi61-werks AND
           /qaps/mixin = it_zmi61-/qaps/mixin.
    ENDIF.
    DESCRIBE TABLE it_zmi60 LINES lines.

    IF lines <> 0.
      CALL METHOD incompatibilidade.
    ENDIF.

    SORT it_zmicig0 BY matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.
    SORT it_zmicig0_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.

    SORT it_zmicig1 BY matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.
    SORT it_zmicig1_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.

    SORT it_zmicig2 BY matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.
    SORT it_zmicig2_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.

    DELETE ADJACENT DUPLICATES FROM it_zmicig0
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.
    DELETE  ADJACENT DUPLICATES FROM it_zmicig0_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.

    DELETE  ADJACENT DUPLICATES FROM  it_zmicig1
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.
    DELETE ADJACENT DUPLICATES FROM it_zmicig1_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.

    DELETE  ADJACENT DUPLICATES FROM it_zmicig2
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.
    DELETE  ADJACENT DUPLICATES FROM it_zmicig2_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.

    it_zmicig0_bk = it_zmicig0.
    it_zmicig1_bk = it_zmicig1.
    it_zmicig2_bk = it_zmicig2.

    it_zmicig0_min = it_zmicig0.
    it_zmicig1_min = it_zmicig1.
    it_zmicig2_min = it_zmicig2.

    DATA l_tabix TYPE sy-tabix.                             "#EC NEEDED

    DELETE it_combi_final WHERE costt = 0.
    SORT it_combi_final BY costt combi.

    IF NOT it_combi_final[] IS INITIAL.

      LOOP AT it_pc  INTO wa_pc WHERE flgut = 'X'.
        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low =  wa_pc-comnr ) TO r_matnr.
      ENDLOOP.

      SELECT * INTO TABLE it_zmimi
      FROM /qaps/zmimi
      WHERE werks = /qaps/zmidfcig-werks AND
            matnr IN r_matnr.

      LOOP AT it_zmimi INTO wa_zmimi.
        IF wa_zmimi-matnr  IN r_matnr.
        ELSE.
          DELETE it_zmimi INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      IF NOT it_zmimi[] IS INITIAL.

        CALL METHOD zf_calcula_restricao.
        IF NOT it_rest_comp[] IS INITIAL.
          LOOP AT it_rest_comp INTO wa_rest_comp.
            MOVE-CORRESPONDING wa_rest_comp TO wa_zmic1_comp.
            APPEND wa_zmic1_comp TO it_zmic1_comp.
            CLEAR: wa_rest_comp, wa_zmic1_comp.
          ENDLOOP.
        ENDIF.

        SELECT *
          FROM /qaps/zmic1
          APPENDING TABLE it_zmic1_comp
          WHERE matnr = /qaps/zmidfcig-matnr
            AND werks = /qaps/zmidfcig-werks
            AND /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

        IF NOT it_zmic1_comp[] IS INITIAL.
          SORT it_zmic1_comp BY werks /qaps/comnr.
          SORT it_zmimi BY werks matnr.
          LOOP AT it_zmimi INTO wa_zmimi.
            tabix = sy-tabix.
            READ TABLE it_zmic1_comp INTO wa_zmic1_comp WITH KEY werks = wa_zmimi-werks
                                                           /qaps/comnr = wa_zmimi-matnr
                                                        BINARY SEARCH.
            IF sy-subrc = 0.
              IF wa_zmic1_comp-/qaps/gemng > 0 AND wa_zmic1_comp-/qaps/gemng >= wa_zmimi-/qaps/rmeng.
                wa_zmimi-/qaps/rmeng = wa_zmic1_comp-/qaps/gemng.
              ELSEIF wa_zmic1_comp-/qaps/lemng > 0 AND wa_zmic1_comp-/qaps/lemng <= wa_zmimi-/qaps/rmeng.
                wa_zmimi-/qaps/rmeng = wa_zmic1_comp-/qaps/lemng.
              ELSEIF wa_zmic1_comp-/qaps/eqmng > 0 AND wa_zmic1_comp-/qaps/eqmng > wa_zmimi-/qaps/rmeng.
                wa_zmimi-/qaps/rmeng = wa_zmic1_comp-/qaps/eqmng.
              ENDIF.
            ENDIF.
            MODIFY it_zmimi FROM wa_zmimi INDEX tabix.
            CLEAR: wa_zmimi, wa_zmic1_comp.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF NOT it_zmimi[] IS INITIAL.
        it_zmi60_bk[] = it_zmi60[].
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        DELETE  ADJACENT DUPLICATES FROM it_zmicig0 COMPARING /qaps/costt .
        READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.

        DATA l_flag_incomp  TYPE c.

        LOOP AT it_zmicig1 INTO wa_zmicig1  WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
          LOOP AT it_zmi60_bk INTO wa_zmi60_bk WHERE matnr = wa_zmicig1-/qaps/comnr.
            DELETE it_zmi60 WHERE matnr = wa_zmi60_bk-matnr.
          ENDLOOP.
        ENDLOOP.

        it_zmi60_bk[] = it_zmi60[].
        LOOP AT it_zmicig1 INTO wa_zmicig1  WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
          LOOP AT it_zmi60_bk INTO wa_zmi60_bk WHERE matnr NE wa_zmicig1-/qaps/comnr.
            IF wa_zmi60_bk-/qaps/mixin NE wa_zmicig0-/qaps/cignr.
              LOOP AT it_zmi61 INTO wa_zmi61 WHERE /qaps/comnr = wa_zmicig1-/qaps/comnr.
                l_flag_incomp = 'X'.
                DELETE it_zmi60 WHERE matnr = wa_zmi61-/qaps/comnr.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        IF  l_flag_incomp IS INITIAL.
          REFRESH it_zmi60[].
        ENDIF.

        it_combi_fim[] = it_combi_final[].

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *
        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.
          CALL METHOD condicao_minimo_nivel01.
        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        DELETE it_zmicig0 WHERE /qaps/cignr = 0000999995.

        DELETE it_zmicig1 WHERE /qaps/cignr = 0000999995.

        DELETE it_zmicig2 WHERE /qaps/cignr = 0000999995.


        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.


            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        it_combi_finalx[] =  it_combi_final[].
        READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
        xcombi =  wa_combi_finalx-combi.
        xcombi_min =  wa_combi_finalx-combi.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

        IF  sy-subrc = 0.
          CALL METHOD verifica_cond_minimo.
          IF  it_min[] IS INITIAL.
            CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

            IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
              RETURN.
            ENDIF.
          ENDIF.

          CALL METHOD condicao_minimo_nivel02.

        ELSE.
          CALL METHOD del_comd_min.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
        ENDIF.

        SORT it_min  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

        SORT it_min_bk  BY comnr.
        DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        CALL METHOD contingencia CHANGING cv_has_result = cv_has_result.
*

        DATA  ll_tabix  TYPE sy-tabix.
        DATA  l_ind  TYPE sy-tabix.

        ind_re = 0.

        DO 20 TIMES.

          l_ind =  l_ind + 1.
          IF l_ind > 20.
            EXIT.
          ENDIF.

          LOOP AT it_combi_final ASSIGNING FIELD-SYMBOL(<fs>).
            <fs>-combi = |{ <fs>-combi ALPHA = IN WIDTH = 10 }|.
          ENDLOOP.

          it_combi_finalx[] =  it_combi_final[].
          DELETE it_zmicig0 WHERE /qaps/costt = 0.
          SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
          DELETE  ADJACENT DUPLICATES FROM it_zmicig0 COMPARING /qaps/costt .
          LOOP AT it_zmicig0 INTO wa_zmicig0.
            ll_tabix = sy-tabix.
            xcombi =   wa_zmicig0-/qaps/cignr.
            xcombi_min =  wa_zmicig0-/qaps/cignr.
            wa_combi_finalx-combi =  wa_zmicig0-/qaps/cignr.
            CALL METHOD verifica_cond_minimo.
            LOOP AT it_min INTO wa_min.
              READ TABLE it_min_bk INTO wa_min_bk WITH KEY comnr = wa_min-comnr.
              IF sy-subrc = 0.
                READ TABLE it_zmicig1 INTO wa_zmicig1  WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                                /qaps/comnr = wa_min-comnr.
                IF   sy-subrc = 0.
                  READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
                  IF sy-subrc = 0.
                    IF  wa_zmicig1-/qaps/cmeng LT  wa_zmimi-/qaps/rmeng.
                      DELETE it_zmicig0 INDEX ll_tabix.
                      DELETE it_zmicig1 WHERE /qaps/cignr EQ wa_zmicig0-/qaps/cignr.
                      DELETE it_zmicig2 WHERE /qaps/cignr EQ wa_zmicig0-/qaps/cignr.
                      DELETE it_zmicig0_min WHERE /qaps/cignr EQ wa_zmicig0-/qaps/cignr.
                      DELETE it_zmicig1_min WHERE /qaps/cignr EQ wa_zmicig0-/qaps/cignr.
                      DELETE it_zmicig2_min WHERE /qaps/cignr EQ wa_zmicig0-/qaps/cignr.
                      DELETE it_combi_final  WHERE combi  =  xcombi_min.
                      DELETE it_combi_finalx WHERE combi =  xcombi_min.
                    ENDIF.
                  ENDIF.
                ELSE.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          DELETE it_zmicig0 WHERE /qaps/costt = 0.
          SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
          READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
          xcombi_min = wa_zmicig0-/qaps/cignr.

          READ TABLE it_combi_finalx WITH KEY combi = xcombi_min
                                     INTO wa_combi_finalx.

          IF  sy-subrc = 0.
            CALL METHOD verifica_cond_minimo.
            LOOP AT it_min INTO wa_min.
              READ TABLE it_min_bk INTO wa_min_bk WITH KEY comnr = wa_min-comnr.
              IF sy-subrc = 0.
                READ TABLE it_zmicig1 INTO wa_zmicig1  WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                                /qaps/comnr = wa_min-comnr.
                IF   sy-subrc = 0.
                  READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
                  IF sy-subrc = 0.
                    IF  wa_zmicig1-/qaps/cmeng LT  wa_zmimi-/qaps/rmeng.
                    ENDIF.
                  ENDIF.
                ELSE.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF  it_min[] IS INITIAL.
              IF xcombi_min =  wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig0 WHERE /qaps/cignr NE xcombi_min.
                DELETE it_zmicig1 WHERE /qaps/cignr NE xcombi_min.
                DELETE it_zmicig2 WHERE /qaps/cignr  NE xcombi_min.
                DELETE it_zmicig0_min WHERE /qaps/cignr  NE xcombi_min.
                DELETE it_zmicig1_min WHERE /qaps/cignr  NE xcombi_min.
                DELETE it_zmicig2_min WHERE /qaps/cignr  NE xcombi_min.
                CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

                IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                  RETURN.
                ENDIF.
              ELSE.
                DELETE it_zmicig0 WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig1 WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig2 WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig0_min WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig1_min WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                DELETE it_zmicig2_min WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
                CALL METHOD resultado CHANGING cv_has_result = cv_has_result.

                IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.


            CALL METHOD condicao_minimo_nivel02.

          ELSE.
            DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
            CALL METHOD condicao_minimo_nivel02.
            CALL METHOD del_comd_min.

          ENDIF.

          SORT it_min  BY comnr.
          DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

          SORT it_min_bk  BY comnr.
          DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

        ENDDO.


      ENDIF.
*
    ENDIF.
*
*


    xcombi = wa_zmicig0-/qaps/cignr.
    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
    IF  sy-subrc = 0.
      xcombi = wa_zmicig0-/qaps/cignr.
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      CLEAR it_min[].
      CALL METHOD verifica_cond_minimo.
      IF NOT it_min[] IS INITIAL.
        DELETE it_zmicig0  WHERE /qaps/cignr = xcombi.
      ENDIF.
    ENDIF.


    DELETE it_zmicig0_min WHERE /qaps/cignr <> xcombi.
    DELETE it_zmicig1_min WHERE /qaps/cignr <> xcombi.
    DELETE it_zmicig2_min WHERE /qaps/cignr <> xcombi.
    DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
    DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
    DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.


    DESCRIBE TABLE it_zmicig0 LINES lines.

    IF lines = 0.
      DATA lv_message TYPE string.
      MESSAGE e133(/qaps/zmi) INTO lv_message.
      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
        EXPORTING
          message = lv_message.
    ELSE.

      CALL METHOD load_results.

      LOOP AT it_re INTO wa_re.
        IF NOT /qaps/zmidfcig-/qaps/fllt1e8 IS INITIAL.
          SELECT SINGLE matnr bwkey verpr stprs peinh vjbwh bwph1
                 INTO (wa_cust_matnr-matnr,
                        wa_cust_matnr-bwkey,
                        wa_cust_matnr-verpr,
                        wa_cust_matnr-stprs,
                        wa_cust_matnr-peinh,
                        wa_cust_matnr-vjbwh,
                        wa_cust_matnr-bwph1)
                 FROM mbew
                 WHERE matnr = wa_re-comnr  AND
                       bwkey = /qaps/zmidfcig-werks.

          wa_re-verpr = wa_cust_matnr-vjbwh.
          MODIFY it_re FROM wa_re.

        ELSE.

          SELECT SINGLE matnr
                        werks
                        /qaps/contab
                        /qaps/reposi
                        /qaps/gerenc
                        /qaps/priori
                        /qaps/gerxprio
                      INTO (wa_cust_matnr-matnr,
                             wa_cust_matnr-bwkey,
                             wa_cust_matnr-contab,
                             wa_cust_matnr-reposi,
                             wa_cust_matnr-gerenc,
                             wa_cust_matnr-priori,
                             wa_cust_matnr-gerxprio)
                      FROM /qaps/zmilc1
                      WHERE matnr  = wa_re-comnr  AND
                            werks  = /qaps/zmidfcig-werks AND
                            /qaps/grkey  = /qaps/zmidfcig-/qaps/grkey  AND
                            /qaps/period = /qaps/zmidfcig-/qaps/period AND
                            /qaps/versao = /qaps/zmidfcig-/qaps/versao.

          IF NOT /qaps/zmidfcig-/qaps/flcont IS INITIAL.
            wa_re-verpr = wa_cust_matnr-contab.
          ENDIF.

          IF NOT /qaps/zmidfcig-/qaps/flger  IS INITIAL.
            wa_re-verpr = wa_cust_matnr-gerenc.
          ENDIF.

          IF NOT /qaps/zmidfcig-/qaps/flrep IS INITIAL.
            wa_re-verpr = wa_cust_matnr-reposi.
          ENDIF.

          IF NOT /qaps/zmidfcig-/qaps/flpxg  IS INITIAL.
            wa_re-verpr = wa_cust_matnr-gerxprio.
          ENDIF.

          MODIFY it_re FROM wa_re.

        ENDIF.

      ENDLOOP.

    ENDIF.

    CALL METHOD save_snapshot.
    cv_has_result = abap_true.

  ENDMETHOD.                               " LOAD_MATRIX


  METHOD LOAD_RESULTS.

    DATA: wa_ng    TYPE ty_ng,
          wa_rs_bk TYPE ty_rs.
    REFRESH: it_re, it_ng.
    CLEAR: wa_re, wa_ng.

    LOOP AT it_zmicig1_min INTO wa_zmicig1
      WHERE /qaps/cignr = xcombi.
      wa_re-comnr = wa_zmicig1-/qaps/comnr.

      CLEAR: wa_re-coktx.

      SELECT SINGLE maktx INTO wa_re-coktx
      FROM makt
      WHERE matnr = wa_re-comnr AND
            spras = sy-langu.

      wa_re-cmeng = wa_zmicig1-/qaps/cmeng.
      wa_re-cmuni = /qaps/zmidfcig-/qaps/rmuni.
      APPEND wa_re TO it_re.
*
    ENDLOOP.

    CALL METHOD zf_verifica_rest_minimos.
*
    LOOP AT it_zmicig2_min INTO wa_zmicig2
        WHERE /qaps/cignr = xcombi.

      wa_ng-chkey = wa_zmicig2-/qaps/chkey.
      wa_ng-cmeng = wa_zmicig2-/qaps/cmeng.
      wa_ng-cmuni = wa_zmicig2-/qaps/cmuni.

      CLEAR: wa_ng-chdes, wa_ng-gmeng, wa_ng-flend, wa_ng-lmeng.

      SELECT SINGLE /qaps/chdes INTO wa_ng-chdes FROM /qaps/zmitc
             WHERE /qaps/chkey = wa_zmicig2-/qaps/chkey.

      IF NOT wa_zmicig2-/qaps/emeng IS INITIAL.

        wa_ng-flbeg = '='.
        wa_ng-gmeng = wa_zmicig2-/qaps/emeng.

      ELSE.

        IF NOT wa_zmicig2-/qaps/gmeng IS INITIAL.

          READ TABLE it_rs_bk INTO wa_rs_bk WITH KEY chkey = wa_zmicig2-/qaps/chkey.
          IF sy-subrc = 0.
            wa_zmicig2-/qaps/gmeng =  wa_rs_bk-gmeng.
          ENDIF.
          wa_ng-flbeg = '>='.
          wa_ng-gmeng = wa_zmicig2-/qaps/gmeng.
        ENDIF.

        wa_ng-flbeg = '>='.
        wa_ng-gmeng = wa_zmicig2-/qaps/gmeng.

      ENDIF.

      IF NOT wa_zmicig2-/qaps/lmeng IS INITIAL.
        wa_ng-flend = '<='.
        wa_ng-lmeng = wa_zmicig2-/qaps/lmeng.
      ENDIF.

      APPEND wa_ng TO it_ng.

    ENDLOOP.


    READ TABLE it_zmicig0_min INTO wa_zmicig0 INDEX 1.

    /qaps/zmidfcig-/qaps/costt   = wa_zmicig0-/qaps/costt.
    /qaps/zmidfcig-/qaps/costu   = wa_zmicig0-/qaps/costt / wa_zmicig0-/qaps/rmeng.
    /qaps/zmidfcig-/qaps/gerxprio = wa_zmicig0-/qaps/gerxprio.

    /qaps/zmidfcig-/qaps/tot_ajumer = wa_zmicig0-/qaps/tot_ajumer.
    /qaps/zmidfcig-/qaps/tot_markup = wa_zmicig0-/qaps/tot_markup.

    /qaps/zmidfcig-/qaps/mcost = wa_zmicig0-/qaps/mcost.

  ENDMETHOD.                    " LOAD_RESULTS


  METHOD LOAD_RESULTS_2 .

    DATA wa_ng    TYPE ty_ng.

    REFRESH: it_re, it_ng.
    CLEAR: wa_re, wa_ng.

    LOOP AT it_zmicig1 INTO wa_zmicig1
      WHERE /qaps/cignr = xcombi.
      wa_re-comnr = wa_zmicig1-/qaps/comnr.

      CLEAR: wa_re-coktx.

      SELECT SINGLE maktx INTO wa_re-coktx
      FROM makt
      WHERE matnr = wa_re-comnr AND
            spras = sy-langu.

      wa_re-cmeng = wa_zmicig1-/qaps/cmeng.
      wa_re-cmuni = /qaps/zmidfcig-/qaps/rmuni.
      APPEND wa_re TO it_re.
*
    ENDLOOP.

    CALL METHOD zf_verifica_rest_minimos.

    LOOP AT it_zmicig2 INTO wa_zmicig2
        WHERE /qaps/cignr = xcombi.

      wa_ng-chkey = wa_zmicig2-/qaps/chkey.
      wa_ng-cmeng = wa_zmicig2-/qaps/cmeng.
      wa_ng-cmuni = wa_zmicig2-/qaps/cmuni.

      CLEAR: wa_ng-chdes, wa_ng-gmeng, wa_ng-flend, wa_ng-lmeng.

      SELECT SINGLE /qaps/chdes INTO wa_ng-chdes FROM /qaps/zmitc
             WHERE /qaps/chkey = wa_zmicig2-/qaps/chkey.

      IF NOT wa_zmicig2-/qaps/emeng IS INITIAL.

        wa_ng-flbeg = '='.
        wa_ng-gmeng = wa_zmicig2-/qaps/emeng.

      ELSE.

        IF NOT wa_zmicig2-/qaps/gmeng IS INITIAL.

          READ TABLE it_zmi01_bk INTO wa_zmi01 WITH KEY /qaps/chkey = wa_ng-chkey.
          IF sy-subrc = 0.
            wa_zmicig2-/qaps/gmeng =   wa_zmi01-/qaps/gmeng.
          ENDIF.

          wa_ng-flbeg = '>='.
          wa_ng-gmeng = wa_zmicig2-/qaps/gmeng.

        ELSE.

          wa_ng-flbeg = '>='.

        ENDIF.

        IF NOT wa_zmicig2-/qaps/lmeng IS INITIAL.
          wa_ng-flend = '<='.
          wa_ng-lmeng = wa_zmicig2-/qaps/lmeng.
        ELSE.
          wa_ng-flend = '<='.
        ENDIF.

      ENDIF.

      APPEND wa_ng TO it_ng.

    ENDLOOP.


    READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.

    /qaps/zmidfcig-/qaps/costt   = wa_zmicig0-/qaps/costt.
    /qaps/zmidfcig-/qaps/costu   = wa_zmicig0-/qaps/costt / wa_zmicig0-/qaps/rmeng.
    /qaps/zmidfcig-/qaps/gerxprio = wa_zmicig0-/qaps/gerxprio.
    /qaps/zmidfcig-/qaps/mcost = wa_zmicig0-/qaps/mcost.

  ENDMETHOD.                    " LOAD_RESULTS_2


  METHOD MODIFIES_MATRIX.

    i = y - x.
    k = nv + 1.
    n = 1.

    WHILE n <= x.

      CALL METHOD set_matrix_a EXPORTING p_line = n p_col = i p_valor = 1.

      wa_vc-chave   = i.
      wa_vc-valor   = n.
      APPEND wa_vc TO it_vc.

      i = i + 1.

      IF n <= r1.
        n = n + 1.
      ELSE.

        vark = r1 + r2.

        IF n > vark.
          CALL METHOD stage_650.
        ENDIF.

        CALL METHOD step_500.

        n = n + 1.

      ENDIF.

    ENDWHILE.

  ENDMETHOD.                               " MODIFIES_MATRIX


  METHOD ORDENA_NIVEIS_GARANTIA .

    DATA wa_ng    TYPE ty_ng.

    SORT it_ng BY chkey.
    DELETE ADJACENT DUPLICATES FROM it_ng COMPARING chkey.

    LOOP AT it_ng INTO wa_ng.

      IF wa_ng-chkey = 'N'.
        wa_ng-ord = 1.
      ELSE.
        IF wa_ng-chkey = 'P'.
          wa_ng-ord = 2.
        ELSE.
          IF wa_ng-chkey = 'K'.
            wa_ng-ord = 3.
          ELSE.
            ind = ind + 1.
            wa_ng-ord = ind.
          ENDIF.
        ENDIF.
      ENDIF.

      MODIFY it_ng FROM wa_ng.

    ENDLOOP.

    SORT it_ng BY ord.

  ENDMETHOD.                    " ORDENA_NIVEIS_GARANTIA


  METHOD PROCESS_COMBI_1.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.
    LOOP AT it_min INTO wa_min.

      xcomnr_min = wa_min-comnr.

      IF wa_min-comnr = aux1.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1


  METHOD PROCESS_COMBI_12.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_12


  METHOD PROCESS_COMBI_123.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux3.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_123


  METHOD PROCESS_COMBI_1234.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2  OR
         wa_min-comnr = aux3 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1234


  METHOD PROCESS_COMBI_12345.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2  OR
         wa_min-comnr = aux3 OR wa_min-comnr = aux4 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_123456


  METHOD PROCESS_COMBI_123456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_123456


  METHOD PROCESS_COMBI_1235.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2  OR
         wa_min-comnr = aux3 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1235


  METHOD PROCESS_COMBI_1236.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2  OR
         wa_min-comnr = aux3 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1236


  METHOD PROCESS_COMBI_1237.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2  OR
         wa_min-comnr = aux3 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1237


  METHOD PROCESS_COMBI_124.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_124


  METHOD PROCESS_COMBI_125.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_125


  METHOD PROCESS_COMBI_126.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_126


  METHOD PROCESS_COMBI_127.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux2 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_127


  METHOD PROCESS_COMBI_13.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_13


  METHOD PROCESS_COMBI_134.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.
      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_134


  METHOD PROCESS_COMBI_1345.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1345


  METHOD PROCESS_COMBI_13456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_13456


  METHOD PROCESS_COMBI_134567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_134567


  METHOD PROCESS_COMBI_1346.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1346


  METHOD PROCESS_COMBI_1347.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1347


  METHOD PROCESS_COMBI_135.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_135


  METHOD PROCESS_COMBI_136.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_136


  METHOD PROCESS_COMBI_137.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux3 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_137


  METHOD PROCESS_COMBI_14.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_14


  METHOD PROCESS_COMBI_1456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux4 OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1456


  METHOD PROCESS_COMBI_14567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_14567


  METHOD PROCESS_COMBI_1457.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux4 OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1457


  METHOD PROCESS_COMBI_15.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_15


  METHOD PROCESS_COMBI_1567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux5 OR
         wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_1567


  METHOD PROCESS_COMBI_16.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_16


  METHOD PROCESS_COMBI_17.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux1 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_17


  METHOD PROCESS_COMBI_2.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2


  METHOD PROCESS_COMBI_23.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_23


  METHOD PROCESS_COMBI_234.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_234


  METHOD PROCESS_COMBI_2345.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.
      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2345


  METHOD PROCESS_COMBI_23456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_23456


  METHOD PROCESS_COMBI_234567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_234567


  METHOD PROCESS_COMBI_2346 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2346


  METHOD PROCESS_COMBI_2347 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
         wa_min-comnr = aux4 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2347


  METHOD PROCESS_COMBI_235.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_235


  METHOD PROCESS_COMBI_2356 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
        wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2356


  METHOD PROCESS_COMBI_2357 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3  OR
        wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2357


  METHOD PROCESS_COMBI_236.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_236


  METHOD PROCESS_COMBI_237.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux3 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_237


  METHOD PROCESS_COMBI_24.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_24


  METHOD PROCESS_COMBI_2456 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2456


  METHOD PROCESS_COMBI_24567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_24567


  METHOD PROCESS_COMBI_2457 .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_2457


  METHOD PROCESS_COMBI_25.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.
      IF wa_min-comnr = aux2 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_25


  METHOD PROCESS_COMBI_26.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_26


  METHOD PROCESS_COMBI_27.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux2 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_27


  METHOD PROCESS_COMBI_3.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-/qaps/gemng = xcmeng_min.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_3


  METHOD PROCESS_COMBI_34.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_34


  METHOD PROCESS_COMBI_345.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_345


  METHOD PROCESS_COMBI_3456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4  OR
        wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_3456


  METHOD PROCESS_COMBI_34567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_34567


  METHOD PROCESS_COMBI_3457.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4  OR
         wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_3457


  METHOD PROCESS_COMBI_346.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_346


  METHOD PROCESS_COMBI_347.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux4 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_347


  METHOD PROCESS_COMBI_35.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_35


  METHOD PROCESS_COMBI_36.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_36


  METHOD PROCESS_COMBI_37.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux3 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_37


  METHOD PROCESS_COMBI_4.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_4


  METHOD PROCESS_COMBI_45.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_45


  METHOD PROCESS_COMBI_456.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_456


  METHOD PROCESS_COMBI_4567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux5  OR
        wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_4567


  METHOD PROCESS_COMBI_457.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_457


  METHOD PROCESS_COMBI_46.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_46


  METHOD PROCESS_COMBI_47.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux4 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_47


  METHOD PROCESS_COMBI_5.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux5.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_5


  METHOD PROCESS_COMBI_56.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux5 OR wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_56


  METHOD PROCESS_COMBI_567.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux5 OR wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_567


  METHOD PROCESS_COMBI_57.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.
      IF wa_min-comnr = aux5 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_57


  METHOD PROCESS_COMBI_6.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux6.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_6


  METHOD PROCESS_COMBI_67.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.
    CLEAR: wa_min-comnr.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux6 OR wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_67


  METHOD PROCESS_COMBI_7.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR wa_combi_final-msg.

    "xcombi = xcombi.
    xcombi_aux = xcombi.

    it_pc_bk = it_pc.

    LOOP AT it_min INTO wa_min.

      IF wa_min-comnr = aux7.
        LOOP AT it_pc_bk INTO wa_pc
          WHERE comnr =  wa_min-comnr.
          wa_pc-flgut = 'X'.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        CLEAR wa_rest_comp.
        wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
        wa_rest_comp-werks = /qaps/zmidfcig-werks.
        wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
        wa_rest_comp-/qaps/comnr = wa_min-comnr.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.
          wa_rest_comp-/qaps/gemng = xcmeng_min.
        ENDIF.
        wa_rest_comp-cond_min = 'X'.
        APPEND wa_rest_comp TO it_rest_comp_2.
      ELSE.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
        IF sy-subrc = 0.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr =  wa_min-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CALL METHOD verif_incomp_e_formula.

  ENDMETHOD.                    " PROCESS_COMBI_7


  METHOD PROCESS_IT_MIN .

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR: wa_min-comnr.

    SORT it_min BY comnr combi.
    DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.

    xcombi_aux = xcombi.
    CALL METHOD clear_workarea.
    it_pc_bk = it_pc.

    IF NOT it_desmarcar[] IS INITIAL.
      LOOP AT it_desmarcar INTO wa_desmarcar.
        LOOP AT it_pc_bk INTO wa_pc
         WHERE comnr =  wa_desmarcar-comnr.
          wa_pc-flgut = ' '.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    LOOP AT it_min INTO wa_min.

      xcomnr_min = wa_min-comnr.

      LOOP AT it_pc_bk INTO wa_pc
       WHERE comnr =  xcomnr_min.
        wa_pc-flgut = ' '.
        MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
      ENDLOOP.

    ENDLOOP.

*

    CHECK line_exists( it_pc_bk[ flgut = 'X' ] ).

    xcombi_n2 = xcombi.

    wa_flag_min = 0.
    CALL METHOD formulation.

    CALL METHOD guarantee_levels_min.

    IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
       wa_combi_final-msg IS INITIAL.
      CALL METHOD save_soluctions_min.
    ENDIF.

  ENDMETHOD.                    " PROCESS_IT_MIN


  METHOD PROCESS_IT_MIN_15.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CALL METHOD clear_workarea.
    CLEAR: wa_combi_final-msg, xcond_min, zajustou.

    CLEAR it_rest_comp_2[].

    it_pc_bk = it_pc.

    IF NOT it_desmarcar[] IS INITIAL.
      LOOP AT it_desmarcar INTO wa_desmarcar.
        LOOP AT it_pc_bk INTO wa_pc
         WHERE comnr =  wa_desmarcar-comnr.
          wa_pc-flgut = ' '.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    CLEAR: aux1, aux2, aux3, aux4, aux5, aux6, aux7.

    LOOP AT it_min INTO wa_min.
      CASE sy-tabix.
        WHEN 1.
          aux1 = wa_min-comnr.
        WHEN 2.
          aux2 = wa_min-comnr.
        WHEN 3.
          aux3 = wa_min-comnr.
        WHEN 4.
          aux4 = wa_min-comnr.
        WHEN 5.
          aux5 = wa_min-comnr.
        WHEN 6.
          aux6 = wa_min-comnr.
        WHEN 7.
          aux7 = wa_min-comnr.
      ENDCASE.
    ENDLOOP.

    LOOP AT it_min INTO wa_min.

      tabix = sy-tabix.
      xcomnr_min = wa_min-comnr.
      CLEAR wa_rest_comp.
      wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
      wa_rest_comp-werks = /qaps/zmidfcig-werks.
      wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
      wa_rest_comp-/qaps/comnr = wa_min-comnr.
      READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
      IF sy-subrc = 0.
        xcmeng_min = wa_zmimi-/qaps/rmeng.
        wa_rest_comp-/qaps/gemng = xcmeng_min.
      ENDIF.

      wa_rest_comp-cond_min = 'X'.
      APPEND wa_rest_comp TO it_rest_comp_2.

      LOOP AT it_pc_bk INTO wa_pc
         WHERE comnr =  wa_min-comnr.
        wa_pc-flgut = 'X'.
        MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
      ENDLOOP.

    ENDLOOP.

    IF  NOT aux1 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux1.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux1.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux1.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux2 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux2.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux2.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux2.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux3 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux3.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux3.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux3.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux4 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux4.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux4.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux4.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux5 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux5.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux5.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux5.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux6 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux6.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux6.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux6.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF  NOT aux7 IS INITIAL.
      READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux7.
      IF sy-subrc = 0.
        READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux7.
        IF sy-subrc = 0.
          IF wa_pc-flgut = 'X'.
            LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux7.
              LOOP AT it_pc_bk INTO wa_pc
               WHERE comnr =  wa_zmi61-/qaps/comnr.
                wa_pc-flgut = ' '.
                MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



    IF NOT it_desmarcar[] IS INITIAL.
      LOOP AT it_desmarcar INTO wa_desmarcar.
        LOOP AT it_pc_bk INTO wa_pc
         WHERE comnr =  wa_desmarcar-comnr.
          wa_pc-flgut = ' '.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
    xcombi_n2 = xcombi + 10000000.
    xcombi = xcombi_n2.

    wa_flag_min = 15.

    CALL METHOD formulation.

    CALL METHOD guarantee_levels_min.

    IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
       wa_combi_final-msg IS INITIAL.
      CALL METHOD save_soluctions_min.
    ENDIF.


  ENDMETHOD.                    " PROCESS_IT_MIN_15


  METHOD PRODUCT_FORMULATE.

    DATA lv_has_result TYPE abap_bool.

    CALL METHOD combinations.
    CALL METHOD load_matrix CHANGING cv_has_result = lv_has_result.

    SORT it_re BY  comnr.
    DELETE ADJACENT DUPLICATES FROM it_re
                                     COMPARING comnr.

  ENDMETHOD.                    " PRODUCT_FORMULATE


  METHOD REGRA_1.

    SELECT comp~/qaps/comnr AS comp_mat, mist~/qaps/comnr AS mist_mat
      FROM /qaps/zmic1 AS comp
      LEFT JOIN /qaps/zmirm1 AS mist
      ON  comp~matnr        =  mist~matnr
      AND comp~werks        =  mist~werks
      AND comp~/qaps/grkey  =  mist~/qaps/grkey
      AND comp~/qaps/comnr  =  mist~/qaps/comnr
      WHERE comp~matnr = @us_header-matnr
      AND   comp~werks = @us_header-werks
      AND   comp~/qaps/grkey = @us_header-grkey
      AND   ( comp~/qaps/gemng > 0 OR comp~/qaps/eqmng > 0 )
      INTO TABLE @DATA(lt_data).

    CHECK lines( lt_data ) > 0.
    DELETE lt_data WHERE NOT mist_mat IS INITIAL.

    CHECK lines( lt_data ) > 0.

    DATA(lv_material) = lt_data[ 1 ]-comp_mat.
    SHIFT lv_material LEFT DELETING LEADING '0'.
    DATA lv_message TYPE string.
    MESSAGE e228(/qaps/zmi) WITH lv_material INTO lv_message.
    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE #( type = 'E'
                           message = lv_message ).


  ENDMETHOD.


  METHOD REGRA_2.

    SELECT *
     FROM /qaps/zmic1
     WHERE matnr = @us_header-matnr
     AND   werks = @us_header-werks
     AND   /qaps/grkey = @us_header-grkey
     AND   ( /qaps/gemng > 0 OR /qaps/eqmng > 0 )
     INTO TABLE @DATA(lt_data).


    LOOP AT it_pc INTO wa_pc.                         "06.04.2018
      CHECK wa_pc-flgut = ' '.

      READ TABLE lt_data INTO DATA(wa_restcomp) WITH KEY /qaps/comnr = wa_pc-comnr.
      IF sy-subrc = 0.
        IF wa_restcomp-/qaps/eqmng > 0 OR wa_restcomp-/qaps/gemng > 0.
          DATA lv_message TYPE string.
          MESSAGE e295(/qaps/zmi) WITH wa_pc-comnr INTO lv_message.
          RAISE EXCEPTION TYPE /qaps/cx_general
            EXPORTING
              message = VALUE #( type = 'E'
                                 message = lv_message ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD RESULTADO .

    DATA: wa_rest_comp_aux TYPE ty_rest_comp,
          wa_rest_comp     TYPE ty_rest_comp.

    DATA: lt_mimi TYPE TABLE OF /qaps/zmimi,
          ls_mimi TYPE /qaps/zmimi.

    DATA  l_tabix  TYPE sy-tabix.
    DATA ls_dif TYPE ty_dif.
    DATA: lt_zmicig2 TYPE TABLE OF /qaps/zmicig2
                        WITH KEY matnr werks /qaps/grkey /qaps/cignr,
          ls_zmicig2 TYPE /qaps/zmicig2.
    DESCRIBE TABLE it_zmicig0 LINES lines.

    IF lines GT 1.

      SELECT *
        INTO TABLE lt_mimi
        FROM /qaps/zmimi
        FOR ALL ENTRIES IN it_zmimi
        WHERE werks = it_zmimi-werks
        AND   matnr = it_zmimi-matnr.

      LOOP AT it_zmicig0 INTO wa_zmicig0.
        l_tabix  = sy-tabix.
        LOOP AT it_zmicig1 INTO wa_zmicig1 WHERE /qaps/cignr = wa_zmicig0-/qaps/cignr.
          CLEAR: xcmeng_min.
          READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.
          IF sy-subrc = 0.

            READ TABLE lt_mimi INTO ls_mimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.

            IF sy-subrc EQ 0.
              xcmeng_min = ls_mimi-/qaps/rmeng.
              IF wa_zmicig1-/qaps/cmeng <> 0 AND
                 wa_zmicig1-/qaps/cmeng < xcmeng_min.
                DELETE  it_zmicig0 INDEX l_tabix.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    DATA  l_ind  TYPE sy-tabix.

    DO.

      IF sy-index >= 25.

        IF lines( it_dif ) > 0.

          SORT it_dif BY xdif DESCENDING.

          READ TABLE it_dif INDEX 1 INTO ls_dif.

          RAISE EXCEPTION TYPE /qaps/cx_formulation_error
            EXPORTING
              message = 'Reformulação'
              chkey   = ls_dif-chkey
              divisor = zdiv.

        ELSE.
          lt_zmicig2[] = it_zmicig2[].
          DELETE lt_zmicig2 WHERE /qaps/chkey <> 'N' AND /qaps/chkey <> 'P' .

          SORT lt_zmicig2 BY /qaps/gmeng DESCENDING /qaps/chkey DESCENDING.

          READ TABLE lt_zmicig2 INDEX 1 INTO ls_zmicig2.

          RAISE EXCEPTION TYPE /qaps/cx_formulation_error
            EXPORTING
              message = 'Reformulação'
              chkey   = ls_zmicig2-/qaps/chkey.

        ENDIF.

      ENDIF.

      DESCRIBE TABLE it_zmicig0 LINES lines.

      IF lines GT 0.

        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
        xcombi = wa_zmicig0-/qaps/cignr.

        DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.

        CALL METHOD verifica_niveis_garantia_2.

        IF  it_dif[]  IS INITIAL.
          CALL METHOD zf_ajusta_garantia.
          EXIT.
        ENDIF.

        l_ind =  l_ind + 1.
        IF l_ind > 40.
          CALL METHOD zf_refaz_ate_imcompatib.
          CALL METHOD zf_verifica_tudo.
          IF  zdeusolucao NE c_x.
            CLEAR it_combi_final[].
            CLEAR it_zmicig0[].
            CLEAR it_zmicig1[].
            CLEAR it_zmicig2[].
            CLEAR it_zmimi[].
            EXIT.
          ENDIF.
        ENDIF.

        CLEAR it_rest_comp_2[].

        IF NOT it_dif[]  IS INITIAL.
          CALL METHOD ajusta_margem_seguranca_2.
          LOOP AT it_pc_bk INTO wa_pc_bk.
            l_tabix = sy-tabix.
            READ TABLE it_zmicig1 INTO wa_zmicig1 WITH KEY /qaps/comnr = wa_pc_bk-comnr.
            IF sy-subrc = 0 AND wa_zmicig1-/qaps/flgut EQ 'X'.
              wa_pc_bk-flgut =  'X'.
              MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
              CLEAR wa_rest_comp.
              wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
              wa_rest_comp-werks = /qaps/zmidfcig-werks.
              wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
              wa_rest_comp-/qaps/comnr = wa_pc_bk-comnr.
              READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_pc_bk-comnr.
              IF sy-subrc = 0.
                xcmeng_min = wa_zmimi-/qaps/rmeng.
                wa_rest_comp-/qaps/gemng = xcmeng_min.
                READ TABLE it_rest_comp_bk INTO wa_rest_comp_aux WITH KEY /qaps/comnr = wa_pc_bk-comnr.
                IF sy-subrc = 0.
                  APPEND wa_rest_comp TO it_rest_comp_2.
                ELSE.
                  APPEND wa_rest_comp TO it_rest_comp_2.
                ENDIF.
              ENDIF.
            ELSE.
              wa_pc_bk-flgut =  ' '.
              MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
            ENDIF.
          ENDLOOP.

          APPEND LINES OF it_rest_comp_bk TO it_rest_comp_2.
          CALL METHOD ajustar_limites_rest_comp CHANGING ct_data = it_rest_comp_2.


          DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
          DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
          DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
          DELETE it_combi_final WHERE combi = xcombi.

          CALL METHOD clear_workarea.
          CALL METHOD load_internal_tables.

          tp = -1.
          nv = wa_ct_comp.
          r1 = wa_ct_rest_le.
          r2 = wa_ct_rest_eq.
          r3 = wa_ct_rest_ge.
          x  = r1 + r2 + r3.
          a  = nv + 1.
          y  = x  + r3 + a.
          b  = y  - 1.
          c  = x  + 1.
          z  = a  - 1.

          CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.

          CALL METHOD load_le_restrictions.

          CALL METHOD load_eq_restrictions.

          CALL METHOD load_ge_restrictions.

          CALL METHOD load_cuts.

          CALL METHOD stage_590.
*
          CALL METHOD modifies_matrix.

          DO.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_1000_min.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_2000.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              CALL METHOD stage_3000.
            ENDIF.

            IF NOT wa_combi_final-msg IS INITIAL.
              EXIT.
            ENDIF.

          ENDDO.

        ENDIF.

      ELSE.
        EXIT.
      ENDIF.
    ENDDO.


    DATA l_lines_combi    TYPE sy-tabix.

    DESCRIBE TABLE it_combi_final LINES  l_lines_combi.

    IF NOT it_zmimi[] IS INITIAL.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
      xcombi = wa_zmicig0-/qaps/cignr.
      IF  sy-subrc = 0.
        CLEAR it_min[].
        CALL METHOD verifica_cond_minimo.
        IF NOT it_min[] IS INITIAL.
          IF l_lines_combi GT 1.
            DELETE it_combi_final WHERE combi = xcombi.
            it_combi_finalx[] =  it_combi_final[].
            READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
            READ TABLE it_zmicig0_min INTO wa_zmicig0 WITH KEY /qaps/cignr = wa_combi_finalx-combi.
            IF sy-subrc = 0.
              DELETE it_combi_final WHERE combi = xcombi.
              DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
              DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
              xcombi =  wa_combi_finalx-combi.
              xcombi_min =  wa_combi_finalx-combi.
              LOOP AT it_zmicig0_min INTO wa_zmicig0 WHERE /qaps/cignr = xcombi.
                APPEND  wa_zmicig0 TO it_zmicig0.
              ENDLOOP.
              LOOP AT it_zmicig1_min INTO wa_zmicig1 WHERE /qaps/cignr = xcombi.
                APPEND  wa_zmicig1 TO it_zmicig1.
              ENDLOOP.
              LOOP AT it_zmicig2_min INTO wa_zmicig2 WHERE  /qaps/cignr = xcombi.
                APPEND  wa_zmicig2 TO it_zmicig2.
              ENDLOOP.
              DELETE it_zmicig0 WHERE /qaps/costt = 0.
              SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
              READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
              IF  sy-subrc = 0.
                lt_zmicig2[] = it_zmicig2[].
                DELETE lt_zmicig2 WHERE /qaps/chkey <> 'N' AND /qaps/chkey <> 'P' .

                SORT lt_zmicig2 BY /qaps/gmeng DESCENDING /qaps/chkey DESCENDING.

                READ TABLE lt_zmicig2 INDEX 1 INTO ls_zmicig2.

                RAISE EXCEPTION TYPE /qaps/cx_formulation_error
                  EXPORTING
                    message = 'Reformulação'
                    chkey   = ls_zmicig2-/qaps/chkey.
              ENDIF.
            ELSE.
              DELETE it_zmicig0 WHERE /qaps/costt = 0.
              SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
              READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
              IF  sy-subrc = 0.
                CALL METHOD resultado_3 CHANGING cv_has_result = cv_has_result.

                IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT it_dif[]  IS INITIAL.
      IF NOT it_zmicig0_aux[]  IS INITIAL.
        it_zmicig0[] = it_zmicig0_aux[].
        it_zmicig1[] = it_zmicig1_aux[].
        it_zmicig2[] = it_zmicig2_aux[].
        CALL METHOD resultado_4 CHANGING cv_has_result = cv_has_result.

        IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
          RETURN.
        ENDIF.
      ELSE.
        zsemsolucao = 'X'.
        DATA lv_message_error TYPE string.
        MESSAGE e133(/qaps/zmi) INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
      ENDIF.
    ELSE.
      DESCRIBE TABLE it_zmicig0 LINES lines.

      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
      xcombi = wa_zmicig0-/qaps/cignr.

      DELETE it_zmicig0_min WHERE /qaps/cignr <> xcombi.
      DELETE it_zmicig1_min WHERE /qaps/cignr <> xcombi.
      DELETE it_zmicig2_min WHERE /qaps/cignr <> xcombi.
      DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
      DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
      DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.

      IF lines = 0.
        IF NOT it_zmicig0_aux[]  IS INITIAL.
          it_zmicig0[] = it_zmicig0_aux[].
          it_zmicig1[] = it_zmicig1_aux[].
          it_zmicig2[] = it_zmicig2_aux[].

          CALL METHOD resultado_3 CHANGING cv_has_result = cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ELSE.
          zsemsolucao = 'X'.
          MESSAGE e133(/qaps/zmi) INTO lv_message_error.
          RAISE EXCEPTION TYPE /qaps/cx_div_no_result
            EXPORTING
              message = lv_message_error.

        ENDIF.
      ELSE.
        CALL METHOD load_results_2.

      ENDIF.

      IF lines( it_combi_final ) > 0.
        CALL METHOD save_snapshot.
        cv_has_result = abap_true.
      ELSE.
        MESSAGE e133(/qaps/zmi) INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    " RESULTADO


  METHOD RESULTADO_3.
*
    DATA  l_tabix  TYPE sy-tabix.
*
    CLEAR it_zmi01[].
    SELECT * INTO TABLE it_zmi01 FROM /qaps/zmi01
     WHERE matnr = /qaps/zmidfcig-matnr AND
           werks = /qaps/zmidfcig-werks AND
           /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

    it_zmi01_bk[] = it_zmi01[].

    it_rs[] = it_rs_bk[].

    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
    IF  sy-subrc = 0.

      wa_combi_finalx-combi = wa_zmicig0-/qaps/cignr.

      CALL METHOD verifica_cond_minimo.
      IF  it_min[] IS INITIAL.
      ELSE.
        LOOP AT it_pc INTO wa_pc.
          wa_pc-flgut = ' '.
          MODIFY it_pc FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
        LOOP AT it_zmicig1 INTO wa_zmicig1.
          LOOP AT it_pc INTO wa_pc
            WHERE comnr =  wa_zmicig1-/qaps/comnr.
            wa_pc-flgut = 'X'.
            MODIFY it_pc FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDLOOP.
        CALL METHOD clear_workarea.
        CLEAR: it_pc_bk[], zajustou, xcombi.
        CLEAR: it_re[], it_zmi60[], it_zmi61[], it_zmi60_bk[], it_zmi61_bk[], it_combi_final[],
               it_zmicig0[], it_zmicig1[], it_zmicig2[],
               it_zmicig0_min[], it_zmicig1_min[], it_zmicig2_min[],
               it_combi_final[], it_combi_finalx[], it_min_bk[].
        REFRESH: it_re[],  it_zmi60[], it_zmi61[], it_zmi60_bk[], it_zmi61_bk[], it_combi_final[],
               it_zmicig0[], it_zmicig1[], it_zmicig2[],
               it_zmicig0_min[], it_zmicig1_min[], it_zmicig2_min[],
               it_combi_final[], it_combi_finalx[], it_min_bk[].
        LOOP AT it_pc INTO wa_pc WHERE flgut = 'X'.
          wa_re-comnr = wa_pc-comnr.
          wa_re-coktx = wa_pc-coktx.
          APPEND wa_re TO it_re.
        ENDLOOP.
        CLEAR it_min[].
        CALL METHOD load_matrix CHANGING cv_has_result = cv_has_result.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    " RESULTADO_3


  METHOD RESULTADO_4 .

    DATA: wa_rest_comp_aux TYPE ty_rest_comp,
          wa_rest_comp     TYPE ty_rest_comp.

    DATA  l_tabix  TYPE sy-tabix.

    SELECT * INTO TABLE it_zmimi
    FROM /qaps/zmimi
    WHERE werks = /qaps/zmidfcig-werks AND
          matnr IN r_matnr.

    LOOP AT it_zmimi INTO wa_zmimi.
      IF wa_zmimi-matnr  IN r_matnr.
      ELSE.
        DELETE it_zmimi INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    LOOP AT it_zmicig0 INTO wa_zmicig0.
      l_tabix  = sy-tabix.

*


      DATA  l_ind  TYPE sy-tabix.

      DO.

        DESCRIBE TABLE it_zmicig0 LINES lines.

        IF lines GT 0.

          DELETE it_zmicig0 WHERE /qaps/costt = 0.
          SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
          READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
          xcombi = wa_zmicig0-/qaps/cignr.

          DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
          DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
          DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.

          CALL METHOD verifica_niveis_garantia_2.

          IF  it_dif[]  IS INITIAL.
            EXIT.
          ENDIF.

          l_ind =  l_ind + 1.
          IF l_ind > 40.
            CALL METHOD zf_refaz_ate_imcompatib.
            CALL METHOD zf_verifica_tudo.
            IF  zdeusolucao NE c_x.
              CLEAR it_combi_final[].
              CLEAR it_zmicig0[].
              CLEAR it_zmicig1[].
              CLEAR it_zmicig2[].
              CLEAR it_zmimi[].
              EXIT.
            ENDIF.
          ENDIF.

          CLEAR it_rest_comp_2[].

          IF NOT it_dif[]  IS INITIAL.
            CALL METHOD ajusta_margem_seguranca_2.
            LOOP AT it_pc_bk INTO wa_pc_bk.
              l_tabix = sy-tabix.
              READ TABLE it_zmicig1 INTO wa_zmicig1 WITH KEY /qaps/comnr = wa_pc_bk-comnr.
              IF sy-subrc = 0 AND wa_zmicig1-/qaps/flgut EQ 'X'.
                wa_pc_bk-flgut =  'X'.
                MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
                CLEAR wa_rest_comp.
                wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
                wa_rest_comp-werks = /qaps/zmidfcig-werks.
                wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
                wa_rest_comp-/qaps/comnr = wa_pc_bk-comnr.
                READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_pc_bk-comnr.
                IF sy-subrc = 0.
                  xcmeng_min = wa_zmimi-/qaps/rmeng.
                  wa_rest_comp-/qaps/gemng = xcmeng_min.
                  READ TABLE it_rest_comp_bk INTO wa_rest_comp_aux WITH KEY /qaps/comnr = wa_pc_bk-comnr.
                  IF sy-subrc = 0.
                    IF wa_rest_comp_aux-/qaps/eqmng GE wa_rest_comp-/qaps/gemng OR
                       wa_rest_comp_aux-/qaps/gemng GE wa_rest_comp-/qaps/gemng.
                      APPEND wa_rest_comp TO it_rest_comp_2.
                    ELSE.
                      CLEAR: wa_rest_comp.
                    ENDIF.
                  ELSE.
                    APPEND wa_rest_comp TO it_rest_comp_2.
                  ENDIF.
                ENDIF.
              ELSE.
                wa_pc_bk-flgut =  ' '.
                MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
              ENDIF.
            ENDLOOP.

            DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
            DELETE it_combi_final WHERE combi = xcombi.

            CALL METHOD clear_workarea.
            CALL METHOD load_internal_tables.

            tp = -1.
            nv = wa_ct_comp.
            r1 = wa_ct_rest_le.
            r2 = wa_ct_rest_eq.
            r3 = wa_ct_rest_ge.
            x  = r1 + r2 + r3.
            a  = nv + 1.
            y  = x  + r3 + a.
            b  = y  - 1.
            c  = x  + 1.
            z  = a  - 1.

            CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.

            CALL METHOD load_le_restrictions.

            CALL METHOD load_eq_restrictions.

            CALL METHOD load_ge_restrictions.

            CALL METHOD load_cuts.

            CALL METHOD stage_590.
*
            CALL METHOD modifies_matrix.

            DO.

              IF wa_combi_final-msg IS INITIAL.
                CALL METHOD stage_1000_min.
              ENDIF.

              IF wa_combi_final-msg IS INITIAL.
                CALL METHOD stage_2000.
              ENDIF.

              IF wa_combi_final-msg IS INITIAL.
                CALL METHOD stage_3000.
              ENDIF.

              IF NOT wa_combi_final-msg IS INITIAL.
                EXIT.
              ENDIF.

            ENDDO.

          ENDIF.

        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      DATA l_lines_combi    TYPE sy-tabix.

      DESCRIBE TABLE it_combi_final LINES  l_lines_combi.

      IF NOT it_zmimi[] IS INITIAL.
        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
        xcombi = wa_zmicig0-/qaps/cignr.
        IF  sy-subrc = 0.
          CLEAR it_min[].
          CALL METHOD verifica_cond_minimo.
          IF NOT it_min[] IS INITIAL.
            IF l_lines_combi GT 1.
              DELETE it_combi_final WHERE combi = xcombi.
              it_combi_finalx[] =  it_combi_final[].
              READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
              READ TABLE it_zmicig0_min INTO wa_zmicig0 WITH KEY /qaps/cignr = wa_combi_finalx-combi.
              IF sy-subrc = 0.
                DELETE it_combi_final WHERE combi = xcombi.
                DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
                DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
                DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
                xcombi =  wa_combi_finalx-combi.
                xcombi_min =  wa_combi_finalx-combi.
                LOOP AT it_zmicig0_min INTO wa_zmicig0 WHERE /qaps/cignr = xcombi.
                  APPEND  wa_zmicig0 TO it_zmicig0.
                ENDLOOP.
                LOOP AT it_zmicig1_min INTO wa_zmicig1 WHERE /qaps/cignr = xcombi.
                  APPEND  wa_zmicig1 TO it_zmicig1.
                ENDLOOP.
                LOOP AT it_zmicig2_min INTO wa_zmicig2 WHERE  /qaps/cignr = xcombi.
                  APPEND  wa_zmicig2 TO it_zmicig2.
                ENDLOOP.
                DELETE it_zmicig0 WHERE /qaps/costt = 0.
                SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
                READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
                IF  sy-subrc = 0.
                  CALL METHOD resultado_3 CHANGING cv_has_result = cv_has_result.

                  IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                    RETURN.
                  ENDIF.  "07.03.2012 fim
                ENDIF.
              ELSE.
                DELETE it_zmicig0 WHERE /qaps/costt = 0.
                SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
                READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
                IF  sy-subrc = 0.
                  CALL METHOD resultado_3 CHANGING cv_has_result = cv_has_result.

                  IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                    RETURN.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT it_dif[]  IS INITIAL.
        zsemsolucao = 'X'.
        DATA lv_message_error TYPE string.
        MESSAGE e133(/qaps/zmi) INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
      ELSE.
        DESCRIBE TABLE it_zmicig0 LINES lines.

        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
        xcombi = wa_zmicig0-/qaps/cignr.

        DELETE it_zmicig0_min WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig1_min WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig2_min WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.

        IF lines = 0.
          zsemsolucao = 'X'.
          MESSAGE e133(/qaps/zmi) INTO lv_message_error.
          RAISE EXCEPTION TYPE /qaps/cx_div_no_result
            EXPORTING
              message = lv_message_error.
        ELSE.
          CALL METHOD load_results_2.
        ENDIF.

        CALL METHOD save_snapshot.
        cv_has_result = abap_true.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    " RESULTADO_4


  METHOD SAVE_SNAPSHOT.

    DATA: lt_callstack TYPE  abap_callstack,
          lv_path      TYPE string,
          lv_to        TYPE sy-tabix.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = lt_callstack.

    READ TABLE lt_callstack WITH KEY blockname = 'PRODUCT_FORMULATE'
                            TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      lv_to = sy-tabix.
    ELSE.
      lv_to = 5.
    ENDIF.

    LOOP AT lt_callstack INTO DATA(ls_callstack) FROM 2 TO 5.
      IF sy-tabix = 2.
        lv_path = ls_callstack-blockname && `[` && ls_callstack-line && `]`.
      ELSE.
        lv_path = ls_callstack-blockname && `[` && ls_callstack-line && `]` && ` -> ` && lv_path.
      ENDIF.
    ENDLOOP.

    APPEND INITIAL LINE TO gt_snapshot ASSIGNING FIELD-SYMBOL(<fs_snapshot>).

    SORT it_combi_final BY costt ASCENDING.
    DATA(ls_combi_final) = it_combi_final[ 1 ].

    <fs_snapshot>-combi_final = wa_combi_final.
    <fs_snapshot>-zdiv        = zdiv.
    <fs_snapshot>-zmidfcig    = /qaps/zmidfcig.
    <fs_snapshot>-t_re        = it_re.
    <fs_snapshot>-t_ng        = it_ng.
    <fs_snapshot>-t_pc_0100   = it_pc_0100.
    <fs_snapshot>-t_zmicig0   = it_zmicig0.
    <fs_snapshot>-t_zmicig1   = it_zmicig1.
    <fs_snapshot>-t_zmicig2   = it_zmicig2.
    <fs_snapshot>-t_rest_comp = it_rest_comp.
    <fs_snapshot>-t_matrix    = it_ma.
    <fs_snapshot>-path        = lv_path.
    <fs_snapshot>-reformulate = gv_reformulate.

    SORT <fs_snapshot>-t_matrix BY line col.

    DATA lv_content TYPE string.
    DATA(lt_lines) = it_ma.
    DATA lv_color TYPE string.

*
*
*
*
*
*
*
*
*
*
*



  ENDMETHOD.


  METHOD SAVE_SOLUCTIONS.

    DATA wa_ng    TYPE ty_ng.

    /qaps/zmicig0-mandt  = sy-mandt.
    /qaps/zmicig0-matnr  = /qaps/zmidfcig-matnr.
    /qaps/zmicig0-werks  = /qaps/zmidfcig-werks.
    /qaps/zmicig0-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
    /qaps/zmicig0-/qaps/cignr  = xcombi.
    /qaps/zmicig0-/qaps/descr  = /qaps/zmidfcig-/qaps/descr.
    /qaps/zmicig0-/qaps/period = /qaps/zmidfcig-/qaps/period.
    /qaps/zmicig0-/qaps/descrv  = /qaps/zmidfcig-/qaps/descrv.
    /qaps/zmicig0-/qaps/versao = /qaps/zmidfcig-/qaps/versao.
    /qaps/zmicig0-/qaps/rmeng = /qaps/zmidfcig-/qaps/rmeng.
    /qaps/zmicig0-/qaps/rmuni = /qaps/zmidfcig-/qaps/rmuni.
    /qaps/zmidfcig-/qaps/dtcre = sy-datum.
    /qaps/zmidfcig-/qaps/uscre = sy-uname.
    /qaps/zmicig0-/qaps/dtcre = /qaps/zmidfcig-/qaps/dtcre.
    /qaps/zmicig0-/qaps/uscre = /qaps/zmidfcig-/qaps/uscre.
    /qaps/zmicig0-/qaps/flpxg = /qaps/zmidfcig-/qaps/flpxg.
    /qaps/zmicig0-/qaps/costt = /qaps/zmidfcig-/qaps/costt.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/mcost = /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/gerenc = /qaps/zmidfcig-/qaps/gerenc.

    /qaps/zmicig0-/qaps/mcost   = /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmicig0-/qaps/contab  =  /qaps/zmidfcig-/qaps/contab.
    /qaps/zmicig0-/qaps/gerenc  = /qaps/zmidfcig-/qaps/gerenc.
    /qaps/zmicig0-/qaps/reposi  =  /qaps/zmidfcig-/qaps/reposi.
    /qaps/zmicig0-/qaps/gerxprio =   /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/mcost    =   /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/mcost    = /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmicig0-/qaps/tot_markup = /qaps/zmidfcig-/qaps/tot_markup.
    /qaps/zmicig0-/qaps/tot_ajumer = /qaps/zmidfcig-/qaps/tot_ajumer.

    IF sy-binpt  = 'X'.
      /qaps/zmicig0-/qaps/batchfl =  sy-binpt.
      /qaps/zmicig0-/qaps/descr =  'PROCESSO VIA BATCH'.
      /qaps/zmidfcig-/qaps/descr = 'PROCESSO VIA BATCH'.
    ENDIF.

    /qaps/zmicig0-/qaps/numqual = v_numqual.
    /qaps/zmicig0-/qaps/versaopq = lversao.
    APPEND /qaps/zmicig0 TO it_zmicig0.
    LOOP AT it_pc_0100 INTO wa_pc.

      wa_zmicig1-mandt  = sy-mandt.
      wa_zmicig1-matnr  = /qaps/zmidfcig-matnr.
      wa_zmicig1-werks  = /qaps/zmidfcig-werks.
      wa_zmicig1-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
      wa_zmicig1-/qaps/cignr  = xcombi.
      wa_zmicig1-/qaps/flgut  = wa_pc-flgut.
      wa_zmicig1-/qaps/comnr  = wa_pc-comnr.

      READ TABLE it_re INTO wa_re WITH KEY
                                 comnr = wa_pc-comnr.
      IF sy-subrc = 0.
        wa_zmicig1-/qaps/cmeng = wa_re-cmeng.
        wa_zmicig1-/qaps/cmuni = wa_re-cmuni.
      ELSE.
        CLEAR: wa_zmicig1-/qaps/cmeng, wa_zmicig1-/qaps/cmuni.
      ENDIF.

      IF  wa_zmicig1-/qaps/cmeng <> 0.
        APPEND wa_zmicig1 TO it_zmicig1.
      ENDIF.

    ENDLOOP.

    LOOP AT it_ng INTO wa_ng.
      wa_zmicig2-mandt  = sy-mandt.
      wa_zmicig2-matnr  = /qaps/zmidfcig-matnr.
      wa_zmicig2-werks  = /qaps/zmidfcig-werks.
      wa_zmicig2-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
      wa_zmicig2-/qaps/cignr  = xcombi.
      wa_zmicig2-/qaps/chkey = wa_ng-chkey.
      wa_zmicig2-/qaps/cmeng = wa_ng-cmeng.
      wa_zmicig2-/qaps/cmuni = wa_ng-cmuni.

      CLEAR: wa_zmicig2-/qaps/emeng,
             wa_zmicig2-/qaps/gmeng,
             wa_zmicig2-/qaps/lmeng.

      IF wa_ng-flbeg = '='.
        wa_zmicig2-/qaps/emeng = wa_ng-gmeng.
      ELSE.

        IF wa_ng-flbeg = '>='.
          wa_zmicig2-/qaps/gmeng = wa_ng-gmeng.
        ENDIF.

        IF wa_ng-flend = '<='.
          wa_zmicig2-/qaps/lmeng = wa_ng-lmeng.
        ENDIF.

      ENDIF.

      APPEND wa_zmicig2 TO it_zmicig2.

    ENDLOOP.

  ENDMETHOD.                    " SAVE_SOLUCTIONS


  METHOD SAVE_SOLUCTIONS_MIN.

    DATA wa_ng    TYPE ty_ng.

    CLEAR: wa_min-comnr, xcond_min.

    LOOP AT it_pc_0100 INTO wa_pc.

      wa_zmicig1-mandt  = sy-mandt.
      wa_zmicig1-matnr  = /qaps/zmidfcig-matnr.
      wa_zmicig1-werks  = /qaps/zmidfcig-werks.
      wa_zmicig1-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
      wa_zmicig1-/qaps/cignr  = xcombi.
      wa_zmicig1-/qaps/flgut  = wa_pc-flgut.
      wa_zmicig1-/qaps/comnr  = wa_pc-comnr.

      READ TABLE it_re_re INTO wa_re WITH KEY
                                 comnr = wa_pc-comnr.

      IF sy-subrc = 0.
        wa_zmicig1-/qaps/cmeng = wa_re-cmeng.
        wa_zmicig1-/qaps/cmuni = wa_re-cmuni.
      ELSE.
        CLEAR: wa_zmicig1-/qaps/cmeng, wa_zmicig1-/qaps/cmuni.
      ENDIF.

      IF  wa_zmicig1-/qaps/cmeng <> 0.
        APPEND wa_zmicig1 TO it_zmicig1.
        APPEND wa_zmicig1 TO it_zmicig1_min.

      ENDIF.

    ENDLOOP.

    /qaps/zmicig0-mandt  = sy-mandt.
    /qaps/zmicig0-matnr  = /qaps/zmidfcig-matnr.
    /qaps/zmicig0-werks  = /qaps/zmidfcig-werks.
    /qaps/zmicig0-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
    /qaps/zmicig0-/qaps/cignr = xcombi.
    /qaps/zmicig0-/qaps/descr  = /qaps/zmidfcig-/qaps/descr.
    /qaps/zmicig0-/qaps/period = /qaps/zmidfcig-/qaps/period.
    /qaps/zmicig0-/qaps/versao = /qaps/zmidfcig-/qaps/versao.
    /qaps/zmicig0-/qaps/descrv = /qaps/zmidfcig-/qaps/descrv.
    /qaps/zmicig0-/qaps/rmeng = /qaps/zmidfcig-/qaps/rmeng.
    /qaps/zmicig0-/qaps/rmuni = /qaps/zmidfcig-/qaps/rmuni.
    /qaps/zmidfcig-/qaps/dtcre = sy-datum.
    /qaps/zmidfcig-/qaps/uscre = sy-uname.
    /qaps/zmicig0-/qaps/dtcre = /qaps/zmidfcig-/qaps/dtcre.
    /qaps/zmicig0-/qaps/uscre = /qaps/zmidfcig-/qaps/uscre.
    /qaps/zmicig0-/qaps/flpxg = /qaps/zmidfcig-/qaps/flpxg.
    /qaps/zmicig0-/qaps/costt = /qaps/zmidfcig-/qaps/costt.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.

    /qaps/zmicig0-/qaps/mcost = /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/mcost = /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmicig0-/qaps/contab =  /qaps/zmidfcig-/qaps/contab.
    /qaps/zmicig0-/qaps/gerenc = /qaps/zmidfcig-/qaps/gerenc.
    /qaps/zmicig0-/qaps/reposi = /qaps/zmidfcig-/qaps/reposi.
    /qaps/zmicig0-/qaps/gerxprio =  /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/mcost =  /qaps/zmidfcig-/qaps/mcost.
    /qaps/zmicig0-/qaps/gerxprio = /qaps/zmidfcig-/qaps/gerxprio.
    /qaps/zmicig0-/qaps/tot_ajumer = /qaps/zmidfcig-/qaps/tot_ajumer.
    /qaps/zmicig0-/qaps/tot_markup = /qaps/zmidfcig-/qaps/tot_markup.
    /qaps/zmicig0-/qaps/mcost = /qaps/zmidfcig-/qaps/mcost.

    IF sy-binpt  = 'X'.
      /qaps/zmicig0-/qaps/batchfl =  sy-binpt.
      /qaps/zmicig0-/qaps/descr =  'PROCESSO VIA BATCH'.
      /qaps/zmidfcig-/qaps/descr = 'PROCESSO VIA BATCH'.
    ENDIF.

    /qaps/zmicig0-/qaps/numqual = v_numqual.
    /qaps/zmicig0-/qaps/versaopq = lversao.
    APPEND /qaps/zmicig0 TO it_zmicig0_min.
    APPEND /qaps/zmicig0 TO it_zmicig0.


    LOOP AT it_ng INTO wa_ng.
      wa_zmicig2-mandt  = sy-mandt.
      wa_zmicig2-matnr  = /qaps/zmidfcig-matnr.
      wa_zmicig2-werks  = /qaps/zmidfcig-werks.
      wa_zmicig2-/qaps/grkey  = /qaps/zmidfcig-/qaps/grkey.
      wa_zmicig2-/qaps/cignr = xcombi.
      wa_zmicig2-/qaps/chkey = wa_ng-chkey.
      wa_zmicig2-/qaps/cmeng = wa_ng-cmeng.
      wa_zmicig2-/qaps/cmuni = wa_ng-cmuni.

      CLEAR: wa_zmicig2-/qaps/emeng,
             wa_zmicig2-/qaps/gmeng,
             wa_zmicig2-/qaps/lmeng.

      IF wa_ng-flbeg = '='.
        wa_zmicig2-/qaps/emeng = wa_ng-gmeng.
      ELSE.

        IF wa_ng-flbeg = '>='.
          wa_zmicig2-/qaps/gmeng = wa_ng-gmeng.
        ENDIF.

        IF wa_ng-flend = '<='.
          wa_zmicig2-/qaps/lmeng = wa_ng-lmeng.
        ENDIF.

      ENDIF.

      APPEND wa_zmicig2 TO it_zmicig2.
      APPEND wa_zmicig2 TO it_zmicig2_min.

    ENDLOOP.

    SORT it_zmicig0 BY matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.
    SORT it_zmicig0_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.

    SORT it_zmicig1 BY matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.
    SORT it_zmicig1_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.

    SORT it_zmicig2 BY matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.
    SORT it_zmicig2_min BY matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.

    DELETE ADJACENT DUPLICATES FROM it_zmicig0
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.
    DELETE  ADJACENT DUPLICATES FROM it_zmicig0_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/period /qaps/versao.

    DELETE  ADJACENT DUPLICATES FROM  it_zmicig1
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.
    DELETE ADJACENT DUPLICATES FROM it_zmicig1_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/comnr.

    DELETE  ADJACENT DUPLICATES FROM it_zmicig2
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.
    DELETE  ADJACENT DUPLICATES FROM it_zmicig2_min
    COMPARING matnr werks /qaps/grkey /qaps/cignr /qaps/chkey.

  ENDMETHOD.                    " SAVE_SOLUCTIONS_MIN


  METHOD SET_MATRIX_A.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = p_line
                                               col   = p_col.
    IF sy-subrc = 0.
      wa_ma-valor = p_valor.
      MODIFY TABLE it_ma FROM wa_ma.
    ELSE.
      MESSAGE e000(/qaps/zmi) WITH TEXT-m01.
    ENDIF.

  ENDMETHOD.                               " SET_MATRIX_A


  METHOD STAGE_1000.

    DATA wa_incompat1     TYPE /qaps/zmi61.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                               col = 1.
    IF sy-subrc = 0.
      valork = wa_ma-valor.
    ELSE.
      valork = 0.
    ENDIF.

    cp = 1.
    n  = 2.

    WHILE n <= b.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                                 col = n.
      IF sy-subrc <> 0.
        wa_ma-valor  = 0.
      ENDIF.

      valor1 = wa_ma-valor.

      IF  valor1 < valork.
        CALL METHOD stage_1080.
      ENDIF.

      n = n + 1.

    ENDWHILE.

    IF valork >=  valor_0001.                                 "LINE 1060
      CALL METHOD step_4000.

      LOOP AT it_re INTO wa_re.
        IF   wa_re-cmeng = 0.
          DELETE it_re INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
*
      REFRESH: it_re_bk.
      it_re_bk = it_re.

      LOOP AT it_re INTO wa_re.

        REFRESH: it_incompat1.

        SELECT * INTO TABLE it_incompat1
        FROM /qaps/zmi61
        WHERE   matnr = wa_re-comnr AND
                werks = /qaps/zmidfcig-werks.

        IF sy-subrc = 0.
          LOOP AT it_incompat1 INTO wa_incompat1.
            READ TABLE it_re_bk INTO wa_re_bk
                  WITH KEY comnr = wa_incompat1-/qaps/comnr.
            IF sy-subrc = 0.
              IF xcombi_aux IS INITIAL.
                LOOP AT it_combi_final INTO wa_combi_final
                  WHERE combi = xcombi.
                  IF     wa_combi_final-msg IS INITIAL.
                    IF xcombi = '0000999995' OR
                       xcombi = '999996    ' OR
                       xcombi = '999997    ' OR
                       xcombi = '999998    ' OR
                       xcombi = '999999    '.
                      wa_combi_final-costt = ze.
                    ELSE.
                      wa_combi_final-costt = 0.
                    ENDIF.
                    wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
                    wa_combi_final-it_re = it_re.
                    MODIFY it_combi_final FROM wa_combi_final.
                  ENDIF.
                ENDLOOP.
              ELSE.
                wa_combi_final-combi = xcombi.
                IF xcombi = '0000999995' OR
                   xcombi = '999996    ' OR
                   xcombi = '999997    ' OR
                   xcombi = '999998    ' OR
                   xcombi = '999999    '.
                  wa_combi_final-costt = ze.
                ELSE.
                  wa_combi_final-costt = 0.
                ENDIF.
                wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
                APPEND wa_combi_final TO it_combi_final.
                CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
                CLEAR: xcombi_aux.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDLOOP.

      /qaps/zmidfcig-/qaps/costt = ze.
      /qaps/zmidfcig-/qaps/contab = zcontab.
      /qaps/zmidfcig-/qaps/gerenc = zgerenc.
      /qaps/zmidfcig-/qaps/reposi = zreposi.
      /qaps/zmidfcig-/qaps/gerxprio = zgerxprio.
      /qaps/zmidfcig-/qaps/tot_markup = ztotmkp.
      /qaps/zmidfcig-/qaps/tot_ajumer = ztotajmkt.
      /qaps/zmidfcig-/qaps/mcost = zmcost.

      IF  wa_combi_final-msg IS INITIAL.
        LOOP AT it_combi_final INTO wa_combi_final
          WHERE combi = xcombi.
          IF   wa_combi_final-msg IS INITIAL.
            wa_combi_final-combi = xcombi.
            wa_combi_final-costt = /qaps/zmidfcig-/qaps/costt.
            wa_combi_final-msg = 'SOLUÇÃO ÒTIMA'.
            wa_combi_final-it_re = it_re.
            MODIFY it_combi_final FROM wa_combi_final.
          ENDIF.
        ENDLOOP.
        CALL METHOD guarantee_levels.
        CALL METHOD save_soluctions.
      ENDIF.

      IF /qaps/zmidfcig-/qaps/rmeng = 0.
        /qaps/zmidfcig-/qaps/costu = 0.
      ELSE.
        /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
      ENDIF.

    ENDIF.

  ENDMETHOD.                               " STAGE_1000_MIN


  METHOD STAGE_1000_MIN.

    DATA wa_incompat1     TYPE /qaps/zmi61.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                               col = 1.
    IF sy-subrc = 0.
      valork = wa_ma-valor.
    ELSE.
      valork = 0.
    ENDIF.

    cp = 1.
    n  = 2.

    WHILE n <= b.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                                 col = n.
      IF sy-subrc <> 0.
        wa_ma-valor  = 0.
      ENDIF.

      valor1 = wa_ma-valor.

      IF  valor1 < valork.
        CALL METHOD stage_1080.
      ENDIF.

      n = n + 1.

    ENDWHILE.

    IF valork >=  valor_0001.                                 "LINE 1060
      CALL METHOD step_4000.

      LOOP AT it_re INTO wa_re.
        IF  wa_re-cmeng = 0.
          DELETE it_re INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
*
      REFRESH: it_re_bk.
      it_re_bk = it_re.

      SELECT * "INTO TABLE it_incompat1
        FROM /qaps/zmi61
        FOR ALL ENTRIES IN @it_re
        WHERE   matnr = @it_re-comnr AND
                werks = @/qaps/zmidfcig-werks
        INTO TABLE @DATA(lt_incompat1).


      LOOP AT it_re INTO wa_re.

        REFRESH: it_incompat1.


        it_incompat1 = lt_incompat1.
        DELETE it_incompat1 WHERE matnr <> wa_re-comnr.

        IF sy-subrc = 0.
          LOOP AT it_incompat1 INTO wa_incompat1.
            READ TABLE it_re_bk INTO wa_re_bk
                  WITH KEY comnr = wa_incompat1-/qaps/comnr.
            IF sy-subrc = 0.
              IF xcombi_aux IS INITIAL.
                LOOP AT it_combi_final INTO wa_combi_final
                  WHERE combi = xcombi.
                  IF     wa_combi_final-msg IS INITIAL.
                    IF xcombi = '0000999995' OR
                       xcombi = '999996    ' OR
                       xcombi = '999997    ' OR
                       xcombi = '999998    ' OR
                       xcombi = '999999    '.
                      wa_combi_final-costt = ze.
                    ELSE.
                      wa_combi_final-costt = 0.
                    ENDIF.
                    wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
                    MODIFY it_combi_final FROM wa_combi_final.
                  ENDIF.
                ENDLOOP.
              ELSE.
                wa_combi_final-combi = xcombi.
                IF xcombi = '0000999995' OR
                   xcombi = '999996    ' OR
                   xcombi = '999997    ' OR
                   xcombi = '999998    ' OR
                   xcombi = '999999    '.
                  wa_combi_final-costt = ze.
                ELSE.
                  wa_combi_final-costt = 0.
                ENDIF.
                wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
                wa_combi_final-it_re = it_re.
                APPEND wa_combi_final TO it_combi_final.
                CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      /qaps/zmidfcig-/qaps/costt = ze.
      /qaps/zmidfcig-/qaps/contab = zcontab.
      /qaps/zmidfcig-/qaps/gerenc = zgerenc.
      /qaps/zmidfcig-/qaps/reposi = zreposi.
      /qaps/zmidfcig-/qaps/gerxprio = zgerxprio.
      /qaps/zmidfcig-/qaps/tot_markup = ztotmkp.
      /qaps/zmidfcig-/qaps/tot_ajumer = ztotajmkt.
      /qaps/zmidfcig-/qaps/mcost = zmcost.

      IF  wa_combi_final-msg IS INITIAL.

        wa_combi_final-combi = xcombi.
        wa_combi_final-costt = /qaps/zmidfcig-/qaps/costt.
        wa_combi_final-it_re = it_re.
        wa_combi_final-msg = 'SOLUÇÃO ÒTIMA'.
        wa_combi_final-it_re = it_re.
        APPEND wa_combi_final TO it_combi_final.
        CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
        CALL METHOD guarantee_levels.
        CALL METHOD save_soluctions.

      ENDIF.

      IF /qaps/zmidfcig-/qaps/rmeng = 0.
        /qaps/zmidfcig-/qaps/costu = 0.
      ELSE.
        /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
      ENDIF.

    ENDIF.

  ENDMETHOD.                               " STAGE_1000


  METHOD STAGE_1080.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                               col = n.
    IF sy-subrc = 0.
      valork = wa_ma-valor.
    ELSE.
      valork = 0.
    ENDIF.

    cp = n.

  ENDMETHOD.                               " STAGE_1080


  METHOD STAGE_2000.

    k = 1.

    WHILE k <= x.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                                 col = cp.
      IF sy-subrc <> 0.
        wa_ma-valor  = 0.
      ENDIF.

      IF wa_ma-valor <= valor_0001p.

        CALL METHOD step_2160.

      ELSE.


        CLEAR: valor1, valor2, valor3.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                                   col = y.
        IF sy-subrc <> 0.
          CALL METHOD set_matrix_a EXPORTING p_line = k p_col = y p_valor = 0.
        ENDIF.

        valor1 = wa_ma-valor.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                                   col = cp.
        IF sy-subrc <> 0.
          CALL METHOD set_matrix_a EXPORTING p_line = k p_col = cp p_valor = 0.
        ENDIF.

        valor2 = wa_ma-valor.
        valor3 = valor1 / valor2.

        READ TABLE it_vb INTO wa_vb WITH KEY chave = k.

        tabix = sy-tabix.

        IF sy-subrc = 0.
          wa_vb-valor = valor3.
          MODIFY it_vb FROM wa_vb INDEX tabix.
        ELSE.
          wa_vb-chave = k.
          wa_vb-valor = valor3.
          APPEND wa_vb TO it_vb.
        ENDIF.

      ENDIF.

      k = k + 1.

    ENDWHILE.


    READ TABLE it_vb INTO wa_vb WITH KEY chave = 1.

    IF sy-subrc <> 0.
      wa_vb-chave = 1.
      wa_vb-valor = 0.
      APPEND wa_vb TO it_vb.
    ENDIF.

    j  = wa_vb-valor.
    lp = 1.
    n  = 2.

    WHILE n <= x.

      READ TABLE it_vb INTO wa_vb WITH KEY chave = n.

      IF sy-subrc <> 0.
        wa_vb-chave = n.
        wa_vb-valor = 0.
        APPEND wa_vb TO it_vb.
      ENDIF.

      IF wa_vb-valor < j.
        CALL METHOD stage_2180.
      ENDIF.

      n = n + 1.

    ENDWHILE.


    IF j = exp1.

      CALL METHOD step_5010.

    ELSE.

      n = 1.

      WHILE n <= b.

        READ TABLE it_vc INTO wa_vc WITH KEY chave = n.

        IF sy-subrc <> 0.
          wa_vc-chave = n.
          wa_vc-valor = 0.
          APPEND wa_vc TO it_vc.
        ENDIF.

        IF wa_vc-valor = lp.
          wa_vc-valor = 0.
          MODIFY it_vc FROM wa_vc INDEX sy-tabix.
        ENDIF.

        IF n = cp.
          wa_vc-valor = lp.
          MODIFY it_vc FROM wa_vc INDEX sy-tabix.
        ENDIF.

        n = n + 1.

      ENDWHILE.

    ENDIF.

  ENDMETHOD.                               " STAGE_2000


  METHOD STAGE_2180.

    READ TABLE it_vb INTO wa_vb WITH KEY chave = n.

    IF sy-subrc <> 0.
      wa_vb-chave = n.
      wa_vb-valor = 0.
      APPEND wa_vb TO it_vb.
    ENDIF.

    j  = wa_vb-valor.
    lp = n.

  ENDMETHOD.                               " STAGE_2180


  METHOD STAGE_3000.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                               col = cp.
    IF sy-subrc = 0.
      pv = wa_ma-valor.
    ELSE.
      pv = 0.
    ENDIF.

    IF pv = 1.

      CALL METHOD step_3060.

    ELSE.

      n = 1.

      WHILE n <= y.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                                   col = n.
        IF sy-subrc <> 0.
          CALL METHOD set_matrix_a EXPORTING p_line = lp p_col = n p_valor = 0.
        ENDIF.

        valor1 = wa_ma-valor.
        TRY.
            valor2 = valor1 / pv.
          CATCH cx_sy_zerodivide.
        ENDTRY.
        CALL METHOD set_matrix_a EXPORTING p_line = lp p_col = n p_valor = valor2.

        n = n + 1.

      ENDWHILE.

      CALL METHOD step_3060.

    ENDIF.

  ENDMETHOD.                               " STAGE_3000


  METHOD STAGE_5050.

    var1 = r2 + r3.

    IF var1 <> 0.

      n = y - r2 - r3 .

      WHILE n <= b.

        READ TABLE it_vc INTO wa_vc WITH KEY chave = n.

        IF sy-subrc <> 0.
          wa_vc-chave = n.
          wa_vc-valor = 0.
          APPEND wa_vc TO it_vc.
        ENDIF.

        IF wa_vc-valor <> 0.

          READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = wa_vc-valor
                                                     col = y.
          IF sy-subrc <> 0.
            CALL METHOD set_matrix_a EXPORTING p_line = wa_vc-valor p_col = y p_valor = 0.
          ENDIF.

          valor1 = wa_ma-valor.

          IF valor1 > valor_0001p.
            IF xcombi_aux IS INITIAL.
              LOOP AT it_combi_final INTO wa_combi_final
                WHERE combi = xcombi.
                wa_combi_final-costt = 0.
                wa_combi_final-msg = TEXT-s99.
                MODIFY it_combi_final FROM wa_combi_final.
              ENDLOOP.
            ELSE.
              wa_combi_final-combi = xcombi.
              wa_combi_final-costt = 0.
              wa_combi_final-msg = TEXT-s99.
              APPEND  wa_combi_final TO it_combi_final.
              CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
              CLEAR: xcombi_aux.
            ENDIF.
          ENDIF.

        ENDIF.

        n = n + 1.

      ENDWHILE.

    ENDIF.

  ENDMETHOD.                               " STAGE_5050


  METHOD STAGE_5130.

    d = 0.
    n = 1.

    DO b TIMES.

      IF n > b.
        EXIT.
      ENDIF.

      READ TABLE it_vc INTO wa_vc WITH KEY chave = n.

      IF sy-subrc <> 0.
        wa_vc-chave = n.
        wa_vc-valor = 0.
        APPEND wa_vc TO it_vc.
      ENDIF.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                                 col = n.
      IF sy-subrc <> 0.
        CALL METHOD set_matrix_a EXPORTING p_line = c p_col = y p_valor = 0.
      ENDIF.

      IF wa_vc-valor = 0 AND wa_ma-valor = 0.
        d = 1.
        n = n + 1.
        EXIT.
      ENDIF.

      n = n + 1.

    ENDDO.

    IF d = 1.
      MESSAGE e134(/qaps/zmi).
    ENDIF.

  ENDMETHOD.                               " STAGE_5130


  METHOD STAGE_590.

    n = 1.

    WHILE n <= c.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                 col = a.  " --> K
      IF sy-subrc = 0.
        valor1 = wa_ma-valor.
        IF n <> c AND valor1 < 0.
          MESSAGE e131(/qaps/zmi).
        ENDIF.
      ENDIF.

      CALL METHOD set_matrix_a EXPORTING p_line = n p_col = y p_valor = valor1.
      CALL METHOD set_matrix_a EXPORTING p_line = n p_col = a p_valor = 0.

      n = n + 1.

    ENDWHILE.

  ENDMETHOD.                                                    " STAGE_590


  METHOD STAGE_650.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                               col = k.
    IF sy-subrc = 0.
      var1 = wa_ma-valor.
    ELSE.
      var1 = 0.
    ENDIF.

    CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = -1.

    k = k + 1.

  ENDMETHOD.                                                    " STAGE_650


  METHOD STEP_2160.

    READ TABLE it_vb INTO wa_vb WITH KEY chave = k.

    tabix = sy-tabix.

    IF sy-subrc <> 0.
      wa_vb-chave = k.
      wa_vb-valor =  exp1.
      APPEND wa_vb TO it_vb.
    ELSE.
      wa_vb-valor = exp1.
      MODIFY it_vb FROM wa_vb INDEX tabix.
    ENDIF.

  ENDMETHOD.                                                    " STEP_2160


  METHOD STEP_3060.

    n = 1.

    WHILE n <= c.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                 col = cp.
      IF sy-subrc <> 0.
        CALL METHOD set_matrix_a EXPORTING p_line = n p_col = cp p_valor = 0.
      ENDIF.

      IF n = lp OR wa_ma-valor = 0.
*
      ELSE.

        p = wa_ma-valor * -1.
        k = 1.

        WHILE k <= y.

          READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                     col = k.
          IF sy-subrc <> 0.
            CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = 0.
          ENDIF.
          valor1 = wa_ma-valor.

          READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                                     col = k.
          IF sy-subrc <> 0.
            CALL METHOD set_matrix_a EXPORTING p_line = lp p_col = k p_valor = 0.
          ENDIF.


          valor2 = wa_ma-valor.
          valor3 = valor1 + valor2 * p.

          READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                     col = k.
          IF sy-subrc = 0.
            CALL METHOD set_matrix_a EXPORTING p_line = n p_col = k p_valor = valor3.
          ENDIF.

          k = k + 1.

        ENDWHILE.

      ENDIF.

      n = n + 1.

    ENDWHILE.

  ENDMETHOD.                                                    " STEP_3060


  METHOD STEP_4000.

    CALL METHOD stage_5050.

    n = 1.

    DO nv TIMES.

      IF n > nv.
        EXIT.
      ENDIF.

      var1 = 0.

      READ TABLE it_vc INTO wa_vc WITH KEY chave = n.

      IF sy-subrc <> 0.
        wa_vc-chave = n.
        wa_vc-valor = 0.
        APPEND wa_vc TO it_vc.
      ENDIF.

      var1 = wa_vc-valor.

      IF var1 <> 0.

        CLEAR: valor1, valor2, valor3.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = var1
                                                   col = y.
        IF sy-subrc <> 0.
          wa_ma-valor = 0.
        ENDIF.

        valor1 = wa_ma-valor * 1000.
        valor2 = valor1 * /qaps/zmidfcig-/qaps/rmeng.

        LOOP AT it_re INTO wa_re.
          IF sy-tabix = n.
            wa_re-cmeng = wa_ma-valor.
            MODIFY it_re FROM wa_re.
          ENDIF.
        ENDLOOP.

      ENDIF.

      n = n + 1.

    ENDDO.


    CLEAR: xrmeng.
    REFRESH: it_re_re.
    it_re_re = it_re.

    LOOP AT it_re_re INTO wa_re_re.
      xrmeng = xrmeng + wa_re_re-cmeng.
    ENDLOOP.

    FIELD-SYMBOLS <fs> TYPE ty_re.
    DATA ls_rest_comp TYPE ty_rest_comp.
    DATA lv_can_adjust TYPE abap_bool.

    IF xrmeng <> /qaps/zmidfcig-/qaps/rmeng.

      LOOP AT it_re_re ASSIGNING <fs>.

        CHECK NOT line_exists( it_rest_comp_ini[ /qaps/comnr = <fs>-comnr ] ).
        lv_can_adjust = abap_true.
        EXIT.

      ENDLOOP.

      IF lv_can_adjust = abap_false.
        REFRESH it_rest_comp_ini.
        lv_can_adjust = abap_true.
      ENDIF.

      IF lv_can_adjust = abap_true AND lines( it_re_re ) > 0.

        DO.

          SORT it_re_re BY cmeng DESCENDING.

          IF xrmeng <> /qaps/zmidfcig-/qaps/rmeng.

            IF xrmeng > /qaps/zmidfcig-/qaps/rmeng.

              LOOP AT it_re_re ASSIGNING <fs>.

                CLEAR ls_rest_comp.

                READ TABLE it_rest_comp_ini WITH KEY /qaps/comnr = <fs>-comnr
                                                               TRANSPORTING NO FIELDS.
*
                IF sy-subrc NE 0.
                  <fs>-cmeng = <fs>-cmeng - '0.001'.
                  CLEAR: xrmeng.
                  EXIT.
                ENDIF.

              ENDLOOP.

            ELSE.

              LOOP AT it_re_re ASSIGNING <fs>.

                CLEAR ls_rest_comp.

                READ TABLE it_rest_comp_ini WITH KEY /qaps/comnr = <fs>-comnr
                                        TRANSPORTING NO FIELDS.
                IF sy-subrc NE 0.
                  <fs>-cmeng = <fs>-cmeng + '0.001'.
                  CLEAR: xrmeng.
                  EXIT.
                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDIF.

          CLEAR: xrmeng.

          LOOP AT it_re_re INTO wa_re_re.
            xrmeng = xrmeng + wa_re_re-cmeng.
          ENDLOOP.

          IF xrmeng = /qaps/zmidfcig-/qaps/rmeng.
            EXIT.
          ENDIF.

        ENDDO.

      ENDIF.

    ENDIF.

    REFRESH: it_re.
    it_re = it_re_re.

    LOOP AT it_re INTO wa_re.
      IF NOT /qaps/zmidfcig-/qaps/fllt1e8 IS INITIAL.
        SELECT SINGLE matnr bwkey verpr stprs peinh vjbwh bwph1
               INTO (wa_cust_matnr-matnr,
                      wa_cust_matnr-bwkey,
                      wa_cust_matnr-verpr,
                      wa_cust_matnr-stprs,
                      wa_cust_matnr-peinh,
                      wa_cust_matnr-vjbwh,
                      wa_cust_matnr-bwph1)
               FROM mbew
               WHERE matnr = wa_re-comnr  AND
                     bwkey = /qaps/zmidfcig-werks.

        IF sy-subrc <> 0.
          wa_cust_matnr-vjbwh = 0.
        ENDIF.

        wa_re-vjbwh = wa_cust_matnr-vjbwh.
        custox1 = wa_re-cmeng * wa_cust_matnr-vjbwh.

        SELECT SINGLE /qaps/contab /qaps/reposi
                    /qaps/gerenc /qaps/priori
                    /qaps/gerxprio /qaps/totmkp
                    /qaps/totajmkt /qaps/mcost
          INTO (wa_cust_matnr-contab,
                wa_cust_matnr-reposi,
                wa_cust_matnr-gerenc,
                wa_cust_matnr-priori,
                wa_cust_matnr-gerxprio,
                wa_cust_matnr-totmkp,
                wa_cust_matnr-totajmkt,
                wa_cust_matnr-mcost)
          FROM /qaps/zmilc1
          WHERE matnr = wa_re-comnr
            AND werks = /qaps/zmidfcig-werks
            AND /qaps/grkey  = /qaps/zmidfcig-/qaps/grkey
            AND /qaps/period = /qaps/zmidfcig-/qaps/period
            AND /qaps/versao = /qaps/zmidfcig-/qaps/versao.

        IF sy-subrc <> 0.
          wa_cust_matnr-contab = 0.
          wa_cust_matnr-gerenc = 0.
          wa_cust_matnr-reposi = 0.
          wa_cust_matnr-gerxprio = 0.
          wa_cust_matnr-totajmkt = 0.
          wa_cust_matnr-totmkp = 0.
          wa_cust_matnr-mcost = 0.
        ENDIF.

        xcontab   = wa_re-cmeng * wa_cust_matnr-contab.
        xgerenc   = wa_re-cmeng * wa_cust_matnr-gerenc.
        xreposi   = wa_re-cmeng * wa_cust_matnr-reposi.
        xgerxprio = wa_re-cmeng * wa_cust_matnr-gerxprio.
        xtotmkp   = wa_re-cmeng * wa_cust_matnr-totmkp.
        xtotajmkt = wa_re-cmeng * wa_cust_matnr-totajmkt.
        xmcost    = wa_re-cmeng * wa_cust_matnr-mcost.

      ELSE.

        SELECT SINGLE matnr werks
              /qaps/contab /qaps/reposi /qaps/gerenc
              /qaps/priori /qaps/gerxprio
              /qaps/totmkp /qaps/totajmkt
              /qaps/mcost
              INTO (wa_cust_matnr-matnr,
                    wa_cust_matnr-bwkey,
                    wa_cust_matnr-contab,
                    wa_cust_matnr-reposi,
                    wa_cust_matnr-gerenc,
                    wa_cust_matnr-priori,
                    wa_cust_matnr-gerxprio,
                    wa_cust_matnr-totmkp,
                    wa_cust_matnr-totajmkt,
                    wa_cust_matnr-mcost)
               FROM /qaps/zmilc1
               WHERE matnr  = wa_re-comnr
                 AND werks  = /qaps/zmidfcig-werks
                 AND /qaps/grkey = /qaps/zmidfcig-/qaps/grkey
                 AND /qaps/period = /qaps/zmidfcig-/qaps/period
                 AND /qaps/versao = /qaps/zmidfcig-/qaps/versao.

        IF sy-subrc <> 0.
          wa_cust_matnr-contab = 0.
          wa_cust_matnr-reposi = 0.
          wa_cust_matnr-gerenc = 0.
          wa_cust_matnr-gerxprio = 0.
          wa_cust_matnr-totajmkt = 0.
          wa_cust_matnr-totmkp = 0.
          wa_cust_matnr-mcost = 0.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flcont IS INITIAL.
          wa_re-bwph1 = wa_cust_matnr-contab.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flger IS INITIAL.
          wa_re-bwph1 = wa_cust_matnr-gerenc.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flrep IS INITIAL.
          wa_re-bwph1 = wa_cust_matnr-reposi.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flpxg  IS INITIAL.
          wa_re-bwph1 = wa_cust_matnr-gerxprio.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flcont IS INITIAL.
          custox1     = wa_re-cmeng * wa_cust_matnr-contab.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flger IS INITIAL.
          custox1     = wa_re-cmeng * wa_cust_matnr-gerenc.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flrep IS INITIAL.
          custox1     = wa_re-cmeng * wa_cust_matnr-reposi.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flpxg  IS INITIAL.
          custox1     = wa_re-cmeng * wa_cust_matnr-gerxprio.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flcont IS INITIAL.
          xcontab      = wa_re-cmeng * wa_cust_matnr-contab.
          xgerenc      = wa_re-cmeng * wa_cust_matnr-gerenc.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flger IS INITIAL.
          xgerenc      = wa_re-cmeng * wa_cust_matnr-gerenc.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flrep IS INITIAL.
          xreposi      = wa_re-cmeng * wa_cust_matnr-reposi.
          xgerenc      = wa_re-cmeng * wa_cust_matnr-gerenc.
        ENDIF.

        IF NOT /qaps/zmidfcig-/qaps/flpxg  IS INITIAL.
          xgerxprio    = wa_re-cmeng * wa_cust_matnr-gerxprio.
          xgerenc      = wa_re-cmeng * wa_cust_matnr-gerenc.
        ENDIF.

        xtotmkp   = wa_re-cmeng * wa_cust_matnr-totmkp.
        xtotajmkt = wa_re-cmeng * wa_cust_matnr-totajmkt.
        xmcost    = wa_re-cmeng * wa_cust_matnr-mcost.

      ENDIF.

      ze = ze + custox1.

      zcontab = zcontab + xcontab.
      zgerenc = zgerenc + xgerenc.
      zreposi = zreposi + xreposi.
      zgerxprio = zgerxprio + xgerxprio.
      ztotmkp   = ztotmkp + xtotmkp.
      ztotajmkt = ztotajmkt + xtotajmkt.
      zmcost    = zmcost + xmcost.

      CLEAR: wa_cust_matnr,xtotmkp, xtotajmkt.

    ENDLOOP.

  ENDMETHOD.                                                    " STEP_4000


  METHOD STEP_500.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                               col = y.

    IF sy-subrc = 0.
      valor2 = wa_ma-valor.
    ELSE.
      valor2 = 0.
    ENDIF.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                               col = y.
    IF sy-subrc = 0.
      valor1 = wa_ma-valor.
    ELSE.
      valor1 = 0.
    ENDIF.

    valor3 = valor1 + valor2 * -1000.

    CALL METHOD set_matrix_a EXPORTING p_line = c p_col = y p_valor = valor3.

    CALL METHOD step_510a530.

  ENDMETHOD.                                                    " STEP_500


  METHOD STEP_5010.

    CALL METHOD stage_5050.

    LOOP AT it_combi_final INTO wa_combi_final
      WHERE combi = xcombi.
      wa_combi_final-msg = TEXT-s01.
      MODIFY it_combi_final FROM wa_combi_final.
    ENDLOOP.

  ENDMETHOD.                                                    " STEP_5010


  METHOD STEP_510A530.

    varj = nv + r3.

    j = 1.

    DO varj TIMES.

      IF j > varj.
        EXIT.
      ENDIF.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                 col = j.

      IF sy-subrc = 0.
        valor2  = wa_ma-valor.
      ELSE.
        valor2 = 0.
      ENDIF.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                                 col = j.
      IF sy-subrc = 0.
        valor1 = wa_ma-valor.
      ELSE.
        valor1 = 0.
      ENDIF.

      valor3 = valor1 + valor2 * -1000.

      CALL METHOD set_matrix_a EXPORTING p_line = c p_col = j p_valor = valor3.

      j = j + 1.

    ENDDO.

  ENDMETHOD.                               " STEP_510A530


  METHOD TEST_IT_MIN15.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR: wa_min-comnr.

    xcombi_n  =   xcombi.
    CLEAR: wa_min-comnr.

    xcond_min = ' '.

    LOOP AT it_zmicig1_min INTO wa_zmicig1
     WHERE /qaps/cignr = xcombi.

      READ TABLE it_rest_comp_2 INTO wa_rest_comp
             WITH KEY /qaps/comnr = wa_zmicig1-/qaps/comnr.

      IF sy-subrc = 0.
        IF  NOT wa_rest_comp-/qaps/gemng  IS INITIAL.
          IF wa_zmicig1-/qaps/cmeng <  wa_rest_comp-/qaps/gemng.
            wa_min-comnr = wa_zmicig1-/qaps/comnr.
            APPEND wa_min TO it_min2.
            xcond_min = 'X'.
            xcond_min15 = 'X'.
            DELETE it_combi_final WHERE combi  = xcombi.
            DELETE it_zmicig0_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
          ENDIF.
        ENDIF.

        IF  NOT wa_rest_comp-/qaps/eqmng  IS INITIAL.
          IF wa_zmicig1-/qaps/cmeng <> wa_rest_comp-/qaps/eqmng.
            wa_min-comnr = wa_zmicig1-/qaps/comnr.
            APPEND wa_min TO it_min2.
            xcond_min = 'X'.
            xcond_min15 = 'X'.
            DELETE it_combi_final WHERE combi  = xcombi.
            DELETE it_zmicig0_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
          ENDIF.
        ENDIF.

      ELSE.

        CLEAR: xcmeng_min.
        READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.
        IF sy-subrc = 0.
          xcmeng_min = wa_zmimi-/qaps/rmeng.

          IF wa_zmicig1-/qaps/cmeng <> 0 AND
             wa_zmicig1-/qaps/cmeng < xcmeng_min.
            xcond_min = 'X'.
            xcond_min15 = 'X'.
            wa_min-comnr = wa_zmicig1-/qaps/comnr.
            APPEND wa_min TO it_min2.
            DELETE it_combi_final WHERE combi  = xcombi.
            DELETE it_zmicig0_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2_min WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.


    SORT it_min BY comnr.
    SORT it_min2 BY comnr.

    DELETE ADJACENT DUPLICATES FROM it_min
                                      COMPARING comnr.
    DELETE ADJACENT DUPLICATES FROM it_min2
                                      COMPARING comnr.

  ENDMETHOD.                    " TEST_IT_MIN15


  METHOD UPDATE_RESTRICAO.
    REFRESH: it_rest_comp_2.
    CALL METHOD zf_calcula_restricao.
    it_rest_comp_2[] = it_rest_comp[].
  ENDMETHOD.


  METHOD VALIDACOES_PRELIMINARES .

    CALL METHOD regra_1 EXPORTING us_header = us_header.
    CALL METHOD regra_2 EXPORTING us_header = us_header.

  ENDMETHOD.


  METHOD VALIDAR_REST_COMP.

    DATA: lv_gemng TYPE /qaps/zmic1-/qaps/gemng,
          lv_eqmng TYPE /qaps/zmic1-/qaps/eqmng,
          lv_lmeng TYPE /qaps/quan_5_dec.

    CHECK lines( ct_data ) > 0.

    SELECT *
     FROM /qaps/zmic1
     FOR ALL ENTRIES IN @ct_data
     WHERE matnr = @ct_data-matnr
     AND   werks = @ct_data-werks
     AND   /qaps/grkey = @ct_data-/qaps/grkey
     AND   /qaps/comnr = @ct_data-/qaps/comnr
     INTO TABLE @DATA(lt_zmic1).

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      CHECK <fs_data>-auto = abap_true.

      CLEAR: lv_gemng,
             lv_eqmng.

      DATA(ls_zmic1) = VALUE #( lt_zmic1[ /qaps/comnr = <fs_data>-/qaps/comnr ] OPTIONAL ).

      CHECK ( ls_zmic1-/qaps/gemng <> 0 OR ls_zmic1-/qaps/eqmng <> 0 ).

      lv_gemng = ls_zmic1-/qaps/gemng.
      lv_eqmng = ls_zmic1-/qaps/eqmng.

      IF <fs_data>-/qaps/gemng < lv_gemng OR <fs_data>-/qaps/gemng < lv_eqmng.

        IF lv_gemng > lv_eqmng.
          <fs_data>-/qaps/gemng = lv_gemng.
        ELSE.
          <fs_data>-/qaps/gemng = lv_eqmng.
        ENDIF.

      ENDIF.

    ENDLOOP.

    "Limite superior
    DATA(lt_superior) = ct_data.
    DELETE lt_superior WHERE auto IS INITIAL.

    IF lines( lt_superior ) > 0.

      SELECT *
        FROM /qaps/zmi11
        FOR ALL ENTRIES IN @lt_superior
        WHERE matnr = @lt_superior-/qaps/comnr
        AND   werks = @lt_superior-werks
        AND   /qaps/grkey = @lt_superior-/qaps/grkey
        AND  /qaps/cmeng <> 0
        INTO TABLE @DATA(lt_zmi11).

      LOOP AT lt_superior ASSIGNING <fs_data>.

        CLEAR lv_lmeng.

        LOOP AT lt_zmi11 INTO DATA(ls_zmi11)
          WHERE matnr = <fs_data>-/qaps/comnr.

          TRY.

              CLEAR <fs_data>-/qaps/gemng.
              IF it_zmi01[ /qaps/chkey = ls_zmi11-/qaps/chkey ]-/qaps/lmeng > 0
                  AND <fs_data>-/qaps/chkey = ls_zmi11-/qaps/chkey.
                lv_lmeng = lv_lmeng + it_zmi01[ /qaps/chkey = ls_zmi11-/qaps/chkey ]-/qaps/lmeng / ls_zmi11-/qaps/cmeng..
              ENDIF.

            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ENDLOOP.

        lv_lmeng = round( val  = lv_lmeng
                          dec  = 3
                          mode = cl_abap_math=>round_down ).

        <fs_data>-/qaps/lemng = lv_lmeng.

      ENDLOOP.

      DELETE lt_superior WHERE /qaps/lemng = 0.

      IF lines( lt_superior ) > 0.
        APPEND LINES OF lt_superior TO ct_data.
      ENDIF.

    ENDIF.

    "Componentes forçados
*
*
*
*
*

  ENDMETHOD.


  METHOD VALIDAR_SNAPSHOT .

    DATA: lt_restricoes TYPE TABLE OF /qaps/zmic1,
          lv_upper      TYPE /qaps/zmi_lemng.

    LOOP AT gt_snapshot ASSIGNING FIELD-SYMBOL(<fs_snapshot>).

      LOOP AT <fs_snapshot>-t_ng INTO DATA(ls_ng).


        CHECK ls_ng-cmeng > 0 AND
          ( ls_ng-chkey = 'N' OR ls_ng-chkey = 'P' OR ls_ng-chkey = 'K' ).

        lv_upper = ceil( ls_ng-lmeng ) - '0.001'.

        IF ls_ng-cmeng >= ls_ng-gmeng AND ls_ng-cmeng <= lv_upper.
          <fs_snapshot>-valido = 'X'.
        ELSE.
          <fs_snapshot>-valido = ''.
          EXIT.
        ENDIF.

      ENDLOOP.



*
*
*
*
*
*

    ENDLOOP.

    IF line_exists( gt_snapshot[ valido = 'X' ] ).
      cv_valido = abap_true.
    ELSE.
      cv_valido = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_CONDICOES.

    DATA lt_restricoes TYPE TABLE OF /qaps/zmic1.
    DATA lr_cignr TYPE RANGE OF /qaps/zmificnr.

    LOOP AT it_combi_final ASSIGNING FIELD-SYMBOL(<fs_snapshot>).

      SELECT *
        FROM /qaps/zmic1
        WHERE matnr = @/qaps/zmidfcig-matnr
        AND   werks = @/qaps/zmidfcig-werks
        AND   /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
        INTO TABLE @lt_restricoes.

      DATA(lt_zmicig1) = it_zmicig1.
      DELETE lt_zmicig1 WHERE /qaps/cignr <> <fs_snapshot>-combi.

      LOOP AT lt_restricoes INTO DATA(ls_rest_comp).

        DATA(ls_zmicig1) = VALUE #( lt_zmicig1[ /qaps/comnr = ls_rest_comp-/qaps/comnr ] OPTIONAL ).

        IF NOT ls_zmicig1-matnr IS INITIAL.

          IF NOT ls_rest_comp-/qaps/gemng IS INITIAL.
            IF ls_zmicig1-/qaps/cmeng < ls_rest_comp-/qaps/gemng.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_snapshot>-combi ) TO lr_cignr.
              EXIT.
            ENDIF.
          ENDIF.

          IF NOT ls_rest_comp-/qaps/eqmng IS INITIAL.
            IF ls_zmicig1-/qaps/cmeng <> ls_rest_comp-/qaps/eqmng.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_snapshot>-combi ) TO lr_cignr.
              EXIT.
            ENDIF.
          ENDIF.

        ELSE.
          IF ls_rest_comp-/qaps/lemng IS INITIAL.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_snapshot>-combi ) TO lr_cignr.
            EXIT.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    CHECK lines( lr_cignr ) >  0.
    DELETE it_combi_final WHERE combi IN lr_cignr.

  ENDMETHOD.


  METHOD VERIFICA_COND_MINIMO.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    CLEAR:  ind_min,
            xcond_min,
            xcombi_n,
            xcombi,
            wa_min,
            xcombi_aux,
            xcombi_n2,
            zajustou.

    ind_min = ind_min + 1.

    REFRESH: it_min, it_min2.", it_min3, it_min4.", it_rest_comp_2.

    xcombi =  wa_combi_finalx-combi.
    xcombi_min =  wa_combi_finalx-combi.

    REFRESH: it_min, it_min2.", it_min3, it_min4.

    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.

    IF  sy-subrc = 0.
      xcombi_n = xcombi.
      CLEAR: wa_min-comnr.

      IF NOT it_zmicomp[] IS INITIAL.
        DELETE it_zmicomp WHERE /qaps/gemng = 0 AND /qaps/eqmng = 0 AND /qaps/lemng = 0.
        SORT it_zmicomp BY matnr werks /qaps/grkey /qaps/comnr.
        DELETE ADJACENT DUPLICATES FROM it_zmicomp
                        COMPARING  matnr werks /qaps/grkey /qaps/comnr.

        LOOP AT it_zmicomp INTO wa_zmicomp.
          MOVE-CORRESPONDING wa_zmicomp TO wa_rest_comp.
          APPEND wa_rest_comp TO  it_rest_comp_2.
        ENDLOOP.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM it_rest_comp_2 COMPARING ALL FIELDS.
      DELETE it_rest_comp_2 WHERE /qaps/lemng <> 0.

      LOOP AT it_zmicig1 INTO wa_zmicig1
      WHERE /qaps/cignr = xcombi_n.
        READ TABLE it_rest_comp_2 INTO wa_rest_comp
               WITH KEY /qaps/comnr = wa_zmicig1-/qaps/comnr.

        IF sy-subrc = 0.
          IF  NOT wa_rest_comp-/qaps/gemng  IS INITIAL.
            IF wa_zmicig1-/qaps/cmeng <  wa_rest_comp-/qaps/gemng.
              xcond_min = 'X'.
              wa_min-combi = wa_zmicig1-/qaps/cignr.
              wa_min-comnr = wa_zmicig1-/qaps/comnr.
              wa_min-ind = wa_min-ind + 1.
              APPEND wa_min TO it_min.
            ENDIF.
          ENDIF.

          IF  NOT wa_rest_comp-/qaps/eqmng  IS INITIAL.
            IF wa_zmicig1-/qaps/cmeng <> wa_rest_comp-/qaps/eqmng.
              xcond_min = 'X'.
              wa_min-combi = wa_zmicig1-/qaps/cignr.
              wa_min-comnr = wa_zmicig1-/qaps/comnr.
              wa_min-ind = wa_min-ind + 1.
              APPEND wa_min TO it_min.
            ENDIF.
          ENDIF.


        ELSE.

          CLEAR: xcmeng_min.
          READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_zmicig1-/qaps/comnr.
          IF sy-subrc = 0.
            xcmeng_min = wa_zmimi-/qaps/rmeng.
            IF wa_zmicig1-/qaps/cmeng <> 0 AND
               wa_zmicig1-/qaps/cmeng < xcmeng_min.
              xcond_min = 'X'.
              wa_min-combi = wa_zmicig1-/qaps/cignr.
              wa_min-comnr = wa_zmicig1-/qaps/comnr.
              wa_min-ind = wa_min-ind + 1.
              APPEND wa_min TO it_min.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

*
*
*
*
*

    ENDIF.

    "--Ajuste 10.02.2024
    IF lines( it_min ) > 0.
      DATA(lv_ind) = lines( it_min ).
      LOOP AT it_min_bk ASSIGNING FIELD-SYMBOL(<fs_min_bk>).
        CHECK NOT line_exists( it_min[ comnr = <fs_min_bk>-comnr ] ).
        lv_ind = lv_ind + 1.
        <fs_min_bk>-ind = lv_ind.
        <fs_min_bk>-combi = xcombi_n.
      ENDLOOP.
      APPEND LINES OF it_min_bk TO it_min.
    ENDIF.
    "--Fim Ajuste 10.02.2024

    SORT it_min BY comnr.
    DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.
    DESCRIBE TABLE it_min LINES lines.

  ENDMETHOD.                    " VERIFICA_COND_MINIMO


  METHOD VERIFICA_NIVEIS_GARANTIA .
    CLEAR:  it_dif[].
  ENDMETHOD.                    " VERIFICA_NIVEIS_GARANTIA


  METHOD VERIFICA_NIVEIS_GARANTIA_2 .

    DATA wa_dif   TYPE ty_dif.
    DATA vl_gmeng TYPE string.
    CLEAR:  it_dif[], wa_dif, vl_gmeng.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_n.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - 0 .
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0'.
          wa_dif-chkey = c_n.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vl_gmeng.
    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_p.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - 0 .
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0'.
          wa_dif-chkey = c_p.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vl_gmeng.
    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_k.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0'.
          wa_dif-chkey = c_k.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

*
*
*

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_b.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_b.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_zn.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_zn.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_ca.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_ca.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_s.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_s.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    SORT it_dif BY xdif DESCENDING.

  ENDMETHOD.                    " VERIFICA_NIVEIS_GARANTIA_2


  METHOD VERIFICA_NIVEIS_GARANTIA_3 .

    DATA wa_dif   TYPE ty_dif.
    DATA vl_gmeng TYPE string.
    CLEAR:  it_dif[], wa_dif, vl_gmeng.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_n.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 1000 ) .
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0.001'.
          wa_dif-chkey = c_n.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vl_gmeng.
    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_p.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 1000 ) .
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0.001'.
          wa_dif-chkey = c_p.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vl_gmeng.
    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_k.
    IF sy-subrc = 0.
      vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 1000 ) .
      IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
      ELSE.
        wa_dif-xdif =   wa_zmicig2-/qaps/gmeng - wa_zmicig2-/qaps/cmeng.
        IF wa_dif-xdif > '0.001'.
          wa_dif-chkey = c_k.
          APPEND wa_dif TO it_dif.
        ELSE.
          wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
          wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
          MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.

*
*
*

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                   /qaps/chkey = c_b.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_b.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                   /qaps/chkey = c_zn.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_zn.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                   /qaps/chkey = c_ca.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_ca.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                   /qaps/chkey = c_s.
    IF sy-subrc = 0.
      IF (  wa_zmicig2-/qaps/cmeng GE  wa_zmicig2-/qaps/gmeng ).
      ELSE.
        wa_dif-xdif =  wa_zmicig2-/qaps/gmeng -  wa_zmicig2-/qaps/cmeng.
        wa_dif-chkey = c_s.
        APPEND wa_dif TO it_dif.
      ENDIF.
    ENDIF.

    SORT it_dif BY xdif DESCENDING.

  ENDMETHOD.                    " VERIFICA_NIVEIS_GARANTIA_3


  METHOD VERIF_INCOMP_E_FORMULA .

    IF  it_zmi61[] IS INITIAL.

      xcombi_n2 = xcombi.
      xcombi = xcombi_n2.

      CALL METHOD formulation.

      CALL METHOD guarantee_levels_min.

      IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
         wa_combi_final-msg IS INITIAL.
        CALL METHOD save_soluctions_min.
      ENDIF.

    ELSE.


      IF  NOT aux1 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux1.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux1.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux1.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux2 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux2.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux2.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux2.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux3 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux3.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux3.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux3.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux4 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux4.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux4.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux4.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux5 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux5.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux5.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux5.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux6 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux6.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux6.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux6.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF  NOT aux7 IS INITIAL.
        READ TABLE it_zmi61 INTO wa_zmi61 WITH KEY matnr = aux7.
        IF sy-subrc = 0.
          READ TABLE it_pc_bk INTO wa_pc WITH KEY comnr = aux7.
          IF sy-subrc = 0.
            IF wa_pc-flgut = 'X'.
              LOOP AT it_zmi61  INTO wa_zmi61 WHERE matnr = aux7.
                LOOP AT it_pc_bk INTO wa_pc
                 WHERE comnr =  wa_zmi61-/qaps/comnr.
                  wa_pc-flgut = ' '.
                  MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
                ENDLOOP.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


      IF NOT it_desmarcar[] IS INITIAL.
        LOOP AT it_desmarcar INTO wa_desmarcar.
          LOOP AT it_pc_bk INTO wa_pc
           WHERE comnr =  wa_desmarcar-comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

      xcombi_n2 = xcombi  + 55.
      xcombi = xcombi_n2.

      CALL METHOD clear_workarea.

      CALL METHOD formulation.

      CALL METHOD guarantee_levels_min.

      IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
         wa_combi_final-msg IS INITIAL.
        CALL METHOD save_soluctions_min.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    " VERIF_INCOMP_E_FORMULA


  METHOD ZF_AJUSTA_GARANTIA .

    LOOP AT it_zmicig2 INTO wa_zmicig2.
      tabix = sy-tabix.
      IF wa_zmicig2-/qaps/cmeng < wa_zmicig2-/qaps/gmeng.
        IF wa_zmicig2-/qaps/chkey = 'N' OR wa_zmicig2-/qaps/chkey = 'P' OR
           wa_zmicig2-/qaps/chkey = 'K'.
          wa_zmicig2-/qaps/cmeng = wa_zmicig2-/qaps/gmeng.
          MODIFY it_zmicig2 FROM  wa_zmicig2 INDEX tabix.
        ENDIF.
      ENDIF.
      CLEAR wa_zmicig2.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZF_CALCULA_RESTRICAO .

    DATA: it_caract TYPE TABLE OF ty_caract,
          wa_caract TYPE ty_caract,
          wa_rs     TYPE ty_rs.

    DATA: wa_rest_comp TYPE ty_rest_comp,
          wa_zmi11     TYPE /qaps/zmi11.

    DATA:  vl_lines TYPE i.

    wa_caract-chkey = 'N'.
    APPEND wa_caract TO it_caract.
    CLEAR wa_caract.
    wa_caract-chkey = 'P'.
    APPEND wa_caract TO it_caract.
    CLEAR wa_caract.
    wa_caract-chkey = 'K'.
    APPEND wa_caract TO it_caract.
    CLEAR wa_caract.

    IF NOT it_pc[] IS INITIAL.

      it_pc_aux[] = it_pc[].

      SELECT *
        FROM /qaps/zmi11
        FOR ALL ENTRIES IN @it_pc_aux
        WHERE matnr = @it_pc_aux-comnr
          AND werks = @/qaps/zmidfcig-werks
          AND /qaps/grkey = @/qaps/zmidfcig-/qaps/grkey
          AND /qaps/cmeng > 0
          INTO TABLE @DATA(lt_zmi11_aux).

      SORT it_caract BY chkey ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_caract COMPARING ALL FIELDS.

      LOOP AT it_pc_aux INTO wa_pc.

        IF wa_pc-flgut = 'X'.
          LOOP AT it_caract INTO wa_caract.

            CALL METHOD zf_verifica_flags.

            it_zmi11_aux[] = lt_zmi11_aux.
            DELETE it_zmi11_aux WHERE /qaps/chkey <> wa_caract-chkey.

            IF NOT it_zmi11_aux[] IS INITIAL.

              DESCRIBE TABLE it_zmi11_aux LINES vl_lines.
              IF vl_lines = 1.
                READ TABLE it_zmi11_aux INTO wa_zmi11 INDEX 1.
                READ TABLE it_rs INTO wa_rs WITH KEY chkey  = wa_caract-chkey.

                IF sy-subrc = 0.

                  IF wa_rs-gmeng > 0 AND  wa_zmi11-/qaps/cmeng > 0.

                    vl_gemng = ( wa_rs-gmeng / wa_zmi11-/qaps/cmeng ).
                    wa_rest_comp-/qaps/gemng = ( vl_gemng * 1000 ).

                    IF frac( wa_rest_comp-/qaps/gemng ) > 0.
                      wa_rest_comp-/qaps/gemng = ( vl_gemng * 1000 ) + 1.
                    ENDIF.

                    wa_rest_comp-/qaps/gemng = trunc( wa_rest_comp-/qaps/gemng ).
                    wa_rest_comp-/qaps/gemng = wa_rest_comp-/qaps/gemng / 1000.

                    READ TABLE it_zmimi_aux INTO wa_zmimi WITH KEY matnr = wa_zmi11-matnr
                                                                   werks = wa_zmi11-werks.
                    IF sy-subrc = 0 .
                      wa_rest_comp-matnr       = /qaps/zmidfcig-matnr.
                      wa_rest_comp-werks       = wa_zmi11-werks.
                      wa_rest_comp-/qaps/grkey = wa_zmi11-/qaps/grkey.
                      wa_rest_comp-/qaps/comnr = wa_zmimi-matnr.

                      IF wa_rest_comp-/qaps/gemng < wa_zmimi-/qaps/rmeng AND wa_rest_comp-/qaps/gemng > 0.
                        wa_rest_comp-/qaps/gemng = wa_zmimi-/qaps/rmeng.
                      ENDIF.
                    ENDIF.

                    CLEAR: wa_rest_comp-/qaps/eqmng,
                           wa_rest_comp-/qaps/lemng.

                    IF NOT wa_rest_comp-matnr IS INITIAL
                      AND NOT line_exists( it_rest_comp[ matnr = wa_rest_comp-matnr
                                                         werks = wa_rest_comp-werks
                                                         /qaps/grkey = wa_rest_comp-/qaps/grkey
                                                         /qaps/comnr = wa_rest_comp-/qaps/comnr
                                                          auto = 'X' ] ).
                      wa_rest_comp-auto = abap_true.
                      wa_rest_comp-cond_min = ''.
                      wa_rest_comp-/qaps/chkey = wa_caract-chkey.
                      APPEND wa_rest_comp TO it_rest_comp.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR: wa_zmi11.

          ENDLOOP.


        ENDIF.
        CLEAR: wa_rest_comp.
      ENDLOOP.

    ENDIF.


    SORT it_rest_comp BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_rest_comp
    COMPARING matnr werks /qaps/grkey /qaps/comnr /qaps/gemng.

    CALL METHOD validar_rest_comp CHANGING ct_data = it_rest_comp.

    SORT it_rest_comp BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_rest_comp
    COMPARING matnr werks /qaps/grkey /qaps/comnr /qaps/gemng.

  ENDMETHOD.


  METHOD ZF_GRAVAR_RE_HIST.

    DATA: ls_re_hist TYPE ty_re_hist,
          ls_re      TYPE ty_re.

    READ TABLE it_re_hist WITH KEY combi = p_wa_combi_total-combi
                          TRANSPORTING NO FIELDS.

    CHECK sy-subrc NE 0.

    LOOP AT it_re INTO ls_re.

      ls_re_hist-combi = p_wa_combi_total-combi.
      ls_re_hist-costt = p_wa_combi_total-costt.
      ls_re_hist-comnr    = ls_re-comnr.
      ls_re_hist-coktx    = ls_re-coktx.
      ls_re_hist-cmeng    = ls_re-cmeng.
      ls_re_hist-cmeng_ng = ls_re-cmeng_ng.
      ls_re_hist-cmuni    = ls_re-cmuni.
      ls_re_hist-stprs    = ls_re-stprs.
      ls_re_hist-verpr    = ls_re-verpr.
      ls_re_hist-vjbwh    = ls_re-vjbwh.
      ls_re_hist-bwph1    = ls_re-bwph1.

      APPEND ls_re_hist TO it_re_hist.

    ENDLOOP.

  ENDMETHOD.                    "zf_gravar_re_hist


  METHOD ZF_REFAZ_ATE_IMCOMPATIB .

    CLEAR it_combi_final[].
    CLEAR it_zmicig0[].
    CLEAR it_zmicig1[].
    CLEAR it_zmicig2[].

    CALL METHOD clear_workarea.
    CLEAR: wa_combi_final-msg, xcond_min, zajustou.
    CLEAR it_rest_comp_2[].
    CLEAR it_zmi60[].

    CALL METHOD combinations.

    REFRESH: it_pc_bk.
    it_pc_bk[] = it_pc[].
    it_rs[] = it_rs_bk[].

    xcombi = '999995    '.

    wa_combi_final-combi  = xcombi.
    APPEND wa_combi_final TO it_combi_final.
    CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.

    CALL METHOD load_internal_tables.

    tp = -1.
    nv = wa_ct_comp.
    r1 = wa_ct_rest_le.
    r2 = wa_ct_rest_eq.
    r3 = wa_ct_rest_ge.
    x  = r1 + r2 + r3.
    a  = nv + 1.
    y  = x  + r3 + a.
    b  = y  - 1.
    c  = x  + 1.
    z  = a  - 1.

    CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
    CALL METHOD load_le_restrictions.
    CALL METHOD load_eq_restrictions.
    CALL METHOD load_ge_restrictions.
    CALL METHOD load_cuts.
    CALL METHOD stage_590.
    CALL METHOD modifies_matrix.

    DO.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_1000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_2000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        CALL METHOD stage_3000.
      ENDIF.

      IF NOT wa_combi_final-msg IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

    DELETE it_combi_final WHERE costt = 0.

    IF lines( it_combi_final ) = 0.
      DATA lv_message_error TYPE string.
      MESSAGE e133(/qaps/zmi) INTO lv_message_error.
      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
        EXPORTING
          message = lv_message_error.
    ENDIF.


    LOOP AT it_combi_final INTO wa_combi_final.
      IF  wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
        wa_combi_final-costt = 0.
        MODIFY it_combi_final FROM wa_combi_final.
      ENDIF.
    ENDLOOP.


    CLEAR : xcombi.
    CALL METHOD clear_workarea.

    REFRESH: it_zmi60.

    IF NOT it_zmi61[] IS INITIAL.
      SELECT * INTO TABLE it_zmi60
        FROM /qaps/zmi60
        FOR ALL ENTRIES IN it_zmi61
        WHERE matnr = it_zmi61-matnr AND
              werks = it_zmi61-werks AND
           /qaps/mixin = it_zmi61-/qaps/mixin.
    ENDIF.

    SORT it_zmi60 BY matnr werks /qaps/mixin.
    DELETE ADJACENT DUPLICATES FROM it_zmi60
                    COMPARING matnr werks /qaps/mixin.


    DESCRIBE TABLE it_zmi60 LINES lines.

    IF lines <> 0.

      LOOP AT it_zmi60 INTO wa_zmi60.
        CLEAR: wa_combi_final-costt.
        wa_combi_final-combi  = wa_zmi60-/qaps/mixin.
        APPEND wa_combi_final TO it_combi_final.
        CALL METHOD zf_gravar_re_hist EXPORTING p_wa_combi_total = wa_combi_final.
      ENDLOOP.

      LOOP AT it_zmi60 INTO wa_zmi60.
        it_pc_bk = it_pc.

        LOOP AT it_zmi61 INTO wa_zmi61 WHERE /qaps/mixin = wa_zmi60-/qaps/mixin.
          LOOP AT it_pc_bk INTO wa_pc
            WHERE comnr = wa_zmi61-/qaps/comnr.
            wa_pc-flgut = ' '.
            MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
          ENDLOOP.
        ENDLOOP.

        xcombi = wa_zmi60-/qaps/mixin.
        xcombi_atual = xcombi.

        CALL METHOD load_internal_tables.

        tp = -1.
        nv = wa_ct_comp.
        r1 = wa_ct_rest_le.
        r2 = wa_ct_rest_eq.
        r3 = wa_ct_rest_ge.
        x  = r1 + r2 + r3.
        a  = nv + 1.
        y  = x  + r3 + a.
        b  = y  - 1.
        c  = x  + 1.
        z  = a  - 1.

        CALL METHOD fill_matrix_a EXPORTING p_nrlin = c p_nrcol = y.
        CALL METHOD load_le_restrictions.
        CALL METHOD load_eq_restrictions.
        CALL METHOD load_ge_restrictions.
        CALL METHOD load_cuts.
        CALL METHOD stage_590.
        CALL METHOD modifies_matrix.

        DO.

          IF wa_combi_final-msg IS INITIAL.
            CALL METHOD stage_1000.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            CALL METHOD stage_2000.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            CALL METHOD stage_3000.
          ENDIF.

          IF NOT wa_combi_final-msg IS INITIAL.
            EXIT.
          ENDIF.

        ENDDO.
        CALL METHOD clear_workarea.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    " ZF_REFAZ_ATE_IMCOMPATIB


  METHOD ZF_VERIFICA_FLAGS .

    DATA wa_zmi11         TYPE /qaps/zmi11.

    IF NOT it_zmi11_aux[] IS INITIAL.

      LOOP AT it_zmi11_aux INTO wa_zmi11.
        READ TABLE it_pc INTO wa_pc WITH KEY comnr = wa_zmi11-matnr
                                    BINARY SEARCH.
        IF wa_pc-flgut = ' '.
          DELETE it_zmi11_aux WHERE matnr = wa_pc-comnr.
        ENDIF.
        CLEAR: wa_zmi11, wa_pc.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD ZF_VERIFICA_REST_MINIMOS.

    DATA wa_rest_comp     TYPE ty_rest_comp.

    SORT it_re BY comnr.
    SORT it_zmimi BY matnr.
    SORT it_rest_verif BY /qaps/comnr.

    LOOP AT it_rest_verif INTO wa_rest_comp.
      IF wa_rest_comp-/qaps/gemng > 0 OR wa_rest_comp-/qaps/eqmng > 0.
        READ TABLE it_re INTO wa_re WITH KEY comnr = wa_rest_comp-/qaps/comnr.

        IF sy-subrc <> 0.
          DATA lv_message_error TYPE string.
          MESSAGE e133(/qaps/zmi) INTO lv_message_error.
          RAISE EXCEPTION TYPE /qaps/cx_div_no_result
            EXPORTING
              message = lv_message_error.
        ENDIF.
      ENDIF.
      CLEAR: wa_rest_comp, wa_re.
    ENDLOOP.

    LOOP AT it_re INTO wa_re.
      READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_re-comnr
                                        BINARY SEARCH.
      IF sy-subrc = 0.
        IF wa_re-cmeng < wa_zmimi-/qaps/rmeng.
          MESSAGE e133(/qaps/zmi) INTO lv_message_error.
          RAISE EXCEPTION TYPE /qaps/cx_div_no_result
            EXPORTING
              message = lv_message_error.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR it_rest_verif.

  ENDMETHOD.                    " ZF_VERIFICA_REST_MINIMOS


  METHOD ZF_VERIFICA_TUDO .

    it_combi_finalx[] =  it_combi_final[].
    LOOP AT it_combi_finalx INTO wa_combi_finalx.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
      IF  sy-subrc = 0.
        CALL METHOD verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          CALL METHOD verifica_niveis_garantia_3.
          IF  it_dif[]  IS INITIAL.
            DELETE it_zmicig0 WHERE /qaps/cignr NE xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr NE xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr NE xcombi.
            DELETE it_combi_final WHERE combi NE xcombi.
            zdeusolucao = c_x.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    " ZF_VERIFICA_TUDO


  METHOD ZF_VER_COMP_MIN.

    DATA wa_restcomp      TYPE /qaps/zmic1.

    SELECT *
      FROM /qaps/zmimi
      INTO TABLE it_zmimi_aux
      FOR ALL ENTRIES IN it_pc
      WHERE matnr = it_pc-comnr
        AND werks = /qaps/zmidfcig-werks.

    IF sy-subrc = 0.
      SELECT * INTO TABLE it_restcomp
        FROM /qaps/zmic1
        FOR ALL ENTRIES IN it_zmimi_aux
        WHERE /qaps/comnr = it_zmimi_aux-matnr
          AND matnr = /qaps/zmidfcig-matnr
          AND werks = /qaps/zmidfcig-werks
          AND /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
    ENDIF.

    "Verifica se todos os componentes possuem cond. minímo
    LOOP AT it_pc INTO DATA(ls_pc).

      CHECK NOT line_exists( it_zmimi_aux[ matnr = ls_pc-comnr ] ).
      DATA lv_message TYPE string.
      MESSAGE e257(/qaps/zmi) WITH ls_pc-comnr INTO lv_message.
      RAISE EXCEPTION TYPE /qaps/cx_general
        EXPORTING
          message = VALUE bapiret2( type = 'E'
                                    message = lv_message ).

    ENDLOOP.

    IF NOT it_zmicomp[] IS INITIAL.                           "05.01.2023
      DELETE it_zmicomp WHERE /qaps/gemng = 0 AND /qaps/eqmng = 0 AND /qaps/lemng = 0.
      SORT it_zmicomp BY matnr werks /qaps/grkey /qaps/comnr.
      DELETE ADJACENT DUPLICATES FROM it_zmicomp
                      COMPARING  matnr werks /qaps/grkey /qaps/comnr.

      LOOP AT it_zmicomp INTO wa_zmicomp.
        MOVE-CORRESPONDING wa_zmicomp TO wa_restcomp.
        APPEND wa_restcomp TO  it_restcomp.
      ENDLOOP.
    ENDIF.

    SORT it_zmimi_aux BY matnr.
    SORT it_restcomp BY /qaps/comnr.

    LOOP AT it_pc INTO wa_pc.
      IF wa_pc-flgut = 'X'.
        READ TABLE it_restcomp INTO wa_restcomp WITH KEY /qaps/comnr = wa_pc-comnr.
        IF sy-subrc = 0.
          READ TABLE it_zmimi_aux INTO wa_zmimi WITH KEY matnr = wa_restcomp-/qaps/comnr.
          IF sy-subrc = 0.
            IF wa_restcomp-/qaps/gemng < wa_zmimi-/qaps/rmeng AND wa_restcomp-/qaps/gemng > 0 OR
               wa_restcomp-/qaps/eqmng < wa_zmimi-/qaps/rmeng AND wa_restcomp-/qaps/eqmng > 0 OR
               wa_restcomp-/qaps/lemng < wa_zmimi-/qaps/rmeng AND wa_restcomp-/qaps/lemng > 0.
              MESSAGE e286(/qaps/zmi) WITH wa_pc-comnr.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: wa_pc,wa_restcomp, wa_zmimi.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    " ZF_VER_COMP_MIN
ENDCLASS.
