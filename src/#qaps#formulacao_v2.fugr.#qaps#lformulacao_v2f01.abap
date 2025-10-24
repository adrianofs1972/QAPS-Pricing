*----------------------------------------------------------------------*
***INCLUDE /QAPS/LZMICIGAR_BURNF01.
*&---------------------------------------------------------------------*
*&      Form  LOAD_INTERNAL_TABLES
*&---------------------------------------------------------------------*
FORM load_internal_tables.

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
*   APPENDING CORRESPONDING FIELDS OF TABLE it_rest_comp
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

  PERFORM zf_calcula_restricao.

**-- Inclui restrições de Componentes digitadas na Tela - 01.11.2022
*  LOOP AT it_zmic1_aux INTO wa_zmic1_aux.
*    MOVE-CORRESPONDING wa_zmic1_aux TO wa_rest_comp.
*    APPEND wa_rest_comp TO it_rest_comp.
*    CLEAR: wa_rest_comp, wa_zmic1_aux.
*  ENDLOOP.

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

  PERFORM ajustar_limites_rest_comp CHANGING it_rest_comp[].

  it_rest_comp_bk[] = it_rest_comp[].
  it_rest_verif[]   = it_rest_comp[].

  LOOP AT it_rest_comp INTO wa_rest_comp.
    READ TABLE it_comp INTO wa_comp WITH KEY comnr = wa_rest_comp-/qaps/comnr.

    IF sy-subrc <> 0.
      DELETE it_rest_comp.
    ENDIF.

  ENDLOOP.

*  delete it_rest_comp_2 where cond_min = 'X'.

  IF NOT it_rest_comp[] IS INITIAL.
*    it_rest_comp_2[] = it_rest_comp[].
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
    MESSAGE e198.
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
*    APPEND INITIAL LINE TO it_prop_carac ASSIGNING FIELD-SYMBOL(<fs_prop_carac>).
*    <fs_prop_carac> =  CORRESPONDING #( ls_v_comp ).
*    APPEND wa_prop_comp TO it_prop_carac.
  ENDLOOP.

*  LOOP AT it_comp INTO wa_comp.
*
*    SELECT a~matnr a~werks a~/qaps/grkey a~/qaps/rmeng a~/qaps/rmuni
*           b~/qaps/chkey b~/qaps/cmeng
*      INTO TABLE it_prop_comp
*      FROM /qaps/zmi10 AS a INNER JOIN /qaps/zmi11 AS b
*        ON a~matnr = b~matnr
*       AND a~werks = b~werks
*       AND a~/qaps/grkey = b~/qaps/grkey
*     WHERE a~matnr = wa_comp-comnr
*       AND a~werks = /qaps/zmidfcig-werks
*       AND a~/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
*
*    LOOP AT it_prop_comp INTO wa_prop_comp.
*      APPEND wa_prop_comp TO it_prop_carac.
*    ENDLOOP.
*
*  ENDLOOP.

*  BREAK c060863.

  SORT it_prop_carac BY chkey matnr.

  IF it_rest_comp_ini[] IS INITIAL.
    it_rest_comp_ini[] = it_rest_comp[].
  ENDIF.

ENDFORM.                               " LOAD_INTERNAL_TABLES
FORM ajustar_limites_rest_comp CHANGING ct_data TYPE tt_rest_comp.

  DATA: lt_gmeng TYPE tt_rest_comp,
        lt_lmeng TYPE tt_rest_comp.

  "Quando houver mais de 1 limite superior ou inferior para o mesmo componente
  "utilizar o mais restritivo
*  SORT ct_data BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng.

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


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOAD_LE_RESTRICTIONS
*&---------------------------------------------------------------------*
FORM load_le_restrictions.

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
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      READ TABLE it_comp INTO wa_comp
        WITH KEY comnr = wa_rest_mist-/qaps/comnr.

      IF sy-subrc = 0.
        k = sy-tabix.
        xrmeng = wa_rest_mist-/qaps/rmeng.
        PERFORM set_matrix_a USING n k xrmeng.

      ENDIF.

    ENDIF.

  ENDLOOP.

  k = 0.

  LOOP AT it_rest_comp INTO wa_rest_comp.

    IF NOT wa_rest_comp-/qaps/lemng IS INITIAL.

      n = n + 1.

      IF xcomnr <> wa_rest_comp-/qaps/comnr.
        xrmeng = ( wa_rest_comp-/qaps/lemng * /qaps/zmidfcig-/qaps/rmeng ).

        PERFORM set_matrix_a USING n a xrmeng.

        xcomnr = wa_rest_comp-/qaps/comnr.
      ENDIF.

      READ TABLE it_prop_carac INTO wa_prop_carac
            WITH KEY matnr = wa_rest_comp-/qaps/comnr.

      k = sy-tabix.

      PERFORM set_matrix_a USING n k 1.

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
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      LOOP AT it_prop_carac INTO wa_prop_carac
        WHERE chkey = wa_zmi01-/qaps/chkey.

        IF k <> a.
          xrmeng = wa_prop_carac-cmeng.
          PERFORM set_matrix_a USING n k xrmeng.
        ENDIF.

        k = k + 1.

      ENDLOOP.

      k = 1.

    ENDIF.

  ENDLOOP.

ENDFORM.                               " LOAD_LE_RESTRICTIONS
*&---------------------------------------------------------------------*
*&      Form  LOAD_EQ_RESTRICTIONS
*&---------------------------------------------------------------------*
*      JOGA NA MATRIZ(A) E MATRIZ(B) AS RESTRIÇÕES = (IGUAL)
*----------------------------------------------------------------------*
FORM load_eq_restrictions.

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
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      READ TABLE it_comp INTO wa_comp
            WITH KEY comnr = wa_rest_mist-/qaps/comnr.

      IF sy-subrc = 0.
        k = sy-tabix.
        xrmeng = wa_rest_mist-/qaps/rmeng.
        PERFORM set_matrix_a USING n k xrmeng.
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
            PERFORM set_matrix_a USING n a 0.
          ELSE.
            xrmeng = /qaps/zmidfcig-/qaps/rmeng.
            PERFORM set_matrix_a USING n a xrmeng.
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
        PERFORM set_matrix_a USING n a xrmeng.

        xcomnr = wa_rest_comp-/qaps/comnr.

      ENDIF.

      READ TABLE it_prop_carac INTO wa_prop_carac
            WITH KEY matnr = wa_rest_comp-/qaps/comnr.

      k = sy-tabix.

      PERFORM set_matrix_a USING n k 1.

    ENDIF.

  ENDLOOP.

*--
  k = 1.

  CLEAR xchkey.

  LOOP AT it_zmi01 INTO wa_zmi01.

    IF NOT wa_zmi01-/qaps/emeng IS INITIAL.

      IF xchkey <> wa_zmi01-/qaps/chkey.

        n      = n + 1.
        k      = 1.
        xchkey = wa_zmi01-/qaps/chkey.

        xrmeng = ( wa_zmi01-/qaps/emeng * /qaps/zmidfcig-/qaps/rmeng ).
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      LOOP AT it_prop_carac INTO wa_prop_carac
        WHERE chkey = wa_zmi01-/qaps/chkey.

        IF k <> a.
          xrmeng = wa_prop_carac-cmeng.
          PERFORM set_matrix_a USING n k xrmeng.
        ENDIF.

        k = k + 1.

      ENDLOOP.

      k = 1.

    ENDIF.

  ENDLOOP.

ENDFORM.                               " LOAD_EQ_RESTRICTIONS
*&---------------------------------------------------------------------*
*&      Form  LOAD_GE_RESTRICTIONS
*&---------------------------------------------------------------------*
FORM load_ge_restrictions.

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
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      READ TABLE it_comp INTO wa_comp
            WITH KEY comnr = wa_rest_mist-/qaps/comnr.

      IF sy-subrc = 0.
        k = sy-tabix.
        xrmeng = wa_rest_mist-/qaps/rmeng.
        PERFORM set_matrix_a USING n k xrmeng.
      ENDIF.

    ENDIF.

  ENDLOOP.

  k = 0.

  LOOP AT it_rest_comp INTO wa_rest_comp.

    IF NOT wa_rest_comp-/qaps/gemng IS INITIAL.

      n = n + 1.

      IF xcomnr <> wa_rest_comp-/qaps/comnr.

        xrmeng = ( wa_rest_comp-/qaps/gemng * /qaps/zmidfcig-/qaps/rmeng ).
        PERFORM set_matrix_a USING n a xrmeng.

        xcomnr = wa_rest_comp-/qaps/comnr.

      ENDIF.

      READ TABLE it_prop_carac INTO wa_prop_carac
            WITH KEY matnr = wa_rest_comp-/qaps/comnr.

      k = sy-tabix.

      PERFORM set_matrix_a USING n k 1.

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
        PERFORM set_matrix_a USING n a xrmeng.

      ENDIF.

      LOOP AT it_prop_carac INTO wa_prop_carac
        WHERE chkey = wa_zmi01-/qaps/chkey.

        IF k <> a.
          xrmeng = wa_prop_carac-cmeng.
          PERFORM set_matrix_a USING n k xrmeng.
        ENDIF.

        k = k + 1.

      ENDLOOP.

      k = 1.

    ENDIF.

  ENDLOOP.

ENDFORM.                               " LOAD_GE_RESTRICTIONS
*&---------------------------------------------------------------------*
*&      Form  MODIFIES_MATRIX
*&---------------------------------------------------------------------*
FORM modifies_matrix.

  i = y - x.
  k = nv + 1.
  n = 1.

  WHILE n <= x.

    PERFORM set_matrix_a USING n i 1.

    wa_vc-chave   = i.
    wa_vc-valor   = n.
    APPEND wa_vc TO it_vc.

    i = i + 1.

    IF n <= r1.
      n = n + 1.
    ELSE.

      vark = r1 + r2.

      IF n > vark.
        PERFORM stage_650.
      ENDIF.

      PERFORM step_500.

      n = n + 1.

    ENDIF.

  ENDWHILE.

ENDFORM.                               " MODIFIES_MATRIX
*&---------------------------------------------------------------------*
*&      Form  LOAD_CUTS
*&---------------------------------------------------------------------*
FORM load_cuts RAISING /qaps/cx_general.

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
        MESSAGE e183 WITH wa_comp-comnr INTO DATA(lv_message).
        RAISE EXCEPTION TYPE /qaps/cx_general
          EXPORTING
            message = VALUE #( type = 'E' message = lv_message ).
      ENDIF.

      k      = k + 1.
      tp_n   = tp * -1.
      CLEAR: valor1.
      valorx = wa_cust_matnr-vjbwh / zdiv.
      valor1 = ( valorx * tp_n ).
      PERFORM set_matrix_a USING n k valor1.
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
        MESSAGE e183 WITH wa_comp-comnr INTO lv_message.
        RAISE EXCEPTION TYPE /qaps/cx_general
          EXPORTING
            message = VALUE #( type = 'E' message = lv_message ).
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
      PERFORM set_matrix_a USING n k valor1.

    ENDLOOP.

  ENDIF.

ENDFORM.                               " LOAD_CUTS
*&---------------------------------------------------------------------*
*&      Form  STEP_500
*&---------------------------------------------------------------------*
FORM step_500.

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

  PERFORM set_matrix_a USING c y valor3.

  PERFORM step_510a530.

ENDFORM.                                                    " STEP_500
*&---------------------------------------------------------------------*
*&      Form  STEP_510A530
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM step_510a530.

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

    PERFORM set_matrix_a USING c j valor3.

    j = j + 1.

  ENDDO.

ENDFORM.                               " STEP_510A530

*&---------------------------------------------------------------------*
*&      Form  STAGE_590
*&---------------------------------------------------------------------*
FORM stage_590.

  n = 1.

  WHILE n <= c.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                               col = a.  " --> K
    IF sy-subrc = 0.
      valor1 = wa_ma-valor.
      IF n <> c AND valor1 < 0.
        MESSAGE e131.
      ENDIF.
    ENDIF.

    PERFORM set_matrix_a USING n y valor1.
    PERFORM set_matrix_a USING n a 0.

    n = n + 1.

  ENDWHILE.

ENDFORM.                                                    " STAGE_590
*&---------------------------------------------------------------------*
*&      Form  STAGE_650
*&---------------------------------------------------------------------*
FORM stage_650.

  READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                             col = k.
  IF sy-subrc = 0.
    var1 = wa_ma-valor.
  ELSE.
    var1 = 0.
  ENDIF.

  PERFORM set_matrix_a USING n k -1.

  k = k + 1.

ENDFORM.                                                    " STAGE_650
*&---------------------------------------------------------------------*
*&      Form  STAGE_1000_MIN
*&---------------------------------------------------------------------*
FORM stage_1000_min.

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
      PERFORM stage_1080.
    ENDIF.

    n = n + 1.

  ENDWHILE.

  IF valork >=  valor_0001.                                 "LINE 1060
    PERFORM step_4000.

    LOOP AT it_re INTO wa_re.
*      IF WA_RE-COKTX IS INITIAL OR
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

*      SELECT * INTO TABLE it_incompat1
*      FROM /qaps/zmi61
*      WHERE   matnr = wa_re-comnr AND
*              werks = /qaps/zmidfcig-werks.

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
              PERFORM zf_gravar_re_hist USING wa_combi_final.
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
      PERFORM zf_gravar_re_hist USING wa_combi_final.
      PERFORM guarantee_levels.
      PERFORM save_soluctions.

    ENDIF.

    IF /qaps/zmidfcig-/qaps/rmeng = 0.
      /qaps/zmidfcig-/qaps/costu = 0.
    ELSE.
      /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
    ENDIF.

  ENDIF.

ENDFORM.                               " STAGE_1000
*&---------------------------------------------------------------------*
*&      Form  STAGE_1000
*&---------------------------------------------------------------------*
FORM stage_1000.

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
      PERFORM stage_1080.
    ENDIF.

    n = n + 1.

  ENDWHILE.

  IF valork >=  valor_0001.                                 "LINE 1060
    PERFORM step_4000.

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
              PERFORM zf_gravar_re_hist USING wa_combi_final.
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
      PERFORM guarantee_levels.
      PERFORM save_soluctions.
    ENDIF.

    IF /qaps/zmidfcig-/qaps/rmeng = 0.
      /qaps/zmidfcig-/qaps/costu = 0.
    ELSE.
      /qaps/zmidfcig-/qaps/costu = /qaps/zmidfcig-/qaps/costt / /qaps/zmidfcig-/qaps/rmeng.
    ENDIF.

  ENDIF.

ENDFORM.                               " STAGE_1000_MIN
*&---------------------------------------------------------------------*
*&      Form  STAGE_1080
*&---------------------------------------------------------------------*
FORM stage_1080.

  READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = c
                                             col = n.
  IF sy-subrc = 0.
    valork = wa_ma-valor.
  ELSE.
    valork = 0.
  ENDIF.

  cp = n.

ENDFORM.                               " STAGE_1080
*&---------------------------------------------------------------------*
*&      Form  STAGE_2000
*&---------------------------------------------------------------------*
FORM stage_2000.

  k = 1.

  WHILE k <= x.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                               col = cp.
    IF sy-subrc <> 0.
      wa_ma-valor  = 0.
    ENDIF.

    IF wa_ma-valor <= valor_0001p.

      PERFORM step_2160.

    ELSE.

*---  FORMATA B(K)

      CLEAR: valor1, valor2, valor3.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                                 col = y.
      IF sy-subrc <> 0.
        PERFORM set_matrix_a USING k y 0.
      ENDIF.

      valor1 = wa_ma-valor.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = k
                                                 col = cp.
      IF sy-subrc <> 0.
        PERFORM set_matrix_a USING k cp 0.
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

*--

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
      PERFORM stage_2180.
    ENDIF.

    n = n + 1.

  ENDWHILE.

*-

  IF j = exp1.

    PERFORM step_5010.

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

ENDFORM.                               " STAGE_2000
*&---------------------------------------------------------------------*
*&      Form  STEP_2160
*&---------------------------------------------------------------------*
FORM step_2160.

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

ENDFORM.                                                    " STEP_2160
*&---------------------------------------------------------------------*
*&      Form  STAGE_2180
*&---------------------------------------------------------------------*
FORM stage_2180.

  READ TABLE it_vb INTO wa_vb WITH KEY chave = n.

  IF sy-subrc <> 0.
    wa_vb-chave = n.
    wa_vb-valor = 0.
    APPEND wa_vb TO it_vb.
  ENDIF.

  j  = wa_vb-valor.
  lp = n.

ENDFORM.                               " STAGE_2180
*&---------------------------------------------------------------------*
*&      Form  STAGE_3000
*&---------------------------------------------------------------------*
FORM stage_3000.

  READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                             col = cp.
  IF sy-subrc = 0.
    pv = wa_ma-valor.
  ELSE.
    pv = 0.
  ENDIF.

  IF pv = 1.

    PERFORM step_3060.

  ELSE.

    n = 1.

    WHILE n <= y.

      READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                                 col = n.
      IF sy-subrc <> 0.
        PERFORM set_matrix_a USING lp n 0.
      ENDIF.

      valor1 = wa_ma-valor.
      valor2 = valor1 / pv.
      PERFORM set_matrix_a USING lp n valor2.

      n = n + 1.

    ENDWHILE.

    PERFORM step_3060.

  ENDIF.

ENDFORM.                               " STAGE_3000
*&---------------------------------------------------------------------*
*&      Form  STEP_3060
*&---------------------------------------------------------------------*
FORM step_3060.

  n = 1.

  WHILE n <= c.

    READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                               col = cp.
    IF sy-subrc <> 0.
      PERFORM set_matrix_a USING n cp 0.
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
          PERFORM set_matrix_a USING n k 0.
        ENDIF.
        valor1 = wa_ma-valor.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = lp
                                                   col = k.
        IF sy-subrc <> 0.
          PERFORM set_matrix_a USING lp k 0.
        ENDIF.


        valor2 = wa_ma-valor.
        valor3 = valor1 + valor2 * p.

        READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = n
                                                   col = k.
        IF sy-subrc = 0.
          PERFORM set_matrix_a USING n k valor3.
        ENDIF.

        k = k + 1.

      ENDWHILE.

    ENDIF.

    n = n + 1.

  ENDWHILE.

ENDFORM.                                                    " STEP_3060
*&---------------------------------------------------------------------*
*&      Form  STEP_4000
*&---------------------------------------------------------------------*
FORM step_4000.

  PERFORM stage_5050.

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

**---

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
*                                    INTO ls_rest_comp.
              IF sy-subrc NE 0.
*            IF ( ls_rest_comp-/qaps/lemng = 0 AND ls_rest_comp-/qaps/eqmng = 0 ).
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

ENDFORM.                                                    " STEP_4000
*&---------------------------------------------------------------------*
*&      Form  STEP_5010
*&---------------------------------------------------------------------*
FORM step_5010.

  PERFORM stage_5050.

  LOOP AT it_combi_final INTO wa_combi_final
    WHERE combi = xcombi.
    wa_combi_final-msg = TEXT-s01.
    MODIFY it_combi_final FROM wa_combi_final.
  ENDLOOP.

ENDFORM.                                                    " STEP_5010
*&---------------------------------------------------------------------*
*&      Form  STAGE_5050
*&---------------------------------------------------------------------*
FORM stage_5050.

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
          PERFORM set_matrix_a USING wa_vc-valor y 0.
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
            PERFORM zf_gravar_re_hist USING wa_combi_final.
            CLEAR: xcombi_aux.
          ENDIF.
        ENDIF.

      ENDIF.

      n = n + 1.

    ENDWHILE.

  ENDIF.

ENDFORM.                               " STAGE_5050
*&---------------------------------------------------------------------*
*&      Form  STAGE_5130
*&---------------------------------------------------------------------*
FORM stage_5130.

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
      PERFORM set_matrix_a USING c y 0.
    ENDIF.

    IF wa_vc-valor = 0 AND wa_ma-valor = 0.
      d = 1.
      n = n + 1.
      EXIT.
    ENDIF.

    n = n + 1.

  ENDDO.

  IF d = 1.
    MESSAGE e134.
  ENDIF.

ENDFORM.                               " STAGE_5130
*&---------------------------------------------------------------------*
*&      Form  GUARANTEE_LEVELS  - TELA 0300
*&---------------------------------------------------------------------*
FORM guarantee_levels.

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

ENDFORM.                               " GUARANTEE_LEVELS
*&---------------------------------------------------------------------*
*&      Form  FILL_MATRIX_A
*&---------------------------------------------------------------------*
FORM fill_matrix_a USING VALUE(p_nrlin) VALUE(p_nrcol).

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

ENDFORM.                               " FILL_MATRIX_A
*&---------------------------------------------------------------------*
*&      Form  SET_MATRIX_A
*&---------------------------------------------------------------------*
FORM set_matrix_a USING VALUE(p_line) VALUE(p_col) VALUE(p_valor).

  READ TABLE it_ma INTO wa_ma WITH TABLE KEY line  = p_line
                                             col   = p_col.
  IF sy-subrc = 0.
    wa_ma-valor = p_valor.
    MODIFY TABLE it_ma FROM wa_ma.
  ELSE.
    MESSAGE e000 WITH TEXT-m01.
  ENDIF.

ENDFORM.                               " SET_MATRIX_A

*&---------------------------------------------------------------------*
*&      Form  PRODUCT_FORMULATE
*&---------------------------------------------------------------------*
FORM product_formulate RAISING /qaps/cx_formulation_error
                               /qaps/cx_div_no_result
                               /qaps/cx_general.

  DATA lv_has_result TYPE abap_bool.

  PERFORM combinations.
  PERFORM load_matrix CHANGING lv_has_result.

  SORT it_re BY  comnr.
  DELETE ADJACENT DUPLICATES FROM it_re
                                   COMPARING comnr.

ENDFORM.                    " PRODUCT_FORMULATE
*&---------------------------------------------------------------------*
*&      Form  CLEAR_WORKAREA
*&---------------------------------------------------------------------*
FORM clear_workarea.

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

ENDFORM.                    " CLEAR_WORKAREA

*&---------------------------------------------------------------------*
*&      Form  SAVE_SOLUCTIONS
*&---------------------------------------------------------------------*
FORM save_soluctions.

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
*---
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

ENDFORM.                    " SAVE_SOLUCTIONS
*&---------------------------------------------------------------------*
*&      Form  COND_MIN_REFORMULATE
*&---------------------------------------------------------------------*
FORM formulation RAISING /qaps/cx_general.

  PERFORM load_internal_tables.

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

  PERFORM fill_matrix_a USING c y.

  PERFORM load_le_restrictions.

  PERFORM load_eq_restrictions.

  PERFORM load_ge_restrictions.

  PERFORM load_cuts.

  PERFORM stage_590.
*
  PERFORM modifies_matrix.

  DO.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_1000_min.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_2000.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_3000.
    ENDIF.

    IF NOT wa_combi_final-msg IS INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.                    " FORMULATION
*&---------------------------------------------------------------------*
*&      Form  SAVE_SOLUCTIONS_MIN
*&---------------------------------------------------------------------*
FORM save_soluctions_min.

  DATA wa_ng    TYPE ty_ng.
*--  XCMENG_MIN = 15 / 1000.

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
*--

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

*---

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

ENDFORM.                    " SAVE_SOLUCTIONS_MIN
*&---------------------------------------------------------------------*
*&      Form  GUARANTEE_LEVELS_MIN
*&---------------------------------------------------------------------*
FORM guarantee_levels_min.

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

ENDFORM.                               " GUARANTEE_LEVELS_MIN
*&---------------------------------------------------------------------*
*&      Form  LOAD_RESULTS
*&---------------------------------------------------------------------*
FORM load_results RAISING /qaps/cx_div_no_result /qaps/cx_general.

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

  PERFORM zf_verifica_rest_minimos.
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

*-----  PEGAR  DA TABELA

  READ TABLE it_zmicig0_min INTO wa_zmicig0 INDEX 1.

  /qaps/zmidfcig-/qaps/costt   = wa_zmicig0-/qaps/costt.
  /qaps/zmidfcig-/qaps/costu   = wa_zmicig0-/qaps/costt / wa_zmicig0-/qaps/rmeng.
  /qaps/zmidfcig-/qaps/gerxprio = wa_zmicig0-/qaps/gerxprio.

  /qaps/zmidfcig-/qaps/tot_ajumer = wa_zmicig0-/qaps/tot_ajumer.
  /qaps/zmidfcig-/qaps/tot_markup = wa_zmicig0-/qaps/tot_markup.

  /qaps/zmidfcig-/qaps/mcost = wa_zmicig0-/qaps/mcost.

ENDFORM.                    " LOAD_RESULTS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_IT_MIN_15
*&---------------------------------------------------------------------*
FORM process_it_min_15 RAISING /qaps/cx_general.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
  CLEAR: wa_combi_final-msg, xcond_min, zajustou.

*--  refresh  rest_comp_2  19.07.2011
  CLEAR it_rest_comp_2[].
*--  refresh  rest_comp_2  19.07.2011

  it_pc_bk = it_pc.

*--- Desmarca para incompatibilidades 18.07.2011
  IF NOT it_desmarcar[] IS INITIAL.
    LOOP AT it_desmarcar INTO wa_desmarcar.
      LOOP AT it_pc_bk INTO wa_pc
       WHERE comnr =  wa_desmarcar-comnr.
        wa_pc-flgut = ' '.
        MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
*--- Desmarca para incompatibilidades 18.07.2011

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

*--  AUX1
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

*--  AUX2
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

*--  AUX3
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

*--  AUX4
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

*--  AUX5
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

*--  AUX6
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

*--  AUX7
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

*--- Q&A Systems 15.07

*--- Q&A Systems 18.07.2011

  IF NOT it_desmarcar[] IS INITIAL.
    LOOP AT it_desmarcar INTO wa_desmarcar.
      LOOP AT it_pc_bk INTO wa_pc
       WHERE comnr =  wa_desmarcar-comnr.
        wa_pc-flgut = ' '.
        MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
*--- Q&A Systems 18.07.2011
  xcombi_n2 = xcombi + 10000000.
  xcombi = xcombi_n2.

  wa_flag_min = 15.

  PERFORM formulation.

  PERFORM guarantee_levels_min.

  IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
     wa_combi_final-msg IS INITIAL.
    PERFORM save_soluctions_min.
  ENDIF.

*  delete it_rest_comp_2 where cond_min = 'X'.

ENDFORM.                    " PROCESS_IT_MIN_15
FORM load_matrix CHANGING cv_has_result TYPE abap_bool
                  RAISING /qaps/cx_formulation_error
                          /qaps/cx_div_no_result
                          /qaps/cx_general.

  DATA: wa_zmi60_bk  TYPE /qaps/zmi60,
        wa_rest_comp TYPE ty_rest_comp.

  PERFORM first_formulation.

  IF NOT it_ng IS INITIAL  AND
    wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
*    PERFORM VERIFICA_NIVEIS_GARANTIA.
    IF NOT it_dif[]  IS INITIAL.
*      PERFORM  AJUSTA_MARGEM_SEGURANCA.
      DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
      DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
      DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
      DELETE it_combi_final WHERE combi = xcombi.
    ENDIF.
  ENDIF.


  DELETE it_combi_final WHERE costt = 0.

  IF lines( it_combi_final ) = 0.
    MESSAGE e133 INTO DATA(lv_message_error).
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

*--- CHECK IF THERE IS REST. DE INCOMP.

  CLEAR : xcombi.
  PERFORM clear_workarea.

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
    PERFORM incompatibilidade.
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

*-- HAVE NO INCOMPATIBILITY
  it_zmicig0_bk = it_zmicig0.
  it_zmicig1_bk = it_zmicig1.
  it_zmicig2_bk = it_zmicig2.

  it_zmicig0_min = it_zmicig0.
  it_zmicig1_min = it_zmicig1.
  it_zmicig2_min = it_zmicig2.

*-- MINIMUM CONDITIONS
  DATA l_tabix TYPE sy-tabix.                               "#EC NEEDED

  DELETE it_combi_final WHERE costt = 0.
  SORT it_combi_final BY costt combi.

  IF NOT it_combi_final[] IS INITIAL.

    LOOP AT it_pc  INTO wa_pc WHERE flgut = 'X'.
      r_matnr-sign = 'I'.
      r_matnr-option = 'EQ'.
      r_matnr-low =  wa_pc-comnr.
      APPEND r_matnr.
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

      PERFORM zf_calcula_restricao.
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
*--  SELECT ONLY THE INCOMPATIBILITIES
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

*--  FIRST LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *
      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.
        PERFORM condicao_minimo_nivel01.
      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

      DELETE it_zmicig0 WHERE /qaps/cignr = 0000999995.
*      DELETE it_zmicig0 WHERE /qaps/cignr = 0000999996.
*      DELETE it_zmicig0 WHERE /qaps/cignr = 0000999997.
*      DELETE it_zmicig0 WHERE /qaps/cignr = 0000999998.
*      DELETE it_zmicig0 WHERE /qaps/cignr = 0000999999.

      DELETE it_zmicig1 WHERE /qaps/cignr = 0000999995.
*      DELETE it_zmicig1 WHERE /qaps/cignr = 0000999996.
*      DELETE it_zmicig1 WHERE /qaps/cignr = 0000999997.
*      DELETE it_zmicig1 WHERE /qaps/cignr = 0000999998.
*      DELETE it_zmicig1 WHERE /qaps/cignr = 0000999999.

      DELETE it_zmicig2 WHERE /qaps/cignr = 0000999995.
*      DELETE it_zmicig2 WHERE /qaps/cignr = 0000999996.
*      DELETE it_zmicig2 WHERE /qaps/cignr = 0000999997.
*      DELETE it_zmicig2 WHERE /qaps/cignr = 0000999998.
*      DELETE it_zmicig2 WHERE /qaps/cignr = 0000999999.

**--  SECOND LEVEL

      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- THIRD LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
*          BREAK c060863.
          PERFORM resultado CHANGING cv_has_result.


          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- FOURTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- FIFTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

*-- SIXTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- SEVENTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- EIGHTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- NINTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

**-- TENTH LEVEL
      it_combi_finalx[] =  it_combi_final[].
      READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
      xcombi =  wa_combi_finalx-combi.
      xcombi_min =  wa_combi_finalx-combi.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi. "#EC *

      IF  sy-subrc = 0.
        PERFORM verifica_cond_minimo.
        IF  it_min[] IS INITIAL.
          PERFORM resultado CHANGING cv_has_result.

          IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        PERFORM condicao_minimo_nivel02.

      ELSE.
        PERFORM del_comd_min.
        DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
      ENDIF.

      SORT it_min  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

      SORT it_min_bk  BY comnr.
      DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.

*******************************************************
*                     Contingência
*******************************************************
      PERFORM contingencia CHANGING cv_has_result.
*******************************************************
*
*******************************************************

      DATA  ll_tabix  TYPE sy-tabix.
      DATA  l_ind  TYPE sy-tabix.

**--  VARIAS VEZES
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
          PERFORM verifica_cond_minimo.
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
          PERFORM verifica_cond_minimo.
          LOOP AT it_min INTO wa_min.
            READ TABLE it_min_bk INTO wa_min_bk WITH KEY comnr = wa_min-comnr.
            IF sy-subrc = 0.
              READ TABLE it_zmicig1 INTO wa_zmicig1  WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
                                                              /qaps/comnr = wa_min-comnr.
              IF   sy-subrc = 0.
                READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_min-comnr.
                IF sy-subrc = 0.
                  IF  wa_zmicig1-/qaps/cmeng LT  wa_zmimi-/qaps/rmeng.
*--MARCA INCOMPATIBILIDADES  -- 18.07.2011
*                    DELETE IT_zmicig0 INDEX LL_TABIX.
*                    DELETE IT_zmicig1 WHERE /qaps/cignr EQ WA_zmicig0-/qaps/cignr.
*                    DELETE IT_zmicig2 WHERE /qaps/cignr EQ WA_zmicig0-/qaps/cignr.
*                    DELETE IT_zmicig0_MIN WHERE /qaps/cignr EQ WA_zmicig0-/qaps/cignr.
*                    DELETE IT_zmicig1_MIN WHERE /qaps/cignr EQ WA_zmicig0-/qaps/cignr.
*                    DELETE IT_zmicig2_MIN WHERE /qaps/cignr EQ WA_zmicig0-/qaps/cignr.
*                    DELETE IT_COMBI_FINAL WHERE COMBI =  XCOMBI_MIN.
*                    DELETE IT_COMBI_FINALX WHERE COMBI =  XCOMBI_MIN.
*--MARCA INCOMPATIBILIDADES  -- 18.07.2011
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
              PERFORM resultado CHANGING cv_has_result.

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
              PERFORM resultado CHANGING cv_has_result.

              IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.


          PERFORM condicao_minimo_nivel02.

        ELSE.
          DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
          PERFORM condicao_minimo_nivel02.
          PERFORM del_comd_min.

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
***  RESULTADO.


  xcombi = wa_zmicig0-/qaps/cignr.
  DELETE it_zmicig0 WHERE /qaps/costt = 0.
  SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
  READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
  IF  sy-subrc = 0.
    xcombi = wa_zmicig0-/qaps/cignr.
    READ TABLE it_combi_finalx INTO wa_combi_finalx INDEX 1.
    CLEAR it_min[].
    PERFORM verifica_cond_minimo.
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
    MESSAGE e133 INTO DATA(lv_message).
    RAISE EXCEPTION TYPE /qaps/cx_div_no_result
      EXPORTING
        message = lv_message.
  ELSE.

    PERFORM  load_results.

*--- PREÇO
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

*  PERFORM grava_dados_0200.
*  CALL SCREEN 0300.
  PERFORM save_snapshot.
  cv_has_result = abap_true.

ENDFORM.                               " LOAD_MATRIX
*&---------------------------------------------------------------------*
*&      Form  COMBINATIONS
*&---------------------------------------------------------------------*
FORM combinations.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  LOOP AT it_re INTO wa_re.
    wa_comp-comnr = wa_re-comnr.
    APPEND wa_comp TO it_comp.
    wa_pc-comnr = wa_re-comnr.
    wa_pc-coktx = wa_re-coktx.
    APPEND wa_pc  TO it_pc_bk.
  ENDLOOP.

*---
  PERFORM zf_calcula_restricao.

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

*-- Componentes temporários
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

*---
  DATA l_tabix_61  TYPE sy-tabix.

  LOOP AT it_zmi61  INTO  wa_zmi61.
    l_tabix_61  = sy-tabix.
    READ TABLE it_comp INTO wa_comp WITH KEY comnr =  wa_zmi61-/qaps/comnr.
    IF sy-subrc NE 0.
      DELETE it_zmi61_bk INDEX   l_tabix_61.
    ENDIF.
  ENDLOOP.

*--

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

ENDFORM.                    " COMBINATIONS
*&---------------------------------------------------------------------*
*&      Form  DEL_COMD_MIN
*&---------------------------------------------------------------------*
FORM del_comd_min.

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

ENDFORM.                    " DEL_COMD_MIN
*&---------------------------------------------------------------------*
*&      Form  ORDENA_NIVEIS_GARANTIA
*&---------------------------------------------------------------------*
FORM ordena_niveis_garantia .

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

ENDFORM.                    " ORDENA_NIVEIS_GARANTIA
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEIS_GARANTIA
*&---------------------------------------------------------------------*
FORM verifica_niveis_garantia .
  CLEAR:  it_dif[].
ENDFORM.                    " VERIFICA_NIVEIS_GARANTIA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1
*&---------------------------------------------------------------------*
FORM process_combi_1.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2
*&---------------------------------------------------------------------*
FORM process_combi_2.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_3
*&---------------------------------------------------------------------*
FORM process_combi_3.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_3
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_4
*&---------------------------------------------------------------------*
FORM process_combi_4.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_4
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_5
*&---------------------------------------------------------------------*
FORM process_combi_5.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_5
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_6
*&---------------------------------------------------------------------*
FORM process_combi_6.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_6
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_7
*&---------------------------------------------------------------------*
FORM process_combi_7.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_7
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_12
*&---------------------------------------------------------------------*
FORM process_combi_12.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_12
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_13
*&---------------------------------------------------------------------*
FORM process_combi_13.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_13
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_14
*&---------------------------------------------------------------------*
FORM process_combi_14.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_14
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_15
*&---------------------------------------------------------------------*
FORM process_combi_15.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_15
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_16
*&---------------------------------------------------------------------*
FORM process_combi_16.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_16
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_17
*&---------------------------------------------------------------------*
FORM process_combi_17.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_17
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_23
*&---------------------------------------------------------------------*
FORM process_combi_23.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_23
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_24
*&---------------------------------------------------------------------*
FORM process_combi_24.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_24
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_25
*&---------------------------------------------------------------------*
FORM process_combi_25.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_25
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_26
*&---------------------------------------------------------------------*
FORM process_combi_26.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_26
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_27
*&---------------------------------------------------------------------*
FORM process_combi_27.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_27
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_34
*&---------------------------------------------------------------------*
FORM process_combi_34.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_34
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_35
*&---------------------------------------------------------------------*
FORM process_combi_35.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_35
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_36
*&---------------------------------------------------------------------*
FORM process_combi_36.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_36
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_37
*&---------------------------------------------------------------------*
FORM process_combi_37.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_37
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_45
*&---------------------------------------------------------------------*
FORM process_combi_45.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_45
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_46
*&---------------------------------------------------------------------*
FORM process_combi_46.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_46
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_47
*&---------------------------------------------------------------------*
FORM process_combi_47.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_47
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_56
*&---------------------------------------------------------------------*
FORM process_combi_56.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_56
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_57
*&---------------------------------------------------------------------*
FORM process_combi_57.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_57
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_67
*&---------------------------------------------------------------------*
FORM process_combi_67.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_67
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_123
*&---------------------------------------------------------------------*
FORM process_combi_123.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_123
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_124
*&---------------------------------------------------------------------*
FORM process_combi_124.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_124
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_125
*&---------------------------------------------------------------------*
FORM process_combi_125.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_125
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_126
*&---------------------------------------------------------------------*
FORM process_combi_126.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_126
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_127
*&---------------------------------------------------------------------*
FORM process_combi_127.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_127
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_134
*&---------------------------------------------------------------------*
FORM process_combi_134.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_134
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_135
*&---------------------------------------------------------------------*
FORM process_combi_135.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_135
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_136
*&---------------------------------------------------------------------*
FORM process_combi_136.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_136
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_137
*&---------------------------------------------------------------------*
FORM process_combi_137.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_137
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_234
*&---------------------------------------------------------------------*
FORM process_combi_234.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_234
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_235
*&---------------------------------------------------------------------*
FORM process_combi_235.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_235
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_236
*&---------------------------------------------------------------------*
FORM process_combi_236.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_236
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_237
*&---------------------------------------------------------------------*
FORM process_combi_237.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_237
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_345
*&---------------------------------------------------------------------*
FORM process_combi_345.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_345
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_346
*&---------------------------------------------------------------------*
FORM process_combi_346.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_346
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_347
*&---------------------------------------------------------------------*
FORM process_combi_347.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_347
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_456
*&---------------------------------------------------------------------*
FORM process_combi_456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_457
*&---------------------------------------------------------------------*
FORM process_combi_457.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_457
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_567
*&---------------------------------------------------------------------*
FORM process_combi_567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1234
*&---------------------------------------------------------------------*
FORM process_combi_1234.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1234
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1235
*&---------------------------------------------------------------------*
FORM process_combi_1235.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1235
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1236
*&---------------------------------------------------------------------*
FORM process_combi_1236.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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
  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1236
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1237
*&---------------------------------------------------------------------*
FORM process_combi_1237.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1237
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1345
*&---------------------------------------------------------------------*
FORM process_combi_1345.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1345
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1346
*&---------------------------------------------------------------------*
FORM process_combi_1346.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1346
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1347
*&---------------------------------------------------------------------*
FORM process_combi_1347.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1347
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1456
*&---------------------------------------------------------------------*
FORM process_combi_1456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1457
*&---------------------------------------------------------------------*
FORM process_combi_1457.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1457
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_1567
*&---------------------------------------------------------------------*
FORM process_combi_1567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_1567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2345
*&---------------------------------------------------------------------*
FORM process_combi_2345.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2345
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2346
*&---------------------------------------------------------------------*
FORM process_combi_2346 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2346
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2347
*&---------------------------------------------------------------------*
FORM process_combi_2347 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2347
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2356
*&---------------------------------------------------------------------*
FORM process_combi_2356 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2356
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2357
*&---------------------------------------------------------------------*
FORM process_combi_2357 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2357
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2456
*&---------------------------------------------------------------------*
FORM process_combi_2456 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_2457
*&---------------------------------------------------------------------*
FORM process_combi_2457 .

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_2457
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_3456
*&---------------------------------------------------------------------*
FORM process_combi_3456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_3456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_3457
*&---------------------------------------------------------------------*
FORM process_combi_3457.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_3457
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_4567
*&---------------------------------------------------------------------*
FORM process_combi_4567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_4567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_12345
*&---------------------------------------------------------------------*
FORM process_combi_12345.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_123456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_13456
*&---------------------------------------------------------------------*
FORM process_combi_13456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_13456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_14567
*&---------------------------------------------------------------------*
FORM process_combi_14567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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
  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_14567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_23456
*&---------------------------------------------------------------------*
FORM process_combi_23456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_23456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_24567
*&---------------------------------------------------------------------*
FORM process_combi_24567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_24567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_34567
*&---------------------------------------------------------------------*
FORM process_combi_34567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_34567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_123456
*&---------------------------------------------------------------------*
FORM process_combi_123456.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_123456
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_134567
*&---------------------------------------------------------------------*
FORM process_combi_134567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_134567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMBI_234567
*&---------------------------------------------------------------------*
FORM process_combi_234567.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  PERFORM clear_workarea.
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

  PERFORM verif_incomp_e_formula.

ENDFORM.                    " PROCESS_COMBI_234567
*&---------------------------------------------------------------------*
*&      Form  PROCESS_IT_MIN
*&---------------------------------------------------------------------*
FORM process_it_min RAISING /qaps/cx_general.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  CLEAR: wa_min-comnr.

  SORT it_min BY comnr combi.
  DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.

  xcombi_aux = xcombi.
  PERFORM clear_workarea.
  it_pc_bk = it_pc.

*--- Desmarca para incompatibilidades 18.07.2011
  IF NOT it_desmarcar[] IS INITIAL.
    LOOP AT it_desmarcar INTO wa_desmarcar.
      LOOP AT it_pc_bk INTO wa_pc
       WHERE comnr =  wa_desmarcar-comnr.
        wa_pc-flgut = ' '.
        MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
*--- Desmarca para incompatibilidades 18.07.2011

  LOOP AT it_min INTO wa_min.

    xcomnr_min = wa_min-comnr.

    LOOP AT it_pc_bk INTO wa_pc
     WHERE comnr =  xcomnr_min.
      wa_pc-flgut = ' '.
      MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
    ENDLOOP.

  ENDLOOP.

*  loop at it_zmi60  into wa_zmi60.
*    LOOP AT IT_PC_BK INTO WA_PC
*     WHERE COMNR =  wa_zmi60-matnr.
*      WA_PC-FLGUT = ' '.
*      MODIFY IT_PC_BK FROM WA_PC INDEX SY-TABIX.
*    ENDLOOP.
*  endloop.
*

  CHECK line_exists( it_pc_bk[ flgut = 'X' ] ).

  xcombi_n2 = xcombi.

  wa_flag_min = 0.
  PERFORM formulation.

  PERFORM guarantee_levels_min.

  IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
     wa_combi_final-msg IS INITIAL.
    PERFORM save_soluctions_min.
  ENDIF.

ENDFORM.                    " PROCESS_IT_MIN
*&---------------------------------------------------------------------*
*&      Form  CONDICAO_MINIMO_NIVEL01
*&---------------------------------------------------------------------*
FORM condicao_minimo_nivel01 RAISING /qaps/cx_general.

  DATA  l_tabix TYPE sy-tabix.

* 18.07.2011
*---  MARCA OS MATERIAIS INCOMPATIVEIS PARA DESMARCAR
  CLEAR it_desmarcar[].
  REFRESH it_desmarcar.

  PERFORM update_restricao.

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
*---  MARCA OS MATERIAIS INCOMPATIVEIS PARA DESMARCAR


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
    PERFORM process_it_min.
    CLEAR:  zajustou..
    wa_msg0 = wa_combi_final-msg.

    CLEAR: wa_combi_final-msg, wa_msg15.
    xcond_min15 = ' '.
    xcombi = wa_combi_finalx-combi + 200.
    PERFORM process_it_min_15.
    CLEAR:  zajustou.
    wa_msg15 = wa_combi_final-msg.

    LOOP AT it_min INTO wa_min.
      APPEND wa_min TO it_min_bk.
    ENDLOOP.

  ELSE.

*---  2 componentes

    IF lines = 2.

      CLEAR xcombi_aux.
      CLEAR: xcond_min.

      xcombi = xcombi  + 10 .
      CLEAR: wa_combi_final-msg.
      it_pc_bk = it_pc.
      PERFORM process_it_min.
      CLEAR:  zajustou..

      xcombi = xcombi + 10.
      CLEAR: wa_combi_final-msg.
      PERFORM process_it_min_15.
      CLEAR:  zajustou..

      READ TABLE it_min INTO wa_min INDEX 1.
      aux1 = wa_min-comnr.

      READ TABLE it_min INTO wa_min INDEX 2.
      aux2 = wa_min-comnr.

      PERFORM update_restricao.

      CLEAR: wa_combi_final-msg.
      xcombi = xcombi + 100.
      PERFORM process_combi_1.
      CLEAR:  zajustou..

      PERFORM update_restricao.

      CLEAR: wa_combi_final-msg.
      xcombi = xcombi + 1111.
      PERFORM process_combi_2.
      CLEAR:  zajustou..

      PERFORM update_restricao.

      LOOP AT it_min INTO wa_min.
        APPEND wa_min TO it_min_bk.
      ENDLOOP.

    ELSE.

*---  3 componentes

      IF lines = 3.
        CLEAR xcombi_aux.
        CLEAR: xcond_min.
        xcombi = xcombi + 1000.
        CLEAR: wa_combi_final-msg.
        PERFORM process_it_min.
        CLEAR:  zajustou..

        CLEAR: wa_combi_final-msg.
        xcombi =  xcombi + 888  + 1033.
        PERFORM process_it_min_15.
        CLEAR:  zajustou..

        READ TABLE it_min INTO wa_min INDEX 1.
        aux1 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 2.
        aux2 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 3.
        aux3 = wa_min-comnr.

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30001  + 1033.
        PERFORM process_combi_1.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30002  + 1033.
        PERFORM process_combi_2.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30003  + 1033.
        PERFORM process_combi_3.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 200012  + 1033.
        PERFORM process_combi_12.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100013  + 1033.
        PERFORM process_combi_13.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100023  + 1033.
        PERFORM process_combi_23.
        CLEAR:  zajustou..

        LOOP AT it_min INTO wa_min.
          APPEND wa_min TO it_min_bk.
        ENDLOOP.

      ELSE.

*---  4 componentes

        IF lines EQ 4.
          CLEAR xcombi_aux.
          CLEAR: xcond_min.
          xcombi = 10000.

          CLEAR: wa_combi_final-msg.
          PERFORM process_it_min.

          CLEAR: wa_combi_final-msg.
          xcombi = 88888.
          PERFORM process_it_min_15.
***--
          READ TABLE it_min INTO wa_min INDEX 1.
          aux1 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 2.
          aux2 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 3.
          aux3 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 4.
          aux4 = wa_min-comnr.

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100001.
          PERFORM process_combi_1.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100002.
          PERFORM process_combi_2.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100003.
          PERFORM process_combi_3.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100004.
          PERFORM process_combi_4.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100012.
          PERFORM process_combi_12.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100013.
          PERFORM process_combi_13.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100014.
          PERFORM process_combi_14.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100023.
          PERFORM process_combi_23.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100024.
          PERFORM process_combi_24.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100034.
          PERFORM process_combi_34.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000123.
          PERFORM process_combi_123.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000124.
          PERFORM process_combi_124.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000134.
          PERFORM process_combi_134.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000234.
          PERFORM process_combi_234.
          CLEAR:  zajustou..

          LOOP AT it_min INTO wa_min.
            APPEND wa_min TO it_min_bk.
          ENDLOOP.

        ELSE.

*---  5 componentes

          IF lines EQ 5.
            CLEAR xcombi_aux.

            CLEAR: xcond_min.
            xcombi = 100000.
            CLEAR: wa_combi_final-msg.
            PERFORM process_it_min.
            CLEAR:  zajustou..

            CLEAR: wa_combi_final-msg.
            xcombi = 88888.
            PERFORM process_it_min_15.
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

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001.
            PERFORM process_combi_1.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000002.
            PERFORM process_combi_2.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000003.
            PERFORM process_combi_3.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000004.
            PERFORM process_combi_4.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000005.
            PERFORM process_combi_5.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000012.
            PERFORM process_combi_12.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000013.
            PERFORM process_combi_13.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000014.
            PERFORM process_combi_14.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000015.
            PERFORM process_combi_15.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000023.
            PERFORM process_combi_23.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000024.
            PERFORM process_combi_24.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000025.
            PERFORM process_combi_25.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000034.
            PERFORM process_combi_34.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000035.
            PERFORM process_combi_35.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000123.
            PERFORM process_combi_123.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000124.
            PERFORM process_combi_124.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000125.
            PERFORM process_combi_125.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000134.
            PERFORM process_combi_134.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000135.
            PERFORM process_combi_135.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000234.
            PERFORM process_combi_234.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000235.
            PERFORM process_combi_235.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000345.
            PERFORM process_combi_345.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001234.
            PERFORM process_combi_1234.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001235.
            PERFORM process_combi_1235.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000002345.
            PERFORM process_combi_2345.
            CLEAR:  zajustou..

            LOOP AT it_min INTO wa_min.
              APPEND wa_min TO it_min_bk.
            ENDLOOP.

          ELSE.

*---  6 componentes

            IF lines EQ 6.

              CLEAR: xcond_min.
              xcombi = 100000.
              CLEAR: wa_combi_final-msg.
              PERFORM process_it_min.
              CLEAR:  zajustou..

              CLEAR: wa_combi_final-msg.
              xcombi = 88888.
              PERFORM process_it_min_15.
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

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001.
              PERFORM process_combi_1.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002.
              PERFORM process_combi_2.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003.
              PERFORM process_combi_3.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000004.
              PERFORM process_combi_4.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000005.
              PERFORM process_combi_5.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000006.
              PERFORM process_combi_6.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000012.
              PERFORM process_combi_12.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000013.
              PERFORM process_combi_13.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000014.
              PERFORM process_combi_14.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000016.
              PERFORM process_combi_16.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000023.
              PERFORM process_combi_23.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000024.
              PERFORM process_combi_24.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000025.
              PERFORM process_combi_25.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000026.
              PERFORM process_combi_26.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000034.
              PERFORM process_combi_34.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000035.
              PERFORM process_combi_35.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000036.
              PERFORM process_combi_36.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000123.
              PERFORM process_combi_123.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000124.
              PERFORM process_combi_124.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000125.
              PERFORM process_combi_125.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000126.
              PERFORM process_combi_126.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000134.
              PERFORM process_combi_134.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000135.
              PERFORM process_combi_135.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000136.
              PERFORM process_combi_136.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000234.
              PERFORM process_combi_234.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000235.
              PERFORM process_combi_235.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000236.
              PERFORM process_combi_236.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000345.
              PERFORM process_combi_345.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000346.
              PERFORM process_combi_346.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001234.
              PERFORM process_combi_1234.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001235.
              PERFORM process_combi_1235.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001236.
              PERFORM process_combi_1236.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002345.
              PERFORM process_combi_2345.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002346.
              PERFORM process_combi_2346.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002356.
              PERFORM process_combi_2356.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003456.
              PERFORM process_combi_3456.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              LOOP AT it_min INTO wa_min.
                APPEND wa_min TO it_min_bk.
              ENDLOOP.

            ELSE.

*---  7 componentes

              IF lines EQ 7.

                CLEAR: xcond_min.
                xcombi = 100000.
                CLEAR: wa_combi_final-msg.
                PERFORM process_it_min.
                CLEAR:  zajustou..

                CLEAR: wa_combi_final-msg.
                xcombi = 88888.
                PERFORM process_it_min_15.
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

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001.
                PERFORM process_combi_1.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002.
                PERFORM process_combi_2.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003.
                PERFORM process_combi_3.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000004.
                PERFORM process_combi_4.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000005.
                PERFORM process_combi_5.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000006.
                PERFORM process_combi_6.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000007.
                PERFORM process_combi_7.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000012.
                PERFORM process_combi_12.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000013.
                PERFORM process_combi_13.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000014.
                PERFORM process_combi_14.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000016.
                PERFORM process_combi_16.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000017.
                PERFORM process_combi_17.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000023.
                PERFORM process_combi_23.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000024.
                PERFORM process_combi_24.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000025.
                PERFORM process_combi_25.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000026.
                PERFORM process_combi_26.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000027.
                PERFORM process_combi_27.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000034.
                PERFORM process_combi_34.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000035.
                PERFORM process_combi_35.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000036.
                PERFORM process_combi_36.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000037.
                PERFORM process_combi_37.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000045.
                PERFORM process_combi_45.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000046.
                PERFORM process_combi_46.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000047.
                PERFORM process_combi_47.
                CLEAR:  zajustou.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000056.
                PERFORM process_combi_56.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000057.
                PERFORM process_combi_57.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000067.
                PERFORM process_combi_67.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000123.
                PERFORM process_combi_123.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000124.
                PERFORM process_combi_124.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000125.
                PERFORM process_combi_125.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000126.
                PERFORM process_combi_126.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000127.
                PERFORM process_combi_127.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000134.
                PERFORM process_combi_134.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000135.
                PERFORM process_combi_135.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000136.
                PERFORM process_combi_136.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000137.
                PERFORM process_combi_137.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000234.
                PERFORM process_combi_234.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000235.
                PERFORM process_combi_235.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000236.
                PERFORM process_combi_236.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000237.
                PERFORM process_combi_237.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000345.
                PERFORM process_combi_345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000346.
                PERFORM process_combi_346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000347.
                PERFORM process_combi_347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000456.
                PERFORM process_combi_456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000457.
                PERFORM process_combi_457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000567.
                PERFORM process_combi_567.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001234.
                PERFORM process_combi_1234.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001235.
                PERFORM process_combi_1235.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001236.
                PERFORM process_combi_1236.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001237.
                PERFORM process_combi_1237.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001345.
                PERFORM process_combi_1345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001347.
                PERFORM process_combi_1346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001347.
                PERFORM process_combi_1347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001456.
                PERFORM process_combi_1456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001457.
                PERFORM process_combi_1457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001567.
                PERFORM process_combi_1567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002345.
                PERFORM process_combi_2345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002346.
                PERFORM process_combi_2346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002347.
                PERFORM process_combi_2347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002356.
                PERFORM process_combi_2356.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002357.
                PERFORM process_combi_2357.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002456.
                PERFORM process_combi_2456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002457.
                PERFORM process_combi_2457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10003456.
                PERFORM process_combi_3456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10003457.
                PERFORM process_combi_3457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10004567.
                PERFORM process_combi_4567.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10012345.
                PERFORM process_combi_12345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10013456.
                PERFORM process_combi_13456.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10014567.
                PERFORM process_combi_14567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10023456.
                PERFORM process_combi_23456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10024567.
                PERFORM process_combi_24567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10034567.
                PERFORM process_combi_34567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100123456.
                PERFORM process_combi_123456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100134567.
                PERFORM process_combi_134567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100234567.
                PERFORM process_combi_234567.
                CLEAR:  zajustou..

                LOOP AT it_min INTO wa_min.
                  APPEND wa_min TO it_min_bk.
                ENDLOOP.

              ENDIF.
            ENDIF.
          ENDIF.
****---
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.
****
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

ENDFORM.                    " CONDICAO_MINIMO_NIVEL01
*&---------------------------------------------------------------------*
*&      Form  CONDICAO_MINIMO_NIVEL02
*&---------------------------------------------------------------------*
FORM condicao_minimo_nivel02 RAISING /qaps/cx_general.

  DATA l_tabix TYPE sy-tabix.

*-- Q&A Systems - 18.07.2011
*---  MARCA OS MATERIAIS INCOMPATIVEIS PARA DESMARCAR
  CLEAR it_desmarcar[].
  REFRESH it_desmarcar.

  PERFORM update_restricao.

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
*-- Q&A Systems - 18.07.2011

  DELETE it_zmicig0 WHERE /qaps/cignr =  xcombi_min.
  DELETE it_zmicig1 WHERE /qaps/cignr =  xcombi_min.
  DELETE it_zmicig2 WHERE /qaps/cignr =  xcombi_min.
  DELETE it_zmicig0_min WHERE /qaps/cignr =  xcombi_min.
  DELETE it_zmicig1_min WHERE /qaps/cignr =  xcombi_min.
  DELETE it_zmicig2_min WHERE /qaps/cignr =  xcombi_min.
  DELETE it_combi_final WHERE combi =  xcombi_min.

**----cond mínimo
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
    PERFORM process_it_min.
    CLEAR:  zajustou..
    wa_msg0 = wa_combi_final-msg.

    CLEAR: wa_combi_final-msg, wa_msg15.
    xcond_min15 = ' '.
    xcombi = wa_combi_finalx-combi + 200.
    PERFORM process_it_min_15.
    CLEAR:  zajustou.
    wa_msg15 = wa_combi_final-msg.

    LOOP AT it_min INTO wa_min.
      APPEND wa_min TO it_min_bk.
    ENDLOOP.

  ELSE.

*---  2 componentes

    IF lines = 2.

      CLEAR xcombi_aux.
      CLEAR: xcond_min.

      xcombi = xcombi  + 10 .
      CLEAR: wa_combi_final-msg.
      it_pc_bk = it_pc.
      PERFORM process_it_min.
      CLEAR:  zajustou..

      xcombi = xcombi + 10.
      CLEAR: wa_combi_final-msg.
      PERFORM process_it_min_15.
      CLEAR:  zajustou..

      READ TABLE it_min INTO wa_min INDEX 1.
      aux1 = wa_min-comnr.

      READ TABLE it_min INTO wa_min INDEX 2.
      aux2 = wa_min-comnr.

      PERFORM update_restricao.

      CLEAR: wa_combi_final-msg.
      xcombi = xcombi + 100.
      PERFORM process_combi_1.
      CLEAR:  zajustou..

      PERFORM update_restricao.

      CLEAR: wa_combi_final-msg.
      xcombi = xcombi + 1111.
      PERFORM process_combi_2.
      CLEAR:  zajustou..

      PERFORM update_restricao.

      LOOP AT it_min INTO wa_min.
        APPEND wa_min TO it_min_bk.
      ENDLOOP.

    ELSE.

*---  3 componentes

      IF lines = 3.
        CLEAR xcombi_aux.
        CLEAR: xcond_min.
        xcombi = xcombi + 1000.
        CLEAR: wa_combi_final-msg.
        PERFORM process_it_min.
        CLEAR:  zajustou..

        CLEAR: wa_combi_final-msg.
        xcombi =  xcombi + 888  + 1033.
        PERFORM process_it_min_15.
        CLEAR:  zajustou..

        READ TABLE it_min INTO wa_min INDEX 1.
        aux1 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 2.
        aux2 = wa_min-comnr.

        READ TABLE it_min INTO wa_min INDEX 3.
        aux3 = wa_min-comnr.

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30001  + 1033.
        PERFORM process_combi_1.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30002  + 1033.
        PERFORM process_combi_2.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 30003  + 1033.
        PERFORM process_combi_3.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 200012  + 1033.
        PERFORM process_combi_12.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100013  + 1033.
        PERFORM process_combi_13.
        CLEAR:  zajustou..

        PERFORM update_restricao.

        CLEAR: wa_combi_final-msg.
        xcombi = xcombi + 100023  + 1033.
        PERFORM process_combi_23.
        CLEAR:  zajustou..

        LOOP AT it_min INTO wa_min.
          APPEND wa_min TO it_min_bk.
        ENDLOOP.

      ELSE.

*---  4 componentes

        IF lines EQ 4.
          CLEAR xcombi_aux.
          CLEAR: xcond_min.
          xcombi = 10000.

          CLEAR: wa_combi_final-msg.
          PERFORM process_it_min.

          CLEAR: wa_combi_final-msg.
          xcombi = 88888.
          PERFORM process_it_min_15.
***--
          READ TABLE it_min INTO wa_min INDEX 1.
          aux1 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 2.
          aux2 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 3.
          aux3 = wa_min-comnr.

          READ TABLE it_min INTO wa_min INDEX 4.
          aux4 = wa_min-comnr.

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100001.
          PERFORM process_combi_1.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100002.
          PERFORM process_combi_2.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100003.
          PERFORM process_combi_3.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100004.
          PERFORM process_combi_4.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100012.
          PERFORM process_combi_12.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100013.
          PERFORM process_combi_13.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100014.
          PERFORM process_combi_14.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100023.
          PERFORM process_combi_23.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100024.
          PERFORM process_combi_24.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 100034.
          PERFORM process_combi_34.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000123.
          PERFORM process_combi_123.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000124.
          PERFORM process_combi_124.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000134.
          PERFORM process_combi_134.
          CLEAR:  zajustou..

          PERFORM update_restricao.

          CLEAR: wa_combi_final-msg.
          xcombi = 10000234.
          PERFORM process_combi_234.
          CLEAR:  zajustou..

          LOOP AT it_min INTO wa_min.
            APPEND wa_min TO it_min_bk.
          ENDLOOP.

        ELSE.

*---  5 componentes

          IF lines EQ 5.
            CLEAR xcombi_aux.

            CLEAR: xcond_min.
            xcombi = 100000.
            CLEAR: wa_combi_final-msg.
            PERFORM process_it_min.
            CLEAR:  zajustou..

            CLEAR: wa_combi_final-msg.
            xcombi = 88888.
            PERFORM process_it_min_15.
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

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001.
            PERFORM process_combi_1.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000002.
            PERFORM process_combi_2.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000003.
            PERFORM process_combi_3.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000004.
            PERFORM process_combi_4.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000005.
            PERFORM process_combi_5.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000012.
            PERFORM process_combi_12.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000013.
            PERFORM process_combi_13.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000014.
            PERFORM process_combi_14.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000015.
            PERFORM process_combi_15.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000023.
            PERFORM process_combi_23.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000024.
            PERFORM process_combi_24.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000025.
            PERFORM process_combi_25.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000034.
            PERFORM process_combi_34.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 10000035.
            PERFORM process_combi_35.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000123.
            PERFORM process_combi_123.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000124.
            PERFORM process_combi_124.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000125.
            PERFORM process_combi_125.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000134.
            PERFORM process_combi_134.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000135.
            PERFORM process_combi_135.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000234.
            PERFORM process_combi_234.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000235.
            PERFORM process_combi_235.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 100000345.
            PERFORM process_combi_345.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001234.
            PERFORM process_combi_1234.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000001235.
            PERFORM process_combi_1235.
            CLEAR:  zajustou..

            PERFORM update_restricao.

            CLEAR: wa_combi_final-msg.
            xcombi = 1000002345.
            PERFORM process_combi_2345.
            CLEAR:  zajustou..

            LOOP AT it_min INTO wa_min.
              APPEND wa_min TO it_min_bk.
            ENDLOOP.

          ELSE.

*---  6 componentes

            IF lines EQ 6.

              CLEAR: xcond_min.
              xcombi = 100000.
              CLEAR: wa_combi_final-msg.
              PERFORM process_it_min.
              CLEAR:  zajustou..

              CLEAR: wa_combi_final-msg.
              xcombi = 88888.
              PERFORM process_it_min_15.
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

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001.
              PERFORM process_combi_1.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002.
              PERFORM process_combi_2.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003.
              PERFORM process_combi_3.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000004.
              PERFORM process_combi_4.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000005.
              PERFORM process_combi_5.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000006.
              PERFORM process_combi_6.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000012.
              PERFORM process_combi_12.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000013.
              PERFORM process_combi_13.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000014.
              PERFORM process_combi_14.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000016.
              PERFORM process_combi_16.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000023.
              PERFORM process_combi_23.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000024.
              PERFORM process_combi_24.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000025.
              PERFORM process_combi_25.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000026.
              PERFORM process_combi_26.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000034.
              PERFORM process_combi_34.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000035.
              PERFORM process_combi_35.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 10000036.
              PERFORM process_combi_36.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000123.
              PERFORM process_combi_123.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000124.
              PERFORM process_combi_124.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000125.
              PERFORM process_combi_125.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000126.
              PERFORM process_combi_126.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000134.
              PERFORM process_combi_134.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000135.
              PERFORM process_combi_135.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000136.
              PERFORM process_combi_136.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000234.
              PERFORM process_combi_234.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000235.
              PERFORM process_combi_235.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000236.
              PERFORM process_combi_236.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000345.
              PERFORM process_combi_345.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 100000346.
              PERFORM process_combi_346.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001234.
              PERFORM process_combi_1234.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001235.
              PERFORM process_combi_1235.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000001236.
              PERFORM process_combi_1236.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002345.
              PERFORM process_combi_2345.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002346.
              PERFORM process_combi_2346.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000002356.
              PERFORM process_combi_2356.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              CLEAR: wa_combi_final-msg.
              xcombi = 1000003456.
              PERFORM process_combi_3456.
              CLEAR:  zajustou..

              PERFORM update_restricao.

              LOOP AT it_min INTO wa_min.
                APPEND wa_min TO it_min_bk.
              ENDLOOP.

            ELSE.

*---  7 componentes

              IF lines EQ 7.

                CLEAR: xcond_min.
                xcombi = 100000.
                CLEAR: wa_combi_final-msg.
                PERFORM process_it_min.
                CLEAR:  zajustou..

                CLEAR: wa_combi_final-msg.
                xcombi = 88888.
                PERFORM process_it_min_15.
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

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001.
                PERFORM process_combi_1.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000002.
                PERFORM process_combi_2.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000003.
                PERFORM process_combi_3.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000004.
                PERFORM process_combi_4.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000005.
                PERFORM process_combi_5.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000006.
                PERFORM process_combi_6.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000007.
                PERFORM process_combi_7.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000012.
                PERFORM process_combi_12.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000013.
                PERFORM process_combi_13.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000014.
                PERFORM process_combi_14.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000016.
                PERFORM process_combi_16.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000017.
                PERFORM process_combi_17.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000023.
                PERFORM process_combi_23.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000024.
                PERFORM process_combi_24.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000025.
                PERFORM process_combi_25.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000026.
                PERFORM process_combi_26.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000027.
                PERFORM process_combi_27.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000034.
                PERFORM process_combi_34.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000035.
                PERFORM process_combi_35.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000036.
                PERFORM process_combi_36.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000037.
                PERFORM process_combi_37.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000045.
                PERFORM process_combi_45.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000046.
                PERFORM process_combi_46.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000047.
                PERFORM process_combi_47.
                CLEAR:  zajustou.

                CLEAR: wa_combi_final-msg.
                xcombi = 10000056.
                PERFORM process_combi_56.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000057.
                PERFORM process_combi_57.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000067.
                PERFORM process_combi_67.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000123.
                PERFORM process_combi_123.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000124.
                PERFORM process_combi_124.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000125.
                PERFORM process_combi_125.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000126.
                PERFORM process_combi_126.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000127.
                PERFORM process_combi_127.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000134.
                PERFORM process_combi_134.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000135.
                PERFORM process_combi_135.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000136.
                PERFORM process_combi_136.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000137.
                PERFORM process_combi_137.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000234.
                PERFORM process_combi_234.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000235.
                PERFORM process_combi_235.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000236.
                PERFORM process_combi_236.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000237.
                PERFORM process_combi_237.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000345.
                PERFORM process_combi_345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000346.
                PERFORM process_combi_346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000347.
                PERFORM process_combi_347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000456.
                PERFORM process_combi_456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000457.
                PERFORM process_combi_457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100000567.
                PERFORM process_combi_567.
                CLEAR:  zajustou..


                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 1000001234.
                PERFORM process_combi_1234.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001235.
                PERFORM process_combi_1235.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001236.
                PERFORM process_combi_1236.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001237.
                PERFORM process_combi_1237.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001345.
                PERFORM process_combi_1345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001347.
                PERFORM process_combi_1346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001347.
                PERFORM process_combi_1347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001456.
                PERFORM process_combi_1456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001457.
                PERFORM process_combi_1457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10001567.
                PERFORM process_combi_1567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002345.
                PERFORM process_combi_2345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002346.
                PERFORM process_combi_2346.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002347.
                PERFORM process_combi_2347.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002356.
                PERFORM process_combi_2356.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002357.
                PERFORM process_combi_2357.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002456.
                PERFORM process_combi_2456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10002457.
                PERFORM process_combi_2457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10003456.
                PERFORM process_combi_3456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10003457.
                PERFORM process_combi_3457.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10004567.
                PERFORM process_combi_4567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10012345.
                PERFORM process_combi_12345.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10013456.
                PERFORM process_combi_13456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10014567.
                PERFORM process_combi_14567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10023456.
                PERFORM process_combi_23456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10024567.
                PERFORM process_combi_24567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 10034567.
                PERFORM process_combi_34567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100123456.
                PERFORM process_combi_123456.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100134567.
                PERFORM process_combi_134567.
                CLEAR:  zajustou..

                PERFORM update_restricao.

                CLEAR: wa_combi_final-msg.
                xcombi = 100234567.
                PERFORM process_combi_234567.
                CLEAR:  zajustou..

                LOOP AT it_min INTO wa_min.
                  APPEND wa_min TO it_min_bk.
                ENDLOOP.

              ENDIF.
            ENDIF.
          ENDIF.
****---

        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.
****

  DELETE it_combi_final WHERE costt = 0.
  SORT it_combi_final BY costt combi.

*  PERFORM zf_validar_formulas_sem_minimo.

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

*-- VERIFICA SE AS SOLUCOES CONTEM ALGUM MATERIAL QUE ESTA NA IT_MIN_BK -- INICIO
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
      PERFORM verifica_cond_minimo.
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

*--
  DELETE it_combi_final WHERE costt = 0.
  SORT it_combi_final BY costt combi.
  DELETE  ADJACENT DUPLICATES FROM it_combi_final COMPARING costt.


  READ TABLE it_combi_final INTO wa_combi_final  INDEX 1.
  xcombi = wa_combi_final-combi.
  CLEAR  it_min[].

*-- VERIFICA SE AS SOLUCOES CONTEM ALGUM MATERIAL QUE ESTA NA IT_MIN_BK--  FIM
  PERFORM verificar_condicoes.

ENDFORM.                    " CONDICAO_MINIMO_NIVEL02
FORM verificar_condicoes.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TEST_IT_MIN15
*&---------------------------------------------------------------------*
FORM test_it_min15.

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

ENDFORM.                    " TEST_IT_MIN15
*&---------------------------------------------------------------------*
*&      Form  RESULTADO
*&---------------------------------------------------------------------*
FORM resultado CHANGING cv_has_result TYPE abap_bool
                  RAISING /qaps/cx_formulation_error
                          /qaps/cx_div_no_result
                          /qaps/cx_general.

  DATA: wa_rest_comp_aux TYPE ty_rest_comp,
        wa_rest_comp     TYPE ty_rest_comp.

  DATA: lt_mimi TYPE TABLE OF /qaps/zmimi,
        ls_mimi TYPE /qaps/zmimi.

  DATA  l_tabix  TYPE sy-tabix.
  DATA ls_dif TYPE ty_dif.
  DATA: lt_zmicig2 TYPE TABLE OF /qaps/zmicig2
                      WITH KEY matnr werks /qaps/grkey /qaps/cignr,
        ls_zmicig2 TYPE /qaps/zmicig2.
*--
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
*            xcmeng_min = wa_zmimi-/qaps/rmeng.
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
***  Resultado.

    IF sy-index >= 25.

*      BREAK-POINT.

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

      PERFORM verifica_niveis_garantia_2.

      IF  it_dif[]  IS INITIAL.
        PERFORM zf_ajusta_garantia.
        EXIT.
      ENDIF.

*--  tira loop    19.07.2011
      l_ind =  l_ind + 1.
      IF l_ind > 40.
*-- tenta novamente   21.07.20011
        PERFORM zf_refaz_ate_imcompatib.
*-- Verifica se tem alguma solucao que atendeu tudo   21.07.20011
        PERFORM  zf_verifica_tudo.
        IF  zdeusolucao NE c_x.
          CLEAR it_combi_final[].
          CLEAR it_zmicig0[].
          CLEAR it_zmicig1[].
          CLEAR it_zmicig2[].
          CLEAR it_zmimi[].
          EXIT.
        ENDIF.
*-- tenta novamente   21.07.20011
      ENDIF.
*--  tira loop    19.07.2011

      CLEAR it_rest_comp_2[].

      IF NOT it_dif[]  IS INITIAL.
        PERFORM  ajusta_margem_seguranca_2.
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
*--Q&A Systems  restrição de componente maior que a condição de minimo  --  inicio   16.07
*              APPEND wa_rest_comp TO it_rest_comp_2.
              READ TABLE it_rest_comp_bk INTO wa_rest_comp_aux WITH KEY /qaps/comnr = wa_pc_bk-comnr.
              IF sy-subrc = 0.

                APPEND wa_rest_comp TO it_rest_comp_2.

              ELSE.
                APPEND wa_rest_comp TO it_rest_comp_2.
              ENDIF.
*--Q&A Systems restrição de componente maior que a condição de minimo  --  fim
            ENDIF.
          ELSE.
            wa_pc_bk-flgut =  ' '.
            MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
          ENDIF.
        ENDLOOP.

        APPEND LINES OF it_rest_comp_bk TO it_rest_comp_2.
        PERFORM ajustar_limites_rest_comp CHANGING it_rest_comp_2.

        DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
        DELETE it_combi_final WHERE combi = xcombi.

        PERFORM clear_workarea.
        PERFORM load_internal_tables.

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

        PERFORM fill_matrix_a USING c y.
        PERFORM load_le_restrictions.
        PERFORM load_eq_restrictions.
        PERFORM load_ge_restrictions.
        PERFORM load_cuts.
        PERFORM stage_590.
        PERFORM modifies_matrix.

        DO.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_1000_min.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_2000.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_3000.
          ENDIF.

          IF NOT wa_combi_final-msg IS INITIAL.
            EXIT.
          ENDIF.

        ENDDO.

      ENDIF.

    ELSE.
      EXIT.
    ENDIF.
*---
  ENDDO.

*  break c060863.

  DATA l_lines_combi    TYPE sy-tabix.

  DESCRIBE TABLE it_combi_final LINES  l_lines_combi.

*--  verifica se o ajuste nos niveis de garantia queimou a cond de min.
  IF NOT it_zmimi[] IS INITIAL.
    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
    xcombi = wa_zmicig0-/qaps/cignr.
    IF  sy-subrc = 0.
      CLEAR it_min[].
      PERFORM verifica_cond_minimo.
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
**********************************************************
*              PERFORM resultado_2 CHANGING cv_has_result.
*              IF cv_has_result = abap_true.
*                RETURN.
*              ENDIF.  "07.03.2012 início]
*              IF lines( it_dif ) > 0.
*              BREAK c060863.
              lt_zmicig2[] = it_zmicig2[].
              DELETE lt_zmicig2 WHERE /qaps/chkey <> 'N' AND /qaps/chkey <> 'P' .

              SORT lt_zmicig2 BY /qaps/gmeng DESCENDING /qaps/chkey DESCENDING.

              READ TABLE lt_zmicig2 INDEX 1 INTO ls_zmicig2.

              RAISE EXCEPTION TYPE /qaps/cx_formulation_error
                EXPORTING
                  message = 'Reformulação'
                  chkey   = ls_zmicig2-/qaps/chkey.
***********************************************************
            ENDIF.
          ELSE.
            DELETE it_zmicig0 WHERE /qaps/costt = 0.
            SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
            READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
            IF  sy-subrc = 0.
              PERFORM resultado_3 CHANGING cv_has_result.

              IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*----

  IF NOT it_dif[]  IS INITIAL.
*-- Alterado por Q&A Systems -- 07.03.2012  - inicio
    IF NOT it_zmicig0_aux[]  IS INITIAL.
      it_zmicig0[] = it_zmicig0_aux[].
      it_zmicig1[] = it_zmicig1_aux[].
      it_zmicig2[] = it_zmicig2_aux[].
      PERFORM resultado_4 CHANGING cv_has_result.

      IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
        RETURN.
      ENDIF.
    ELSE.
*-- Alterado por Q&A Systems -- 07.03.2012  - Fim
      zsemsolucao = 'X'.
      MESSAGE e133 INTO DATA(lv_message_error).
      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
        EXPORTING
          message = lv_message_error.
*-- Alterado por Q&A Systems  -- 07.03.2012  - INICIO
    ENDIF.
*-- Alterado por Q&A Systems -- 07.03.2012  - Fim
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
*-- Alterado por Q&A Systems -- 07.03.2012  - INICIO
      IF NOT it_zmicig0_aux[]  IS INITIAL.
        it_zmicig0[] = it_zmicig0_aux[].
        it_zmicig1[] = it_zmicig1_aux[].
        it_zmicig2[] = it_zmicig2_aux[].

        PERFORM resultado_3 CHANGING cv_has_result.

        IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
          RETURN.
        ENDIF.
      ELSE.
*-- Alterado por Q&A Systems -- 07.03.2012  - Fim
        zsemsolucao = 'X'.
        MESSAGE e133 INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
*-- Alterado por Q&A Systems -- 07.03.2012  - INICIO

      ENDIF.
*-- Alterado por Q&A Systems -- 07.03.2012  - Fim
    ELSE.
      PERFORM  load_results_2.

    ENDIF.

*  CALL SCREEN 0200.
    IF lines( it_combi_final ) > 0.
      PERFORM save_snapshot.
      cv_has_result = abap_true.
    ELSE.
      MESSAGE e133 INTO lv_message_error.
      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
        EXPORTING
          message = lv_message_error.
    ENDIF.

  ENDIF.

ENDFORM.                    " RESULTADO
*&---------------------------------------------------------------------*
*&      Form  RESULTADO_2
*&---------------------------------------------------------------------*
*FORM resultado_2 CHANGING cv_has_result TYPE abap_bool
*                  RAISING /qaps/cx_formulation_error
*                         /qaps/cx_div_no_result.
*
*  DATA wa_rest_comp     TYPE ty_rest_comp.
*
*  DATA  l_tabix  TYPE sy-tabix.
*
*  BREAK c060863.
**  CLEAR it_zmi01[].
**  SELECT * INTO TABLE it_zmi01 FROM /qaps/zmi01
**   WHERE matnr = /qaps/zmidfcig-matnr AND
**         werks = /qaps/zmidfcig-werks AND
**         /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
*
*  it_zmi01_bk[] = it_zmi01[].
*
*  DO.
****  Resultado.
*
*    DESCRIBE TABLE it_zmicig0 LINES lines.
*
*    IF lines GT 0.
*
*      DELETE it_zmicig0 WHERE /qaps/costt = 0.
*      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
*      READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
*      xcombi = wa_zmicig0-/qaps/cignr.
*
*      DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
*      DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
*      DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.
*
*      PERFORM verifica_niveis_garantia_2.
*
*      IF  it_dif[]  IS INITIAL.
*        EXIT.
*      ENDIF.
*
*      CLEAR it_rest_comp_2[].
*
*      IF NOT it_dif[]  IS INITIAL.
*        PERFORM  ajusta_margem_seguranca_2.
*        LOOP AT it_pc_bk INTO wa_pc_bk.
*          l_tabix = sy-tabix.
*          READ TABLE it_zmicig1 INTO wa_zmicig1 WITH KEY /qaps/comnr = wa_pc_bk-comnr.
*          IF sy-subrc = 0 AND wa_zmicig1-/qaps/flgut EQ 'X'.
*            wa_pc_bk-flgut =  'X'.
*            MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
*            CLEAR wa_rest_comp.
*            wa_rest_comp-matnr = /qaps/zmidfcig-matnr.
*            wa_rest_comp-werks = /qaps/zmidfcig-werks.
*            wa_rest_comp-/qaps/grkey = /qaps/zmidfcig-/qaps/grkey.
*            wa_rest_comp-/qaps/comnr = wa_pc_bk-comnr.
*            READ TABLE it_zmimi INTO wa_zmimi WITH KEY matnr = wa_pc_bk-comnr.
*            IF sy-subrc = 0.
*              xcmeng_min = wa_zmimi-/qaps/rmeng.
*              wa_rest_comp-/qaps/gemng = xcmeng_min.
**--Q&A Systems  restrição de componente maior que a condição de minimo  --  inicio   16.07
**              APPEND wa_rest_comp TO it_rest_comp_2.
*              READ TABLE it_rest_comp_bk INTO wa_rest_comp_aux WITH KEY /qaps/comnr = wa_pc_bk-comnr.
*              IF sy-subrc = 0.
*                IF wa_rest_comp_aux-/qaps/eqmng GE wa_rest_comp-/qaps/gemng OR
*                   wa_rest_comp_aux-/qaps/gemng GE wa_rest_comp-/qaps/gemng.
*                  APPEND wa_rest_comp TO it_rest_comp_2.
*                ELSE.
*                  CLEAR: wa_rest_comp.
*                ENDIF.
*              ELSE.
*                APPEND wa_rest_comp TO it_rest_comp_2.
*              ENDIF.
**--Q&A Systems restrição de componente maior que a condição de minimo  --  fim
*            ENDIF.
*          ELSE.
*            wa_pc_bk-flgut =  ' '.
*            MODIFY it_pc_bk FROM wa_pc_bk INDEX l_tabix.
*          ENDIF.
*        ENDLOOP.
*
*        DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
*        DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
*        DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
*        DELETE it_combi_final WHERE combi = xcombi.
*
*        PERFORM clear_workarea.
*        PERFORM load_internal_tables.
*
*        tp = -1.
*        nv = wa_ct_comp.
*        r1 = wa_ct_rest_le.
*        r2 = wa_ct_rest_eq.
*        r3 = wa_ct_rest_ge.
*        x  = r1 + r2 + r3.
*        a  = nv + 1.
*        y  = x  + r3 + a.
*        b  = y  - 1.
*        c  = x  + 1.
*        z  = a  - 1.
*
*        PERFORM fill_matrix_a USING c y.
*
*        PERFORM load_le_restrictions.
*
*        PERFORM load_eq_restrictions.
*
*        PERFORM load_ge_restrictions.
*
*        PERFORM load_cuts.
*
*        PERFORM stage_590.
**
*        PERFORM modifies_matrix.
*
*        DO.
*
*          IF wa_combi_final-msg IS INITIAL.
*            PERFORM stage_1000_min.
*          ENDIF.
*
*          IF wa_combi_final-msg IS INITIAL.
*            PERFORM stage_2000.
*          ENDIF.
*
*          IF wa_combi_final-msg IS INITIAL.
*            PERFORM stage_3000.
*          ENDIF.
*
*          IF NOT wa_combi_final-msg IS INITIAL.
*            EXIT.
*          ENDIF.
*
*        ENDDO.
*
*      ENDIF.
*    ENDIF.
**---
*  ENDDO.
*
**----
*
*  IF NOT it_dif[]  IS INITIAL.
**-- Alterado por Q&A Systems -- 07.03.2012  - inicio
*    IF NOT it_zmicig0_aux[]  IS INITIAL.
*      it_zmicig0[] = it_zmicig0_aux[].
*      it_zmicig1[] = it_zmicig1_aux[].
*      it_zmicig2[] = it_zmicig2_aux[].
*      PERFORM resultado_3 CHANGING cv_has_result.
*
*      IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
*        RETURN.
*      ENDIF.
*    ELSE.
**-- Alterado por Q&A Systems -- 07.03.2012  - Fim
*      zsemsolucao = 'X'.
*      MESSAGE e133 INTO DATA(lv_message_error).
*      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
*        EXPORTING
*          message = lv_message_error.
**-- Alterado por Q&A Systems  -- 07.03.2012  - INICIO
*    ENDIF.
**-- Alterado por Q&A Systems -- 07.03.2012  - Fim
*  ELSE.
*    DESCRIBE TABLE it_zmicig0 LINES lines.
*
*    DELETE it_zmicig0 WHERE /qaps/costt = 0.
*    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
*    READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
*    xcombi = wa_zmicig0-/qaps/cignr.
*
*    DELETE it_zmicig0_min WHERE /qaps/cignr <> xcombi.
*    DELETE it_zmicig1_min WHERE /qaps/cignr <> xcombi.
*    DELETE it_zmicig2_min WHERE /qaps/cignr <> xcombi.
*    DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
*    DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
*    DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.
*
*    IF lines = 0.
**-- Alterado por Q&A Systems -- 07.03.2012  - INICIO
*      IF NOT it_zmicig0_aux[]  IS INITIAL.
*        it_zmicig0[] = it_zmicig0_aux[].
*        it_zmicig1[] = it_zmicig1_aux[].
*        it_zmicig2[] = it_zmicig2_aux[].
*
*        PERFORM resultado_3 CHANGING cv_has_result.
*
*        IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
*          RETURN.
*        ENDIF.
*      ELSE.
*
*      ENDIF.
**-- Alterado por Q&A Systems -- 07.03.2012  - Fim
*      zsemsolucao = 'X'.
*      MESSAGE e133 INTO lv_message_error.
*      RAISE EXCEPTION TYPE /qaps/cx_div_no_result
*        EXPORTING
*          message = lv_message_error.
*    ELSE.
**-- verifica cond. de minimo
*      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
*      READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
*      IF  sy-subrc = 0.
*        PERFORM verifica_cond_minimo.
*        IF  it_min[] IS INITIAL.
*        ELSE.
*          LOOP AT it_min INTO wa_min.
*            LOOP AT it_pc INTO wa_pc
*              WHERE comnr =  wa_min-comnr.
*              wa_pc-flgut = ' '.
*              MODIFY it_pc FROM wa_pc INDEX sy-tabix.
*            ENDLOOP.
*          ENDLOOP.
*          PERFORM clear_workarea.
*          CLEAR: it_pc_bk[], zajustou.
*          CLEAR: it_re[], it_zmi61[], it_combi_final[],
*                 it_zmicig0[], it_zmicig1[], it_zmicig2[],
*                 it_zmicig0_min[], it_zmicig1_min[], it_zmicig2_min[].
*          LOOP AT it_pc INTO wa_pc WHERE flgut = 'X'.
*            wa_re-comnr = wa_pc-comnr.
*            wa_re-coktx = wa_pc-coktx.
*            APPEND wa_re TO it_re.
*          ENDLOOP.
*          PERFORM load_matrix CHANGING cv_has_result.
*          CLEAR it_min[].
*        ENDIF.
*      ENDIF.
*      PERFORM  load_results_2.
*
*    ENDIF.
*
**  CALL SCREEN 0200.
*    PERFORM save_snapshot.
*    cv_has_result = abap_true.
*
*  ENDIF.
*
*ENDFORM.                    " RESULTADO_2
*&---------------------------------------------------------------------*
*&      Form  RESULTADO_3
*&---------------------------------------------------------------------*
FORM resultado_3 CHANGING cv_has_result TYPE abap_bool
                  RAISING /qaps/cx_formulation_error
                          /qaps/cx_div_no_result
                          /qaps/cx_general.
*
  DATA  l_tabix  TYPE sy-tabix.
*
  CLEAR it_zmi01[].
  SELECT * INTO TABLE it_zmi01 FROM /qaps/zmi01
   WHERE matnr = /qaps/zmidfcig-matnr AND
         werks = /qaps/zmidfcig-werks AND
         /qaps/grkey = /qaps/zmidfcig-/qaps/grkey.

  it_zmi01_bk[] = it_zmi01[].

*-- para cotacao
  it_rs[] = it_rs_bk[].

*-- verifica cond. de minimo
  SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
  READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
  IF  sy-subrc = 0.

    wa_combi_finalx-combi = wa_zmicig0-/qaps/cignr.

    PERFORM verifica_cond_minimo.
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
      PERFORM clear_workarea.
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
      PERFORM load_matrix CHANGING cv_has_result.
    ENDIF.
  ENDIF.

ENDFORM.                    " RESULTADO_3
*&---------------------------------------------------------------------*
*&      Form  RESULTADO_4
*&---------------------------------------------------------------------*
FORM resultado_4 CHANGING cv_has_result TYPE abap_bool
                  RAISING /qaps/cx_formulation_error
                         /qaps/cx_div_no_result
                         /qaps/cx_general.

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

*--
*  DESCRIBE TABLE IT_ZMICIG0 LINES LINES.
*
*  IF LINES GT 1.
*    LOOP AT IT_ZMICIG0 INTO WA_ZMICIG0.
*      L_TABIX  = SY-TABIX.
*      LOOP AT IT_ZMICIG1 INTO WA_ZMICIG1 WHERE /QAPS/CIGNR = WA_ZMICIG0-/QAPS/CIGNR.
*        CLEAR: XCMENG_MIN.
*        READ TABLE IT_ZMIMI INTO WA_ZMIMI WITH KEY MATNR = WA_ZMICIG1-/QAPS/COMNR.
*        IF SY-SUBRC = 0.
*          XCMENG_MIN = WA_ZMIMI-/QAPS/RMENG.
*          IF WA_ZMICIG1-/QAPS/CMENG <> 0 AND
*             WA_ZMICIG1-/QAPS/CMENG < XCMENG_MIN.
*            DELETE  IT_ZMICIG0 INDEX L_TABIX.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.


    DATA  l_ind  TYPE sy-tabix.

    DO.
***  Resultado.

      DESCRIBE TABLE it_zmicig0 LINES lines.

      IF lines GT 0.

        DELETE it_zmicig0 WHERE /qaps/costt = 0.
        SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
        READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
        xcombi = wa_zmicig0-/qaps/cignr.

        DELETE it_zmicig0 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr <> xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr <> xcombi.

        PERFORM verifica_niveis_garantia_2.

        IF  it_dif[]  IS INITIAL.
          EXIT.
        ENDIF.

*--  tira loop    19.07.2011
        l_ind =  l_ind + 1.
        IF l_ind > 40.
*-- tenta novamente   21.07.20011
          PERFORM zf_refaz_ate_imcompatib.
*-- Verifica se tem alguma solucao que atendeu tudo   21.07.20011
          PERFORM  zf_verifica_tudo.
          IF  zdeusolucao NE c_x.
            CLEAR it_combi_final[].
            CLEAR it_zmicig0[].
            CLEAR it_zmicig1[].
            CLEAR it_zmicig2[].
            CLEAR it_zmimi[].
            EXIT.
          ENDIF.
*-- tenta novamente   21.07.20011
        ENDIF.
*--  tira loop    19.07.2011

        CLEAR it_rest_comp_2[].

        IF NOT it_dif[]  IS INITIAL.
          PERFORM  ajusta_margem_seguranca_2.
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
*--Q&A Systems  restrição de componente maior que a condição de minimo  --  inicio   16.07
*              APPEND wa_rest_comp TO it_rest_comp_2.
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
*--Q&A Systems restrição de componente maior que a condição de minimo  --  fim
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

          PERFORM clear_workarea.
          PERFORM load_internal_tables.

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

          PERFORM fill_matrix_a USING c y.

          PERFORM load_le_restrictions.

          PERFORM load_eq_restrictions.

          PERFORM load_ge_restrictions.

          PERFORM load_cuts.

          PERFORM stage_590.
*
          PERFORM modifies_matrix.

          DO.

            IF wa_combi_final-msg IS INITIAL.
              PERFORM stage_1000_min.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              PERFORM stage_2000.
            ENDIF.

            IF wa_combi_final-msg IS INITIAL.
              PERFORM stage_3000.
            ENDIF.

            IF NOT wa_combi_final-msg IS INITIAL.
              EXIT.
            ENDIF.

          ENDDO.

        ENDIF.

      ELSE.
        EXIT.
      ENDIF.
*---
    ENDDO.

    DATA l_lines_combi    TYPE sy-tabix.

    DESCRIBE TABLE it_combi_final LINES  l_lines_combi.

*--  verifica se o ajuste nos niveis de garantia queimou a cond de min.
    IF NOT it_zmimi[] IS INITIAL.
      DELETE it_zmicig0 WHERE /qaps/costt = 0.
      SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
      READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.
      xcombi = wa_zmicig0-/qaps/cignr.
      IF  sy-subrc = 0.
        CLEAR it_min[].
        PERFORM verifica_cond_minimo.
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
                PERFORM resultado_3 CHANGING cv_has_result.

                IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                  RETURN.
                ENDIF.  "07.03.2012 fim
              ENDIF.
            ELSE.
              DELETE it_zmicig0 WHERE /qaps/costt = 0.
              SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
              READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
              IF  sy-subrc = 0.
                PERFORM resultado_3  CHANGING cv_has_result.

                IF cv_has_result = abap_true." AND gv_exec_by_fm = abap_true.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*----

    IF NOT it_dif[]  IS INITIAL.
      zsemsolucao = 'X'.
      MESSAGE e133 INTO DATA(lv_message_error).
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
        MESSAGE e133 INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
      ELSE.
        PERFORM  load_results_2.
      ENDIF.

*  CALL SCREEN 0200.
      PERFORM save_snapshot.
      cv_has_result = abap_true.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " RESULTADO_4

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_COND_MINIMO
*&---------------------------------------------------------------------*
FORM verifica_cond_minimo.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  CLEAR:  ind_min,
          xcond_min,
          xcombi_n,
          xcombi,
          wa_min,
*          wa_min3,
*          wa_min4,
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
**
    CLEAR: wa_min-comnr.
*    REFRESH: it_pc_bk_01.

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

*        IF  NOT wa_rest_comp-/qaps/lemng  IS INITIAL.
*          IF wa_zmicig1-/qaps/cmeng > wa_rest_comp-/qaps/lemng.
*            xcond_min = 'X'.
*            wa_min-combi = wa_zmicig1-/qaps/cignr.
*            wa_min-comnr = wa_zmicig1-/qaps/comnr.
*            wa_min-ind = wa_min-ind + 1.
*            APPEND wa_min TO it_min.
*          ENDIF.
*        ENDIF.

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

*    data(lt_zmicig1) = it_zmicig1.
*    delete lt_zmicig1 where /qaps/cignr <> xcombi_n.
*
*    loop at it_rest_comp_2 into data(ls_rest_comp_2).
*
*      check ls_rest_comp_2-/qaps/gemng > 0 or ls_rest_comp_2-/qaps/eqmng > 0.
*      check not line_exists( lt_zmicig1[ /qaps/comnr = ls_rest_comp_2-/qaps/comnr ] ).
*
*      wa_min-combi = xcombi_n.
*      wa_min-comnr = ls_rest_comp_2-/qaps/comnr.
*      wa_min-ind = wa_min-ind + 1.
*      APPEND wa_min TO it_min.
*
*      break c060863.
*
*    ENDLOOP.

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

*----cond mínimo
  SORT it_min BY comnr.
  DELETE ADJACENT DUPLICATES FROM it_min COMPARING comnr.
  DESCRIBE TABLE it_min LINES lines.

ENDFORM.                    " VERIFICA_COND_MINIMO

*&---------------------------------------------------------------------*
*&      Form  VERIF_INCOMP_E_FORMULA
*&---------------------------------------------------------------------*
FORM verif_incomp_e_formula RAISING /qaps/cx_general.

*---
  IF  it_zmi61[] IS INITIAL.

    xcombi_n2 = xcombi.
    xcombi = xcombi_n2.

    PERFORM formulation.

    PERFORM guarantee_levels_min.

    IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
       wa_combi_final-msg IS INITIAL.
      PERFORM save_soluctions_min.
    ENDIF.

  ELSE.


*--  AUX1
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

*--  AUX2
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

*--  AUX3
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

*--  AUX4
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

*--  AUX5
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

*--  AUX6
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

*--  AUX7
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

*--- Q&A Systems 15.07

*--- Desmarca para incompatibilidades 18.07.2011
    IF NOT it_desmarcar[] IS INITIAL.
      LOOP AT it_desmarcar INTO wa_desmarcar.
        LOOP AT it_pc_bk INTO wa_pc
         WHERE comnr =  wa_desmarcar-comnr.
          wa_pc-flgut = ' '.
          MODIFY it_pc_bk FROM wa_pc INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
*--- Desmarca para incompatibilidades 18.07.2011

    xcombi_n2 = xcombi  + 55.
    xcombi = xcombi_n2.

    PERFORM clear_workarea.

    PERFORM formulation.

    PERFORM guarantee_levels_min.

    IF wa_combi_final-msg = 'SOLUÇÃO ÒTIMA' OR
       wa_combi_final-msg IS INITIAL.
      PERFORM save_soluctions_min.
    ENDIF.

  ENDIF.

ENDFORM.                    " VERIF_INCOMP_E_FORMULA
*&---------------------------------------------------------------------*
*&      Form  LOAD_RESULTS_2
*&---------------------------------------------------------------------*
FORM load_results_2 RAISING /qaps/cx_div_no_result /qaps/cx_general.

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

  PERFORM zf_verifica_rest_minimos.

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

*-----  PEGAR  DA TABELA

  READ TABLE it_zmicig0 INTO wa_zmicig0 INDEX 1.

  /qaps/zmidfcig-/qaps/costt   = wa_zmicig0-/qaps/costt.
  /qaps/zmidfcig-/qaps/costu   = wa_zmicig0-/qaps/costt / wa_zmicig0-/qaps/rmeng.
  /qaps/zmidfcig-/qaps/gerxprio = wa_zmicig0-/qaps/gerxprio.
  /qaps/zmidfcig-/qaps/mcost = wa_zmicig0-/qaps/mcost.

ENDFORM.                    " LOAD_RESULTS_2
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEIS_GARANTIA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_niveis_garantia_2 .

  DATA wa_dif   TYPE ty_dif.
  DATA vl_gmeng TYPE string.
  CLEAR:  it_dif[], wa_dif, vl_gmeng.

  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_n.
  IF sy-subrc = 0.
    vl_gmeng = wa_zmicig2-/qaps/gmeng - 0 .
    IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
    ELSE.
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
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
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng.
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
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
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

*  DATA: vl_gmeng TYPE string.
*  CLEAR:  it_dif[], wa_dif.
*
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_n.
*  IF sy-subrc = 0.
*    vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 100 ) .
*    IF ( wa_zmicig2-/qaps/cmeng GE vl_gmeng ).
*    ELSE.
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_n.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
*        wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR vl_gmeng.
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_p.
*  IF sy-subrc = 0.
*    vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 100 ) .
*    IF ( wa_zmicig2-/qaps/cmeng GE vl_gmeng ).
*    ELSE.
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_p.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
*        wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR vl_gmeng.
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_k.
*  IF sy-subrc = 0.
*    vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 100 ) .
*    IF ( wa_zmicig2-/qaps/cmeng GE vl_gmeng  ).
*    ELSE.
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_k.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng  = vl_gmeng * 1000.
*        wa_zmicig2-/qaps/cmeng  = ( wa_zmicig2-/qaps/cmeng ) / 1000 .
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.

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

ENDFORM.                    " VERIFICA_NIVEIS_GARANTIA_2
*&---------------------------------------------------------------------*
*&      Form  AJUSTA_MARGEM_SEGURANCA_2
*&---------------------------------------------------------------------*
FORM ajusta_margem_seguranca_2 .

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

ENDFORM.                    " AJUSTA_MARGEM_SEGURANCA_2
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_TUDO
*&---------------------------------------------------------------------*
FORM zf_verifica_tudo .

  it_combi_finalx[] =  it_combi_final[].
  LOOP AT it_combi_finalx INTO wa_combi_finalx.
    xcombi =  wa_combi_finalx-combi.
    xcombi_min =  wa_combi_finalx-combi.
    DELETE it_zmicig0 WHERE /qaps/costt = 0.
    SORT it_zmicig0 BY /qaps/costt /qaps/cignr.
    READ TABLE it_zmicig0 INTO wa_zmicig0 WITH KEY /qaps/cignr = xcombi.
    IF  sy-subrc = 0.
      PERFORM verifica_cond_minimo.
      IF  it_min[] IS INITIAL.
        PERFORM verifica_niveis_garantia_3.
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

ENDFORM.                    " ZF_VERIFICA_TUDO
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEIS_GARANTIA_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_niveis_garantia_3 .

  DATA wa_dif   TYPE ty_dif.
  DATA vl_gmeng TYPE string.
  CLEAR:  it_dif[], wa_dif, vl_gmeng.

  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/chkey = c_n.
  IF sy-subrc = 0.
    vl_gmeng = wa_zmicig2-/qaps/gmeng - ( 1 / 1000 ) .
    IF (  wa_zmicig2-/qaps/cmeng GE  vl_gmeng ).
    ELSE.
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
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
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng.
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
*      wa_dif-xdif = vl_gmeng - wa_zmicig2-/qaps/cmeng .
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

*  DATA: vl_gmeng TYPE string.
*  CLEAR:  it_dif[], wa_dif.
*
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
*                                                 /qaps/chkey = c_n.
*  IF sy-subrc = 0.
*    vl_gmeng =  wa_zmicig2-/qaps/gmeng - ( 1 / 100 ).
*    IF (  wa_zmicig2-/qaps/cmeng GE vl_gmeng ).
*    ELSE.
*      wa_dif-xdif =  vl_gmeng - wa_zmicig2-/qaps/cmeng.
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_n.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng = vl_gmeng * 1000 .
*        wa_zmicig2-/qaps/cmeng = ( wa_zmicig2-/qaps/cmeng ) / 1000.
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR vl_gmeng.
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
*                                                 /qaps/chkey = c_p.
*  IF sy-subrc = 0.
*    vl_gmeng =  wa_zmicig2-/qaps/gmeng - ( 1 / 100 ).
*    IF (  wa_zmicig2-/qaps/cmeng GE vl_gmeng ).
*    ELSE.
*      wa_dif-xdif =  vl_gmeng - wa_zmicig2-/qaps/cmeng.
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_p.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng = vl_gmeng * 1000 .
*        wa_zmicig2-/qaps/cmeng = ( wa_zmicig2-/qaps/cmeng ) / 1000.
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR vl_gmeng.
*  READ TABLE it_zmicig2 INTO wa_zmicig2 WITH KEY /qaps/cignr = wa_zmicig0-/qaps/cignr
*                                                 /qaps/chkey = c_k.
*  IF sy-subrc = 0.
*    vl_gmeng =  wa_zmicig2-/qaps/gmeng - ( 1 / 100 ).
*    IF (  wa_zmicig2-/qaps/cmeng GE vl_gmeng ).
*    ELSE.
*      wa_dif-xdif =  vl_gmeng - wa_zmicig2-/qaps/cmeng.
*      IF wa_dif-xdif > '0.005'.
*        wa_dif-chkey = c_k.
*        APPEND wa_dif TO it_dif.
*      ELSE.
*        wa_zmicig2-/qaps/cmeng = vl_gmeng  * 1000 .
*        wa_zmicig2-/qaps/cmeng = ( wa_zmicig2-/qaps/cmeng ) / 1000.
*        MODIFY it_zmicig2 FROM wa_zmicig2 INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDIF.

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

ENDFORM.                    " VERIFICA_NIVEIS_GARANTIA_3
*&---------------------------------------------------------------------*
*&      Form  ZF_REFAZ_ATE_IMCOMPATIB
*&---------------------------------------------------------------------*
FORM zf_refaz_ate_imcompatib RAISING /qaps/cx_general /qaps/cx_div_no_result.

  CLEAR it_combi_final[].
  CLEAR it_zmicig0[].
  CLEAR it_zmicig1[].
  CLEAR it_zmicig2[].

  PERFORM clear_workarea.
  CLEAR: wa_combi_final-msg, xcond_min, zajustou.
  CLEAR it_rest_comp_2[].
  CLEAR it_zmi60[].

  PERFORM combinations.

  REFRESH: it_pc_bk.
  it_pc_bk[] = it_pc[].
  it_rs[] = it_rs_bk[].

*--- Primeiro com todos os produtos. 1
  xcombi = '999995    '.

  wa_combi_final-combi  = xcombi.
  APPEND wa_combi_final TO it_combi_final.
  PERFORM zf_gravar_re_hist USING wa_combi_final.

  PERFORM load_internal_tables.

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

  PERFORM fill_matrix_a USING c y.
  PERFORM load_le_restrictions.
  PERFORM load_eq_restrictions.
  PERFORM load_ge_restrictions.
  PERFORM load_cuts.
  PERFORM stage_590.
  PERFORM modifies_matrix.

  DO.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_1000.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_2000.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_3000.
    ENDIF.

    IF NOT wa_combi_final-msg IS INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

  DELETE it_combi_final WHERE costt = 0.

  IF lines( it_combi_final ) = 0.
    MESSAGE e133 INTO DATA(lv_message_error).
    RAISE EXCEPTION TYPE /qaps/cx_div_no_result
      EXPORTING
        message = lv_message_error.
  ENDIF.

*  DELETE it_combi_final WHERE combi <> xcombi.

  LOOP AT it_combi_final INTO wa_combi_final.
    IF  wa_combi_final-msg = 'OCORRERAM INCOMPATIBILIDADES'.
      wa_combi_final-costt = 0.
      MODIFY it_combi_final FROM wa_combi_final.
    ENDIF.
  ENDLOOP.

*---- verifica se existe rest de incomp

  CLEAR : xcombi.
  PERFORM clear_workarea.

*-- ESQ -- 16.05.2013 - Incompatibilidades - Inicio
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

*-- ESQ -- 16.05.2013 - Incompatibilidades - Fim

  DESCRIBE TABLE it_zmi60 LINES lines.

  IF lines <> 0.

    LOOP AT it_zmi60 INTO wa_zmi60.
      CLEAR: wa_combi_final-costt.
      wa_combi_final-combi  = wa_zmi60-/qaps/mixin.
      APPEND wa_combi_final TO it_combi_final.
      PERFORM zf_gravar_re_hist USING wa_combi_final.
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

      PERFORM load_internal_tables.

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

      PERFORM fill_matrix_a USING c y.
      PERFORM load_le_restrictions.
      PERFORM load_eq_restrictions.
      PERFORM load_ge_restrictions.
      PERFORM load_cuts.
      PERFORM stage_590.
      PERFORM modifies_matrix.

      DO.

        IF wa_combi_final-msg IS INITIAL.
          PERFORM stage_1000.
        ENDIF.

        IF wa_combi_final-msg IS INITIAL.
          PERFORM stage_2000.
        ENDIF.

        IF wa_combi_final-msg IS INITIAL.
          PERFORM stage_3000.
        ENDIF.

        IF NOT wa_combi_final-msg IS INITIAL.
          EXIT.
        ENDIF.

      ENDDO.
      PERFORM clear_workarea.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " ZF_REFAZ_ATE_IMCOMPATIB

*&---------------------------------------------------------------------*
*&      Form  ZF_VER_COMP_MIN
*&---------------------------------------------------------------------*
*       Verifica se Rest.Componentes e "<" Retrições de Mínimos
*----------------------------------------------------------------------*
FORM zf_ver_comp_min RAISING /qaps/cx_general.

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
    MESSAGE e257 WITH ls_pc-comnr INTO DATA(lv_message).
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
            MESSAGE e286 WITH wa_pc-comnr.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: wa_pc,wa_restcomp, wa_zmimi.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ZF_VER_COMP_MIN


*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_REST_MINIMOS
*&---------------------------------------------------------------------*
FORM zf_verifica_rest_minimos RAISING /qaps/cx_div_no_result /qaps/cx_general.

  DATA wa_rest_comp     TYPE ty_rest_comp.

  SORT it_re BY comnr.
  SORT it_zmimi BY matnr.
  SORT it_rest_verif BY /qaps/comnr.

*-- Verifica se a Restrição de Componentes/Mínimo esta na resposta
  LOOP AT it_rest_verif INTO wa_rest_comp.
    IF wa_rest_comp-/qaps/gemng > 0 OR wa_rest_comp-/qaps/eqmng > 0.
      READ TABLE it_re INTO wa_re WITH KEY comnr = wa_rest_comp-/qaps/comnr.

      IF sy-subrc <> 0.
*        PERFORM zf_desabilita.
        MESSAGE e133 INTO DATA(lv_message_error).
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
*        PERFORM zf_desabilita.
        MESSAGE e133 INTO lv_message_error.
        RAISE EXCEPTION TYPE /qaps/cx_div_no_result
          EXPORTING
            message = lv_message_error.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CLEAR it_rest_verif.

ENDFORM.                    " ZF_VERIFICA_REST_MINIMOS
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTA_GARANTIA
*&---------------------------------------------------------------------*
FORM zf_ajusta_garantia .

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALCULA_RESTRICAO
*&---------------------------------------------------------------------*
FORM zf_calcula_restricao .

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

*  BREAK c060863.
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

*    BREAK c060863.
    LOOP AT it_pc_aux INTO wa_pc.

      IF wa_pc-flgut = 'X'.
        LOOP AT it_caract INTO wa_caract.

          PERFORM zf_verifica_flags.

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

*        IF wa_forced-n = 'X'.
*          append value ty_rest_comp(
*              matnr       = /qaps/zmidfcig-matnr
*              werks       = /qaps/zmidfcig-werks
*              /qaps/grkey = /qaps/zmidfcig-/qaps/grkey
*              /qaps/comnr = wa_forced-comnr
**              /qaps/gemng =
**              /qaps/eqmng =
**              /qaps/lemng =
*              /qaps/chkey = 'N'
*              forced      = 'X'
*          ) to it_rest_comp.
***          BREAK c060863.
*        ENDIF.

      ENDIF.
      CLEAR: wa_rest_comp.
    ENDLOOP.

  ENDIF.

*  DELETE it_rest_comp WHERE matnr IS INITIAL.

  SORT it_rest_comp BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_rest_comp
  COMPARING matnr werks /qaps/grkey /qaps/comnr /qaps/gemng.

  PERFORM validar_rest_comp CHANGING it_rest_comp.

  SORT it_rest_comp BY matnr werks /qaps/grkey /qaps/comnr /qaps/gemng DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_rest_comp
  COMPARING matnr werks /qaps/grkey /qaps/comnr /qaps/gemng.

ENDFORM.
FORM validar_rest_comp CHANGING ct_data TYPE tt_rest_comp.

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
*  if line_exists( ct_data[ forced = 'X' ] ).
*
*    SELECT *
*      FROM /qaps/zmimi
*      FOR ALL ENTRIES IN @ct_data
*      WHERE matnr = @ct_data-/qaps/comnr
*      AND   werks = @ct_data-werks
**      AND   /qaps/grkey = @ct_data-/qaps/grkey
**      AND   /qaps/comnr = @ct_data-/qaps/comnr
*      INTO TABLE @DATA(lt_zmimi).
*
*    loop at ct_data ASSIGNING <fs_data>.
*
*      check <fs_data>-forced = 'X'.
*      <fs_data>-/qaps/gemng = lt_zmimi[ matnr = <fs_data>-/qaps/comnr ]-/qaps/rmeng.
*
*    ENDLOOP.
*
*  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_FLAGS
*&---------------------------------------------------------------------*
FORM zf_verifica_flags .

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form   zf_gravar_re_hist
*&---------------------------------------------------------------------*
FORM zf_gravar_re_hist USING p_wa_combi_total TYPE ty_combi.

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

ENDFORM.                    "zf_gravar_re_hist

*&---------------------------------------------------------------------*
*&      Form  CLEAR_REFORMULATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_reformulate .

  REFRESH: it_re_hist,
            it_ng,
            it_dif,
*            it_caract,
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
*            it_zmiqual01,
            it_zmic1_comp,
            it_zmic1_comp.

ENDFORM.
FORM update_restricao.
  REFRESH: it_rest_comp_2.
  PERFORM zf_calcula_restricao.
  it_rest_comp_2[] = it_rest_comp[].
ENDFORM.
FORM save_snapshot.

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

*  LOOP AT lt_lines INTO DATA(ls_lines).
*
*    IF ls_lines-valor <> 0.
*
*      IF ls_lines-valor < 0.
*        lv_color = 'color:red'.
*      ELSE.
*        lv_color = 'color:black'.
*      ENDIF.
*
*      lv_content = lv_content && `<tr><td style="font-size:11px;border:1px;` && lv_color && `">` && ls_lines-valor &&  `</td>`.
*    ELSE.
*      lv_content = lv_content && `<tr><td style="font-size:11px;border:1px">-</td>`.
*    ENDIF.
*
*    DATA(lt_col) = it_ma.
*    DELETE lt_col WHERE line <> ls_lines-line.
*
*    LOOP AT lt_col INTO DATA(ls_col).
*
*      IF ls_col-valor <> 0.
*
*        IF ls_lines-valor < 0.
*          lv_color = 'color:red'.
*        ELSE.
*          lv_color = 'color:black'.
*        ENDIF.
*
*        lv_content = lv_content && `<td style="font-size:11px;border:1px;` && lv_color && `">` && ls_col-valor &&  `</td>`.
*      ELSE.
*        lv_content = lv_content && `<td style="font-size:11px;border:1px">-</td>`.
*      ENDIF.
*
*    ENDLOOP.
*
*    lv_content = lv_content && `</tr>`.
*
*  ENDLOOP.

*  <fs_snapshot>-matrix_html = `<html><body><table>` && lv_content && `</table></body></html>`.

*  BREAK abap.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_version
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_version.

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

ENDFORM.
FORM validar_snapshot CHANGING cv_valido TYPE abap_bool.

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

*    SELECT *
*      FROM /qaps/zmic1
*      WHERE matnr = @<fs_snapshot>-zmidfcig-matnr
*      AND   werks = @<fs_snapshot>-zmidfcig-werks
*      AND   /qaps/grkey = @<fs_snapshot>-zmidfcig-/qaps/grkey
*      INTO TABLE @lt_restricoes.

*    <fs_snapshot>-valido = abap_true.

*    LOOP AT lt_restricoes INTO DATA(ls_rest_comp).
*
*      DATA(ls_re) = VALUE #( <fs_snapshot>-t_re[ comnr = ls_rest_comp-/qaps/comnr ] OPTIONAL ).
*
*      IF NOT ls_re-comnr IS INITIAL.
*
*        IF NOT ls_rest_comp-/qaps/gemng IS INITIAL.
*          IF ls_re-cmeng < ls_rest_comp-/qaps/gemng.
*            <fs_snapshot>-valido = abap_false.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*        IF NOT ls_rest_comp-/qaps/eqmng IS INITIAL.
*          IF ls_re-cmeng <> ls_rest_comp-/qaps/eqmng.
*            <fs_snapshot>-valido = abap_false.
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*      ELSE.
*        IF ls_rest_comp-/qaps/lemng IS INITIAL.
*          <fs_snapshot>-valido = abap_false.
*          EXIT.
*        endif.
*      ENDIF.
*
*    ENDLOOP.

  ENDLOOP.

  IF line_exists( gt_snapshot[ valido = 'X' ] ).
    cv_valido = abap_true.
  ELSE.
    cv_valido = abap_false.
  ENDIF.

ENDFORM.
FORM exec_simulation_old RAISING /qaps/cx_general.

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
                MESSAGE e017 INTO DATA(lv_message_material).
                RAISE EXCEPTION TYPE /qaps/cx_general
                  EXPORTING
                    message = VALUE bapiret2( type = 'E'
                                              message = lv_message_material ).
              ENDIF.
              it_rs_bk[] = it_rs[].
              it_pc_0100[] = it_pc[].
*              PERFORM zf_ver_rest_mistura.
              PERFORM zf_ver_comp_min.
              PERFORM product_formulate.

              PERFORM clear_workarea.
              PERFORM clear_reformulate.

            CATCH /qaps/cx_div_no_result.
*              BREAK-POINT.
              PERFORM clear_workarea.
              PERFORM clear_reformulate.
          ENDTRY.

        ENDDO.

        lv_valid = 'X'.

        IF lines( gt_snapshot ) > 0.
          PERFORM validar_snapshot CHANGING lv_div_valido.
          IF lv_div_valido = abap_true.
            PERFORM fill_version.
*            CALL SCREEN 200.
            RETURN.
          ELSE.
            zsemsolucao = abap_true.
            MESSAGE e133 INTO DATA(lv_message).
            RAISE EXCEPTION TYPE /qaps/cx_general
              EXPORTING
                message = VALUE bapiret2( type = 'E'
                                          message = lv_message ).
          ENDIF.
        ELSE.
          zsemsolucao = abap_true.
          MESSAGE e133 INTO lv_message.
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
        PERFORM clear_workarea.

        CHECK gv_reformulate >= 10.
        EXIT.

    ENDTRY.

  ENDWHILE.

ENDFORM.
FORM exec_simulation RAISING /qaps/cx_general
                             /qaps/cx_div_no_result
                             /qaps/cx_formulation_error.

  TYPES: BEGIN OF ts_div,
           zdiv TYPE i,
         END OF ts_div.

  DATA lv_valid TYPE c.
  DATA lo_excep TYPE REF TO /qaps/cx_formulation_error.
  FIELD-SYMBOLS <fs_rs> TYPE ty_rs.
  DATA lv_div_valido TYPE abap_bool.

  DATA lt_div TYPE TABLE OF ts_div.

*  TRY.
  REFRESH gt_initial.

  CLEAR: lv_valid,
         gv_reformulate..

  WHILE lv_valid = ''.

    TRY.

        PERFORM execute.

        lv_valid = 'X'.

        PERFORM clear_workarea.
        PERFORM clear_reformulate.

*      CATCH /qaps/cx_div_no_result.
*
*        IF gv_reformulate >= 10.
*          lv_valid = 'X'.
*          PERFORM clear_workarea.
*          PERFORM clear_reformulate.
*        ELSE.
*          PERFORM f_reformulate_v2.
*        ENDIF.
**          ENDTRY.
*
*      CATCH /qaps/cx_formulation_error INTO lo_excep.
*
*        PERFORM f_reformulate_v1 USING lo_excep.
**          BREAK c060863.
*        CHECK gv_reformulate >= 10.
*        lv_valid = 'X'.

    ENDTRY.

  ENDWHILE.

  PERFORM finalize_simulation.

ENDFORM.
FORM f_reformulate_v2.

  gv_reformulate = gv_reformulate + 1.
*  gv_key_increment = lo_excep->get_increment( ).
*  zdiv_reformulate = lo_excep->get_previous_div( ).

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

*  BREAK c060863.

  REFRESH it_rest_comp_2.
  CLEAR zajustou.
  PERFORM clear_workarea.

  REFRESH: it_re,it_pc_bk.

ENDFORM.
FORM f_reformulate_v1 USING lo_excep TYPE REF TO /qaps/cx_formulation_error.

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

*  BREAK c060863.

  REFRESH it_rest_comp_2.
  CLEAR zajustou.
  PERFORM clear_workarea.

  REFRESH: it_re,it_pc_bk.

ENDFORM.
FORM execute RAISING /qaps/cx_general /qaps/cx_formulation_error /qaps/cx_div_no_result.

  LOOP AT it_pc INTO wa_pc WHERE flgut = 'X'.
    wa_re-comnr = wa_pc-comnr.
    wa_re-coktx = wa_pc-coktx.
    APPEND wa_re TO it_re.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE e017 INTO DATA(lv_message_material).
    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE bapiret2( type = 'E'
                                  message = lv_message_material ).
  ENDIF.
  it_rs_bk[] = it_rs[].
  it_pc_0100[] = it_pc[].
*              PERFORM zf_ver_rest_mistura.
  PERFORM zf_ver_comp_min.
  PERFORM product_formulate.

ENDFORM.
FORM finalize_simulation RAISING /qaps/cx_general.

  DATA lv_valid TYPE c.
  DATA lv_div_valido TYPE abap_bool.

  lv_valid = 'X'.

  IF lines( gt_snapshot ) > 0.
    PERFORM validar_snapshot CHANGING lv_div_valido.
    IF lv_div_valido = abap_true.
      PERFORM fill_version.
*            CALL SCREEN 200.
      RETURN.
    ELSE.
      zsemsolucao = abap_true.
      MESSAGE e133 INTO DATA(lv_message).
      RAISE EXCEPTION TYPE /qaps/cx_general
        EXPORTING
          message = VALUE bapiret2( type = 'E'
                                    message = lv_message ).
    ENDIF.
  ELSE.
    zsemsolucao = abap_true.
    MESSAGE e133 INTO lv_message.
    RAISE EXCEPTION TYPE /qaps/cx_general
      EXPORTING
        message = VALUE bapiret2( type = 'E'
                                  message = lv_message ).
  ENDIF.


ENDFORM.
FORM fill_restricao_item.

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

ENDFORM.
FORM fill_components.

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

*    CHECK NOT ls_pc-gemng IS INITIAL OR
*          NOT ls_pc-eqmng IS INITIAL OR
*          NOT ls_pc-lemng IS INITIAL.

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

*  BREAK-POINT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INCOMPATIBILIDADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incompatibilidade RAISING /qaps/cx_general.

  LOOP AT it_zmi60 INTO wa_zmi60.
    CLEAR: wa_combi_final-costt.
    wa_combi_final-combi  = wa_zmi60-/qaps/mixin.
    APPEND wa_combi_final TO it_combi_final.
    PERFORM zf_gravar_re_hist USING wa_combi_final.
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

    PERFORM load_internal_tables.

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

    PERFORM fill_matrix_a USING c y.
    PERFORM load_le_restrictions.
    PERFORM load_eq_restrictions.
    PERFORM load_ge_restrictions.
    PERFORM load_cuts.
    PERFORM stage_590.
    PERFORM modifies_matrix.

    DO.

      IF wa_combi_final-msg IS INITIAL.
        PERFORM stage_1000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        PERFORM stage_2000.
      ENDIF.

      IF wa_combi_final-msg IS INITIAL.
        PERFORM stage_3000.
      ENDIF.

      IF NOT wa_combi_final-msg IS INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

    IF NOT it_ng IS INITIAL  AND
      wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
*        PERFORM VERIFICA_NIVEIS_GARANTIA.
      IF NOT it_dif[]  IS INITIAL.
*          PERFORM  AJUSTA_MARGEM_SEGURANCA.
        DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
        DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
        DELETE it_combi_final WHERE combi = xcombi.
        PERFORM clear_workarea.

        xcombi_atual = xcombi.
        PERFORM load_internal_tables.

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

        PERFORM fill_matrix_a USING c y.
        PERFORM load_le_restrictions.
        PERFORM load_eq_restrictions.
        PERFORM load_ge_restrictions.
        PERFORM load_cuts.
        PERFORM stage_590.
        PERFORM modifies_matrix.

        DO.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_1000.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_2000.
          ENDIF.

          IF wa_combi_final-msg IS INITIAL.
            PERFORM stage_3000.
          ENDIF.

          IF NOT wa_combi_final-msg IS INITIAL.
            EXIT.
          ENDIF.

        ENDDO.

        IF NOT it_ng IS INITIAL  AND
          wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
*            PERFORM VERIFICA_NIVEIS_GARANTIA.
          IF NOT it_dif[]  IS INITIAL.
*              PERFORM  AJUSTA_MARGEM_SEGURANCA.
            DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
            DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
            DELETE it_combi_final WHERE combi = xcombi.
            PERFORM clear_workarea.

            xcombi_atual = xcombi.
            PERFORM load_internal_tables.

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

            PERFORM fill_matrix_a USING c y.
            PERFORM load_le_restrictions.
            PERFORM load_eq_restrictions.
            PERFORM load_ge_restrictions.
            PERFORM load_cuts.
            PERFORM stage_590.
            PERFORM modifies_matrix.

            DO.

              IF wa_combi_final-msg IS INITIAL.
                PERFORM stage_1000.
              ENDIF.

              IF wa_combi_final-msg IS INITIAL.
                PERFORM stage_2000.
              ENDIF.

              IF wa_combi_final-msg IS INITIAL.
                PERFORM stage_3000.
              ENDIF.

              IF NOT wa_combi_final-msg IS INITIAL.
                EXIT.
              ENDIF.

            ENDDO.

            IF NOT it_ng IS INITIAL  AND
              wa_combi_final-msg EQ 'SOLUÇÃO ÒTIMA'.
*                PERFORM VERIFICA_NIVEIS_GARANTIA.
              IF NOT it_dif[]  IS INITIAL.
*                  PERFORM  AJUSTA_MARGEM_SEGURANCA.
                DELETE it_zmicig0 WHERE /qaps/cignr = xcombi.
                DELETE it_zmicig1 WHERE /qaps/cignr = xcombi.
                DELETE it_zmicig2 WHERE /qaps/cignr = xcombi.
                DELETE it_combi_final WHERE combi = xcombi.
                PERFORM clear_workarea.

                xcombi_atual = xcombi.
                PERFORM load_internal_tables.

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

                PERFORM fill_matrix_a USING c y.
                PERFORM load_le_restrictions.
                PERFORM load_eq_restrictions.
                PERFORM load_ge_restrictions.
                PERFORM load_cuts.
                PERFORM stage_590.
                PERFORM modifies_matrix.

                DO.

                  IF wa_combi_final-msg IS INITIAL.
                    PERFORM stage_1000.
                  ENDIF.

                  IF wa_combi_final-msg IS INITIAL.
                    PERFORM stage_2000.
                  ENDIF.

                  IF wa_combi_final-msg IS INITIAL.
                    PERFORM stage_3000.
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

    PERFORM clear_workarea.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIRST_FORMULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM first_formulation RAISING /qaps/cx_general.

  DATA lt_matrixaa TYPE TABLE OF /qaps/matrixaa.

  REFRESH: it_pc_bk.
  it_pc_bk = it_pc.

*--- FIRST WITH ALL PRODUCTS. 1
  xcombi = '0000999995'.

  wa_combi_final-combi  = xcombi.
  APPEND wa_combi_final TO it_combi_final.
  PERFORM zf_gravar_re_hist USING wa_combi_final.

  PERFORM load_internal_tables.

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



  PERFORM fill_matrix_a USING c y.
  PERFORM load_le_restrictions.
  PERFORM load_eq_restrictions.
  PERFORM load_ge_restrictions.
  PERFORM load_cuts.

  IF gv_matrix = abap_true. " 'X'.
    DELETE FROM /qaps/matrixaa.
    DELETE FROM /qaps/zmatrixaa.
    DELETE FROM /qaps/zmatr_rest.
    COMMIT WORK AND WAIT.
*    lt_matrixaa = CORRESPONDING #( it_ma ).
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

  PERFORM stage_590.
  PERFORM modifies_matrix.

  DO.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_1000.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_2000.
    ENDIF.

    IF wa_combi_final-msg IS INITIAL.
      PERFORM stage_3000.
    ENDIF.

    IF NOT wa_combi_final-msg IS INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONTINGENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM contingencia CHANGING cv_has_result TYPE abap_bool
                  RAISING /qaps/cx_formulation_error
                         /qaps/cx_div_no_result
                          /qaps/cx_general.

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
      PERFORM resultado CHANGING cv_has_result.

      IF cv_has_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    PERFORM condicao_minimo_nivel02.

  ELSE.
    PERFORM del_comd_min.
    DELETE it_combi_final WHERE combi = wa_combi_finalx-combi.
  ENDIF.

  SORT it_min  BY comnr.
  DELETE  ADJACENT DUPLICATES FROM it_min COMPARING comnr.

  SORT it_min_bk  BY comnr.
  DELETE  ADJACENT DUPLICATES FROM it_min_bk COMPARING comnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDACOES_PRELIMINARES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validacoes_preliminares USING us_header TYPE  /qaps/s_formulacao
                             RAISING /qaps/cx_general.

  PERFORM regra_1 USING us_header.
  PERFORM regra_2 USING us_header.

ENDFORM.
FORM regra_1 USING us_header TYPE  /qaps/s_formulacao
                             RAISING /qaps/cx_general.

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

  MESSAGE e228 WITH lv_material INTO DATA(lv_message).
  RAISE EXCEPTION TYPE /qaps/cx_general
    EXPORTING
      message = VALUE #( type = 'E'
                         message = lv_message ).


ENDFORM.
FORM regra_2 USING us_header TYPE  /qaps/s_formulacao
                             RAISING /qaps/cx_general.

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
        MESSAGE e295 WITH wa_pc-comnr INTO DATA(lv_message).
        RAISE EXCEPTION TYPE /qaps/cx_general
          EXPORTING
            message = VALUE #( type = 'E'
                               message = lv_message ).
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
