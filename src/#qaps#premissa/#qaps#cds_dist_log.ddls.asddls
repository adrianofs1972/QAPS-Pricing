@AbapCatalog.sqlViewName: '/QAPS/V_DST_LOG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Centro_POR x Cais x Porto x Centro'
define view /QAPS/CDS_DIST_LOG as 
select from /qaps/centro_por as CENTRO_POR 
inner join /qaps/porto as porto on CENTRO_POR.id_porto = porto.id_porto
inner join /qaps/centro as centro on CENTRO_POR.werks = centro.werks
left outer join /qaps/cais as cais on porto.id_porto = cais.id_porto {
    CENTRO_POR.mandt,
    centro.id_grp_planta,
    centro.id_centro,
    CENTRO_POR.werks,
    porto.id_porto,
    porto.cod_porto,
    porto.porto,
    cais.id_cais,
    cais.cod_cais,
    cais.cais,
    cais.id_cidade,
    CENTRO_POR.ativo
}
