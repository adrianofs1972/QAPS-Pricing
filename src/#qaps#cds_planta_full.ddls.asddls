@AbapCatalog.sqlViewName: '/QAPS/V_PLANTA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Planta Full'
define view /qaps/cds_planta_full as 
select from /qaps/centro as centro
left outer join /qaps/grp_planta as grp_planta on centro.id_grp_planta = grp_planta.id_grp_planta 
left outer join /qaps/cidade as cidade on centro.id_cidade = cidade.id_cidade
left outer join /qaps/ponto  as ponto_werks on centro.id_centro = ponto_werks.id_externo
left outer join /qaps/ponto  as ponto_grp_planta on centro.id_grp_planta = ponto_grp_planta.id_externo
{
    centro.mandt,
    ponto_werks.id_ponto as id_ponto_werks,
    ponto_grp_planta.id_ponto as id_ponto_grp_planta,
    centro.id_centro,
    centro.werks,
    centro.id_grp_planta,
    grp_planta.codigo,
    grp_planta.descricao,
    centro.id_cidade,
    cidade.cidade
}
where centro.mandt = $session.client
