@AbapCatalog.sqlViewName: '/QAPS/V_CENTRO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Centro Full'
define view /QAPS/CDS_CENTRO_FULL 
as select from /qaps/centro as centro
left outer join /qaps/grp_planta as grp_planta
on centro.id_grp_planta = grp_planta.id_grp_planta 
{
    centro.id_grp_planta,
    grp_planta.codigo,
    grp_planta.descricao,
    centro.id_centro,
    centro.werks
}
