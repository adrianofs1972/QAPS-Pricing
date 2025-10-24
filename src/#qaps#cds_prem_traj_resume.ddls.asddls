@AbapCatalog.sqlViewName: '/QAPS/V_DISTR_TR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premisa x Trajeto'
define view /QAPS/CDS_PREM_TRAJ_RESUME as 
select distinct from /qaps/prem_distr as distrib 
inner join /qaps/prem_traj as trajeto
on distrib.id_distribuicao = trajeto.id_distribuicao{
    distrib.mandt as Mandt,
    distrib.id_distribuicao,    
    distrib.id_premissa,
    distrib.id_item,
    distrib.modalidade,
    distrib.id_parent,
    distrib.tipo_origem,
    distrib.id_origem,
    trajeto.id_trajeto
}
