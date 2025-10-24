@AbapCatalog.sqlViewName: '/QAPS/V_PRT_CNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PREMISSA TRAJETO'
define view /QAPS/CDS_PREM_TRAJ_COUNT as 
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa 
inner join /QAPS/CDS_PREM_TRAJ_RESUME as distrib on  item.id_item = distrib.id_item                                                  
{
    header.mandt,
    header.id_simulacao,
    header.id_premissa,
    item.id_item,
    distrib.id_distribuicao,
    count( * ) as qty        
}
where header.mandt = $session.client
group by header.mandt,
    header.id_simulacao,
    header.id_premissa,
    item.id_item,
    distrib.id_distribuicao
