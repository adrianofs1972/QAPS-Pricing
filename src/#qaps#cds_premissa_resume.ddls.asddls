@AbapCatalog.sqlViewName: '/QAPS/V_PRM_RES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Resumo Distribuição'
define view /QAPS/CDS_PREMISSA_RESUME as 
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa 
inner join /QAPS/CDS_PREM_DISTR_RESUME as distrib on  item.id_item = distrib.id_item                                                  
{
    header.mandt,
    header.id_simulacao,
    header.id_premissa,
    item.id_item,
    //distrib.id_distribuicao,
    count( * ) as qty        
}
where header.mandt = $session.client
group by header.mandt,
    header.id_simulacao,
    header.id_premissa,
    item.id_item
