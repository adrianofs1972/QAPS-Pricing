@AbapCatalog.sqlViewName: '/QAPS/V_PR_HD_ST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Header - Status'
define view /QAPS/CDS_PRM_HDR_STATUS 
as select from /qaps/prem_item as item
inner join /qaps/prem_hdr as header on item.id_premissa = header.id_premissa
inner join /qaps/prem_distr as distrib on item.id_item = distrib.id_item 
{
        header.id_simulacao,
        item.id_premissa,
        item.id_item,
        distrib.periodo,
//        distrib.id_distribuicao,       
        sum( distrib.percentual ) as soma
}
group by header.id_simulacao,
         item.id_premissa,
         item.id_item,
         distrib.periodo    
//         distrib.id_distribuicao         
         
         
