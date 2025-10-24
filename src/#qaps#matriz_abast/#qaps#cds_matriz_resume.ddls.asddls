@AbapCatalog.sqlViewName: '/QAPS/V_MTZ_RES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Resumo Distribuição'
define view /QAPS/CDS_MATRIZ_RESUME as 
select distinct from /qaps/matriz_hdr as header
inner join /qaps/matriz_itm as item on header.id_matriz_abast = item.id_matriz_abast 
inner join /QAPS/CDS_DISTRIB_RESUME as distrib on  item.id_item = distrib.id_item
and item.id_item = distrib.id_item
{
    header.mandt,
    header.id_simulacao,
    header.id_matriz_abast,     
    item.id_item,
    //distrib.id_distribuicao,
    count( * ) as qty        
}
where header.mandt = $session.client
group by header.mandt,
    header.id_simulacao,
    header.id_matriz_abast,     
    item.id_item
    //distrib.id_distribuicao
