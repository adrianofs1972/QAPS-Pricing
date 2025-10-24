@AbapCatalog.sqlViewName: '/QAPS/V_DIST_SUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Distribuição'
define view /QAPS/CDS_DISTRIB_SUMMARY 
as select from /qaps/matriz_itm as item
inner join /qaps/matriz_dst as distrib on item.id_item = distrib.id_item 
{
    key '1' as caso,
    key item.id_matriz_abast,
    key item.id_item,
    count( * ) as qty
    
}
where distrib.percentual > 0
group by item.id_matriz_abast,
    item.id_item
union
select from /qaps/matriz_itm as item
inner join /qaps/matriz_dst as distrib on item.id_item = distrib.id_item 
{
    key '2' as caso,
    key item.id_matriz_abast,
    key item.id_item,
    count( * ) as qty
    
}
where distrib.percentual = 0
group by item.id_matriz_abast,
    item.id_item
union    
select from /qaps/matriz_itm as item
inner join /qaps/matriz_dst as distrib on item.id_item = distrib.id_item 
{
    key '3' as caso,
    key item.id_matriz_abast,
    key item.id_item,
    count( * ) as qty
    
}
where distrib.percentual > 0 and distrib.percentual < 100 
group by item.id_matriz_abast,
    item.id_item    
