@AbapCatalog.sqlViewName: '/QAPS/V_MT_HD_ST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Header - Status'
define view /QAPS/CDS_MATRIZ_HDR_STATUS 
as select from /qaps/matriz_itm as item
inner join /qaps/matriz_dst as distrib on item.id_item = distrib.id_item 
{
        item.id_matriz_abast,
        item.id_item,
        distrib.periodo,
        sum( distrib.percentual ) as soma
}
group by item.id_matriz_abast,
         item.id_item,         
         distrib.periodo
