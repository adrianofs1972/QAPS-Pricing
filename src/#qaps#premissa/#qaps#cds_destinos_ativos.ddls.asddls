@AbapCatalog.sqlViewName: '/QAPS/V_DEST_ATV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Destinos Ativos'
define view /QAPS/CDS_DESTINOS_ATIVOS as 
select distinct from /qaps/cds_prem_tree_destino 
{
    mandt,
    id_ponto,
    tipo,
    id_grp_planta,
    codigo,
    id_ponto_centro,
    id_centro,
    werks,
//    id_porto,
//    tipo_ponto,
//    cod_porto,
    ativo    
}
