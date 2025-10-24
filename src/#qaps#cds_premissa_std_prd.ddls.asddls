@AbapCatalog.sqlViewName: '/QAPS/V_PRM_STDP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Std Prd x Premissa'
define view /QAPS/CDS_PREMISSA_STD_PRD as
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item
on header.id_premissa = item.id_premissa
inner join /qaps/prem_distr as distr
on distr.id_item = item.id_item
{    
    header.id_simulacao,
    header.id_premissa,
    header.id_grp_planta,
    header.id_centro,
    item.id_item,
    item.tipo_regra,
    item.matnr,
    distr.id_distribuicao,
    distr.modalidade,
    distr.tipo_origem,
    distr.id_origem    
}
where header.mandt = $session.client
and   distr.modalidade = 'P'  
