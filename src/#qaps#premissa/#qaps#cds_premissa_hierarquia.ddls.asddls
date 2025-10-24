@AbapCatalog.sqlViewName: '/QAPS/V_PRM_HIER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Hierarquia'
define view /QAPS/CDS_PREMISSA_HIERARQUIA 
as select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa
inner join /qaps/prem_distr as distr on item.id_item = distr.id_item
left outer join /qaps/cds_ponto as ponto
on distr.id_origem = ponto.id_ponto
{
    header.id_premissa,    
    header.id_grp_planta,
    header.id_centro,
    item.id_item,
    item.tipo_regra,
    item.matnr,
    item.mat_planejado,
    item.id_grupo_produto,
    item.agregador,
    distr.id_distribuicao,
    distr.modalidade,
    distr.id_origem,
    ponto.dsc_tipo_ponto,
    ponto.codigo,
    ponto.descricao
   
        
}
