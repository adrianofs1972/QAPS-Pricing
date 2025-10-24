@AbapCatalog.sqlViewName: '/QAPS/V_PRM_HD_I'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Header x Item'
define view /QAPS/CDS_PRM_HEADER_ITEM 
as 
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa
left outer join /qaps/v_ponto as grp_planta on header.id_grp_planta = grp_planta.id_externo
left outer join /qaps/v_ponto as centro on header.id_centro = centro.id_externo
{
    header.mandt,
    header.id_simulacao,
    header.id_premissa,    
    header.id_grp_planta,
    grp_planta.codigo as cod_grp_planta,
    header.id_centro,
    centro.codigo as werks,
    item.id_item,                
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    item.agregador,
    item.mat_planejado,
    item.id_item_matriz
    
}
where header.mandt = $session.client
    
