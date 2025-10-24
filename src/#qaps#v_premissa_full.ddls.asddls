@AbapCatalog.sqlViewName: '/QAPS/V_PRM_FULL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ClientDependent: true
@EndUserText.label: 'Premissa - Header x Item x Distrib'
define view /QAPS/V_PREMISSA_FULL as 
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa
inner join /qaps/prem_distr as distrib 
on    item.id_premissa = distrib.id_premissa
and   item.id_item = distrib.id_item  
inner join /qaps/simulacao as simul on header.id_simulacao = simul.id_simulacao
left outer join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
left outer join /qaps/centro as centro on header.id_centro = centro.id_centro 
left outer join /qaps/v_ponto as origem on distrib.id_origem = origem.id_ponto
left outer join /qaps/prem_traj as prem_trajeto on distrib.id_distribuicao = prem_trajeto.id_distribuicao
left outer join /qaps/trajeto as trajeto on prem_trajeto.id_trajeto = trajeto.id_trajeto  
{
    header.mandt,
    header.id_simulacao,
    simul.status,
    header.id_premissa,    
    header.id_grp_planta,
    grp_planta.codigo as grp_planta,
    header.id_centro,
    centro.werks,
    item.id_item,
    item.id_item_matriz,
    distrib.id_distribuicao,
    distrib.modalidade,            
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    item.agregador,
    item.mat_planejado,        
    item.id_parent as id_item_parent,
    distrib.tipo_origem,
    distrib.id_origem,    
    distrib.id_parent as id_distrib_parent,    
    origem.dsc_tipo_ponto as dsc_tipo_origem,
    origem.codigo as cod_origem,
    trajeto.codigo as cod_trajeto,
    trajeto.descricao as dsc_trajeto
    
}
where header.mandt = $session.client
