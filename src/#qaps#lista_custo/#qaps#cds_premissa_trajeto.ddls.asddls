@AbapCatalog.sqlViewName: '/QAPS/V_PRM_TRJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Trajeto'
define view /QAPS/CDS_PREMISSA_TRAJETO as
select distinct from /qaps/prem_hdr as header
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa
inner join /qaps/prem_distr as distrib 
on    item.id_premissa = distrib.id_premissa
and   item.id_item = distrib.id_item  
inner join /qaps/prem_traj as prm_traj 
on    distrib.id_premissa = prm_traj.id_premissa
and   distrib.id_item = prm_traj.id_item
and   distrib.id_distribuicao = prm_traj.id_distribuicao
inner join /qaps/trajeto as traj
on  prm_traj.id_trajeto =  traj.id_trajeto   
inner join /qaps/v_ponto as origem on traj.id_origem = origem.id_ponto
inner join /qaps/v_ponto as destino on traj.id_destino = destino.id_ponto
//Grp Planta
left outer join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
//Centro
left outer join /qaps/centro as centro on header.id_centro = centro.id_centro
{
    header.mandt,
    header.id_simulacao,
    header.id_premissa,
    header.id_grp_planta,
    grp_planta.codigo as cod_grp_planta,
    header.id_centro,
    centro.werks,
    item.id_item,                
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    item.agregador,
    item.mat_planejado,    
    distrib.id_distribuicao,
    distrib.modalidade,
    prm_traj.id_prem_trajeto,
    traj.id_trajeto,
    traj.codigo as cod_trajeto,
    traj.descricao,
    traj.id_origem,
    origem.codigo as cod_origem,    
    origem.descricao as dsc_origem,
    traj.id_destino,
    destino.codigo as cod_destino,
    destino.descricao as dsc_destino
}
where header.mandt = $session.client
    
