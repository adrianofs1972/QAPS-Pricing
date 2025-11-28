@AbapCatalog.sqlViewName: '/QAPS/V_PRM_DEST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Destinos'
define view /qaps/cds_prem_tree_destino 
as
select distinct from /qaps/cds_premissa_tree_base as destino
left outer join /qaps/centro_por as centro_por  on    destino.werks =  centro_por.werks
left outer join /qaps/porto as porto            on    centro_por.id_porto =  porto.id_porto
left outer join /qaps/ponto as ponto            on    destino.id_grp_planta = ponto.id_externo
{       
    destino.mandt,
    ponto.id_ponto as id_ponto,
    destino.tipo,
    destino.id_grp_planta,
    destino.codigo,
    destino.id_ponto_centro,
    destino.id_centro,
    destino.werks,
    centro_por.id_porto,   
    destino.tipo as tipo_ponto,    
    porto.cod_porto,
    centro_por.ativo     
}
where centro_por.ativo = 'X'
and   destino.tipo = 'G'  
and   destino.mandt = $session.client
union
select distinct from /qaps/cds_premissa_tree_base as destino
left outer join /qaps/centro_por as centro_por  on    destino.werks =  centro_por.werks
left outer join /qaps/porto as porto            on    centro_por.id_porto =  porto.id_porto
left outer join /qaps/ponto as ponto            on    destino.id_centro = ponto.id_externo
{       
    destino.mandt,
//    ponto.id_ponto as id_ponto,
    ponto.id_externo as  id_ponto,
    destino.tipo,
    destino.id_grp_planta,
    destino.codigo,
    destino.id_ponto_centro,
    destino.id_centro,
    destino.werks,
    centro_por.id_porto,   
    destino.tipo as tipo_ponto,    
    porto.cod_porto,
    centro_por.ativo      
}
where centro_por.ativo = 'X'
and   destino.tipo = 'C'  
and   destino.mandt = $session.client
    
