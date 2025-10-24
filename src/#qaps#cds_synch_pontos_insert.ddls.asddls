@AbapCatalog.sqlViewName: '/QAPS/V_SYNC_INS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: false
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sincronização de Pontos - Insert'
define view /QAPS/CDS_SYNCH_PONTOS_INSERT as 
select from /qaps/porto as porto 
left outer join /qaps/ponto as ponto on porto.id_porto = ponto.id_externo   {    
    key 'P' as tipo_ponto, 
    porto.id_porto as id,       
    ponto.id_externo    
}
where porto.mandt = $session.client
and  ponto.id_externo is null
union 
select from /qaps/cais as cais 
left outer join /qaps/ponto as ponto on cais.id_cais = ponto.id_externo   {
    key 'I' as tipo_ponto,    
    cais.id_cais  as id,    
    ponto.id_externo    
}
where cais.mandt = $session.client
and  ponto.id_externo is null
union 
select from /qaps/regiao_prc as regiao 
left outer join /qaps/ponto as ponto on regiao.id_regiao = ponto.id_externo   {
    key 'R' as tipo_ponto,    
    regiao.id_regiao  as id,    
    ponto.id_externo    
}
where regiao.mandt = $session.client
and  ponto.id_externo is null
union
select from /qaps/grp_planta as grp_planta 
left outer join /qaps/ponto as ponto on grp_planta.id_grp_planta = ponto.id_externo   {
    key 'G' as tipo_ponto,    
    grp_planta.id_grp_planta  as id,    
    ponto.id_externo    
}
where grp_planta.mandt = $session.client
and  ponto.id_externo is null
union
select from /qaps/cidade as cidade 
left outer join /qaps/ponto as ponto on cidade.id_cidade = ponto.id_externo   {
    key 'C' as tipo_ponto,    
    cidade.id_cidade  as id,    
    ponto.id_externo    
}
where cidade.mandt = $session.client
and  ponto.id_externo is null
union
select from /qaps/centro as centro 
left outer join /qaps/ponto as ponto on centro.id_centro = ponto.id_externo   {
    key 'W' as tipo_ponto,    
    centro.id_centro  as id,    
    ponto.id_externo    
}
where centro.mandt = $session.client
and  ponto.id_externo is null
union
select from /qaps/cliente as cliente 
left outer join /qaps/ponto as ponto on cliente.id_cliente = ponto.id_externo   {
    key 'K' as tipo_ponto,    
    cliente.id_cliente  as id,    
    ponto.id_externo    
}
where cliente.mandt = $session.client
and  ponto.id_externo is null
union
select from /qaps/fornecedor as fornec 
left outer join /qaps/ponto as ponto on fornec.id_fornecedor = ponto.id_externo   {
    key 'F' as tipo_ponto,    
    fornec.id_fornecedor  as id,    
    ponto.id_externo    
}
where fornec.mandt = $session.client
and  ponto.id_externo is null
