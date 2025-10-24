@AbapCatalog.sqlViewName: '/QAPS/V_DST_PNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz Distrib x Ponto'
define view /QAPS/CDS_DISTRIB_PONTO 
as 
select distinct from /qaps/matriz_dst as dst
inner join /qaps/v_ponto as ponto on dst.id_origem = ponto.id_ponto
left outer join /qaps/cais as cais on ponto.id_externo = cais.id_cais
left outer join /qaps/v_ponto as porto on cais.id_porto = porto.id_externo   
{   
    ponto.cod_porto,
    ponto.porto,   
    dst.id_matriz_abast,
    dst.id_item,
    porto.id_externo as id_porto,
    porto.id_ponto as id_ponto_porto
        
}
where tipo_origem = 'I'
union
select distinct from /qaps/matriz_dst as dst
inner join /qaps/v_ponto as ponto on dst.id_origem = ponto.id_ponto
left outer join /qaps/porto as porto on ponto.id_externo = porto.id_porto   
{   
    ponto.cod_porto,
    ponto.porto,
    dst.id_matriz_abast,
    dst.id_item,
    porto.id_porto,
    ponto.id_externo as id_ponto_porto
        
}
where tipo_origem = 'P'
