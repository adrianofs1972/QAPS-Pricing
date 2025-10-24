@AbapCatalog.sqlViewName: '/QAPS/V_MAT_SUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Agrupamento por Porto'
define view /QAPS/CDS_MAT_DIST_SUM 
as 
select distinct from /qaps/matriz_dst as distr
inner join /qaps/v_ponto as ponto on distr.id_origem = ponto.id_ponto
inner join /qaps/cais as cais on ponto.id_externo = cais.id_cais
inner join /qaps/v_ponto as ponto_porto on cais.id_porto = ponto_porto.id_externo
inner join /qaps/porto as porto on ponto_porto.id_externo = porto.id_porto
{
    id_item,
    ponto_porto.id_ponto as id_porto,
    porto.cod_porto,
    porto.porto,
    modalidade,
    periodo,
    'P' as tipo_origem,
    percentual    
}
where tipo_origem = 'I'
union
select distinct from /qaps/matriz_dst as distr
inner join /qaps/v_ponto as ponto on distr.id_origem = ponto.id_ponto
inner join /qaps/porto as porto on ponto.id_externo = porto.id_porto
{
    id_item,
    ponto.id_ponto as id_porto,
    porto.cod_porto,
    porto.porto,
    modalidade,
    periodo,
    'P' as tipo_origem,
    percentual    
}
where tipo_origem = 'P'
