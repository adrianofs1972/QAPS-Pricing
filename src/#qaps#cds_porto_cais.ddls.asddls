@AbapCatalog.sqlViewName: '/QAPS/V_PRT_CAIS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Porto Cais'
define view /QAPS/CDS_PORTO_CAIS as 
select from /qaps/porto as PORTO 
left outer join /qaps/cais as CAIS
on PORTO.id_porto = CAIS.id_porto
left outer join /qaps/v_ponto as ponto_porto on PORTO.id_porto = ponto_porto.id_externo
left outer join /qaps/v_ponto as ponto_cais on CAIS.id_cais = ponto_cais.id_externo 
{   
    ponto_porto.id_ponto as id_ponto_porto,
    PORTO.id_porto,
    PORTO.cod_porto,
    PORTO.porto,
    PORTO.id_cidade,
    ponto_cais.id_ponto as id_ponto_cais,    
    CAIS.id_cais,    
    CAIS.cod_cais,
    CAIS.cais       
}
union
select from /qaps/cais as CAIS 
left outer join /qaps/porto as PORTO
on CAIS.id_porto = PORTO.id_porto 
left outer join /qaps/v_ponto as ponto_porto on PORTO.id_porto = ponto_porto.id_externo
left outer join /qaps/v_ponto as ponto_cais on CAIS.id_cais = ponto_cais.id_externo 
{   
    ponto_porto.id_ponto as id_ponto_porto,
    PORTO.id_porto,
    PORTO.cod_porto,
    PORTO.porto,
    PORTO.id_cidade,
    ponto_cais.id_ponto as id_ponto_cais,    
    CAIS.id_cais,    
    CAIS.cod_cais,
    CAIS.cais       
}
