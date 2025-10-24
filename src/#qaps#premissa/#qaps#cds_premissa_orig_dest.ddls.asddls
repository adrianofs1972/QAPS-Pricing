@AbapCatalog.sqlViewName: '/QAPS/V_PRM_OD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Origem/Destino - Ativos'
define view /QAPS/CDS_PREMISSA_ORIG_DEST 
as select from /qaps/centro_por as centro_por
left outer join /qaps/centro as centro on centro_por.werks = centro.werks
left outer join /qaps/grp_planta as grp_planta on centro.id_grp_planta = grp_planta.id_grp_planta
left outer join /qaps/cds_ponto as porto on centro_por.id_porto = porto.id_externo
left outer join /qaps/cais as cais on porto.id_externo = cais.id_porto
left outer join /qaps/cds_ponto as ponto_cais on cais.id_cais = ponto_cais.id_externo
{
    centro_por.mandt,
    centro.id_grp_planta,
    grp_planta.codigo,
    grp_planta.descricao,
    centro.id_centro,
    centro_por.werks,    
    centro_por.ativo,
    centro_por.id_porto,
    porto.id_ponto as id_ponto_porto,
    porto.codigo as cod_porto,
    porto.descricao as porto,
    ponto_cais.id_ponto as id_ponto_cais,
    cais.id_cais,
    cais.cod_cais,
    cais.cais
        
}
