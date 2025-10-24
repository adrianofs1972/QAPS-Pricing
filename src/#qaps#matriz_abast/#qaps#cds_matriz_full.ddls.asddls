@AbapCatalog.sqlViewName: '/QAPS/V_MTZ_FULL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Full'
define view /QAPS/CDS_MATRIZ_FULL as 
select distinct from /qaps/matriz_hdr as header
left outer join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
left outer join /qaps/centro as centro on header.id_centro = centro.id_centro
left outer join /qaps/matriz_itm as item on header.id_matriz_abast = item.id_matriz_abast 
inner join /qaps/matriz_dst as distrib on  item.id_item = distrib.id_item
inner join /qaps/cds_ponto as origem on  distrib.id_origem = origem.id_ponto
//left outer join /qaps/cais as cais on  ponto.id_externo = cais.id_cais
and item.id_item = distrib.id_item
left outer join /qaps/grp_prod as grp_prod on item.id_grupo_produto = grp_prod.id_grupo_produto
{
    header.mandt,
    header.id_matriz_abast,
    header.id_simulacao,
    header.id_grp_planta,
    grp_planta.codigo as cod_grp_planta,
    grp_planta.descricao as dsc_grp_planta,
    header.id_centro,   
    centro.werks,     
    item.id_item,    
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    grp_prod.descricao as dsc_grupo_produto,
    item.agregador,
    item.mat_planejado,    
    distrib.id_distribuicao,
    distrib.periodo,
    distrib.modalidade,
    distrib.tipo_origem,    
    distrib.id_origem,
    origem.codigo,
    origem.cod_porto,
    distrib.percentual
    //cais.cod_cais,
    //cais.cais
//    ponto.tipo_ponto,
//    ponto.dsc_tipo_ponto,
//    ponto.codigo,
//    ponto.descricao    
}
where header.mandt = $session.client
