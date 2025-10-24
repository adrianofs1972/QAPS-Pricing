@AbapCatalog.sqlViewName: '/QAPS/V_PRM_MAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa x Matriz Full'
define view /QAPS/CDS_PRM_MAT_FULL 
as 
select distinct from /qaps/prem_hdr as header
//Item
inner join /qaps/prem_item as item on header.id_premissa = item.id_premissa
//Distribuição
inner join /qaps/prem_distr as distrib 
    on    item.id_premissa = distrib.id_premissa
    and   item.id_item = distrib.id_item

//Item Premissa x Item Matriz

left outer join /qaps/matriz_itm as mat_item 
on  item.id_item_matriz = mat_item.id_item
and header.id_matriz_abast = mat_item.id_matriz_abast
//Item Matriz x Distrib Matriz
left outer join /qaps/matriz_dst as mat_distrib on  mat_item.id_item = mat_distrib.id_item
left outer join /qaps/v_ponto as ponto_distrib on distrib.id_origem = ponto_distrib.id_ponto

//Grp Planta
left outer join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
//Centro
left outer join /qaps/centro as centro on header.id_centro = centro.id_centro    

//Matriz Distrib x Ponto (Cais/Porto) 
left outer join /qaps/v_ponto as ponto_prem on mat_distrib.id_origem = ponto_prem.id_ponto
left outer join /QAPS/CDS_PORTO_CAIS as cais on ponto_prem.id_externo = cais.id_cais
{
    header.mandt,
    header.id_simulacao,
    header.id_premissa,
    header.id_grp_planta,
    grp_planta.codigo as cod_grp_planta,
    //header.id_centro,
    centro.werks,
    item.id_item,                
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    item.agregador,
    item.mat_planejado,
    //item.id_item_matriz,
    distrib.id_distribuicao,
    distrib.modalidade,    
    distrib.tipo_origem,
    distrib.id_origem, 
    
    ponto_distrib.codigo as cod_origem,    
    mat_item.id_item as id_matriz_item,
    mat_distrib.tipo_origem as id_tipo_origem_matriz,        
    mat_distrib.id_origem as id_origem_matriz,    
    ponto_prem.dsc_tipo_ponto,
    ponto_prem.codigo,    
    ponto_prem.descricao,   
    cais.id_porto,
    cais.cod_porto,
    cais.porto,
    cais.cod_cais     
}
where header.mandt = $session.client
//and   mat_distrib.tipo_origem = 'I'
    
