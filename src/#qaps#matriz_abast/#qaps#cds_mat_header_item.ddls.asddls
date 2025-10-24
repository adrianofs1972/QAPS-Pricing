@AbapCatalog.sqlViewName: '/QAPS/V_M_HD_ITM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz Header x Item'
define view /QAPS/CDS_MAT_HEADER_ITEM as 
select distinct from /qaps/matriz_hdr as header
inner join /qaps/matriz_itm as item on header.id_matriz_abast = item.id_matriz_abast 
inner join /qaps/matriz_dst as distr on item.id_item = distr.id_item
inner join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
inner join /qaps/centro as centro on header.id_centro = centro.id_centro
left outer join /qaps/v_ponto as cais on distr.id_origem = cais.id_ponto
{
    header.id_simulacao,
    header.id_matriz_abast,
    header.id_grp_planta,
    grp_planta.descricao as grp_planta,
    header.id_centro,
    centro.werks,
    item.id_item,
    item.tipo_regra,
    item.matnr,
    item.id_grupo_produto,
    item.agregador,
    item.mat_planejado,
    distr.id_distribuicao,
    distr.modalidade,
    distr.tipo_origem,
    distr.id_origem,
    cais.codigo
}
