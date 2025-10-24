@AbapCatalog.sqlViewName: '/QAPS/V_MAT_HIER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Hierarquia'
define view /QAPS/CDS_MATRIZ_HIERARQUIA 
as select distinct from /qaps/matriz_hdr as header
inner join /qaps/matriz_itm as item on header.id_matriz_abast = item.id_matriz_abast
inner join /qaps/matriz_dst as distr on item.id_item = distr.id_item
{
    header.id_matriz_abast,    
    header.id_grp_planta,
    header.id_centro,
    item.id_item,
    item.tipo_regra,
    item.matnr,
    item.mat_planejado,
    item.id_grupo_produto,
    item.agregador,
    distr.id_distribuicao,
    distr.modalidade,
    distr.id_origem
        
}
