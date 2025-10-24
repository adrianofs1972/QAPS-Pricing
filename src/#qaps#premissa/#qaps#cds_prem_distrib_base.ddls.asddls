@AbapCatalog.sqlViewName: '/QAPS/V_PRM_DBAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Base Distribuição'
@ClientDependent: true
define view /QAPS/CDS_PREM_DISTRIB_BASE 
as select distinct from /qaps/prem_hdr as hdr
inner join /qaps/prem_item as itm on hdr.id_premissa = itm.id_premissa
left outer join /qaps/cds_ponto as grp_planta on hdr.id_grp_planta = grp_planta.id_externo
left outer join /qaps/cds_ponto as centro on hdr.id_centro = centro.id_externo 
{
        hdr.id_simulacao,
        hdr.id_premissa,
        hdr.id_grp_planta,
        hdr.id_centro,
        itm.id_item,
        itm.id_item_matriz,
        itm.tipo_regra,
        itm.matnr,
        itm.id_grupo_produto,
        itm.agregador,
        itm.mat_planejado,
        grp_planta.id_ponto as id_ponto_grp_planta,   
        grp_planta.codigo,     
        centro.id_ponto as id_ponto_centro,
        centro.codigo as werks          
}
where hdr.mandt = $session.client
