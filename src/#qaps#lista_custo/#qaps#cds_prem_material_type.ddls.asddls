@AbapCatalog.sqlViewName: '/QAPS/V_MAT_TYPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Material Type'
define view /QAPS/CDS_PREM_MATERIAL_TYPE as 
select from /QAPS/CDS_PRM_FULL as premissa
//Grp Planta
left outer join /qaps/grp_planta as grp_planta on premissa.id_grp_planta = grp_planta.id_grp_planta
//Centro
left outer join /qaps/centro as centro on premissa.id_centro = centro.id_centro
 {
    premissa.id_simulacao,
    premissa.id_grp_planta,   
    grp_planta.codigo as cod_grp_planta, 
    premissa.id_centro,
    centro.werks,
    premissa.tipo_regra,
    premissa.matnr,
    premissa.id_grupo_produto,
    premissa.agregador,
    premissa.mat_planejado
}
