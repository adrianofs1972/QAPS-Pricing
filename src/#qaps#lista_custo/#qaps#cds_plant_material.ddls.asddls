@AbapCatalog.sqlViewName: '/QAPS/V_PLNT_MAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Planta x Material - MARC'
define view /QAPS/CDS_PLANT_MATERIAL 
as select from /QAPS/CDS_MAT_PLANT as MARC
inner join /qaps/material as material  on MARC.matnr = material.matnr
///Centro
left outer join /qaps/centro as centro on MARC.werks = centro.werks
//Grp Planta
left outer join /qaps/grp_planta as grp_planta on centro.id_grp_planta = grp_planta.id_grp_planta

left outer join /qaps/grp_prod as grp_produto on material.id_grupo_produto = grp_produto.id_grupo_produto
left outer join /qaps/categ_trns as categ on material.id_categoria = categ.id_categoria

{
    MARC.mandt,
    centro.id_grp_planta,    
    grp_planta.codigo as cod_grp_planta,
    MARC.werks,
    MARC.matnr,
    material.id_grupo_produto,
    grp_produto.descricao,    
    material.id_categoria,
    categ.descricao as categoria,
    material.agregador,
    material.classif_perigosa,
    material.mat_planejado
        
}
