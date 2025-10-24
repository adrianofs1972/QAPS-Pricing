@AbapCatalog.sqlViewName: '/QAPS/V_MAT_ALL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Material Base'
define view /QAPS/I_MATERIAL as select from /qaps/material as material
inner join /qaps/grp_prod as grp_produto on material.id_grupo_produto = grp_produto.id_grupo_produto
left outer join makt on material.matnr = makt.matnr
                     and makt.spras = $session.system_language
left outer join makt as makt_plan on material.mat_planejado = makt_plan.matnr
                                  and makt_plan.spras = $session.system_language  
left outer join /qaps/categ_trns as CATEGORIA on material.id_categoria = CATEGORIA.id_categoria
{
    
    material.matnr,
    makt.maktx as dsc_material, 
    material.agregador,
    material.mat_planejado,
    makt_plan.maktx,
    material.id_grupo_produto,
    grp_produto.descricao as grp_produto,
    material.classif_perigosa,
    material.id_categoria,  
    CATEGORIA.descricao as dsc_categoria
    
}

