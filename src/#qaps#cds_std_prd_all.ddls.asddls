@AbapCatalog.sqlViewName: '/QAPS/V_STP_ALL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Header x PA x CP'
define view /QAPS/CDS_STD_PRD_ALL 
as 
select from /qaps/std_prd_h as header 
inner join /qaps/std_prd_pa as pa on header.id_std_producao = pa.id_std_producao
inner join /qaps/std_prd_cp as cp on pa.id_std_prod_pa = cp.id_std_prod_pa
                                  and pa.id_std_producao = cp.id_std_producao
left outer join /qaps/v_ponto as grp_planta on pa.id_grp_planta = grp_planta.id_externo
{
    '1' as caso,
    header.id_std_producao,    
    header.codigo,
    header.descricao,
    pa.id_std_prod_pa,
    pa.categoria,
    pa.id_grp_planta,    
    pa.werks,
    pa.matnr,
    grp_planta.codigo as cod_grp_planta,
    grp_planta.descricao as dsc_grp_planta,    
    cp.id_std_prod_cp,
    cp.componente,
    cp.menge,
    cp.meins
}
where grp_planta.codigo <> '' 
union all
select from /qaps/std_prd_h as header 
inner join /qaps/std_prd_pa as pa 
on header.id_std_producao = pa.id_std_producao
left outer join /qaps/v_ponto as grp_planta
on pa.id_grp_planta = grp_planta.id_externo
inner join /qaps/std_prd_cp as cp
on pa.id_std_prod_pa = pa.id_std_prod_pa
and pa.id_std_producao = pa.id_std_producao
{
    '2' as caso,
    header.id_std_producao,    
    header.codigo,
    header.descricao,
    pa.id_std_prod_pa,
    pa.categoria,
    pa.id_grp_planta,
    pa.werks,
    pa.matnr,
    grp_planta.codigo as cod_grp_planta,
    grp_planta.descricao as dsc_grp_planta,    
    cp.id_std_prod_cp,
    cp.componente,
    cp.menge,
    cp.meins
}
where pa.werks <> ''
