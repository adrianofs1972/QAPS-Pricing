@AbapCatalog.sqlViewName: '/QAPS/V_STP_FULL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Std Produção Full'
define view /QAPS/CDS_STD_PRD_FULL 
as select from /qaps/std_prd_h as header 
inner join /qaps/std_prd_pa as pa 
on header.id_std_producao = pa.id_std_producao
left outer join /qaps/v_ponto as grp_planta
on pa.id_grp_planta = grp_planta.id_externo
{
    header.id_std_producao,
    header.codigo,
    header.descricao,
    pa.categoria,
    pa.id_grp_planta,
    pa.matnr,
    grp_planta.codigo as cod_grp_planta,
    grp_planta.descricao as dsc_grp_planta,
    pa.werks
}
