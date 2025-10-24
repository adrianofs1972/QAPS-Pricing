@AbapCatalog.sqlViewName: '/QAPS/V_PRM_ST_B'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa x Std Produção'
define view /QAPS/CDS_PREM_STD_PROD_BASE 
as 
select distinct from /QAPS/CDS_SIMULACAO_STD_PROD as std_producao
left outer join /qaps/grp_planta as GRP_PLANTA on std_producao.id_grp_planta = GRP_PLANTA.id_grp_planta
left outer join /qaps/centro as CENTRO on std_producao.id_centro = CENTRO.id_centro
{
  std_producao.id_simulacao,
  std_producao.id_std_producao,
  std_producao.codigo as cod_stp_producao,
  std_producao.descricao as dsc_std_producao,
  std_producao.id_grp_planta,
  GRP_PLANTA.codigo as COD_GRP_PLANTA,
  std_producao.id_centro,
  std_producao.werks,
  std_producao.matnr
  
}


