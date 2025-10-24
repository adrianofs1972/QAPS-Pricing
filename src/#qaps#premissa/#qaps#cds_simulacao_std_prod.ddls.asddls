@AbapCatalog.sqlViewName: '/QAPS/V_SIM_STDP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Simulação x Std Produção'
define view /QAPS/CDS_SIMULACAO_STD_PROD 
as select from /qaps/simulacao as simulacao 
inner join /qaps/std_prd_h as header on simulacao.id_std_producao = header.id_std_producao 
inner join /qaps/std_prd_pa as item on header.id_std_producao = item.id_std_producao 
left outer join /qaps/centro as centro on item.werks = centro.werks
{
    
    simulacao.mandt as Mandt,
    simulacao.id_simulacao,    
    simulacao.id_std_producao,
    header.codigo,
    header.descricao,    
    item.id_grp_planta,
    centro.id_centro,
    item.werks,
    item.matnr    
}
