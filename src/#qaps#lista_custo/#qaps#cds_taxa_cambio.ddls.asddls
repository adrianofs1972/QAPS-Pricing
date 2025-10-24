@AbapCatalog.sqlViewName: '/QAPS/V_TX_CMB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Taxa de Câmbio Ativa por período'
define view /QAPS/CDS_TAXA_CAMBIO 
as select from /qaps/tx_cambio as cambio
inner join /qaps/tx_cmb_ver as versao
on   cambio.id_taxa_cambio = versao.id_taxa_cambio
and cambio.periodo = versao.periodo
inner join /qaps/fonte_cmb as fonte
on cambio.id_fonte = fonte.id_fonte
{
    cambio.mandt,
    cambio.id_taxa_cambio,
    fonte.descricao as fonte,
    cambio.periodo,
    cambio.moeda_local,
    cambio.moeda_final,
    cambio.id_fonte,
    cambio.taxa,
    cambio.data,
    cambio.hora,    
    versao.versao,
    versao.ativo        
}
where versao.ativo = 'X'
