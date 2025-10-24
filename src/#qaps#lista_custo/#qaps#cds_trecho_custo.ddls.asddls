@AbapCatalog.sqlViewName: '/QAPS/V_TRC_CST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Trecho x Custo'
define view /QAPS/CDS_TRECHO_CUSTO 
as select distinct from /qaps/var_input as input
inner join /qaps/custo_elm as custo_elm
on input.id_custo_elementar = custo_elm.id_custo_elementar 
inner join /qaps/trecho as trecho
on input.id_trecho = trecho.id_trecho
{
    input.id_custo_elementar,
    trecho.id_trecho,
    trecho.cod_trecho,
    custo_elm.importacao,
    custo_elm.nacional,
    custo_elm.transferencia,
    max(input.valor) as valor 
}
where custo_elm.tipo_variavel = 'F' 
group by input.id_custo_elementar,
         trecho.id_trecho,
         trecho.cod_trecho,
         custo_elm.importacao,
         custo_elm.nacional,
         custo_elm.transferencia
