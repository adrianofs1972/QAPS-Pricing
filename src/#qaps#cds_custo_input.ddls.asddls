@AbapCatalog.sqlViewName: '/QAPS/V_CST_INP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Custo Elementar x Var Input'
define view /QAPS/CDS_CUSTO_INPUT as 
select distinct from /qaps/custo_elm as custo
inner join /qaps/var_input as input
on custo.id_custo_elementar = input.id_custo_elementar
inner join /qaps/simulacao as simulacao
on input.id_simulacao = simulacao.id_simulacao
{
    input.id_simulacao,
    simulacao.status,
    custo.id_custo_elementar,
    custo.descricao,
    custo.importacao,
    custo.nacional,
    custo.transferencia,
    custo.producao,
    custo.tipo_dado,
    custo.tipo_variavel,
    input.id_var_input,
    input.tipo_destino,
    input.id_destino,
    input.tipo_origem,
    input.id_origem,
    input.tipo_regra,
    input.matnr,
    input.agregador,
    input.mat_planejado,
    input.id_grupo_produto,
    input.id_modal,
    input.id_processo,
    input.id_trecho
}
