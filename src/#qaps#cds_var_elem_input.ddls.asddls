@AbapCatalog.sqlViewName: '/QAPS/V_COST_INP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Variável Elementar x Inputs'
define view /QAPS/CDS_VAR_ELEM_INPUT as 
select distinct from /qaps/custo_elm as custo 
inner join /qaps/var_input as input
on custo.id_custo_elementar = input.id_custo_elementar
left outer join /qaps/v_ponto as destino on input.id_destino = destino.id_ponto
left outer join /qaps/v_ponto as origem  on input.id_origem = origem.id_ponto
left outer join /qaps/grp_prod as grp_prod on input.id_grupo_produto = grp_prod.id_grupo_produto 
{
    input.id_simulacao,
    custo.id_custo_elementar,
    custo.descricao,
    custo.id_tp_lista,
    custo.tipo_dado,
    custo.tipo_variavel,
    custo.moeda,
    custo.importacao,
    custo.nacional,
    custo.producao,
    custo.transferencia,
    input.id_var_input,    
    input.tipo_regra,
    input.matnr,
    input.id_grupo_produto,
    grp_prod.descricao as dsc_grupo_produto,
    input.agregador,
    input.mat_planejado,
    input.tipo_origem,
    input.id_origem,
    origem.codigo as cod_origem,
    input.tipo_destino,
    input.id_destino,
    destino.codigo as cod_destino,
    input.id_modal,
    input.id_categoria,
    input.id_processo,
    input.id_trecho
}
