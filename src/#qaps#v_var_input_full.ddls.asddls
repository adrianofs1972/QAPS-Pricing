@AbapCatalog.sqlViewName: '/QAPS/V_VAR_FULL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ClientDependent: true
@EndUserText.label: 'Var input Join com demais tabelas'
define view /qaps/v_var_input_full as 
select distinct from /qaps/var_input as input
inner join /qaps/custo_elm as custo_elm on input.id_custo_elementar = custo_elm.id_custo_elementar
left outer join /qaps/grp_prod as grp_prod on input.id_grupo_produto = grp_prod.id_grupo_produto    
left outer join /qaps/custo_prc as processo on input.id_processo = processo.id_processo    
left outer join /qaps/cds_ponto as origem  on input.id_origem   = origem.id_ponto
left outer join /qaps/cds_ponto as destino on input.id_destino  = destino.id_ponto    
left outer join /qaps/trecho as trecho on input.id_trecho  = trecho.id_trecho
left outer join /qaps/cds_ponto as origem_trecho  on trecho.id_origem   = origem_trecho.id_ponto
left outer join /qaps/cds_ponto as destino_trecho on trecho.id_destino  = destino_trecho.id_ponto
{    
    input.mandt,
    input.id_simulacao,
    input.id_var_input,
    input.id_custo_elementar,    
    custo_elm.descricao as Custo_elementar,
    custo_elm.importacao,
    custo_elm.nacional,
    custo_elm.transferencia,
    custo_elm.producao,
    input.tipo_regra,
    input.matnr as Matnr,
//    makt.maktx as Material, 
    custo_elm.tipo_variavel,
    input.id_grupo_produto,
    grp_prod.descricao as descricao_produto,
    input.agregador,
    input.mat_planejado,
    //Origem
    @EndUserText.label:'Possui Origem'
    input.id_origem,
    case
        when origem.tipo_ponto <> '' then 'X' 
        else ''
    end as has_origem,
    origem.tipo_ponto       as tipo_origem,
    origem.codigo           as codigo_origem,
    origem.id_externo       as id_externo_origem,
    origem.dsc_tipo_ponto   as dsc_tipo_origem,
    
    //Destino    
    @EndUserText.label:'Possui Destino'
    input.id_destino,
    case
        when destino.tipo_ponto <> '' then 'X' 
        else ''
    end as has_destino,
    destino.tipo_ponto      as tipo_destino,
    destino.codigo          as codigo_destino,
    destino.id_externo      as id_externo_destino,
    destino.dsc_tipo_ponto  as dsc_tipo_destino,
    //Processo
    processo.descricao      as processo,
    input.id_trecho,
    trecho.id_origem as id_origem_trecho,
    origem_trecho.id_externo as id_origem_trecho_ext,
    origem_trecho.codigo as cod_origem_trecho,
    trecho.id_destino  as id_destino_trecho,
    destino_trecho.id_externo as id_destino_trecho_ext,
    destino_trecho.codigo as cod_destino_trecho,
    trecho.id_modal
}
where input.mandt = $session.client
