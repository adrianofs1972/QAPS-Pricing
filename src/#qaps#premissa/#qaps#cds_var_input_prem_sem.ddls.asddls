@AbapCatalog.sqlViewName: '/QAPS/V_INP_PRS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Var Input Premissa sem Destino'
define view /QAPS/CDS_VAR_INPUT_PREM_SEM as
select distinct from /qaps/v_var_input_full as input
left outer join /qaps/cds_ponto as origem on input.id_origem = origem.id_ponto 
left outer join /qaps/cds_ponto as grp_planta on input.id_destino = grp_planta.id_ponto and grp_planta.tipo_ponto = 'G'
left outer join /qaps/cds_ponto as centro on input.id_destino = centro.id_ponto and centro.tipo_ponto = 'W'
{
    '1' as caso,
    input.tipo_regra,
    input.tipo_destino,
    input.id_destino,
    input.tipo_variavel,
    grp_planta.id_externo as id_grp_planta,
    centro.id_externo as id_centro,
    input.importacao,
    input.nacional,    
    grp_planta.codigo as codigo_grp_planta,
    grp_planta.descricao as descricao_destino,
    centro.codigo as codigo_centro,
    centro.descricao as descricao_centro,
    origem.tipo_ponto,
    Matnr,
    id_grupo_produto,
    input.descricao_produto,
    agregador,
    mat_planejado
}
where ( input.nacional = 'X' or input.importacao = 'X' ) 
and   ( grp_planta.tipo_ponto = 'G' or centro.tipo_ponto = 'W'  )
and   input.tipo_variavel = 'G'  
union
select distinct from /qaps/v_var_input_full as input
left outer join /qaps/cds_ponto as origem on input.id_origem = origem.id_ponto 
left outer join /qaps/cds_ponto as grp_planta on input.id_destino = grp_planta.id_ponto and grp_planta.tipo_ponto = 'G'
left outer join /qaps/cds_ponto as centro on input.id_destino = centro.id_ponto and centro.tipo_ponto = 'W'
{
    '2' as caso,
    input.tipo_regra,
    input.tipo_destino,
    input.id_destino,
    input.tipo_variavel,
    grp_planta.id_externo as id_grp_planta,
    centro.id_externo as id_centro,
    input.importacao,
    input.nacional,    
    grp_planta.codigo as codigo_grp_planta,
    grp_planta.descricao as descricao_destino,
    centro.codigo as codigo_centro,
    centro.descricao as descricao_centro,
    origem.tipo_ponto,
    Matnr,
    id_grupo_produto,
    input.descricao_produto,
    agregador,
    mat_planejado
}
where ( input.nacional = 'X' or input.importacao = 'X' ) 
and tipo_destino is null
and   input.tipo_variavel = 'G'
