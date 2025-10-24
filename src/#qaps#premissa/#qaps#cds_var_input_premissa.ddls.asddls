@AbapCatalog.sqlViewName: '/QAPS/V_INP_PRM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Var Input Premissa'
define view /QAPS/CDS_VAR_INPUT_PREMISSA as
select distinct from /qaps/v_var_input_full as input
left outer join /qaps/cds_ponto as origem on input.id_origem = origem.id_ponto 
left outer join /qaps/cds_ponto as grp_planta 
on input.id_destino = grp_planta.id_ponto and grp_planta.tipo_ponto = 'G'
left outer join /qaps/cds_ponto as centro 
on input.id_destino = centro.id_ponto and centro.tipo_ponto = 'W'
{
    input.tipo_regra,
    input.tipo_destino,
    input.id_destino,
    grp_planta.id_externo as id_grp_planta,
    centro.id_externo as id_centro,
    input.importacao,
    input.nacional,    
    grp_planta.codigo as codigo_grp_planta,
    grp_planta.descricao as descricao_destino,
    centro.codigo as codigo_centro,
    centro.descricao as descricao_centro,
    Matnr,
    id_grupo_produto,
    input.descricao_produto,
    agregador,
    mat_planejado
}
where input.nacional = 'X' 
//and   ( destino.tipo_ponto = 'G' or destino.tipo_ponto = 'W'  )  
and  origem.tipo_ponto = 'F'
