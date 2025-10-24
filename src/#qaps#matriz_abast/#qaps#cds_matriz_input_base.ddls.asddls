@AbapCatalog.sqlViewName: '/QAPS/V_MAT_IBAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Base Input'
define view /QAPS/CDS_MATRIZ_INPUT_BASE as 
select distinct from /qaps/v_var_input_full as input
left outer join /qaps/cds_ponto as origem on input.id_origem = origem.id_ponto
left outer join /qaps/cds_ponto as destino on input.id_destino = destino.id_ponto
{
    id_simulacao, 
    tipo_regra,
    importacao,
    nacional,
    Matnr, 
    id_grupo_produto,
    agregador,
    mat_planejado,
    tipo_origem,
    id_origem,
    origem.codigo as cod_origem,
    origem.descricao as dsc_origem,   
    origem.cod_porto as cod_porto_origem,
    origem.porto as porto_origem, 
    origem.descricao,
    input.id_externo_origem,
    has_destino,
    id_externo_destino,
    id_destino, 
    tipo_destino,
    destino.codigo as cod_destino,
    destino.descricao as dsc_destino,
    destino.cod_porto as cod_porto_destino,
    destino.porto as porto_destino    
    
}
where ( tipo_origem = 'P' or tipo_origem = 'I' )
and   ( importacao = 'X' )
union 
select distinct from /qaps/v_var_input_full as input
left outer join /qaps/cds_ponto as origem on input.id_origem = origem.id_ponto
left outer join /qaps/cds_ponto as destino on input.id_destino = destino.id_ponto
{
    id_simulacao, 
    tipo_regra,
    importacao,
    nacional,
    Matnr, 
    id_grupo_produto,
    agregador,
    mat_planejado,
    tipo_origem,
    id_origem,
    origem.codigo as cod_origem,
    origem.descricao as dsc_origem,   
    origem.cod_porto as cod_porto_origem,
    origem.porto as porto_origem, 
    origem.descricao,
    input.id_externo_origem,
    has_destino,
    id_externo_destino,
    id_destino, 
    tipo_destino,
    destino.codigo as cod_destino,
    destino.descricao as dsc_destino,
    destino.cod_porto as cod_porto_destino,
    destino.porto as porto_destino   
    
}
where ( tipo_origem = 'P' or tipo_origem = 'I' )
and   ( nacional = 'X' )
        
