@AbapCatalog.sqlViewName: '/QAPS/V_VAR_RES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Var input Join com demais tabelas - Forma Compacta'
define view /QAPS/V_VAR_INPUT_RESUME as 
select distinct from /QAPS/V_VAR_INPUT_FULL as resume {
    resume.id_simulacao,
    resume.id_var_input,
    resume.id_custo_elementar,    
    resume.Custo_elementar,
    resume.importacao,
    resume.nacional,
    resume.transferencia,
    resume.producao,
    resume.tipo_regra,
    resume.id_grupo_produto,
    case        
        when resume.tipo_regra = 'GP' then resume.descricao_produto
        when resume.tipo_regra = 'AG' then resume.agregador
        when resume.tipo_regra = 'MP' then resume.mat_planejado
        when resume.tipo_regra = 'MA' then resume.Matnr
        else '' 
    end as ID,
    //Origem
    resume.has_origem,
    resume.id_origem,
    resume.tipo_origem,
    resume.id_externo_origem,
    resume.codigo_origem,
    resume.dsc_tipo_origem, 
    //Destino
    resume.has_destino,
    resume.id_destino,
    resume.tipo_destino,
    resume.id_externo_destino,
    resume.codigo_destino,
    resume.dsc_tipo_destino    

}
