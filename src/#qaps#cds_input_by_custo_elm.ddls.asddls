@AbapCatalog.sqlViewName: '/QAPS/V_INP_CUST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Inputs por Custo Elementar'
define view /QAPS/CDS_INPUT_BY_CUSTO_ELM as 
select from /qaps/custo_elm as custo 
inner join /qaps/var_input as input
on custo.id_custo_elementar = input.id_custo_elementar {    
    input.id_simulacao,
    custo.id_custo_elementar,    
    count( * ) as qty
}
group by input.id_simulacao,
         custo.id_custo_elementar
