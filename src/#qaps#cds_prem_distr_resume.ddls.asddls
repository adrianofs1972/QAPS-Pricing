@AbapCatalog.sqlViewName: '/QAPS/V_DISTR_PR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Distribuição Resume - Premissa'
define view /QAPS/CDS_PREM_DISTR_RESUME as 
select distinct from /qaps/prem_distr as distrib {
    mandt as Mandt,
    id_distribuicao,    
    id_premissa,
    id_item,
    modalidade,
    id_parent,
    tipo_origem,
    id_origem
    
}
