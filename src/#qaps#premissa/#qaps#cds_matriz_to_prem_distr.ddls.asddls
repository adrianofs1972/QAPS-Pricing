@AbapCatalog.sqlViewName: '/QAPS/V_MT_PR_DS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz -> Premissa (Distribuição)'
define view /QAPS/CDS_MATRIZ_TO_PREM_DISTR 
as select distinct from /QAPS/CDS_MAT_DIST_SUM  {
    id_item,
    id_porto,
    cod_porto,
    porto,
    modalidade,
    periodo,
    tipo_origem,
    sum(percentual) as percentual
}
group by id_item,
    id_porto,
    cod_porto,
    porto,
    modalidade,
    periodo,
    tipo_origem
