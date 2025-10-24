@AbapCatalog.sqlViewName: '/QAPS/V_DISTR_RS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Distribrib Resume'
define view /QAPS/CDS_DISTRIB_RESUME as select distinct from /qaps/matriz_dst as distrib {
    mandt as Mandt,
    id_distribuicao,    
    id_matriz_abast,
    id_item,
    modalidade,
    id_parent,
    tipo_origem,
    id_origem
    
}
