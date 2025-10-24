@AbapCatalog.sqlViewName: '/QAPS/I_PREM_HDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa Header'
define view /QAPS/I_PREMISSA_HDR as select from /qaps/prem_hdr {
    mandt as Mandt,
    id_premissa,
    id_simulacao,
    bintohex(id_grp_planta) as id_grp_planta,
    bintohex(id_centro) as id_centro    
}
