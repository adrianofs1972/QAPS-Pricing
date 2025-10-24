@AbapCatalog.sqlViewName: '/QAPS/V_WERKS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'View Centro'
define view /qaps/cds_werks
as select from /qaps/centro as centro
association [0..1] to /qaps/cds_werks_text as _werks_text
    on $projection.werks = _werks_text.werks 
{
    centro.id_centro,
    centro.werks,
    _werks_text.werks_text
}
