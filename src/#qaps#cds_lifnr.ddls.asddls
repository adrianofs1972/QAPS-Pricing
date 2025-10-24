@AbapCatalog.sqlViewName: '/QAPS/V_LIFNR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'View Cliente'
define view /QAPS/CDS_LIFNR 
as select from /qaps/fornecedor as fornec
association [0..1] to /QAPS/CDS_LIFNR_TEXT as _lifnr_text
    on $projection.lifnr = _lifnr_text.lifnr 
{
    fornec.id_fornecedor,
    fornec.lifnr,
    _lifnr_text.lifnr_text
}
