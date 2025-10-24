@AbapCatalog.sqlViewName: '/QAPS/V_KUNNR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'View Cliente'
define view /QAPS/CDS_KUNNR 
as select from /qaps/cliente as cliente
association [0..1] to /QAPS/CDS_KUNNR_TEXT as _kunnr_text
    on $projection.kunnr = _kunnr_text.kunnr 
{
    cliente.id_cliente,
    cliente.kunnr,
    _kunnr_text.kunnr_text
}
