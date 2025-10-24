@AbapCatalog.sqlViewName: '/QAPS/V_MAT_PLNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Material x Planta - Premissa'
define view /QAPS/CDS_MAT_PLANT as select from /qaps/v_prm_full {
    mandt,
    werks,
    lpad(matnr, 18, '0') as MATNR
}
where tipo_regra = 'MA'
and werks <> ''
