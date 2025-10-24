@AbapCatalog.sqlViewName: '/QAPS/I_MAT_HDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Header'
define view /QAPS/I_MATRIZ_HDR as select from /qaps/matriz_hdr 
{
    mandt as Mandt,
    id_matriz_abast,
    id_simulacao,
    bintohex(id_grp_planta) as id_grp_planta,
    bintohex(id_centro) as id_centro    
}
