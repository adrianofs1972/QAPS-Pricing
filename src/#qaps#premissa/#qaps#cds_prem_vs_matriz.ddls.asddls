@AbapCatalog.sqlViewName: '/QAPS/PRM_MATRIZ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa vs Matriz Abast'
define view /QAPS/CDS_PREM_VS_MATRIZ 
as select from /QAPS/I_MATRIZ_HDR as mat_hdr
left outer join /QAPS/I_PREMISSA_HDR as prm_hdr 
on prm_hdr.id_grp_planta = mat_hdr.id_grp_planta
and prm_hdr.id_centro = mat_hdr.id_centro
{
    mat_hdr.id_simulacao,
    prm_hdr.id_premissa,
    mat_hdr.id_matriz_abast,
    mat_hdr.id_grp_planta,
    mat_hdr.id_centro 
}
