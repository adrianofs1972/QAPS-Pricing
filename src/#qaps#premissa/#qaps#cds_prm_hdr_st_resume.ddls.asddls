@AbapCatalog.sqlViewName: '/QAPS/V_PR_ST_RS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Premissa - Header - Status Resume'
define view /QAPS/CDS_PRM_HDR_ST_RESUME as 
select distinct from /QAPS/CDS_PRM_HDR_STATUS as status
inner join /qaps/prem_hdr as header on status.id_premissa = header.id_premissa
left outer join /qaps/grp_planta as grp_planta on header.id_grp_planta = grp_planta.id_grp_planta
left outer join /qaps/centro as centro on header.id_centro = centro.id_centro
{
    
    status.id_simulacao,
    status.id_premissa,
    header.id_grp_planta,
    header.id_centro,
    centro.werks,
    id_item,
    case 
        when soma = 0 then 'X'
        else ''
    end as YELLOW,
    case
        when (soma > 0 and soma < 100) or soma > 100 then 'X'
        else ''
    end as RED,
    case
        when soma = 100 then 'X'
        else ''
    end as GREEN
}
