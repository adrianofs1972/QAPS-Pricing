@AbapCatalog.sqlViewName: '/QAPS/V_MAT_HDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'View Matriz Hdr'
define view /qaps/cds_matriz_hdr  
as select from /qaps/matriz_hdr as header 
//inner join /QAPS/CDS_MATRIZ_HDR_ST_RESUME as status on header.id_matriz_abast = status.id_matriz_abast
inner join /qaps/simulacao as simulacao on header.id_simulacao = simulacao.id_simulacao
left outer join /qaps/cds_ponto as grp_planta on header.id_grp_planta   = grp_planta.id_externo
left outer join /qaps/cds_ponto as centro     on header.id_centro       = centro.id_externo
{
    header.mandt,
    header.id_matriz_abast,
    header.id_simulacao,
    simulacao.descricao as dsc_simulacao,    
    simulacao.status,
    //Grupo de Planta
    case 
        when grp_planta.codigo <> '' then 'G'
        else 'C'
    end as tipo,    
    header.id_grp_planta, 
//    status.YELLOW, 
//    status.RED,
//    status.GREEN,
    grp_planta.codigo,
    grp_planta.dsc_tipo_ponto,
    //centro    
    centro.id_externo as id_centro, 
    centro.codigo as werks,
    centro.dsc_tipo_ponto as dsc_werks,  
    header.id_parent,  
    header.created_by,
    header.created_in,
    header.created_on,
    header.modified_by,
    header.modified_in,
    header.modified_on
}
where header.mandt = $session.client
