@AbapCatalog.sqlViewName: '/QAPS/V_MT_ST_RS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Header - Status Resume'
define view /QAPS/CDS_MATRIZ_HDR_ST_RESUME as 
select distinct from /QAPS/CDS_MATRIZ_HDR_STATUS {
    id_matriz_abast,
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
