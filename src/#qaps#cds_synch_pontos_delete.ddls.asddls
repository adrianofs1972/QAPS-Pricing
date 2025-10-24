@AbapCatalog.sqlViewName: '/QAPS/V_SYNC_DEL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Sincronizar Pontos'
define view /QAPS/CDS_SYNCH_PONTOS_DELETE as 
select from /qaps/ponto as ponto
    left outer join /qaps/porto as porto on ponto.id_externo = porto.id_porto
    left outer join /qaps/cais as cais on ponto.id_externo = cais.id_cais
    left outer join /qaps/regiao_prc as regiao on ponto.id_externo = regiao.id_regiao
    left outer join /qaps/grp_planta as grp_planta on ponto.id_externo = grp_planta.id_grp_planta
    left outer join /qaps/cidade as cidade on ponto.id_externo = cidade.id_cidade    
    association[0..1] to /QAPS/CDS_WERKS as centro
    on $projection.id_externo = centro.id_centro    
    association[0..1] to /QAPS/CDS_KUNNR as cliente
    on $projection.id_externo = cliente.id_cliente
    association[0..1] to /QAPS/CDS_LIFNR as fornec
    on $projection.id_externo = fornec.id_fornecedor
{    
    ponto.id_ponto,
    ponto.tipo_ponto,
    ponto.id_externo,
    case ponto.tipo_ponto
        when 'W' then bintohex( centro.id_centro )
        when 'K' then bintohex( cliente.id_cliente )
        when 'F' then bintohex( fornec.id_fornecedor )
        when 'P' then bintohex( porto.id_porto )
        when 'I' then bintohex( cais.id_cais )
        when 'R' then bintohex( regiao.id_regiao )
        when 'G' then bintohex( grp_planta.id_grp_planta )
        when 'C' then bintohex( cidade.id_cidade )
    end as src_codigo    
}
where ponto.mandt = $session.client
