@AbapCatalog.sqlViewName: '/QAPS/V_PONTO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'View Pontos - FULL'
define view /qaps/cds_ponto as 
select from /qaps/ponto as ponto 
    left outer join /qaps/porto as porto on ponto.id_externo = porto.id_porto
    left outer join /qaps/cais as cais on ponto.id_externo = cais.id_cais
    left outer join /qaps/porto as cais_porto on cais.id_porto = cais_porto.id_porto
    left outer join /qaps/regiao_prc as regiao on ponto.id_externo = regiao.id_regiao
    left outer join /qaps/grp_planta as grp_planta on ponto.id_externo = grp_planta.id_grp_planta
    left outer join /qaps/cidade as cidade on ponto.id_externo = cidade.id_cidade    
    left outer join /qaps/cds_werks as centro on ponto.id_externo = centro.id_centro    
    left outer join /QAPS/CDS_KUNNR as cliente on ponto.id_externo = cliente.id_cliente
    left outer join /QAPS/CDS_LIFNR as fornec on ponto.id_externo = fornec.id_fornecedor
    left outer join /QAPS/CDS_TIPO_ORIG_DEST_TEXT as descricao_tipo_ponto on ponto.tipo_ponto = descricao_tipo_ponto.valor
    
{    
    ponto.id_ponto,
    ponto.tipo_ponto,
    descricao_tipo_ponto.texto as dsc_tipo_ponto, 
    ponto.id_externo,
    case ponto.tipo_ponto
        when 'W' then centro.werks
        when 'K' then cliente.kunnr
        when 'F' then fornec.lifnr
        when 'P' then porto.cod_porto
        when 'I' then cais.cod_cais
        when 'R' then regiao.codigo
        when 'G' then grp_planta.codigo
        when 'C' then cidade.cidade
    end as codigo,
    case ponto.tipo_ponto
        when 'W' then centro.werks_text
        when 'K' then cliente.kunnr_text
        when 'F' then fornec.lifnr_text
        when 'P' then porto.porto
        when 'I' then cais.cais
        when 'R' then regiao.descricao
        when 'G' then grp_planta.descricao
        when 'C' then CONCAT(cidade.cidade, CONCAT('-', cidade.uf)) 
    end as descricao,
    case ponto.tipo_ponto
        when 'I' then cais_porto.cod_porto
    end as cod_porto,
    case ponto.tipo_ponto
        when 'I' then cais_porto.porto
    end as porto
    
}
where ponto.mandt = $session.client
