@AbapCatalog.sqlViewName: '/QAPS/V_MT_DS_BS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Matriz de Abastecimento - Destinos - Base'
define view /qaps/cds_matriz_tree_base 
as select from /qaps/centro as centro
inner join /qaps/grp_planta as grp_planta on centro.id_grp_planta = grp_planta.id_grp_planta  
inner join /qaps/cds_ponto as ponto on grp_planta.id_grp_planta = ponto.id_externo
{               
        centro.mandt,        
        ponto.id_ponto,
        centro.id_grp_planta as id,
        'G' as tipo,
        centro.id_centro  as id_centro,
        centro.werks,
        centro.id_grp_planta as id_grp_planta,
        grp_planta.codigo,
        grp_planta.descricao                     
}
where centro.mandt = $session.client
union  
select from /qaps/centro as centro
left outer join /qaps/grp_planta as grp_planta on centro.id_grp_planta = grp_planta.id_grp_planta  
inner join /qaps/cds_ponto as ponto on centro.id_centro = ponto.id_externo
{               
        centro.mandt,
        ponto.id_ponto,
        centro.id_centro  as id,        
        'C' as tipo,
        centro.id_centro  as id_centro,
        centro.werks,
        centro.id_grp_planta as id_grp_planta,
        grp_planta.codigo,
        grp_planta.descricao                     
}
where centro.mandt = $session.client
and   grp_planta.codigo is null


