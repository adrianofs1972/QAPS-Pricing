@AbapCatalog.sqlViewName: '/QAPS/V_TRJ_TRC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Trajetos x Trechos'
define view /QAPS/CDS_TRAJETO_TRECHOS 
as select from /qaps/trajeto  as trajeto
inner join /qaps/traj_trech as traj_trecho on trajeto.id_trajeto = traj_trecho.id_trajeto
inner join /qaps/trecho as trecho on traj_trecho.id_trecho = trecho.id_trecho
inner join /qaps/v_ponto as origem on trecho.id_origem = origem.id_ponto
inner join /qaps/v_ponto as destino on trecho.id_destino = destino.id_ponto
{
    
    traj_trecho.id_traj_trecho,
    traj_trecho.id_trajeto,    
    traj_trecho.ordem,    
    trajeto.codigo,
    trajeto.descricao,    
    trecho.id_trecho,
    trecho.cod_trecho,
    trecho.tipo_origem,
    trecho.id_origem,
    origem.codigo as cod_origem,    
    origem.descricao as dsc_origem,
    trecho.tipo_destino,
    trecho.id_destino,
    destino.codigo as cod_destino,
    destino.descricao as dsc_destino,
    trecho.id_modal,
    trecho.tempo_deslocamento,
    trecho.distancia as Distancia
        
}
