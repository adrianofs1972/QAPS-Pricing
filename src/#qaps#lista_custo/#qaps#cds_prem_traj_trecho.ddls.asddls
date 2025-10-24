@AbapCatalog.sqlViewName: '/QAPS/V_FULL_TRJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Distribuição x Trajeto x Trecho'
define view /QAPS/CDS_PREM_TRAJ_TRECHO 
as select distinct from /qaps/prem_distr as distr
inner join /qaps/prem_traj as prem_traj on distr.id_distribuicao = prem_traj.id_distribuicao
inner join /qaps/trajeto as traj on prem_traj.id_trajeto = traj.id_trajeto
inner join /qaps/traj_trech as traj_trech on traj.id_trajeto = traj_trech.id_trajeto
inner join /qaps/trecho as trecho on traj_trech.id_trecho = trecho.id_trecho
inner join /qaps/var_input as input on trecho.id_trecho = input.id_trecho
{
    distr.id_distribuicao,
    traj.id_trajeto,
    traj.codigo as cod_trajeto,
    traj.id_origem,
    traj.id_destino,
    traj.codigo,
    traj.descricao,
    trecho.cod_trecho,
    trecho.id_origem as id_origem_trecho,
    trecho.id_destino as id_destino_trecho,
    trecho.distancia,
    trecho.tempo_deslocamento,
    trecho.id_modal,
    input.id_custo_elementar,
    input.id_var_input,
    sum(input.valor) as valor
}
group by distr.id_distribuicao,
    traj.id_trajeto,
    traj.codigo,
    traj.id_origem,
    traj.id_destino,
    traj.codigo,
    traj.descricao,
    trecho.cod_trecho,
    trecho.id_origem,
    trecho.id_destino,
    trecho.distancia,
    trecho.tempo_deslocamento,
    trecho.id_modal,
    input.id_custo_elementar,
    input.id_var_input
