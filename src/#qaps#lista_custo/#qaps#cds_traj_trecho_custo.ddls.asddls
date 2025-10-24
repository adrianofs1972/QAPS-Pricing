@AbapCatalog.sqlViewName: '/QAPS/V_TJ_TR_CS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Trajeto x Trecho x Custo'
define view /QAPS/CDS_TRAJ_TRECHO_CUSTO 
as select distinct from /QAPS/CDS_TRAJETO_TRECHOS as traj_techo 
left outer join /qaps/var_input as input
on traj_techo.id_trecho = input.id_trecho
left outer join /qaps/custo_elm as custo_elm
on input.id_custo_elementar = custo_elm.id_custo_elementar
{
    traj_techo.id_traj_trecho,
    traj_techo.id_trajeto,
    traj_techo.ordem,
    traj_techo.codigo,
    traj_techo.descricao,
    traj_techo.id_trecho,
    traj_techo.tipo_origem,
    traj_techo.id_origem,
    traj_techo.cod_origem,
    traj_techo.dsc_origem,
    traj_techo.tipo_destino,
    traj_techo.id_destino,
    traj_techo.cod_destino,
    traj_techo.dsc_destino,
    traj_techo.id_modal,
    traj_techo.tempo_deslocamento,
    traj_techo.Distancia,
    input.id_custo_elementar,
    custo_elm.importacao,
    custo_elm.nacional,
    custo_elm.transferencia,
    input.valor    
}
