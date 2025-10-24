@AbapCatalog.sqlViewName: '/QAPS/DIST_TRAJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Distrib x Trajeto'
define view /QAPS/CDS_DISTRIB_TRAJETO as 
select distinct from /qaps/prem_traj as prem_traj
inner join /qaps/trajeto as traj on prem_traj.id_trajeto = traj.id_trajeto
inner join /qaps/v_ponto as origem on origem.id_ponto = traj.id_origem
inner join /qaps/v_ponto as destino on destino.id_ponto = traj.id_destino 
{
   prem_traj.mandt,
   prem_traj.id_prem_trajeto,
   prem_traj.id_distribuicao,
   prem_traj.id_trajeto,
   traj.codigo as cod_trajeto,
   traj.descricao,
   traj.id_origem,
   origem.codigo as cod_origem,
   traj.id_destino,
   destino.codigo as cod_destino
}
