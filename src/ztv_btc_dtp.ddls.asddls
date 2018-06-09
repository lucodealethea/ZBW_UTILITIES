@AbapCatalog.sqlViewName: 'ZBW_BTC_DTP'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'DTP Variant associated to Batch'
define view ZTV_BTC_DTP as select from rsbatchheader as H
inner join rsbatchctrl as C
    on H.batch_id = C.batch_id
inner join rsbkdtp as D on D.dtp = H.pc_variant 
and D.objvers = 'A' and H.process_type = 'DTP_LOAD' {
  
 D.dtp,
 C.jobname,
 H.pc_logid,
 H.pc_variant  
} 
 