@AbapCatalog.sqlViewName: 'ZBW_BATCH_PC'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Get the batch jobs and process chain variant'
define view ZTV_BATCH_PC as select distinct from tbtcp as it
inner join tbtco as hdr
on hdr.jobname = it.jobname and hdr.jobcount = it.jobcount
left outer join rsbatchheader as btc on hdr.jobcount = btc.jobcount and hdr.jobname = btc.jobname 
inner join rspcchain as pc on pc.variante = btc.pc_variant and pc.objvers = 'A'
inner join rspcchaint as tx on tx.chain_id = pc.chain_id and tx.langu = 'E'
inner join rspcchainattr as at on at.chain_id = pc.chain_id
{
   btc.batch_id,                   
    btc.jobname,                   
    btc.jobcount,                  
    it.stepcount,                 
    it.progname,                  
    it.variant,                   
    btc.server,                    
    btc.host,                      
    btc.wp_no,                     
    btc.wp_pid,                    
    btc.ts_start,                  
    btc.uname,                     
    btc.process_type,              
    btc.pc_variant,                
    btc.pc_instance,               
    btc.pc_logid,                  
    btc.status,                    
    btc.parent_batch_id,           
    btc.start_at_once,
    btc.hold_par_procs,                 
    btc.detlevel,
    pc.auto_repeat,
    at.polling,
    at.applnm,
    tx.txtlg
    
    
}  
 