@AbapCatalog.sqlViewName: 'ZBW_AGGREGATES'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Aggregates and other infos'
define view ZTV_AGGRRSDCUBE as select from rsddaggrdir as A
inner join rsdcube as C
    on C.infocube = A.infocube 
    and C.objvers = A.objvers 
    and A.objvers = 'A' {
    
    A.aggruid,
    A.infocube,
    A.aggrcube,
    C.bwappl,
    A.calls,
    A.num_aggr,
    A.num_entries,
    A.lastcall
    
} 
 