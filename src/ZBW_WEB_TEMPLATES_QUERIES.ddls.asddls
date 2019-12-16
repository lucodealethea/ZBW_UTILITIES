@AbapCatalog.sqlViewName: 'ZVBW_WEB_TEMP_Q'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Queries used in WAD-BI-IP'
define view ZBW_WEB_TEMPLATES_QUERIES as select from rszwbtmpheadtxt as T1
inner join rszwbtmpxref as T2
on T1.objid = T2.objid and T1.objvers = T2.objvers and T1.langu = 'E'
inner join rsrrepdir as T3 on T2.objnm = T3.compuid
and T3.objvers = T2.objvers
inner join rszelttxt as T4 on T3.compuid = T4.eltuid
and T3.objvers = T4.objvers and T4.langu = 'E'
inner join rszcompdir as T5 on T5.compuid = T3.compuid
and T5.objvers = T3.objvers
{
key T1.objid,
key T1.objvers as OBJVERS_WT,
key T1.langu as LANGU_WT,
T1.txtlg as TXTLG_WT,
T2.objnm,
T2.asc_type,
T2.subtype,
T3.compuid as COMPUID_REP,
T3.objvers as OBJVERS_REP,
T3.infocube,
T3.genuniid,
T3.comptype,
T3.compid as COMPID_REP,
case substring(T3.compid,1,1) when '0' then 'Y' else 'N'  end as STD,
T4.eltuid,
T4.objvers,
T4.langu,
T4.line_count,
T4.txtsh,
T4.txtlg,
T5.compuid,
T5.objvers as OBJVERS_COMP,
T5.compid as COMPID_COMP,
T5.version
}
