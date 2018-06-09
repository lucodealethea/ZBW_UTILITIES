@AbapCatalog.sqlViewName: 'ZBPCUJDLINKS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Linkage btw UJD_PACKAGE_LINK & UJD_LINKS'
define view ZTV_UJD_LINKS as select from ujd_package_link as PL
left outer join ujd_link as L
    on L.mandt = PL.mandt
    and L.link_id = PL.service_link_id {
    PL.appset_id,
    PL.app_id,
    PL.service_link_id,
    L.content
} 
 