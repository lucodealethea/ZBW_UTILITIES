* ABAP CDS ZBPC_DIMAPP2 describes BPC Models
* Table Function on ZBPC_DIMAPP2 returns list of fields to build a CDS view or a TF 
* Next step will be to generate automatically if any change occured on BPC model, the Class and TF with Code Composer
@AbapCatalog.sqlViewName: 'ZVBPC_DIMAPP2'
@EndUserText.label: 'MetaData For BPC Dimensions (short)'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

define view ZBPC_DIMAPP2 as
select from uja_dimension as D                                            
inner join uja_dim_appl as DA                                                                        
    on D.mandt = DA.mandt                                                                            
    and D.appset_id = DA.appset_id                                                                   
    and D.dimension = DA.dimension
inner join uja_appl as AP on AP.mandt = DA.mandt and AP.appset_id = DA.appset_id and AP.application_id = DA.application_id     
inner join uja_appset_info as DS on DS.mandt = DA.mandt and DS.appset_id = DA.appset_id 
inner join uja_dimensiont as TX on D.mandt = TX.mandt                                                
and D.appset_id = TX.appset_id                                                                       
and D.dimension = TX.dimension                                                                       
and TX.langu = 'E'                                                                                   
left outer join dd07v as VD on D.dim_type = VD.domvalue_l and VD.domname='UJ_DIM_TYPE' and VD.ddlanguage = 'E'
inner join uja_appl as MD on MD.mandt = DA.mandt and MD.appset_id = DA.appset_id and DA.application_id = MD.application_id                                                                                           
     {                                                                                               
    key D.appset_id,                                                                                     
    key DA.application_id,
    key D.dimension,
    D.tech_name,
    concat('/B28/S_',substring(D.tech_name,7,12)) as FieldName,
    concat(substring(D.tech_name,1,6),substring(D.tech_name,7,12)) as FieldName2, 
    concat(replace(MD.infocube,'/CPMB/','/B28/A'),'7') as ADSO_VIEW,      
    MD.infocube,     
    D.data_table,                                                                                      
    concat('/B28/P',substring(D.tech_name,7,12))as mdata_bw_table,
//descendants table where NIV = 0 equals  data_table where /CPMB/CALC = N
// but have to exclude hier nodes without children with IS_BAS_EQ_NODE = Y 
// this property is custom and exists only in OBS  
    replace(D.tech_name,'/CPMB/','/1CPMB/P') as desc_table,
    D.desc_table as text_tabl,                                                                       
    D.hier_data_table as hier_table
                              
}                                                                                                    

@EndUserText.label: 'Generate MetaData From BPC Models'
@ClientDependent: false
@AccessControl.authorizationCheck: #NOT_REQUIRED
define table function ZBPC_TF_METAGEN

with parameters 
p_appset : UJ_APPSET_ID,
p_app : UJ_APPL_ID
returns {
  
  
  APPLICATION_ID              : UJ_APPL_ID;
  ADSO                        : RSOADSONM;
  ADSO_VIEW                   : TABLE_NAME;
  POSITION                    : TABFDPOS;
  DIMENSION                   : UJ_DIM_NAME;
  FIELDNAME                   : FIELDNAME;
  MDATA_BW_TABLE              : TABLE_NAME;
  ROLLNAME                    : ROLLNAME;
  FIELDS_GROUPBY              : RRTMDXSTATEMENT;
  FIELDS_SELECT               : RRTMDXSTATEMENT;
  FIELDS_GROUPBY_CDS          : RRTMDXSTATEMENT;
  FIELDS_SELECT_CDS           : RRTMDXSTATEMENT;
  FIELDS_TF                   : RRTMDXSTATEMENT;
  
  
}
implemented by method ZCL_BPC_METADATA=>GET_METADATA_FOR_MODEL;

class zcl_bpc_metadata definition
  public
  final
  create public .

public section.
 INTERFACES if_amdp_marker_hdb .

* TYPES:
*  BEGIN OF g_t_app_meta,
*  APPLICATION_ID              TYPE UJ_APPL_ID,
*  ADSO                        TYPE RSOADSONM,
*  ADSO_VIEW                   TYPE TABLE_NAME,
*  POSITION                    TYPE TABFDPOS,
*  DIMENSION                   TYPE UJ_DIM_NAME,
*  FIELDNAME                   TYPE FIELDNAME,
*  MDATA_BW_TABLE              TYPE TABLE_NAME,
*  ROLLNAME                    TYPE ROLLNAME,
*  FIELDS_GROUBY               TYPE RRTMDXSTATEMENT,
*  FIELDS_SELECT               TYPE RRTMDXSTATEMENT,
*  FIELDS_GROUBY_CDS           TYPE RRTMDXSTATEMENT,
*  FIELDS_SELECT_CDS           TYPE RRTMDXSTATEMENT,
*  FIELDS_TF                   TYPE RRTMDXSTATEMENT,
*  END OF g_t_app_meta.
*  TYPES:
*  gtt_app_meta TYPE STANDARD TABLE OF g_t_app_meta.

  CLASS-METHODS:

  GET_METADATA_FOR_MODEL
  FOR TABLE function ZBPC_TF_METAGEN.

protected section.
private section.
endclass.



class zcl_bpc_metadata implementation.

METHOD  GET_METADATA_FOR_MODEL
    BY DATABASE FUNCTION FOR HDB
          LANGUAGE SQLSCRIPT
          OPTIONS READ-ONLY
          USING ZVBPC_DIMAPP2 DD03L
          .

 lt_metadata=
 SELECT
 APPLICATION_ID
,DIMENSION
,TECH_NAME
,FIELDNAME
,INFOCUBE AS ADSO
,ADSO_VIEW
,MDATA_BW_TABLE
FROM ZVBPC_DIMAPP2
WHERE APPSET_ID = :p_appset AND APPLICATION_ID = :p_app
;
lt_max_pos=
SELECT
max(DD.POSITION) as MAX_POS
FROM :lt_metadata as M
INNER JOIN DD03L AS DD ON DD.TABNAME = M.ADSO_VIEW AND M.FIELDNAME = DD.FIELDNAME AND DD.AS4LOCAL = 'A'
;
lt_meta_fields=
SELECT
META.APPLICATION_ID,
META.ADSO,
META.ADSO_VIEW,
DD.POSITION,
META.DIMENSION,
META.FIELDNAME,
META.MDATA_BW_TABLE,
DD2.ROLLNAME,
'"F"."'||META.FIELDNAME||'",' AS FIELDS_GROUPBY,
'"F"."'||META.FIELDNAME||'" '||'AS '||META.DIMENSION||',' AS FIELDS_SELECT,
'F.'||META.FIELDNAME||',' AS FIELDS_GROUPBY_CDS,
'key F.'||META.FIELDNAME||' AS '||META.DIMENSION||',' AS FIELDS_SELECT_CDS,
META.DIMENSION||' : '||DD2.ROLLNAME||' ;' AS FIELDS_TF

FROM :lt_metadata as META
INNER JOIN DD03L AS DD ON DD.TABNAME = META.ADSO_VIEW AND META.FIELDNAME = DD.FIELDNAME AND DD.AS4LOCAL = 'A'

LEFT OUTER JOIN DD03L AS DD2 ON DD2.TABNAME = META."MDATA_BW_TABLE" AND META.FIELDNAME = DD2.FIELDNAME AND DD.AS4LOCAL = 'A'

/*
AND DD.TABCLASS = 'VIEW'
*/
UNION

SELECT
:p_app AS APPLICATION_ID,
( SELECT DISTINCT ADSO FROM :lt_metadata ) AS ADSO,
( SELECT DISTINCT ADSO_VIEW FROM :lt_metadata ) AS ADSO_VIEW,
(SELECT LPAD(TO_VARCHAR(TO_INTEGER(MAX_POS)+1),4,'0') AS MAX_POS FROM :lt_max_pos)  AS POSITION,
'SIGNEDDATA' AS DIMENSION,
'/B28/S_SDATA' AS FIELDNAME,
'' AS MDATA_BW_TABLE,
'/B28/OISDATA' AS ROLLNAME,
'"F"."'||'/B28/S_SDATA"' AS FIELDS_GROUPBY,
'SUM("F"."/B28/S_SDATA")'||' AS SIGNEDDATA' AS FIELDS_SELECT,
'F.'||'/B28/S_SDATA' AS FIELDS_GROUPBY_CDS,
'SUM(F./B28/S_SDATA)'||' AS SIGNEDDATA' AS FIELDS_SELECT_CDS,
'SIGNEDDATA'||' : '||'/B28/OISDATA'||' ;' AS FIELDS_TF

FROM SYS.DUMMY

ORDER BY 4 ASC
;

RETURN
SELECT * from :lt_meta_fields
;

endmethod.
endclass.

*&---------------------------------------------------------------------*
*& Report ZBPC_DIM_APP
*&---------------------------------------------------------------------*
*& Specify Environment and Model
*&---------------------------------------------------------------------*
report zbpc_dim_app.

parameters:
p_env type uj_appset_id default 'xxx' ,
p_app type uj_appl_id default 'PLANNING'.

data:       m3      type string,
            l_title type sy-title.

field-symbols <itab> type standard table.

start-of-selection.

select
   zbpc_tf_metagen~application_id,
   zbpc_tf_metagen~adso,
   zbpc_tf_metagen~adso_view,
   zbpc_tf_metagen~position,
   zbpc_tf_metagen~dimension,
   zbpc_tf_metagen~fieldname,
   zbpc_tf_metagen~mdata_bw_table,
   zbpc_tf_metagen~rollname,
   zbpc_tf_metagen~fields_groupby,
   zbpc_tf_metagen~fields_select,
   zbpc_tf_metagen~fields_groupby_cds,
   zbpc_tf_metagen~fields_select_cds,
   zbpc_tf_metagen~fields_tf
 from
  zbpc_tf_metagen( p_appset = @p_env, p_app = @p_app )
  into table @data(lt_metadata)
  ##db_feature_mode[amdp_table_function].

perform callback_alv
using l_title abap_true lt_metadata.

form callback_alv using
  value(i_title) type sy-title
  value(i_sort)  type abap_bool
  it_data        type any table.

  if it_data is initial.
  concatenate 'No data found with : ' p_env '/' p_app  into m3.
    message i799(rsm1) with m3.

  else.

    if i_sort = abap_true.
      sort it_data.
    endif.

    call function 'RSDU_CALL_ALV_TABLE'
      exporting
        i_title   = i_title
        i_ta_data = it_data.

  endif.

endform.                    "callback_alv
