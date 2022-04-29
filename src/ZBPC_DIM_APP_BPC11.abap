* ABAP CDS ZBPC_DIMAPP2, ZBPC_DIM_ATTR ZVBPC_DIM_HIERS describes BPC Models/Dimensions/Hierarchies
* Table Function on ZBPC_DIMAPP2 returns list of fields to build a CDS view or a TF 
* SE38 ZBPC_DIM_APP returns the list of fields(to build CDS View or AMDP/TF) with option to delete column (sensitive data)
* Next step will be to generate automatically if any change occured on BPC model, the AMDP Class along with TF with Code Composer
* CDS Table Functions for Hierarchy Browsing (Ancestors/Descendants) that can easily be used in UJ_CUSTOM_LOGIC calling an AMDP

@AbapCatalog.sqlViewName: 'ZVBPC_DIM_HIERS'
@EndUserText.label: 'MetaData For BPC Hierarchies'
@ClientHandling.type: #CLIENT_DEPENDENT
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

define view ZBPC_DIM_HIERS as 
select from uja_dim_hier2 as H
inner join uja_dimension as D
    on H.appset_id = D.appset_id and H.dimension = D.dimension and H.mandt = D.mandt 
//and H.mandt = $session.client
inner join uja_dim_hie_map2 as H2 on H2.dim_tech_name = D.tech_name and H2.hier_name = H.hierarchy_name and H2.mandt = H.mandt
inner join rshiedir as DIR on DIR.iobjnm = D.tech_name and DIR.hienm = H.hierarchy_name and DIR.objvers = 'A'    
//b28/h
{
key H.mandt,
key D.appset_id as AppsetId,
key DIR.hieid,
key D.dimension as Dimension,
D.num_hier as NumHier,
D.tech_name as TechName,
DIR.hienm,
H.datefrom,
H.dateto,
H.caption,
D.hier_time_dep as HierTimeDep,
H2.hier_table,
concat('/B28/H',substring(D.tech_name,7,10))as BW,
DIR.hietype,
DIR.startlevel,
DIR.numberlevel,
DIR.rootid,
DIR.maxnodeid,
DIR.timestmp,
D.struc_modif_date as StrucModifDate,
D.struc_modif_time as StrucModifTime,
D.mbr_modif_date as MbrModifDate,
D.mbr_modif_time as MbrModifTime,
D.data_table as DataTable,
D.desc_table as DescTable,
D.hier_data_table as HierDataTable,
D.tda_table as TdaTable    
}

@AbapCatalog.sqlViewName: 'ZVBPC_ATTRIBUTES'
@EndUserText.label: 'BPC Dimension Attributes'
@ClientHandling.type: #CLIENT_DEPENDENT
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

define view ZBPC_ATTRIBUTES as 
select from uja_dim_attr as A
inner join uja_dimension as D 
    on A.appset_id = D.appset_id
    and A.dimension = D.dimension
    and A.mandt = D.mandt 
//    and D.mandt = $session.client
left outer join trese as RW on RW.name = A.attribute_name    
{
key A.mandt
,key A.appset_id
,key A.dimension
,D.dim_type
,D.dim_type_index
,D.ref_dim
,D.num_hier
,D.tech_name
,D.server_version
,D.file_version
,D.file_lock
,D.process_state
,D.locked_by
,D.caption
,D.hier_time_dep
,D.process_date
,D.process_time
,D.struc_modif_date
,D.struc_modif_time
,D.mbr_modif_date
,D.mbr_modif_time
,D.data_table
,concat('/B28/P',substring(D.tech_name,7,12))as mdata_bw_table
,D.desc_table
,D.hier_data_table
,D.tda_table
,A.tech_name as attr_tech_name
,replace(A.tech_name,'/CPMB/','/B28/S_') as bw_field
,case when RW.sourcehint is null then A.attribute_name else concat(A.attribute_name,'1') end as attribute_name
,A.attribute_size
,A.attribute_type
,A.time_dependent
,A.caption as attr_caption
,A.f_display
,A.f_generate
,A.f_uppercase
,A.valid_id    
}

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
    D.num_hier, 
    concat('/B28/S_',substring(D.tech_name,7,12)) as FieldName,
    concat(substring(D.tech_name,1,6),substring(D.tech_name,7,12)) as FieldName2, 
    concat(replace(MD.infocube,'/CPMB/','/B28/A'),'7') as ADSO_VIEW,
    cast(concat('ZVBPC_',DA.application_id) as TABLE_NAME) as CUSTOM_VIEW,
    cast(concat('ZBPC_',DA.application_id) as DDLNAME) as CUSTOM_CDS_VIEW,
    cast(concat('ZBPC_TF_',DA.application_id) as DDLNAME) as CUSTOM_TF,          
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
  CUSTOM_VIEW                 : TABLE_NAME;
  CUSTOM_CDS_VIEW             : DDLNAME;
  CUSTOM_TF                   : DDLNAME;      
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

--------------------BPC11---------------------------------------------------
class zcl_bpc_metadata definition
  public
  final
  create public .

public section.
 INTERFACES if_amdp_marker_hdb .

 TYPES:
  BEGIN OF g_t_app_meta,
  APPLICATION_ID              TYPE UJ_APPL_ID,
  ADSO                        TYPE RSOADSONM,
  ADSO_VIEW                   TYPE TABLE_NAME,
  CUSTOM_VIEW                 TYPE TABLE_NAME,
  CUSTOM_CDS_VIEW             TYPE TABLE_NAME,
  CUSTOM_TF                   TYPE TABLE_NAME,
  POSITION                    TYPE TABFDPOS,
  DIMENSION                   TYPE UJ_DIM_NAME,
  FIELDNAME                   TYPE FIELDNAME,
  MDATA_BW_TABLE              TYPE TABLE_NAME,
  ROLLNAME                    TYPE ROLLNAME,
  FIELDS_GROUBY               TYPE RRTMDXSTATEMENT,
  FIELDS_SELECT               TYPE RRTMDXSTATEMENT,
  FIELDS_GROUBY_CDS           TYPE RRTMDXSTATEMENT,
  FIELDS_SELECT_CDS           TYPE RRTMDXSTATEMENT,
  FIELDS_TF                   TYPE RRTMDXSTATEMENT,
  END OF g_t_app_meta.
  TYPES:
  gtt_app_meta TYPE STANDARD TABLE OF g_t_app_meta.

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
          USING ZVBPC_DIMAPP2 DD03L T000 "DD03K
          .
 declare v_sys char( 10 );

 SELECT LOGSYS INTO v_sys FROM "T000" WHERE "MANDT" = '100';

 lt_metadata=
 SELECT
 APPLICATION_ID
,DIMENSION
,TECH_NAME
,FIELDNAME
,INFOCUBE AS ADSO
,ADSO_VIEW
,CUSTOM_VIEW
,CUSTOM_CDS_VIEW
,CUSTOM_TF
,MDATA_BW_TABLE
FROM ZVBPC_DIMAPP2
WHERE APPSET_ID = :p_appset AND APPLICATION_ID = :p_app
;

lt_max_pos=
SELECT
max(DD.POSITION) as MAX_POS
FROM :lt_metadata as M
INNER JOIN DD03L AS DD ON DD.TABNAME = M.ADSO_VIEW AND M.FIELDNAME = DD.FIELDNAME AND DD.AS4LOCAL = 'A';

lt_meta_fields=
SELECT
META.APPLICATION_ID,
META.ADSO,
META.ADSO_VIEW,
META.CUSTOM_VIEW,
META.CUSTOM_CDS_VIEW,
META.CUSTOM_TF,
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
LEFT OUTER JOIN DD03L AS DD2 ON DD2.TABNAME = META."MDATA_BW_TABLE" AND META.FIELDNAME = DD2.FIELDNAME AND DD2.AS4LOCAL = 'A'
/*
AND DD.TABCLASS = 'VIEW'
*/
UNION

SELECT
:p_app AS APPLICATION_ID,
( SELECT DISTINCT ADSO FROM :lt_metadata ) AS ADSO,
( SELECT DISTINCT ADSO_VIEW FROM :lt_metadata ) AS ADSO_VIEW,
( SELECT DISTINCT CUSTOM_VIEW FROM :lt_metadata ) AS CUSTOM_VIEW,
( SELECT DISTINCT CUSTOM_CDS_VIEW FROM :lt_metadata ) AS CUSTOM_CDS_VIEW,
( SELECT DISTINCT CUSTOM_TF FROM :lt_metadata ) AS CUSTOM_TF,
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

UNION

SELECT
:p_app AS APPLICATION_ID,
( SELECT DISTINCT ADSO FROM :lt_metadata ) AS ADSO,
( SELECT DISTINCT ADSO_VIEW FROM :lt_metadata ) AS ADSO_VIEW,
( SELECT DISTINCT CUSTOM_VIEW FROM :lt_metadata ) AS CUSTOM_VIEW,
( SELECT DISTINCT CUSTOM_CDS_VIEW FROM :lt_metadata ) AS CUSTOM_CDS_VIEW,
( SELECT DISTINCT CUSTOM_TF FROM :lt_metadata ) AS CUSTOM_TF,
'0001'  AS POSITION,
'' AS DIMENSION,
'REQTSN' AS FIELDNAME,
'' AS MDATA_BW_TABLE,
'RSPM_REQUEST_TSN' AS ROLLNAME,
'' AS FIELDS_GROUPBY,
'' AS FIELDS_SELECT,
'' AS FIELDS_GROUPBY_CDS,
'' AS FIELDS_SELECT_CDS,
'' AS FIELDS_TF

FROM SYS.DUMMY

UNION

SELECT
:p_app AS APPLICATION_ID,
( SELECT DISTINCT ADSO FROM :lt_metadata ) AS ADSO,
( SELECT DISTINCT ADSO_VIEW FROM :lt_metadata ) AS ADSO_VIEW,
( SELECT DISTINCT CUSTOM_VIEW FROM :lt_metadata ) AS CUSTOM_VIEW,
( SELECT DISTINCT CUSTOM_CDS_VIEW FROM :lt_metadata ) AS CUSTOM_CDS_VIEW,
( SELECT DISTINCT CUSTOM_TF FROM :lt_metadata ) AS CUSTOM_TF,
'0002'  AS POSITION,
'' AS DIMENSION,
'RECORDMODE' AS FIELDNAME,
'' AS MDATA_BW_TABLE,
'' AS ROLLNAME,
'' AS FIELDS_GROUPBY,
'' AS FIELDS_SELECT,
'' AS FIELDS_GROUPBY_CDS,
'' AS FIELDS_SELECT_CDS,
'' AS FIELDS_TF

FROM SYS.DUMMY


ORDER BY 4 ASC
;

RETURN
SELECT * from :lt_meta_fields
;

endmethod.
endclass.

----------------------------------------------------------------------------
REPORT zbpc_dim_app.

PARAMETERS:
p_env TYPE uj_appset_id DEFAULT 'xxx' ,
p_app TYPE uj_appl_id DEFAULT 'SGA'.

TYPE-POOLS: abap.

DATA:       m3      TYPE string,
            l_title TYPE sy-title,
            ls_metadata TYPE REF TO data,
*Table to hold the components
            tab_return TYPE abap_compdescr_tab,
*Work area for the component table
            components LIKE LINE OF tab_return,
            w_typ TYPE REF TO cl_abap_elemdescr,
            lt_tot_comp    TYPE cl_abap_structdescr=>component_table,
            lt_comp        TYPE cl_abap_structdescr=>component_table,
            la_comp        LIKE LINE OF lt_comp,
            lo_new_type    TYPE REF TO cl_abap_structdescr,
            lo_table_type  TYPE REF TO cl_abap_tabledescr,
            w_tref         TYPE REF TO data,
            w_dy_line      TYPE REF TO data.


FIELD-SYMBOLS: <dyn_tab>  TYPE STANDARD TABLE,
               <dyn_wa>,
               <dyn_field>.

START-OF-SELECTION.

SELECT
   zbpc_tf_metagen~application_id,
   zbpc_tf_metagen~adso,
   zbpc_tf_metagen~adso_view,
   zbpc_tf_metagen~custom_view,
   zbpc_tf_metagen~custom_cds_view,
   zbpc_tf_metagen~custom_tf,
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
 FROM
  zbpc_tf_metagen( p_appset = @p_env, p_app = @p_app )
  INTO TABLE @DATA(lt_metadata)
  ##db_feature_mode[amdp_table_function].

CREATE DATA ls_metadata LIKE LINE OF lt_metadata.

CONCATENATE 'Metadata--' p_app '--' INTO l_title.


*Call Perform to get the Int. Table Components
PERFORM get_int_table_fields USING    lt_metadata
                            CHANGING  tab_return.
* break-point.
* LENGTH, DECIMALS, TYPE_KIND, NAME

* for example only, in case we want to remove some fields,
* DELETE tab_return WHERE name = 'MDATA_BW_TABLE'.
* perform build_another_it using tab_return.

* and pass lt_metadata content into the newly created table
SORT lt_metadata BY POSITION.
PERFORM callback_alv
USING l_title abap_true lt_metadata.
*using l_title abap_true <dyn_tab>.

FORM callback_alv USING
  VALUE(i_title) TYPE sy-title
  VALUE(i_sort)  TYPE abap_bool
  it_data        TYPE ANY TABLE.

  IF it_data IS INITIAL.
  CONCATENATE 'No data found with : ' p_env '/' p_app  INTO m3.
    MESSAGE i799(rsm1) WITH m3.

  ELSE.

    IF i_sort = abap_true.
      SORT it_data.
    ENDIF.

    CALL FUNCTION 'RSDU_CALL_ALV_TABLE'
      EXPORTING
        i_title   = i_title
        i_ta_data = it_data.

  ENDIF.

ENDFORM.                    "callback_alv

FORM get_int_table_fields  USING    t_data TYPE ANY TABLE
                           CHANGING t_return TYPE abap_compdescr_tab.

  DATA:
  oref_table TYPE REF TO cl_abap_tabledescr,
  oref_struc TYPE REF TO cl_abap_structdescr,
  oref_error TYPE REF TO cx_root,
  text TYPE string.
*Get the description of data object type
  TRY.
      oref_table ?=
      cl_abap_tabledescr=>describe_by_data( t_data ).
    CATCH cx_root INTO oref_error.
      text = oref_error->get_text( ).
      WRITE: / text.
      EXIT.
  ENDTRY.
*Get the line type
  TRY.
      oref_struc ?= oref_table->get_table_line_type( ).
    CATCH cx_root INTO oref_error.
      text = oref_error->get_text( ).
*      write: / text.
      EXIT.
  ENDTRY.
**  begin of abap_compdescr,
*    length    type i,
*    decimals  type i,
*    type_kind type abap_typekind,
*    name      type abap_compname,
*  end of abap_compdescr,
  APPEND LINES OF oref_struc->components TO t_return.
  CLEAR: components.

ENDFORM.                    " GET_INT_TABLE_FIELDS

FORM build_another_it USING w_data TYPE ANY TABLE.

 LOOP AT w_data INTO components.
  CASE components-type_kind.
      WHEN 'STRING'.  w_typ = cl_abap_elemdescr=>get_string( ).
      WHEN 'XSTRING'. w_typ = cl_abap_elemdescr=>get_xstring( ).
      WHEN 'I'.       w_typ = cl_abap_elemdescr=>get_i( ).
      WHEN 'F'.       w_typ = cl_abap_elemdescr=>get_f( ).
      WHEN 'D'.       w_typ = cl_abap_elemdescr=>get_d( ).
      WHEN 'T'.       w_typ = cl_abap_elemdescr=>get_t(  ).
      WHEN 'C'.       w_typ = cl_abap_elemdescr=>get_c( p_length = components-length ).
      WHEN 'N'.       w_typ = cl_abap_elemdescr=>get_n( p_length = components-length ).
      WHEN 'X'.       w_typ = cl_abap_elemdescr=>get_x( p_length = components-length ).
      WHEN 'P'.       w_typ = cl_abap_elemdescr=>get_p( p_length = components-length p_decimals = components-decimals ).
  ENDCASE.

 CLEAR la_comp.
    la_comp-type = w_typ.               "Field type
    la_comp-name = components-name.       "Field name   ex: FIELD1
    APPEND la_comp TO lt_tot_comp.      "Add entry to component table

  ENDLOOP.

* Create new type from component table
  lo_new_type = cl_abap_structdescr=>create( lt_tot_comp ).

* Create new table type
  lo_table_type = cl_abap_tabledescr=>create( lo_new_type ).

* Create dynamic internal table and assign to Field Symbol
  CREATE DATA w_tref TYPE HANDLE lo_table_type.
  ASSIGN w_tref->* TO <dyn_tab>.

* Create dynamic work area and assign to Field Symbol
  CREATE DATA w_dy_line LIKE LINE OF <dyn_tab>.
  ASSIGN w_dy_line->* TO <dyn_wa>.

     cl_abap_corresponding=>create(
      source            = lt_metadata
      destination       = <dyn_tab>
      mapping           = VALUE cl_abap_corresponding=>mapping_table(  )
      )->execute( EXPORTING source      = lt_metadata
                  CHANGING  destination = <dyn_tab> ).

ENDFORM.                    "build_another_it

------------------------------------------WBS_L1 Hierarchy (not with Child-Parent CDS Hierarchy ) -------------------
@AbapCatalog.sqlViewName: 'ZVBW_WBS_L1_H'
@EndUserText.label: 'WBS_L1 Hierarchies'
@ClientHandling.type: #CLIENT_DEPENDENT
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
// ZODCDM8
define view ZBW_WBS_L1_H as
select from /b28/hzodcdm8 as H
inner join ZBPC_DIM_HIERS as MH on MH.hieid = H.hieid and MH.AppsetId = 'TRACTEBEL_TEMIS' and MH.Dimension = 'WBS_L1' 
//and MH.mandt = $session.client
left outer join rsthiernode as T on T.hieid = H.hieid and H.nodename = T.nodename and H.objvers = T.objvers and H.objvers = 'A'
and T.langu = $session.system_language

left outer join /b28/tzodcdm8 as MT on MT./b28/s_zodcdm8 = H.nodename and MT.langu = $session.system_language
left outer join /b28/hzodcdm8 as H2 on H.parentid = H2.nodeid and H.hieid = H2.hieid
left outer join /b28/tzodcdm8 as MT2 on MT2./b28/s_zodcdm8 = H2.nodename and MT2.langu = $session.system_language
left outer join /b28/pzodcdm8 as BM on H.nodename = BM./b28/s_zodcdm8 and BM.objvers = H.objvers
{
key MH.mandt,
key H.hieid,
key MH.caption as hier_name,
key cast(H.parentid  as abap.int8) as parent_id,
key cast(H.nodeid as abap.int8) as node_id,
H.nodename,
H.iobjnm,
coalesce(coalesce(T.txtmd,T.txtsh),MT.txtlg) as txtmdtx,
cast(H.parentid as rsparent ) as parentido,
cast(H.nodeid as rshienodid ) as nodeido,
H2.nodename as parent_nodename,
MT2.txtlg as parent_txtmdtx,
// to get the leafs IS_NODE = N or HIERARCHY_TREE_SIZE = 1
BM./b28/s_calc as IS_NODE,
BM./b28/s_hir as HIR,
H.tlevel,
/b28/s_scaling as SCALING
,/b28/s_zophxsg as APPLICANT
,/b28/s_zopvcp6 as BUSCLASS1
,/b28/s_zopu5r4 as BUSCLASS2
,/b28/s_zop5kh0 as BUS_AREA
,/b28/s_zopcv9d as COMP_CODE
,/b28/s_zoptkj3 as COUNTRY
,/b28/s_zopunxq as COUNTRY_SOLDTO
,/b28/s_zopnphb as FUNCTIONAL_AREA
,/b28/s_zope01s as PER_RES
,/b28/s_zop9my8 as PROFIT_CCTR
,/b28/s_zopg426 as RAKEY_L1
,/b28/s_zop31e5 as RESP_CC
,/b28/s_zopoque as SEGMENT
,/b28/s_zop6bdt as SHIP_TO
,/b28/s_zop0txi as SOLD_TO
,/b28/s_zopznyo as WBS_SYS_STATUS
  
}
--------------------------------------------TF for Hierarchy Descendants --------------------------
@EndUserText.label: 'Descendants for WBS_L1 Hierarchy'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_DEPENDENT
// PARENTH1 KQQMSUS1BR85SOLHUEDS62D6P
//SELECT * FROM "ZTF_WBS_L1_D"('ALL_WBSL1','ALL_WBSL1') WHERE START_ID = CHILDID;
define table function ZTF_WBS_L1_D
with parameters 

p_hiename : uj_caption,
p_nodename : rsshnodename,
@Environment.systemField: #CLIENT
iv_client  : abap.clnt
returns {
  mandt    : abap.clnt;
  nodename : rsshnodename;
  nodedesc : /bi0/oibpctxtlg;
  parent_nodename : rsshnodename;
  is_node : esh_e_ltxt_is_node_field;
  start_id: abap.int8;
  parentid: abap.int8;
  childid: abap.int8;
  hieid : rshieid;
  hier_name : uj_caption;
  h_level : rstlevel;
  nodeselected : rsshnodename;  
  
APPLICANT :   /b28/oizophxsg  ;
BUSCLASS1   :   /b28/oizopvcp6  ;
BUSCLASS2   :   /b28/oizopu5r4  ;
BUS_AREA    :   /b28/oizop5kh0  ;
COMP_CODE   :   /b28/oizopcv9d  ;
COUNTRY :   /b28/oizoptkj3  ;
COUNTRY_SOLDTO  :   /b28/oizopunxq  ;
FUNCTIONAL_AREA :   /b28/oizopnphb  ;
PER_RES :   /b28/oizope01s  ;
PROFIT_CCTR :   /b28/oizop9my8  ;
RAKEY_L1    :   /b28/oizopg426  ;
RESP_CC :   /b28/oizop31e5  ;
SEGMENT :   /b28/oizopoque  ;
SHIP_TO :   /b28/oizop6bdt  ;
SOLD_TO :   /b28/oizop0txi  ;
WBS_SYS_STATUS  :   /b28/oizopznyo  ;

SCALING :   /b28/oiscaling  ;
HIR :   /b28/oihir  ;
  
 
}
implemented by method zcl_bpc_amdp_hd_wbs_l1=>get_descendants;
-------------------------------------------------CLASS zcl_bpc_amdp_hd_wbs_l1 --------------------

CLASS zcl_bpc_amdp_hd_wbs_l1 DEFINITION
  public
  final
  create public .

public section.
INTERFACES if_amdp_marker_hdb .
TYPES:
BEGIN OF g_t_wbs_l1_h,
  nodename TYPE RSSHNODENAME,
  nodedesc TYPE /BI0/OIBPCTXTLG,
  parent_nodename TYPE RSSHNODENAME,
  is_node TYPE ESH_E_LTXT_IS_NODE_FIELD,
  start_id TYPE int8,
  parentid TYPE int8,
  childid TYPE int8,
  hieid TYPE RSHIEID,
  hier_name TYPE UJ_CAPTION,
  h_level TYPE RSTLEVEL,


SCALING TYPE    /B28/OISCALING  ,
HIR TYPE    /B28/OIHIR  ,

END OF g_t_wbs_l1_h.

CLASS-METHODS get_descendants
FOR TABLE FUNCTION ZTF_WBS_L1_D
.
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_AMDP_HD_wbs_l1 IMPLEMENTATION.


method get_descendants
  BY DATABASE FUNCTION FOR HDB

  LANGUAGE SQLSCRIPT
  USING ZVBW_WBS_L1_H
  .
--'KZ2TISJ3SHMKUJ02XVQ0GCPS4' NET_INCOME_TE 0 and nodename = 'FD0E' ( should be optional but other hier have several level1 nodes)
-- next select should substituted with a Child-Parent ABAP CDS Hierarchy ---------
cte_hier=
select *
from hierarchy(
source (
select mandt, hieid, hier_name, node_id as node_id,
parent_id as parent_id,
nodename,
parent_nodename,
txtmdtx,
is_node,
:p_nodename as nodeselected,

APPLICANT
,BUSCLASS1
,BUSCLASS2
,BUS_AREA
,COMP_CODE
,COUNTRY
,COUNTRY_SOLDTO
,FUNCTIONAL_AREA
,PER_RES
,PROFIT_CCTR
,RAKEY_L1
,RESP_CC
,SEGMENT
,SHIP_TO
,SOLD_TO
,WBS_SYS_STATUS

,SCALING
,HIR


from ZVBW_WBS_L1_H where hier_name = :p_hiename and mandt = :iv_client )
start where nodename = :p_nodename)
;
cte_desc=
select mandt,
start_id as nodeid,
node_id AS childid,
parent_id AS parentid,
parent_nodename,
nodename,
txtmdtx,
is_node,
hieid, hier_name,
h_level,
nodeselected,

APPLICANT
,BUSCLASS1
,BUSCLASS2
,BUS_AREA
,COMP_CODE
,COUNTRY
,COUNTRY_SOLDTO
,FUNCTIONAL_AREA
,PER_RES
,PROFIT_CCTR
,RAKEY_L1
,RESP_CC
,SEGMENT
,SHIP_TO
,SOLD_TO
,WBS_SYS_STATUS

,SCALING
,HIR

from hierarchy_descendants(
source :cte_hier
START ( SELECT hierarchy_rank AS start_rank,
node_id AS start_id,
hierarchy_level as h_level
FROM :cte_hier ) )
order by nodeid asc,
childid asc
;
RETURN
SELECT
mandt,
nodename,
txtmdtx as nodedesc,
parent_nodename,
is_node,
nodeid as start_id,
parentid,
childid,
hieid,
hier_name,
h_level,
nodeselected,

APPLICANT
,BUSCLASS1
,BUSCLASS2
,BUS_AREA
,COMP_CODE
,COUNTRY
,COUNTRY_SOLDTO
,FUNCTIONAL_AREA
,PER_RES
,PROFIT_CCTR
,RAKEY_L1
,RESP_CC
,SEGMENT
,SHIP_TO
,SOLD_TO
,WBS_SYS_STATUS

,SCALING
,HIR
FROM :cte_desc
;

endmethod.
ENDCLASS.

----------------- with a Child-Parent ABAP CDS Hierarchy weird in 753 ---------
define hierarchy ZBPC_WBS_L1_HIER 
  with parameters
    p_hieid : rshieid   
    , p_id : rshienodid
  as parent child hierarchy (
    source ZBPC_WBS_L1_HS
       child to parent association _relat
//  start where id = :p_id and hieid = :p_hieid
//    = 'ALL_WBSL1' and 'RISK_OPPORT' 'RFZGGCCK2H79IVG7ISTJXRFZK'    
    start where parentid = :p_id and hieid = :p_hieid
//    = '00000003' 'RISK_OPPORT' ie. parentid of base member   
    siblings order by id ascending
    multiple parents allowed
  )
  {
id,
parentid,
    name,
    txtmdtx,
    iobjnm,
    parent_nodename,
    IS_NODE,
HIR,
tlevel,
    
$node.hierarchy_rank as h_rank,
$node.hierarchy_tree_size as h_tree_size,
$node.hierarchy_parent_rank as h_parent_rank,
$node.hierarchy_level as h_level,
$node.hierarchy_is_cycle as h_is_cycle,
$node.hierarchy_is_orphan as h_is_orphan,
$node.node_id as h_node_id,
$node.parent_id as h_parent_id    
    
}
@AbapCatalog.sqlViewName: 'ZVBPC_WBS_L1_HS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'WBS_L1 Hierarchy Source'
define view ZBPC_WBS_L1_HS as select from ZBW_WBS_L1_H
association [1..*] to ZBPC_WBS_L1_HS as _relat
    on $projection.id = _relat.parentid 
//    and $projection.parentid = _relat.id 
    and $projection.hieid = _relat.hieid
  {
    key node_id as id,
    key parent_id as parentid,
    key hieid,
    hier_name,
    nodename as name,
    txtmdtx,
    iobjnm
,parent_nodename
,parent_txtmdtx
,IS_NODE
,HIR
,tlevel
,SCALING
,APPLICANT
,BUSCLASS1
,BUSCLASS2
,BUS_AREA
,COMP_CODE
,COUNTRY
,COUNTRY_SOLDTO
,FUNCTIONAL_AREA
,PER_RES
,PROFIT_CCTR
,RAKEY_L1
,RESP_CC
,SEGMENT
,SHIP_TO
,SOLD_TO
,WBS_SYS_STATUS,
    _relat // Make association public
}

