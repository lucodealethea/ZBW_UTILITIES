* ABAP CDS ZBPC_DIMAPP2 describes BPC Models
* Table Function on ZBPC_DIMAPP2 returns list of fields to build a CDS view or a TF 
* SE38 ZBPC_DIM_APP returns the list of fields(to build CDS View or AMDP/TF) with option to delete column (sensitive data)
* Next step will be to generate automatically if any change occured on BPC model, the AMDP Class along with TF with Code Composer
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

--------------------BPC11-----------------------------
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
REPORT zbpc_dim_app.

PARAMETERS:
p_env TYPE uj_appset_id DEFAULT 'TRACTEBEL_GLO' ,
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
