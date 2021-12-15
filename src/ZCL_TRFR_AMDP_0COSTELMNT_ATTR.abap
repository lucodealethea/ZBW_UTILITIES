CLASS zcl_trfr_amdp_0costelmnt_attr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    TYPES:
      BEGIN OF tn_s_in_global_end_1,
       CO_AREA type /BI0/OICO_AREA, " InfoObject: 0CO_AREA
       COSTELMNT type /BI0/OICOSTELMNT, " InfoObject: 0COSTELMNT
       DATETO type /BI0/OIDATETO, " InfoObject: 0DATETO
       DATEFROM type /BI0/OIDATEFROM, " InfoObject: 0DATEFROM
       CSTELMNTYP type /BI0/OICSTELMNTYP, " InfoObject: 0CSTELMNTYP
       LOGSYS type RSDLOGSYS, " InfoObject: 0LOGSYS
       SEM_POSIT type /BI0/OISEM_POSIT, " InfoObject: 0SEM_POSIT
       UNIT type /BI0/OIUNIT, " InfoObject: 0UNIT
       /BIC/ZRECEIPT type /BIC/OIZRECEIPT, " InfoObject: ZRECEIPT
       /BIC/ZEBITDA type /BIC/OIZEBITDA, " InfoObject: ZEBITDA
       /BIC/ZACCNTGRP type /BIC/OIZACCNTGRP, " InfoObject: ZACCNTGRP
       /BIC/ZHIERNODE type /BIC/OIZHIERNODE, " InfoObject: ZHIERNODE
       RECORD type C length 56,
       SQL__PROCEDURE__SOURCE__RECORD type C length 56,
      END OF tn_s_in_global_end_1 .
    TYPES:
      BEGIN OF tn_s_in_global_end.
        INCLUDE TYPE tn_s_in_global_end_1.
      TYPES END OF tn_s_in_global_end .
    TYPES:
      tn_t_in_global_end TYPE STANDARD TABLE OF tn_s_in_global_end .
    TYPES:
      BEGIN OF tn_s_out_global_end_1,
       CO_AREA type /BI0/OICO_AREA, " InfoObject: 0CO_AREA
       COSTELMNT type /BI0/OICOSTELMNT, " InfoObject: 0COSTELMNT
       DATETO type /BI0/OIDATETO, " InfoObject: 0DATETO
       DATEFROM type /BI0/OIDATEFROM, " InfoObject: 0DATEFROM
       CSTELMNTYP type /BI0/OICSTELMNTYP, " InfoObject: 0CSTELMNTYP
       LOGSYS type RSDLOGSYS, " InfoObject: 0LOGSYS
       SEM_POSIT type /BI0/OISEM_POSIT, " InfoObject: 0SEM_POSIT
       UNIT type /BI0/OIUNIT, " InfoObject: 0UNIT
       /BIC/ZRECEIPT type /BIC/OIZRECEIPT, " InfoObject: ZRECEIPT
       /BIC/ZEBITDA type /BIC/OIZEBITDA, " InfoObject: ZEBITDA
       /BIC/ZACCNTGRP type /BIC/OIZACCNTGRP, " InfoObject: ZACCNTGRP
       /BIC/ZHIERNODE type /BIC/OIZHIERNODE, " InfoObject: ZHIERNODE
       RECORD type C length 56,
       SQL__PROCEDURE__SOURCE__RECORD type C length 56,
      END OF tn_s_out_global_end_1 .
    TYPES:
      BEGIN OF tn_s_out_global_end.
        INCLUDE TYPE tn_s_out_global_end_1.
      TYPES END OF tn_s_out_global_end .
    TYPES:
      tn_t_out_global_end TYPE STANDARD TABLE OF tn_s_out_global_end .
    METHODS: do_transform_costelmnt_attr IMPORTING VALUE(it_intab) TYPE tn_t_out_global_end
                                   EXPORTING VALUE(et_result)   TYPE tn_t_in_global_end.

ENDCLASS.



CLASS ZCL_TRFR_AMDP_0COSTELMNT_ATTR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TRFR_AMDP_0COSTELMNT_ATTR->DO_TRANSFORM_COSTELMNT_ATTR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_INTAB                       TYPE        TN_T_OUT_GLOBAL_END
* | [<---] ET_RESULT                      TYPE        TN_T_IN_GLOBAL_END
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD do_transform_costelmnt_attr BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY
  USING TVARVC /BI0/HCOSTELMNT RSTHIERNODE /BI0/PGL_ACCOUNT.


declare LV_HIER_ID nvarchar( 25 );

SELECT low INTO LV_HIER_ID DEFAULT TO_VARCHAR('SOKITVPWI4H9AXOJ62L3HU16D')
        FROM tvarvc
       WHERE NAME = '0COSTELMNT__HIEID' AND low <> '';

*IBQ only 'T4O225HXYX2MZCDF7DOP8UH08' available
dist_zaccntgrp=
SELECT DISTINCT "IT"."CO_AREA",
CASE "IT"."CO_AREA"
WHEN 'IP01' THEN 'PCMN'
WHEN 'IP03' THEN 'IPRO'
WHEN 'IP04' THEN 'IPPL'
WHEN 'IP05' THEN 'CAFR'
WHEN 'IP06' THEN 'IPSP'
WHEN 'IP07' THEN 'CANL'
WHEN 'IP08' THEN 'IPIT'
WHEN 'IP09' THEN 'CTPK'
WHEN 'A000' THEN 'OPER'
ELSE 'PCMN' END AS "CHRT_ACCTS",
"IT"."COSTELMNT","IT"."DATETO", "IT"."DATEFROM","IT"."CSTELMNTYP",
"IT"."LOGSYS","IT"."SEM_POSIT","IT"."UNIT",
"IT"."/BIC/ZRECEIPT","IT"."/BIC/ZEBITDA","IT"."/BIC/ZACCNTGRP","IT"."/BIC/ZHIERNODE",
"IT"."RECORD",
"IT"."SQL__PROCEDURE__SOURCE__RECORD"
FROM :it_intab AS "IT";

dist_gl = SELECT DISTINCT
"IT"."CO_AREA", "IT"."COSTELMNT",
"IT"."DATETO", "IT"."DATEFROM","IT"."CSTELMNTYP",
"IT"."LOGSYS","IT"."SEM_POSIT","IT"."UNIT",
"IT"."/BIC/ZRECEIPT","IT"."/BIC/ZEBITDA",
"GL"."/BIC/ZACCNTGRP" AS "/BIC/ZACCNTGRP",
COALESCE(CONCAT(CONCAT(SUBSTRING("CEH2"."NODENAME",5,10),'-'),"CEH2T"."TXTMD"),'NONE')  AS  "/BIC/ZHIERNODE",
"IT"."RECORD",
"IT"."SQL__PROCEDURE__SOURCE__RECORD"
FROM :dist_zaccntgrp as "IT"
LEFT OUTER JOIN "/BI0/PGL_ACCOUNT" AS "GL"
ON "IT"."COSTELMNT" = "GL"."GL_ACCOUNT" AND
"IT"."CHRT_ACCTS" = "GL"."CHRT_ACCTS"
LEFT OUTER JOIN  "/BI0/HCOSTELMNT" AS "CEH1"
ON "CEH1"."NODENAME" = CONCAT("IT"."CO_AREA","IT"."COSTELMNT")
AND "CEH1"."IOBJNM" = '0COSTELMNT'
AND "CEH1"."HIEID" = :LV_HIER_ID
AND "CEH1"."INTERVL" <> 'X'
AND "CEH1"."OBJVERS" = 'A'
LEFT OUTER JOIN "/BI0/HCOSTELMNT" AS "CEH2"
ON  "CEH2"."NODEID" = "CEH1"."PARENTID"
AND "CEH2"."HIEID" = "CEH1"."HIEID"
AND "CEH2"."IOBJNM" = '0HIER_NODE'
AND "CEH2"."OBJVERS" = 'A'
LEFT OUTER JOIN "RSTHIERNODE" AS "CEH2T"
ON "CEH2T"."HIEID" = "CEH2"."HIEID"
AND "CEH2T"."NODENAME" = "CEH2"."NODENAME"
AND "CEH2T"."LANGU" = 'E'
AND "CEH2T"."OBJVERS" = 'A'
;

    et_result =
    SELECT "IT"."CO_AREA", "IT"."COSTELMNT","IT"."DATETO", "IT"."DATEFROM","IT"."CSTELMNTYP",
    "IT"."LOGSYS","IT"."SEM_POSIT","IT"."UNIT", "IT"."/BIC/ZRECEIPT","IT"."/BIC/ZEBITDA",
    "IT"."/BIC/ZACCNTGRP", "IT"."/BIC/ZHIERNODE",
    "IT"."RECORD", "IT"."SQL__PROCEDURE__SOURCE__RECORD"
    FROM :dist_gl AS "IT";
* how to invoke :
* METHOD GLOBAL_END BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY
* USING ZCL_TRFR_AMDP_0COSTELMNT_ATTR=>DO_TRANSFORM_COSTELMNT_ATTR.
* Begin of routine - insert your code only below this line ***
*
*-- Note the _M class are not considered for DTP execution.
*-- AMDP Breakpoints must be set in the _A class instead.
*"ZCL_TRFR_AMDP_0COSTELMNT_ATTR=>DO_TRANSFORM_COSTELMNT_ATTR"( it_intab => :intab,
*                                      ET_RESULT => lt_result );
* use either incoming fields value or results
*outtab = select "IT"."CO_AREA", "IT"."COSTELMNT", "IT"."DATETO","IT"."DATEFROM","IT"."CSTELMNTYP",
*                "IT"."LOGSYS","IT"."SEM_POSIT","IT"."UNIT", "IT"."/BIC/ZRECEIPT","IT"."/BIC/ZEBITDA",
*                "IT"."/BIC/ZACCNTGRP", "IT"."/BIC/ZHIERNODE",
*                "IT"."RECORD", "IT"."SQL__PROCEDURE__SOURCE__RECORD"
*           from :intab as "IT"
*           inner join :lt_result as "RES"
*           on "IT"."RECORD" = "RES"."RECORD" ;
* Allow Error Handling for HANA Routines is enabled

* errorTab= SELECT DISTINCT 'Check 0COSTELMNT__HIEID value/existence in TVARVC !' AS ERROR_TEXT, '' AS SQL__PROCEDURE__SOURCE__RECORD
* FROM :outtab as "IT"
* WHERE "IT"."/BIC/ZHIERNODE" = '';
* End of routine - insert your code only before this line ***
*ENDMETHOD.
ENDMETHOD.
ENDCLASS.
