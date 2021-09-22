*&---------------------------------------------------------------------*
*& Report zbw_parse_xml
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_parse_xml.

* DATA DECLARATION

***********************************************************************

TYPES: BEGIN OF ty_output,
          infoprovider TYPE rsdodsobject,
          source       TYPE rsdiobjnm,
          target       TYPE rsohcprcolnm,
        END OF ty_output.

TYPES: BEGIN OF ty_output2,
          iobj TYPE rsdiobjnm,
        END OF ty_output2.

TYPES: BEGIN OF ty_output3,
          object TYPE rsdodsobject,
          source       TYPE rsdiobjnm,
          target       TYPE rsohcprcolnm,
        END OF ty_output3.

DATA: l_hobj_xml_def TYPE xstring,
       lt_xml_info    TYPE TABLE OF smum_xmltb INITIAL SIZE 0,
       lt_return      TYPE STANDARD TABLE OF bapiret2,
       lt_output      TYPE STANDARD TABLE OF ty_output,
       lt_output2     TYPE STANDARD TABLE OF ty_output2,
       lt_output3     TYPE STANDARD TABLE OF ty_output3,
       l_cvalue       TYPE char255,
       l_offset       TYPE i,
       l_output       TYPE ty_output,
       l_output2      TYPE ty_output2,
       l_output3      TYPE ty_output3,
       lv_flg_hcpr    TYPE flag VALUE 'X',
       lv_flg_adso    TYPE flag VALUE '',
       lv_flg_haap    TYPE flag VALUE '',
       lv_idx_collect_haap TYPE sy-tabix.

************************************************************************

* ALV REFERENCES

***********************************************************************

DATA: go_alv     TYPE REF TO cl_salv_table,
       go_columns TYPE REF TO cl_salv_columns,
       go_funcs   TYPE REF TO cl_salv_functions,
       go_ex      TYPE REF TO cx_root.

FIELD-SYMBOLS: <fs_xml_info> TYPE smum_xmltb,
                <fs_any_tab>      TYPE any.
************************************************************************

* SELECTION SCREEN DETAILS

************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_hobj TYPE rsohcprnm.
SELECTION-SCREEN END OF BLOCK blk1.
************************************************************************
* DATA PROCESSSING
************************************************************************
* Select XML definition of CompositeProvider
SELECT SINGLE xml_ui
   FROM rsohcpr
   INTO l_hobj_xml_def
   WHERE hcprnm = p_hobj
   AND objvers = 'A'.

IF sy-subrc <> 0.
   CLEAR lv_flg_hcpr.
* Check if input is an Advanced DSO
   SELECT SINGLE xml_ui
   FROM rsoadso
   INTO l_hobj_xml_def
   WHERE adsonm = p_hobj
   AND objvers = 'A'.
IF sy-subrc <> 0.
   CLEAR: lv_flg_hcpr, lv_flg_adso.
   SELECT SINGLE xml
   FROM rsdhamap
   INTO l_hobj_xml_def
   WHERE haapnm = p_hobj
   AND objvers = 'A'.

   IF sy-subrc <> 0.
     MESSAGE 'Invalid or Inactive CompositeProvider/ADSO /HAAP or CompositeProvider/ADSO/HAAP not of type HCPR/ADSO/HAAP' TYPE 'I'.
     EXIT.
   ELSE.
     lv_flg_haap = 'X'.
   ENDIF.
 ELSE. "lv_flg_adso
 lv_flg_adso = 'X'.
 ENDIF.
ENDIF.

IF lv_flg_hcpr EQ 'X'.
BREAK bb5827.
   ASSIGN lt_output TO <fs_any_tab> .
* Parse XML string to XML table

   CALL FUNCTION 'SMUM_XML_PARSE'
     EXPORTING
       xml_input = l_hobj_xml_def
     TABLES
       xml_table = lt_xml_info
       return    = lt_return.
* Internal table with mapping

   LOOP AT lt_xml_info ASSIGNING <fs_xml_info>.

     IF <fs_xml_info>-cname = 'entity'.

       l_cvalue = <fs_xml_info>-cvalue.
       SEARCH l_cvalue FOR 'composite'.
       l_offset = sy-fdpos.
       l_offset = l_offset - 1.
       TRY.
           l_output-infoprovider = <fs_xml_info>-cvalue(l_offset). "CompositeProvider
         CATCH cx_sy_range_out_of_bounds.
           l_output-infoprovider = <fs_xml_info>-cvalue.
       ENDTRY.

     ELSEIF
      <fs_xml_info>-cname = 'targetName'.
       l_output-target = <fs_xml_info>-cvalue.
     ELSEIF
       <fs_xml_info>-cname = 'sourceName'.
       l_output-source = <fs_xml_info>-cvalue.
       APPEND l_output TO lt_output.
     ENDIF.
   ENDLOOP.

ELSEIF lv_flg_adso = 'X'.

   ASSIGN lt_output2 TO <fs_any_tab> .
* Parse XML string to XML table
   CALL FUNCTION 'SMUM_XML_PARSE'
     EXPORTING
       xml_input = l_hobj_xml_def
     TABLES
       xml_table = lt_xml_info
       return    = lt_return.
* Internal table with mapping

   LOOP AT lt_xml_info ASSIGNING <fs_xml_info>.
     IF <fs_xml_info>-cname = 'infoObjectName'.
       l_output2-iobj = <fs_xml_info>-cvalue.
       APPEND l_output2 TO lt_output2.
     ENDIF.
   ENDLOOP.
ELSEIF lv_flg_haap = 'X'.

   ASSIGN lt_output3 TO <fs_any_tab> .
* Parse XML string to XML table
   CALL FUNCTION 'SMUM_XML_PARSE'
     EXPORTING
       xml_input = l_hobj_xml_def
     TABLES
       xml_table = lt_xml_info
       return    = lt_return.
* Pick-Up index for
CLEAR:lv_idx_collect_haap.
   READ TABLE lt_xml_info ASSIGNING <fs_xml_info> WITH KEY cvalue = '0BW_TGT_INFOSOURCE'.

   IF SY-SUBRC = 0.
    lv_idx_collect_haap = sy-tabix.
   ENDIF.
* Internal table with mapping
l_output3-object = '0BW_TGT_INFOSOURCE'.
   LOOP AT lt_xml_info ASSIGNING <fs_xml_info>.
    IF sy-tabix > lv_idx_collect_haap.
     IF <fs_xml_info>-cname = 'sourceFieldName'.
       l_output3-source = <fs_xml_info>-cvalue.
     ELSEIF <fs_xml_info>-cname = 'targetFieldName'.
       l_output3-target = <fs_xml_info>-cvalue.
           APPEND l_output3 TO lt_output3.
     ENDIF.

    ENDIF.
   ENDLOOP.
   DELETE ADJACENT DUPLICATES FROM lt_output3.
ENDIF.
break BB5827.
* Output to ALV
TRY.
     cl_salv_table=>factory(
     IMPORTING
     r_salv_table = go_alv
     CHANGING
     t_table = <fs_any_tab> ).
     " set column optimized
     go_columns = go_alv->get_columns( ).
     go_columns->set_optimize( ).
     " set functions
     go_funcs = go_alv->get_functions( ).
     go_funcs->set_all( ).
     go_alv->display( ).
   CATCH cx_salv_msg INTO go_ex.
     MESSAGE go_ex TYPE 'E'.
ENDTRY.
