*&---------------------------------------------------------------------*
*& Report zbw_parse_xml
*&---------------------------------------------------------------------*
*& structure the XML for HCPR, ADSO or HAAP
*&---------------------------------------------------------------------*
REPORT zbw_parse_xml.

* DATA DECLARATION

***********************************************************************
TYPES: BEGIN OF ty_dim,
          appset_id	TYPE uj_appset_id,
          dimension	TYPE uj_dim_name,
          dim_type  TYPE uj_dim_type,
          dim_type_index  TYPE uja_dim_type_index,
          ref_dim	TYPE uj_dim_name,
          num_hier  TYPE uj_smallint,
          tech_name	TYPE uj_tech_name,
          caption	TYPE uj_caption,
          hier_time_dep	TYPE uj_flg,
          data_table  TYPE tabname,
          desc_table  TYPE tabname,
          hier_data_table	TYPE tabname,
        END OF ty_dim.

TYPES: BEGIN OF ty_iobj_dim,
    iobjnm       TYPE rsdiobjnm,
    tech_name	   TYPE uj_tech_name,
  END OF ty_iobj_dim.

TYPES: BEGIN OF ty_iobj,
          iobjnm       TYPE rsdiobjnm,
          txtlg        TYPE rstxtlg,
        END OF ty_iobj.

TYPES: BEGIN OF ty_output,
          infoprovider TYPE rsdodsobject,
          source       TYPE rsdiobjnm,
          target       TYPE rsohcprcolnm,
          desc   TYPE rstxtlg,
        END OF ty_output.

TYPES: BEGIN OF ty_output2,
          iobj TYPE rsdiobjnm,
        END OF ty_output2.

TYPES: BEGIN OF ty_output3,
          object TYPE rsdha_haapnm,
          text TYPE rstxtlg,
          source TYPE rsfieldnm,
          target TYPE rsdiobjnm,
          desc   TYPE rstxtlg,
        END OF ty_output3.

DATA: l_hobj_xml_def TYPE xstring,
       lt_xml_info    TYPE TABLE OF smum_xmltb INITIAL SIZE 0,
       lt_return      TYPE STANDARD TABLE OF bapiret2,
       lt_output      TYPE STANDARD TABLE OF ty_output,
       lt_output2     TYPE STANDARD TABLE OF ty_output2,
       lt_output3     TYPE STANDARD TABLE OF ty_output3,
       lt_dim         TYPE STANDARD TABLE OF ty_dim,
       ls_dim TYPE ty_dim,
       lt_iobj_dim    TYPE STANDARD TABLE OF ty_iobj_dim,
       ls_iobj_dim    TYPE ty_iobj_dim,
       lt_iobj        TYPE STANDARD TABLE OF ty_iobj,
       lt_iobjnm        TYPE STANDARD TABLE OF ty_iobj,
       lt_dimension   TYPE STANDARD TABLE OF ty_dim,
       ls_iobj        TYPE ty_iobj,
       l_cvalue       TYPE char255,
       l_offset       TYPE i,
       l_output       TYPE ty_output,
       l_output2      TYPE ty_output2,
       l_output3      TYPE ty_output3,
       lv_flg_hcpr    TYPE flag VALUE 'X',
       lv_flg_adso    TYPE flag VALUE '',
       lv_flg_haap    TYPE flag VALUE '',
       lv_idx_proj_map TYPE sy-tabix,
       lv_idx_main_map TYPE sy-tabix.

************************************************************************

* ALV REFERENCES

***********************************************************************

DATA: go_alv     TYPE REF TO cl_salv_table,
       go_columns TYPE REF TO cl_salv_columns,
       go_funcs   TYPE REF TO cl_salv_functions,
       go_display   TYPE REF TO cl_salv_display_settings,
       go_ex      TYPE REF TO cx_root,
       s_xml_info TYPE smum_xmltb,
       title TYPE char255.

FIELD-SYMBOLS: <fs_xml_info>     TYPE smum_xmltb,
                <fs_any_tab>     TYPE any,
                <fs_output>      TYPE ty_output,
                <fs_output3>     TYPE ty_output3.
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
*BREAK bb5827.
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
       ls_iobj-iobjnm = <fs_xml_info>-cvalue.
       COLLECT ls_iobj INTO lt_iobj.
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
* Pick-up index for projection mapping
   CLEAR: lv_idx_proj_map, ls_iobj.
   REFRESH : lt_iobj.
   READ TABLE lt_xml_info ASSIGNING <fs_xml_info> WITH KEY cvalue = '0BW_PROJECTION'.
   IF sy-subrc = 0.
    lv_idx_proj_map = sy-tabix.
   ENDIF.

* Pick-Up index for main mapping
   CLEAR:lv_idx_main_map.
   LOOP AT lt_xml_info INTO s_xml_info WHERE cvalue = '0BW_TGT_INFOSOURCE' OR  cvalue = '0BW_TGT_INFOPROV'.
   IF sy-subrc = 0.
    lv_idx_main_map = sy-tabix.
   ENDIF.
   ENDLOOP.
* Internal table with 0BW_TGT_INFOSOURCE mapping
   l_output3-object = s_xml_info-cvalue.
* Pick-up the main description
   READ TABLE lt_xml_info ASSIGNING <fs_xml_info> WITH KEY cname = 'description'.
   IF sy-subrc = 0.
   l_output3-text = <fs_xml_info>-cvalue.
   ENDIF.
   LOOP AT lt_xml_info ASSIGNING <fs_xml_info>.
    IF sy-tabix > lv_idx_main_map.
     IF <fs_xml_info>-cname = 'sourceFieldName'.
       l_output3-source = <fs_xml_info>-cvalue.
     ELSEIF <fs_xml_info>-cname = 'targetFieldName'.
       l_output3-target = <fs_xml_info>-cvalue.
       APPEND l_output3 TO lt_output3.
       ls_iobj-iobjnm = <fs_xml_info>-cvalue.
       COLLECT ls_iobj INTO lt_iobj.
     ENDIF.
    ENDIF.
   ENDLOOP.
l_output3-object = '0BW_PROJECTION'.
   LOOP AT lt_xml_info ASSIGNING <fs_xml_info>.
    IF sy-tabix BETWEEN lv_idx_proj_map AND lv_idx_main_map.
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

* get InfoObject Description
SELECT * FROM rsdiobjt
INTO CORRESPONDING FIELDS OF TABLE @lt_iobjnm
FOR ALL ENTRIES IN @lt_iobj
WHERE iobjnm = @lt_iobj-iobjnm AND langu = 'E'.
* get Dimension description
LOOP AT lt_iobj INTO ls_iobj.
  ls_iobj_dim-iobjnm = ls_iobj-iobjnm.
  CONCATENATE '/CPMB/' ls_iobj-iobjnm INTO ls_iobj_dim-tech_name.
  COLLECT ls_iobj_dim INTO lt_iobj_dim.
ENDLOOP.

SELECT * FROM uja_dimension
INTO CORRESPONDING FIELDS OF TABLE @lt_dim
FOR ALL ENTRIES IN @lt_iobj_dim
WHERE tech_name = @lt_iobj_dim-tech_name.

LOOP AT lt_output ASSIGNING <fs_output>.
  IF <fs_output>-SOURCE(6) <> '/CPMB/'.
   READ TABLE lt_iobjnm INTO ls_iobj WITH KEY iobjnm = <fs_output>-target.
    IF sy-subrc = 0.
    <fs_output>-desc = ls_iobj-txtlg.
    ENDIF.
  ELSE. "/CPMB/ dimension

  READ TABLE lt_dim INTO ls_dim WITH KEY tech_name = <fs_output>-source.
  IF sy-subrc = 0.
  <fs_output>-desc = ls_dim-dimension.
  ENDIF.
  ENDIF.

ENDLOOP.

LOOP AT lt_output3 ASSIGNING <fs_output3>.
READ TABLE lt_iobjnm INTO ls_iobj WITH KEY iobjnm = <fs_output3>-target.
IF sy-subrc = 0.
<fs_output3>-desc = ls_iobj-txtlg.
  ELSE.

ENDIF.
ENDLOOP.

WRITE p_hobj TO title.
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
     go_display = go_alv->get_display_settings( ).
     go_display->set_list_header( 'Put a dynamic title here' ).
     go_alv->display( ).
   CATCH cx_salv_msg INTO go_ex.
     MESSAGE go_ex TYPE 'E'.
ENDTRY.
