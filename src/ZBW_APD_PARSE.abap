*&---------------------------------------------------------------------*
*& Report ZBW_APD_PARSE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBW_APD_PARSE.

DATA: ls_rsant_process TYPE rsant_process,
      lv_xml           TYPE string,
      ls_xml_xstr      TYPE xstring,
      lt_result_xml    TYPE STANDARD TABLE OF smum_xmltb,
      ls_result_xml    TYPE smum_xmltb,
      lt_ret           TYPE STANDARD TABLE OF bapiret2,
      lo_alv           TYPE REF TO cl_salv_table,
      lo_col           TYPE REF TO cl_salv_columns_table,
      lo_fun           TYPE REF TO cl_salv_functions_list.

PARAMETERS: p_apd TYPE rsan_process OBLIGATORY.

SELECT SINGLE * FROM rsant_process INTO ls_rsant_process WHERE objvers = 'A' AND process = p_apd.
lv_xml = ls_rsant_process-xml.

CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text           = lv_xml
      mimetype       = 'text/xml'
   IMPORTING
     buffer         = ls_xml_xstr
   EXCEPTIONS
     failed         = 1
     others         = 2.

CALL FUNCTION 'SMUM_XML_PARSE'
  EXPORTING
    xml_input       = ls_xml_xstr
  TABLES
    xml_table       = lt_result_xml
    return          = lt_ret.

CALL METHOD cl_salv_table=>factory
  IMPORTING r_salv_table = lo_alv
  CHANGING t_table = lt_result_xml.

lo_col = lo_alv->get_columns( ).
lo_col->set_optimize( ) .
lo_fun = lo_alv->get_functions( ).
lo_fun->set_all( ).
CALL METHOD lo_alv->display.
