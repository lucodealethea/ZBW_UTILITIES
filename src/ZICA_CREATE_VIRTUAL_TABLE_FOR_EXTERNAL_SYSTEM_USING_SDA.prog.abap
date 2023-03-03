*&---------------------------------------------------------------------*
*& Report ZICA_CREATE_VIRTUAL_TABLE
*&---------------------------------------------------------------------*
*& Use this report to create/delete a virtual table in SAP<SID> schema.
*& Prerequisites:
*&  1. You must have a remote source created in HANA.
*&  2. DB user SAP<SID> has "create virtual table" permission on
*&     the remote source.
*&  "Grant CREATE VIRTUAL TABLE on remote source <rs_name> to SAP<SID>"
*& Result:
*&  A virtual table is created in schema SAP<SID> with prefix '/1BCAMDP/'
*&---------------------------------------------------------------------*
report ZICA_CREATE_VIRTUAL_TABLE.

parameters: vt_name type string obligatory, " the to-be-created virtual table name
            remote  type string, " remote source
            schema  type string, " the DB schema in the remote source
            rt_name type string, " the remote table name
            delete  as checkbox. " delete the virtual table

at selection-screen.
  if delete = abap_false and
     ( remote is initial or schema is initial  or rt_name is initial ).
    message 'Parameters remote, schema, and rt_name are mandatory!' type 'E'.
  endif.

start-of-selection.
  data(lv_virtual_table_name) = |"/1BCAMDP/{ vt_name }"|.
  data(lv_remote_table_name) = |"{ remote }"."NULL"."{ schema }"."{ rt_name }"|.

  data lv_ddl_string type string.
  if delete = abap_false.
    lv_ddl_string = |create virtual table { lv_virtual_table_name } at { lv_remote_table_name }|.
  else.
    lv_ddl_string = |drop table { lv_virtual_table_name }|.
  endif.

  try.
      data(mo_sql) = new cl_sql_statement( ).
      mo_sql->execute_ddl( lv_ddl_string ).
      message 'Action is successfully performed!' type 'S'.
    catch cx_sql_exception into data(lo_exc).
      raise shortdump lo_exc.
  endtry.
