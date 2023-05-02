FUNCTION zhana_exec_sql.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DDL_STMT) TYPE  STRING OPTIONAL
*"     VALUE(IV_DML_STMT) TYPE  STRING OPTIONAL
*"     VALUE(IT_INSERTS) TYPE  STRINGTAB OPTIONAL
*"  EXPORTING
*"     VALUE(EV_MESSAGE) TYPE  STRING
*"     VALUE(EV_ROWS) TYPE  NUM20
*"----------------------------------------------------------------------
DATA: exc TYPE REF TO cx_root,
         lr_insert TYPE REF TO string,
         lv_rows TYPE i,
         lv_count TYPE i.
IF lo_sql_statement IS NOT BOUND.
CREATE OBJECT lo_sql_statement.
ENDIF.
IF iv_ddl_stmt IS NOT INITIAL.
TRY.
         lo_sql_statement->execute_ddl( iv_ddl_stmt ).
CATCH cx_root INTO exc.
         ev_message = exc->get_text( ).
ENDTRY.
RETURN.
ENDIF.
IF iv_dml_stmt IS NOT INITIAL.
TRY.
         ev_message = |{ lo_sql_statement->execute_update( iv_dml_stmt ) } rows processed|.
CATCH cx_root INTO exc.
         ev_message = exc->get_text( ).
ENDTRY.
RETURN.
ENDIF.
LOOP AT it_inserts REFERENCE INTO lr_insert.
TRY.
         lv_rows = lo_sql_statement->execute_update( lr_insert->* ).
ADD lv_rows TO lv_count.
CATCH cx_root INTO exc.
         ev_message = exc->get_text( ).
ENDTRY.
ENDLOOP.
   ev_rows = lv_count.
   ev_message = |{ lv_count } rows inserted|.
ENDFUNCTION.

* create a class zcl_table_repl that replicates a table using

struct_descr ?= cl_abap_structdescr=>describe_by_name( table_name ).
     table_fields = struct_descr->get_ddic_field_list( ).
* importing the fields with their types then drop first the table

lv_sql_stmt = |DROP TABLE "{ schema }"."{ table_name }"|.
CALL FUNCTION 'ZHANA_EXEC_SQL'
       DESTINATION rfc_dest
EXPORTING
         iv_ddl_stmt = lv_sql_stmt
IMPORTING
         ev_message  = lv_message.
         
* Now we need to build the CREATE TABLE statement using the information from the data dictionary 
* and a mapping table that was built by the class constructor. Note 
* I have only done minimal mapping so you may well need to expand this table to support some of the less common datatypes. 


LOOP AT table_fields REFERENCE INTO lr_field.
READ TABLE type_map REFERENCE INTO lr_type_map
WITH KEY erp = lr_field->datatype.
CHECK sy-subrc = 0.
       lv_sql = lv_sql &&
         |"{ lr_field->fieldname }" { lr_type_map->hana }|.
CASE lr_type_map->hana.
WHEN 'NVARCHAR' OR 'FLOAT'.
           lv_sql = lv_sql && |({ lr_field->leng })|.
WHEN 'TINYINT'.
WHEN 'DECIMAL'.
           lv_sql = lv_sql && |({ lr_field->leng },{ lr_field->decimals })|.
ENDCASE.
       lv_sql = lv_sql && ','.
IF lr_field->keyflag EQ 'X'.
IF lv_pkey IS NOT INITIAL.
           lv_pkey = lv_pkey && ','.
ENDIF.
         lv_pkey = lv_pkey && |"{ lr_field->fieldname }"|.
ENDIF.
ENDLOOP.
     rv_sql =
       |CREATE COLUMN TABLE "{ schema }"."{ table_name }" | &&
       |( { lv_sql } PRIMARY KEY ({ lv_pkey }))|.
* create the table with FM
CALL FUNCTION 'ZHANA_EXEC_SQL'
       DESTINATION rfc_dest
EXPORTING
         iv_ddl_stmt = lv_sql_stmt
IMPORTING
         ev_message  = lv_message.
       
* Now the heavy lifting begins. We again use RTTS and the mapping data to generate a 
* series of INSERT sql statements that are batched up and passed across 
* to our RFC-enabled function module for processing.     

WHILE <table> IS NOT INITIAL.
           lv_row_count = 0.
LOOP AT <table> ASSIGNING <row>.
ADD 1 TO lv_row_count.
IF lv_row_count > insert_batch_size.
EXIT.
ENDIF.
CLEAR lv_values.
LOOP AT table_fields REFERENCE INTO lr_table_field.
ASSIGN COMPONENT lr_table_field->fieldname OF STRUCTURE <row> TO <field>.
READ TABLE type_map REFERENCE INTO lr_map
WITH KEY erp = lr_table_field->datatype.
CHECK sy-subrc = 0.
IF lv_values IS NOT INITIAL.
                 lv_values = lv_values && ','.
ENDIF.
CASE lr_map->hana.
WHEN 'NVARCHAR'.
                   lv_value = <field>.
REPLACE ALL OCCURRENCES OF `'` IN lv_value WITH `''`.
                   lv_values = lv_values && |'{ lv_value }'|.
WHEN 'DECIMAL' OR 'INTEGER' OR 'TINYINT' OR 'FLOAT'.
                   lv_values = lv_values && |{ <field> }|.
ENDCASE.
ENDLOOP.
             lv_sql = |insert into "{ schema }"."{ table_name }" values ({ lv_values })|.
APPEND lv_sql TO lt_inserts.
DELETE <table>.
ENDLOOP.
CALL FUNCTION 'ZHANA_EXEC_SQL'
             DESTINATION rfc_dest
EXPORTING
               it_inserts = lt_inserts
IMPORTING
               ev_message = lv_msg
               ev_rows    = lv_insert.
ADD lv_insert TO lv_insert_counter.
"WRITE: /, lv_insert_counter, ` records inserted`.
CLEAR lt_inserts.
ENDWHILE.

* All that’s left 
* to do is define the RFC destination for the NW7.4 on HANA system 
* using transaction SM59 and then we are right to go.

* To execute just call the method passing the table name. 
* (Note I have defaulted parameters for schema, RFC destination and batch size.)

zcl_table_repl=>replicate_table( iv_table_name = 'DD03L' ).
