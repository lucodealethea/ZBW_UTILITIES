CREATE PROCEDURE obj_dependencies_populate_results() language sqlscript
AS
  BEGIN
    DECLARE v_schema_name    nvarchar(256);
    declare v_qualified_name nvarchar(256);
    declare v_size           nvarchar(1);
    declare
    CURSOR param_cursor FOR
      SELECT p_schema_name,
             p_qualified_name,
             p_size
      FROM   resa_dependency_param_table;
      
      truncate TABLE resa_objects_dep_results;

FOR param_row AS param_cursor do v_schema_name := param_row.p_schema_name;
v_qualified_name := param_row.p_qualified_name;
v_size := param_row.p_size ;
insert INTO resa_objects_dep_results
            (
                        row_num,
                        schema_name,
                        object_name,
                        lvl,
                        bw_object,
                        bw_object_type,
                        bw4_provider,
                        dependent_object
            )
SELECT   row_num,
         schema_name,
         object_name,
         lvl,
         bw_object,
         bw_object_type,
         bw4_provider,
         dependent_object
FROM     "GET_OBJECT_DEPENDENCIES"(:v_schema_name, :v_qualified_name, :v_size )
ORDER BY row_num ;

COMMIT;
ENDFOR;
END;
