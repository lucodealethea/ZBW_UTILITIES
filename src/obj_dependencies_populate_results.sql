DROP TABLE xxx_objects_dep_results;
CREATE TABLE xxx_objects_dep_results (
    ROW_NUM INTEGER,
    SCHEMA_NAME NVARCHAR(256) ,
    OBJECT_NAME NVARCHAR(310) ,
    LVL NVARCHAR(3),
	  BW_OBJECT NVARCHAR(256),
    BW_OBJECT_TYPE NVARCHAR(15),
    BW4_PROVIDER NVARCHAR(256),
    DEPENDENT_OBJECT NVARCHAR(4000),
PRIMARY KEY (ROW_NUM, SCHEMA_NAME, OBJECT_NAME)    
);
-- Step 2 Intermediate Parameter Table

DROP TABLE xxx_dependency_param_table;

CREATE TABLE xxx_dependency_param_table (
p_schema_name NVARCHAR(256),
p_qualified_name NVARCHAR(256),
p_size NVARCHAR(1),
p_hpcr NVARCHAR(256)  
PRIMARY KEY (P_SCHEMA_NAME, P_QUALIFIED_NAME)
)
;
INSERT INTO "SAPHANADB".xxx_dependency_param_table VALUES ('SAPHANADB','/BIC/VCP_PAN_6H_V38','X','CP_PAN_6H_V3');
INSERT INTO "SAPHANADB".xxx_dependency_param_table VALUES ('SAPHANADB','/BIC/VD_QMAD10_V018','X','D_QMAD10_V01');
INSERT INTO "SAPHANADB".xxx_dependency_param_table VALUES ('SAPHANADB','/BIC/VV_ATRS018','X','');


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
      FROM   xxx_dependency_param_table;
      
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
-- not finished
CREATE OR REPLACE FUNCTION GET_VIEW_TABLE_COLUMNS 
( IN P_SCHEMA_NAME NVARCHAR(256) DEFAULT 'SYS',
  IN P_OBJ_NAME NVARCHAR(256) DEFAULT 'VIEW_COLUMNS',
  IN P_SIZE NVARCHAR(1) DEFAULT ' ' 
) 
RETURNS TABLE ( 
		 SCHEMA_NAME NVARCHAR(256) ,
		 OBJECT_NAME NVARCHAR(310) ,
             COLUMN_NAME NVARCHAR(256),
             POSITION INTEGER,
             DATA_TYPE_NAME VARCHAR(16),
             LENGTH INTEGER,
             IS_NULLABLE VARCHAR(5),
             COMMENTS NVARCHAR(5000),
             CS_DATA_TYPE_NAME VARCHAR(16)
	 ) 	
LANGUAGE SQLSCRIPT SQL SECURITY DEFINER READS SQL DATA AS
	BEGIN BASIS_INFO = 
SELECT
	"SCHEMA_NAME",
	"OBJECT_NAME",
	:P_SIZE AS "INCLUDE_SIZE_INFORMATION"
	FROM "SYS"."OBJECTS"
	WHERE "SCHEMA_NAME" = :P_SCHEMA_NAME
	AND "OBJECT_NAME" = :P_OBJ_NAME
;
      VIEW_COLS =
SELECT
  C.SCHEMA_NAME, C.VIEW_NAME AS OBJECT_NAME, C.COLUMN_NAME, C.POSITION,
  C.DATA_TYPE_NAME, C.LENGTH, C.IS_NULLABLE, C.COMMENTS, C.CS_DATA_TYPE_NAME
FROM
  SYS.VIEW_COLUMNS AS C
  INNER JOIN :BASIS_INFO AS BI ON C.SCHEMA_NAME = BI.SCHEMA_NAME AND C.VIEW_NAME = BI.OBJECT_NAME
ORDER BY POSITION
;
RETURN
SELECT * FROM :VIEW_COLS;
	
END
;
