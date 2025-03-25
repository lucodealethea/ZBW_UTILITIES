# ZBW_UTILITIES
Get the dependency tree ( CREATE_OR_REPLACE_FUNCTION GET_OBJECT_DEPENDENCIES.sql) in BW/4 for objects such as Calculation Views.

To store results for several objects : obj_dependencies_populate_results.sql

Get the dependency tree in DataSphere ( DSP_OBJECT_DEPENDENCIES_TREE_ORIG_SCRIPT.sql )
as SQL Scripted View after virtualizing the SYS.OBJECTS in a dedicated SPACE; 
or simply with a SQL Script in SAP HANA Data Explorer accessible in SAP DataSphere.

Utilities such as views in BW or BPC, S/4 AE

Added SQL_ON_RSDDSTAT_OLAP.sql

Allows to get Query Statistics in S/4 from Analytical Engine : Abap CDS based or BMT Based ex. on Multi/Virtual Providers such as /ERP/SFIN_V20.
Use RSRTS_ODP_DIS to find out bottlenecks

HRP1001_RELATIONSHIP.sql

Browse HHR OM structure to find out what relationships have been built
