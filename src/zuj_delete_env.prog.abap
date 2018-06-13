*&---------------------------------------------------------------------*
*& Report ZUJ_DELETE_ENV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUJ_DELETE_ENV.

PARAMETERS: P_APPSET  TYPE uja_formula_app-appset_id.

DATA: lo_appset_mgr TYPE REF TO cl_ujaa_appset.

        CREATE OBJECT lo_appset_mgr
          EXPORTING
            i_appset_id = P_APPSET.

          lo_appset_mgr->if_uja_appset_manager~delete( ).
