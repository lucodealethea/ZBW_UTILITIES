TABLE DEFINITION
Store DTP filters with SE38 - ZBW_RSBK_DTP_STORE_FILTER
Key Field DataElement DataType Length Description 
x DTP	RSBKDTPNM	CHAR	30	0	Data Transfer Process ID
x FIELDNM	RSFIELDNM	CHAR	30	0	Field name
x POSIT	/BI0/OIRSTT_COUNT	INT4	10	0	Counter
x SIGN	RSSIGN	CHAR	1	0	Selection criteria: SIGN
x RSOPTION	RSOPTION	CHAR	2	0	Selection criteria: OPTION
x LOW	RSLOW	CHAR	45	0	Selection criteria: From value
x HIGH	RSHIGH	CHAR	45	0	Selection criteria: To value
OR_GROUP	INT4	INT4	10	0	Natural number

*&---------------------------------------------------------------------*
*& Report  RSBK_DTP_STORE_FILTER
*&
*&---------------------------------------------------------------------*
*&
*&ZBW_RSBK_DTP_STORE_FILTER
*&---------------------------------------------------------------------*

REPORT  zbw_rsbk_dtp_store_filter.
TABLES:rsbkdtp.
SELECT-OPTIONS: so_dtp FOR rsbkdtp-dtp.

DATA:
s_dtp LIKE LINE OF so_dtp,
dtp TYPE rsbkdtpnm,
l_r_dtp TYPE REF TO cl_rsbk_dtp.

DATA l_r_dtp_a TYPE REF TO cl_rsbk_dtp_a.
DATA l_r_request TYPE REF TO cl_rsbk_request.


*BREAK BB5827.
IF so_dtp IS NOT INITIAL.

DELETE so_dtp WHERE sign <> 'I' OR option <> 'EQ'.

  LOOP AT so_dtp INTO s_dtp.
    CLEAR: dtp.
    MOVE s_dtp-low TO dtp.
    l_r_dtp = cl_rsbk_dtp=>factory( dtp ).

    TRY.
    l_r_dtp_a ?=
    l_r_dtp->get_obj_ref_objvers( i_objvers = rs_c_objvers-active ).
    l_r_dtp_a->set_simulation( rs_c_true ).
    CATCH cx_rs_version_not_found .
    WRITE:/ ' no active DTP Version'(001). BREAK-POINT. EXIT.
    ENDTRY.


    TRY.

    l_r_request = l_r_dtp->create_request( ).
    l_r_request->set_ctype( rsbc_c_ctype-sync ).
    CATCH cx_root.
    WRITE:/ ' failed to create simulation request'(002). EXIT.

    ENDTRY.

* ====== call save =======
    CALL FUNCTION 'Z__DTP_SHOW_REQUEST_FILTER'
      EXPORTING
       i_r_request       = l_r_request
       i_dtp             = dtp.
    ENDLOOP.

ELSE. "no select-options passed, this will delete our dtp_filters table first, then we use the single ZPRM_PARAMETERS DTP

  DELETE FROM zbw_dtp_filters.
  COMMIT WORK.

  SELECT zparam, zvalue
    FROM zprm_parameters
    INTO TABLE @DATA(param_dtp)
    WHERE zparam = 'FILTER_ACCOUNTG_POC' ORDER BY zparam.


  IF sy-subrc = 0.
    DATA(tmp_dtp) = param_dtp[ 1 ].
    dtp = tmp_dtp-zvalue.

    l_r_dtp = cl_rsbk_dtp=>factory( dtp ).

    TRY.
    l_r_dtp_a ?=
    l_r_dtp->get_obj_ref_objvers( i_objvers = rs_c_objvers-active ).
    l_r_dtp_a->set_simulation( rs_c_true ).
    CATCH cx_rs_version_not_found .
    WRITE:/ ' no active DTP Version'(001). EXIT.
    ENDTRY.


    TRY.

    l_r_request = l_r_dtp->create_request( ).
    l_r_request->set_ctype( rsbc_c_ctype-sync ).
    CATCH cx_root.
    WRITE:/ ' failed to create simulation request'(002). EXIT.

    ENDTRY.

* ====== call save =======
    CALL FUNCTION 'Z__DTP_SHOW_REQUEST_FILTER'
      EXPORTING
       i_r_request       = l_r_request
       i_dtp             = dtp.



  ELSE.
  EXIT.
  ENDIF.

ENDIF.

FUNCTION z__dtp_show_request_filter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_R_REQUEST) TYPE REF TO  IF_RSBK_REQUEST
*"     REFERENCE(I_DTP) TYPE  RSBKDTPNM
*"----------------------------------------------------------------------
  g_r_request ?= i_r_request.
* ===== 1. create container ==============
  IF g_r_custom_container_105 IS INITIAL.
    REFRESH: g_t_range_105.
    CREATE OBJECT g_r_custom_container_105
      EXPORTING
        container_name              = 'FILTER_105'
        parent = cl_gui_container=>screen1
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      MESSAGE x002(zbpcrea).
    ENDIF.
  ENDIF.
* ------ instantiate ALV -----------------
  CREATE OBJECT g_r_grid_105
    EXPORTING
      i_parent = g_r_custom_container_105.
* ===== 2. create viewer instance ========
  CREATE OBJECT g_r_105.

DELETE FROM zbw_dtp_filters WHERE dtp = i_dtp.
COMMIT WORK.
CLEAR: g_s_range_105.
LOOP AT g_t_range_105 INTO g_s_range_105.

  MOVE:
  i_dtp TO g_s_dtp_filters-dtp,
  g_s_range_105-option TO g_s_dtp_filters-rsoption.
  MOVE-CORRESPONDING g_s_range_105 TO g_s_dtp_filters.

  IF g_s_dtp_filters-fieldnm(7) = '/B28/S_'.
    CONCATENATE '/CPMB/'  g_s_dtp_filters-fieldnm+7(7) INTO g_s_dtp_filters-fieldnm.
    SELECT SINGLE dimension INTO g_s_dtp_filters-fieldnm
      FROM uja_dimension WHERE tech_name = g_s_dtp_filters-fieldnm AND appset_id = 'TRACTEBEL_GLO'.
  ENDIF.
  IF g_s_dtp_filters-fieldnm <> 'REQUID'.
  APPEND g_s_dtp_filters TO g_t_dtp_filters.
  ENDIF.
ENDLOOP.
IF g_t_dtp_filters IS NOT INITIAL.
  SORT g_t_dtp_filters BY dtp fieldnm posit sign rsoption low high.
DELETE ADJACENT DUPLICATES FROM g_t_dtp_filters COMPARING dtp fieldnm posit sign rsoption low high.
INSERT zbw_dtp_filters FROM TABLE g_t_dtp_filters.
COMMIT WORK.
ELSE. "we just store the DTP to make sure we collected them as in SO_DTP select-options
  CLEAR:g_s_dtp_filters.
  MOVE:  i_dtp TO g_s_dtp_filters-dtp.
  INSERT zbw_dtp_filters FROM g_s_dtp_filters.
  COMMIT WORK.
ENDIF.

CLEAR: g_r_custom_container_105, g_s_range_105, g_s_dtp_filters.
REFRESH: g_t_range_105, g_t_dtp_filters.

ENDFUNCTION.
LZBWSPECIFICTOP
FUNCTION-POOL ZBWSPECIFIC.

DATA g_r_request TYPE REF TO if_rsbk_request.

DATA g_r_custom_container_105 TYPE REF TO cl_gui_custom_container.

DATA g_r_grid_105 TYPE REF TO cl_gui_alv_grid.

DATA: g_t_range_105 TYPE STANDARD TABLE OF rsbk_s_range,
     g_s_range_105 TYPE rsbk_s_range,

     g_s_dtp_filters TYPE ZBW_DTP_FILTERS,
     g_t_dtp_filters TYPE STANDARD TABLE OF ZBW_DTP_FILTERS.

CLASS lcl_105 DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS register_events
      IMPORTING
        i_activation TYPE char1 DEFAULT rs_c_true .

    METHODS free.
    METHODS refresh_display.
  PRIVATE SECTION.
    METHODS get_data_from_model.
ENDCLASS.                                                   "LCL_105
DATA: g_r_105 TYPE REF TO lcl_105.

class lcl_105 implementation.
* ======== CONSTRUCTOR =============
  method constructor.
    register_events( ).

    get_data_from_model( ).
    refresh_display( ).

  endmethod.                    "constructor

* ======== METHOD REGISTER_EVENTS =============
  method register_events.
*    set handler on_node_focus_changed for g_r_102 activation i_activation.
  endmethod.                    "register_events

* ======== METHOD FREE =============
  method free.
    register_events( rs_c_false ).
  endmethod.                    "free

* ======== refresh display ================
  method refresh_display.

    data: l_s_fcat type lvc_s_fcat,
          l_t_fcat type lvc_t_fcat.

    data: l_t_dd03p type standard table of dd03p,
          l_s_dd03p like line of l_t_dd03p.
    call function 'DDIF_TABL_GET'
      exporting
        name          = 'RSBK_S_RANGE'
      tables
        dd03p_tab     = l_t_dd03p
      exceptions
        illegal_input = 1
        others        = 2.
    if sy-subrc <> 0.
      message id sy-msgid type 'X' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    loop at l_t_dd03p into l_s_dd03p.
      clear l_s_fcat.
      move-corresponding l_s_dd03p to l_s_fcat.
      case l_s_dd03p-fieldname.
        when 'FIELDNM'.
          l_s_fcat-key = 'X'.
          l_s_fcat-outputlen = '20'.
        when 'SIGN'.
        when 'OPTION'.
        when 'LOW' or 'HIGH'.
          l_s_fcat-outputlen = '30'.
        when others.
          continue.
      endcase.
      append l_s_fcat to l_t_fcat.
    endloop.

    data l_s_layout type lvc_s_layo.

    field-symbols: <l_t_data> type table.

    assign g_t_range_105 to <l_t_data>.

    call method g_r_grid_105->set_table_for_first_display
      exporting
        is_layout       = l_s_layout
      changing
        it_outtab       = <l_t_data>
        it_fieldcatalog = l_t_fcat.
  endmethod.                    "refresh_display
* ======== METHOD GET_DATA_FROM_MODEL =============
  method get_data_from_model.
*    rsbkdynp206-processmode = g_r_request->get_processmode( ).
    try.
        data l_th_range type rsbk_th_range.
        l_th_range = g_r_request->get_th_range( rs_c_true ).
      catch cx_rs_not_found.
    endtry.
* move to non-hashed table
    clear g_t_range_105.
    data l_s_range type rsbk_s_range.
    loop at l_th_range into l_s_range.
      append l_s_range to g_t_range_105.
    endloop.
  endmethod.                    "get_data_from_model
endclass.                    "lcl_105 IMPLEMENTATION
