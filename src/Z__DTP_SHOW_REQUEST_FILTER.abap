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

LOOP AT g_t_range_105 INTO g_s_range_105.
  MOVE:
  i_dtp TO g_s_dtp_filters-DTP,
  g_s_range_105-option TO g_s_dtp_filters-rsoption.
  MOVE-CORRESPONDING g_s_range_105 TO g_s_dtp_filters.
  APPEND g_s_dtp_filters TO g_t_dtp_filters.
ENDLOOP.

INSERT zbw_dtp_filters FROM TABLE g_t_dtp_filters.

COMMIT WORK.

  CLEAR: g_r_custom_container_105, g_t_range_105.

ENDFUNCTION.  

*ZBW_DTP_FILTERS table defnition
*DTP	RSBKDTPNM	CHAR	30	0	Data Transfer Process ID
*FIELDNM	RSFIELDNM	CHAR	30	0	Field name
*POSIT	/BI0/OIRSTT_COUNT	INT4	10	0	Counter
*SIGN	RSSIGN	CHAR	1	0	Selection criteria: SIGN
*RSOPTION	RSOPTION	CHAR	2	0	Selection criteria: OPTION
*LOW	RSLOW	CHAR	45	0	Selection criteria: From value
*HIGH	RSHIGH	CHAR	45	0	Selection criteria: To value
*OR_GROUP	INT4	INT4	10	0	Natural number
