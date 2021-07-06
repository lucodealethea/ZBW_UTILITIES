*&---------------------------------------------------------------------*
*& Report  YRS_DOWNLOAD_TRANSPORT_REQUEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  YRS_DOWNLOAD_TRANSPORT_REQUEST.
*======================================================================*
*Initial idea and first release by Igor Yaskevitch (IBS), 2003 *
*Enhancements by Sergey Korolev, 2005 (Added F4 value *
*requests, authority checks, TMS function usage) *
*----------------------------------------------------------------------*
*Function : This is a utility tool for downloading binary *
*files of transport request to a Client PC *
*======================================================================*

PARAMETERS:

 p_reqest TYPE trkorr OBLIGATORY, p_folder(255) TYPE c LOWER CASE, p_sepr OBLIGATORY.
DATA:

 folder TYPE string, retval LIKE TABLE OF ddshretval WITH HEADER LINE, fldvalue LIKE help_info-fldvalue, transdir TYPE text255, filename(255), trfile(20) TYPE c, datatab TYPE TABLE OF x WITH HEADER LINE, len TYPE i, flen TYPE i.
TYPE-POOLS: sabc, stms, trwbo.

INITIALIZATION.

  CONCATENATE sy-sysid 'K*' INTO p_reqest.
  IF sy-opsys = 'Windows NT'. p_sepr = '\'. ELSE. p_sepr = '/'. ENDIF.
*  CALL FUNCTION 'WSAF_BUILD_SEPARATOR'
*    IMPORTING
*    separator = p_sepr
*    EXCEPTIONS
*    separator_not_maintained = 1
*    wrong_call = 2
*    wsaf_config_not_maintained = 3
*    OTHERS = 4.

*  IF sy-subrc NE 0.
*  MESSAGE s001(00)
*  WITH
*  'Unable to find out the separator symbol for the system.'(011).
*  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_reqest.

  DATA: tt_system TYPE TABLE OF tmscsys WITH HEADER LINE, es_selected_request TYPE trwbo_request_header, es_selected_task TYPE trwbo_request_header, iv_organizer_type TYPE trwbo_calling_organizer, is_selection TYPE trwbo_selection.
  iv_organizer_type = 'W'. is_selection-reqstatus = 'R'. CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP' EXPORTING iv_organizer_type = iv_organizer_type is_selection = is_selection IMPORTING es_selected_request = es_selected_request es_selected_task =
 es_selected_task.
  p_reqest = es_selected_request-trkorr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.

  DATA: title TYPE string.
  title = 'Select target folder'(005). CALL METHOD cl_gui_frontend_services=>directory_browse EXPORTING window_title = title CHANGING selected_folder = folder EXCEPTIONS cntl_error = 1 error_no_gui = 2 OTHERS = 3.
  CALL FUNCTION 'CONTROL_FLUSH'
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.
  p_folder = folder.

AT SELECTION-SCREEN ON p_reqest.

  DATA: request_info TYPE stms_wbo_request, request_infos TYPE stms_wbo_requests.
  REFRESH request_infos. CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_REQUEST' EXPORTING iv_request = p_reqest iv_header_only = 'X' IMPORTING et_request_infos = request_infos EXCEPTIONS read_config_failed = 1 table_of_requests_is_empty = 2 system_not_available
 = 3 OTHERS = 4.
  IF sy-subrc <> 0. MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. ENDIF.
  CLEAR request_info. READ TABLE request_infos INTO request_info INDEX 1.
  IF sy-subrc NE 0 OR request_info-e070-trkorr IS INITIAL.
    MESSAGE e398(00) WITH 'Request'(006) p_reqest 'not found'(007).
  ELSEIF request_info-e070-trstatus NE 'R'.
    MESSAGE e398(00) WITH 'You must release request'(008) request_info-e070-trkorr 'before downloading'(009).
  ENDIF.

START-OF-SELECTION.

  folder = p_folder. CONCATENATE p_reqest+3(7) '.' p_reqest(3) INTO trfile.
  CALL FUNCTION 'RSPO_R_SAPGPARAM'
    EXPORTING
      name   = 'DIR_TRANS'
    IMPORTING
      value  = transdir
    EXCEPTIONS
      error  = 0
      OTHERS = 0.
  PERFORM copy_file USING 'cofiles' trfile. trfile(1) = 'R'. PERFORM copy_file USING 'data' trfile. trfile(1) = 'D'. PERFORM copy_file USING 'data' trfile.
*---------------------------------------------------------------------*
* FORM copy_file *
*---------------------------------------------------------------------*
*--> SUBDIR * * --> FNAME *
*---------------------------------------------------------------------*
  FORM copy_file USING subdir fname.

  DATA: auth_filename TYPE authb-filename, gui_filename TYPE string.
  CONCATENATE transdir subdir fname INTO filename SEPARATED BY p_sepr.
  REFRESH datatab. CLEAR flen.
  auth_filename = filename. CALL FUNCTION 'AUTHORITY_CHECK_DATASET' EXPORTING activity = sabc_act_read filename = auth_filename EXCEPTIONS no_authority = 1 activity_unknown = 2 OTHERS = 3.
  IF sy-subrc <> 0. FORMAT COLOR COL_NEGATIVE. WRITE: / 'Read access denied. File'(001), filename. FORMAT COLOR OFF. EXIT. ENDIF.
  OPEN DATASET filename FOR INPUT IN BINARY MODE.
  IF sy-subrc NE 0. FORMAT COLOR COL_TOTAL. WRITE: / 'File open error'(010), filename. FORMAT COLOR OFF. EXIT. ENDIF.
  DO. CLEAR len. READ DATASET filename INTO datatab LENGTH len. flen = flen + len. IF len > 0. APPEND datatab. ENDIF. IF sy-subrc NE 0. EXIT. ENDIF. ENDDO.
  CLOSE DATASET filename. CONCATENATE p_folder '\' fname INTO gui_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = flen
      filename                = gui_filename
      filetype                = 'BIN'
    CHANGING
      data_tab                = datatab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 24.
  IF sy-subrc = 0. WRITE: / 'File'(002), filename, 'downloaded. Length'(003), flen. ELSE. FORMAT COLOR COL_NEGATIVE. WRITE: / 'File download error. Filename:'(004), filename. FORMAT COLOR OFF. ENDIF.
ENDFORM. "copy_file
