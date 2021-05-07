*&---------------------------------------------------------------------*
*& Report ZPA_ADSO_GEN
*&---------------------------------------------------------------------*
*& First Run: creates a aDSO based on .csv file with clean header name *
*& Second Run: fills the aDSO based from .csv file                     *
*& Failure: adjust the aDSO or adjust the file content with regds to   *
*& Escape Character, Coma inside Text Fields. Division Sign etc....... *
*&---------------------------------------------------------------------*
REPORT zpa_adso_gen_load.

* P_ADSO	ADSO Name
* P_DEL	Delimiter
* P_ESC	Escape Character
* P_GENHAN	Generate Hana View
* P_INAREA	InfoArea
**********************************************************************
" local class definition - should be implemented as global classes at customer
******************************************
CLASS lcl_zpa_ut_cl_adsogen DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_file_analysis,
        columnname  TYPE string,
        columnindex TYPE int8,
        max_length  TYPE int8,
        datatype    TYPE datatype_d,
        aggregation TYPE rsdaggrgen,
      END   OF ty_file_analysis .
    TYPES:
      tt_file_analysis TYPE STANDARD TABLE OF ty_file_analysis .
    DATA: mt_msg TYPE string_table.
    METHODS:
      analyze_csv
        IMPORTING iv_string       TYPE string
                  iv_delimiter    TYPE char1
                  iv_escape_char  TYPE char1
                  iv_adsonm       TYPE rsoadsonm
        EXPORTING et_adso_objects TYPE cl_rso_adso_api=>tn_t_object,
      process_csv
        IMPORTING iv_delimiter   TYPE char1
                  iv_escape_char TYPE char1
                  iv_adsonm      TYPE rsoadsonm
                  iv_infoarea    TYPE rsinfoarea
                  iv_genhanaview TYPE abap_bool
        EXPORTING et_adso_msg    TYPE rs_t_msg
        CHANGING
                  cv_string      TYPE string,
      create_adso
        IMPORTING iv_adsonm      TYPE rsoadsonm
                  iv_infoarea    TYPE rsinfoarea
                  it_objects     TYPE cl_rso_adso_api=>tn_t_object
                  iv_genhanaview TYPE abap_bool
        RETURNING VALUE(rt_msg)  TYPE rs_t_msg,
      get_adso_info
        IMPORTING iv_adsonm     TYPE rsdodsobject
        EXPORTING e_tablnm      TYPE rsoadsonm
                  et_components TYPE abap_compdescr_tab
                  eo_strucdescr TYPE REF TO cl_abap_structdescr
                  eo_table      TYPE REF TO data,
      write_to_adso
        IMPORTING
          iv_infoarea    TYPE rsinfoarea
          iv_adsonm      TYPE rsoadsonm
          iv_delimiter   TYPE char1
          iv_escape_char TYPE char1
        CHANGING
          cv_string      TYPE string.

ENDCLASS.
CLASS lcl_zpa_ut_cl_adsogen IMPLEMENTATION.
  METHOD write_to_adso.

    TRY.
        DATA: lt_string  TYPE string_table.
        DATA: lv_line     TYPE string.
        DATA: lt_header  TYPE string_table.
        DATA: lv_error_counter TYPE integer.
        DATA: lt_fields   TYPE string_table,
              lv_float    TYPE f,
              lv_pack(16) TYPE p DECIMALS 6.

        FIELD-SYMBOLS: <lt_target> TYPE STANDARD TABLE.

        " get adso info
        get_adso_info(
          EXPORTING
            iv_adsonm     = iv_adsonm
          IMPORTING
            e_tablnm      = DATA(lv_tablnm)
            et_components = DATA(lt_components)
            eo_strucdescr = DATA(lo_structdescr)
            eo_table      = DATA(lo_table) ).

        ASSIGN lo_table->*  TO <lt_target>.
        SPLIT cv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_string.


        CALL FUNCTION 'RSDS_CONVERT_CSV'
          EXPORTING
            i_data_sep       = iv_delimiter
            i_esc_char       = iv_escape_char
            i_record         = lt_string[ 1 ]
            i_field_count    = 9999
          IMPORTING
            e_t_data         = lt_header
          EXCEPTIONS
            escape_no_close  = 1
            escape_improper  = 2
            conversion_error = 3
            OTHERS           = 4.

        IF sy-subrc <> 0.
          APPEND |Error analyzing the header| TO mt_msg.
          EXIT.
        ENDIF.


        FREE: cv_string.
        " remove header
        DELETE lt_string INDEX 1.

        DATA: lv_counter TYPE i.
        DATA: lv_counter_prev TYPE i.


        " Process File content
        LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<ls_string>).
          CLEAR: lt_fields.
*          lv_line = lv_line && <ls_string>. " this does not work... removed HKURZ 2020-08-10
          " hack to handle newlines within field content (rather often happens in web data)
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_line  WITH ''.
          CALL FUNCTION 'RSDS_CONVERT_CSV'
            EXPORTING
              i_data_sep       = iv_delimiter
              i_esc_char       = iv_escape_char
              i_record         = <ls_string>
              i_field_count    = lines( lt_header )
            IMPORTING
              e_t_data         = lt_fields
            EXCEPTIONS
              escape_no_close  = 1
              escape_improper  = 2
              conversion_error = 3
              OTHERS           = 4.
          IF sy-subrc <> 0.
            lv_error_counter = lv_error_counter + 1.
          ELSE.
            DESCRIBE TABLE lt_fields.
            IF sy-tfill < lv_counter. "concat with next line
              lv_counter_prev = sy-tfill.
              CONTINUE.
            ELSEIF sy-tfill > lv_counter.
              DATA(lv_check) =  sy-tfill - lv_counter_prev .
              IF lv_check = lv_counter. "new line is correct
                CLEAR: lt_fields.
                lv_line =  <ls_string>.
                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_line  WITH ''.
                CALL FUNCTION 'RSDS_CONVERT_CSV'
                  EXPORTING
                    i_data_sep       = iv_delimiter
                    i_esc_char       = iv_escape_char
                    i_record         = lv_line
                    i_field_count    = lines( lt_header )
                  IMPORTING
                    e_t_data         = lt_fields
                  EXCEPTIONS
                    escape_no_close  = 1
                    escape_improper  = 2
                    conversion_error = 3
                    OTHERS           = 4.
                IF sy-subrc <> 0.
                  CLEAR lv_line.
                  CONTINUE. " skip
                ELSE.
                  CLEAR lv_line.
                ENDIF.
              ENDIF.
            ELSEIF sy-tfill = lv_counter.
              CLEAR lv_line.

            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO <lt_target> ASSIGNING FIELD-SYMBOL(<ls_target>).

          LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<comp>).
            ASSIGN COMPONENT <comp>-name OF STRUCTURE <ls_target> TO FIELD-SYMBOL(<target_field>).

            TRY .
                FIND REGEX '\de-\d' IN lt_fields[ sy-tabix ] IGNORING CASE. " scientific notation has to be handled
                IF sy-subrc = 0.
*                    " hack to convert to number
                  lv_float =  lt_fields[ sy-tabix ].
                  lv_pack = lv_float.
                  lt_fields[ sy-tabix ] = lv_pack.
                  FREE: lv_float, lv_pack.
                ENDIF.

                <target_field> = lt_fields[ sy-tabix ].

              CATCH cx_sy_itab_line_not_found.
                CONTINUE.

            ENDTRY.
          ENDLOOP. " inner field loop

        ENDLOOP. " lt_string outer loop

      CATCH cx_root.
        APPEND |Error writing to ADSO { iv_adsonm }.| TO mt_msg.
    ENDTRY.

    " we got so far... now write
    DATA: lv_lines TYPE i.
    DATA: lt_msg TYPE rs_t_msg.

    CALL FUNCTION 'RSDSO_WRITE_API'
      EXPORTING
        i_adsonm            = iv_adsonm
*       I_ALLOW_NEW_SIDS    = RS_C_TRUE
*       I_ACTIVATE_DATA     = RS_C_FALSE
*       IT_AGGREGATION      =
        it_data             = <lt_target>
      IMPORTING
        e_lines_inserted    = lv_lines
        et_msg              = lt_msg
      EXCEPTIONS
        write_failed        = 1
        activation_failed   = 2
        datastore_not_found = 3.

    IF sy-subrc <> 0.
      APPEND |Error writing to ADSO { iv_adsonm }.| TO mt_msg.
    ELSE.
      APPEND |Data successfully written to ADSO { iv_adsonm }.| TO mt_msg.
    ENDIF.

  ENDMETHOD.

  METHOD analyze_csv.
    DATA: lt_file_analysis  TYPE tt_file_analysis.
    DATA: lt_string         TYPE string_table.
    DATA: lt_stringchk      TYPE string_table.
    DATA: lt_fields         TYPE string_table.
    DATA: lt_header         TYPE string_table.
    DATA: lv_headerlines    TYPE integer.

    " For this to work the file needs a header.. no header not supported for now

    TRY.
        SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_string.
        IF lines( lt_string ) = 1. " dirty trick to handle windows and linux based files
          SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.
        ENDIF.

        " Analyze Header
        LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<ls_string>) FROM 1 TO 1.
          CALL FUNCTION 'RSDS_CONVERT_CSV'
            EXPORTING
              i_data_sep       = iv_delimiter
              i_esc_char       = iv_escape_char
              i_record         = <ls_string>
              i_field_count    = 9999
            IMPORTING
              e_t_data         = lt_header
            EXCEPTIONS
              escape_no_close  = 1
              escape_improper  = 2
              conversion_error = 3
              OTHERS           = 4.

          IF sy-subrc <> 0.
            APPEND |Error analyzing the header { <ls_string> }.| TO mt_msg.
            EXIT.
          ENDIF.

          LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<header>).
            APPEND INITIAL LINE TO lt_file_analysis ASSIGNING FIELD-SYMBOL(<ls_file_analysis>).
            CONDENSE <header>.
            REPLACE ALL OCCURRENCES OF ` ` IN <header> WITH '_'.
            TRANSLATE <header> TO UPPER CASE.
            <ls_file_analysis>-columnname  = <header>.
            <ls_file_analysis>-columnindex = sy-tabix.
            ADD 1 TO lv_headerlines.
          ENDLOOP.

        ENDLOOP.

        " now process the rest of the file
        LOOP AT lt_string ASSIGNING <ls_string> FROM 2.
          FREE: lt_fields.

          CALL FUNCTION 'RSDS_CONVERT_CSV'
            EXPORTING
              i_data_sep       = iv_delimiter
              i_esc_char       = iv_escape_char
              i_record         = <ls_string>
              i_field_count    = lines( lt_header )
            IMPORTING
              e_t_data         = lt_fields
            EXCEPTIONS
              escape_no_close  = 1
              escape_improper  = 2
              conversion_error = 3
              OTHERS           = 4.

          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          " Determine Field lengths
          LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
            ASSIGN lt_file_analysis[ columnindex = sy-tabix ] TO FIELD-SYMBOL(<ls_fa>).

            IF <ls_fa>-max_length < strlen( <ls_field> ).
              <ls_fa>-max_length = strlen( <ls_field> ).
            ENDIF.

            CHECK <ls_field> IS NOT INITIAL.

            IF <ls_field> CO '0123456789,.'.
              IF <ls_fa>-datatype <> 'CHAR'. " if already one char has been detected no switch to int4
                <ls_fa>-datatype = 'DEC'.
              ENDIF.
            ELSE.
              <ls_fa>-datatype = 'CHAR'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        " get reserved fieldnames for the system
        SELECT name
          FROM trese
          INTO TABLE @DATA(lt_reserved).

        DATA: lv_counter TYPE i.

        LOOP AT lt_file_analysis ASSIGNING <ls_file_analysis>.
          lv_counter = lv_counter + 1.

          " no "empty" columns
          IF <ls_file_analysis>-max_length = 0.
            <ls_file_analysis>-max_length = 1.
          ENDIF.

          " datatype always needed
          IF <ls_file_analysis>-datatype IS INITIAL.
            <ls_file_analysis>-datatype = 'CHAR'.
          ENDIF.

          " set aggregation
          IF <ls_file_analysis>-datatype = 'DEC'.
            <ls_file_analysis>-aggregation = 'SUM'.
            <ls_file_analysis>-max_length  = 17.
          ENDIF.

          " round up the field lengths? logic -> add 20% for starters
          MULTIPLY <ls_file_analysis>-max_length BY '1.2'.

          IF <ls_file_analysis>-max_length > 250.
            <ls_file_analysis>-max_length = 250. " max reporting length
          ENDIF.

          " check for reserved values like "DAY"
          IF line_exists( lt_reserved[ name = <ls_file_analysis>-columnname ] ).
            <ls_file_analysis>-columnname = |MN_{ <ls_file_analysis>-columnname }|.
          ENDIF.

          " remove special chars
          DATA(lv_fieldname) = <ls_file_analysis>-columnname.
          REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN lv_fieldname WITH ''.

          IF strlen( lv_fieldname ) > 20.
            DATA(lv_tabix) = CONV numc3( sy-tabix ).
            lv_fieldname = |{ lv_fieldname+0(17) }{ lv_tabix }|.
          ENDIF.

          APPEND INITIAL LINE TO et_adso_objects ASSIGNING FIELD-SYMBOL(<ls_adso_object>).

          <ls_adso_object> = VALUE cl_rso_adso_api=>tn_s_object(
              fieldname              = lv_fieldname
              datatp                 = <ls_file_analysis>-datatype
              length                 = <ls_file_analysis>-max_length
              dimension              = 'DEFAULT'
              txtsh                  = <ls_file_analysis>-columnname
              aggregation            = <ls_file_analysis>-aggregation ).
        ENDLOOP.

      CATCH cx_root.
        APPEND |Unhandled error analyzing File.| TO mt_msg.
    ENDTRY.


  ENDMETHOD.
  METHOD process_csv.

    IF cl_rso_adso=>exists_on_db( i_objnm = CONV sobj_name( iv_adsonm ) i_objvers = 'A' ) = abap_true.
      " write mode

      write_to_adso(
        EXPORTING
          iv_adsonm       = iv_adsonm
          iv_infoarea     = iv_infoarea
          iv_delimiter    = iv_delimiter
          iv_escape_char  = iv_escape_char
        CHANGING
          cv_string       = cv_string
      ).


    ELSE.
      " create mode
      analyze_csv(
        EXPORTING
          iv_string       = cv_string
          iv_delimiter    = iv_delimiter
          iv_escape_char  = iv_escape_char
          iv_adsonm       = iv_adsonm
        IMPORTING
          et_adso_objects = DATA(lt_objects) ).

      create_adso(
        EXPORTING
          iv_adsonm      = iv_adsonm
          iv_infoarea    = iv_infoarea
          it_objects     = lt_objects
          iv_genhanaview = iv_genhanaview
        RECEIVING
          rt_msg      = et_adso_msg ).
    ENDIF.

  ENDMETHOD.
  METHOD create_adso.

    " Check ADSO does not yet exist
    IF iv_infoarea IS NOT INITIAL.
      DATA(lv_infoarea) = iv_infoarea.
      " check the infoarea is available
      SELECT SINGLE infoarea
        FROM rsdarea
        WHERE infoarea = @iv_infoarea
          AND objvers = 'A'
        INTO @DATA(lv_area).

      IF sy-subrc <> 0.
        APPEND |Infoarea { iv_infoarea } not found.| TO mt_msg.
        EXIT.
      ENDIF.

    ELSE.
      lv_infoarea = 'NODESNOTCONNECTED'. "default
    ENDIF.

    DATA(ls_adsoflags) = VALUE cl_rso_adso_api=>tn_s_adsoflags( hanamodelfl = iv_genhanaview ).
    DATA(lt_dimension) = VALUE cl_rso_adso_api=>tn_t_dimension( ( name = 'DEFAULT' txtsh = 'Default' ) ).

    TRY.
        cl_rso_adso_api=>create(
          EXPORTING
            i_adsonm                      = iv_adsonm
            i_text                        = CONV rsoadsodescr( iv_adsonm )
            i_infoarea                    = iv_infoarea
            i_s_adsoflags                 = ls_adsoflags
            i_t_object                    = it_objects
            i_t_dimension                 = lt_dimension
          IMPORTING
            e_t_msg                       = rt_msg ).

      CATCH cx_rs_all_msg INTO DATA(lo_error).
        DATA(lv_error_text) = lo_error->get_longtext( ).
        APPEND |{ lv_error_text }.| TO mt_msg.
    ENDTRY.


  ENDMETHOD.
  METHOD get_adso_info.

    IF cl_rso_adso=>exists_on_db( i_objnm = CONV sobj_name( iv_adsonm ) i_objvers = 'A' ) = abap_false.
      APPEND |ADSO { iv_adsonm } not found as active object.| TO mt_msg.
      EXIT.
    ENDIF.

    " get tablename to write
    TRY .
        DATA(lt_tablnm) = cl_rso_adso=>get_tablnm( EXPORTING
            i_adsonm        = iv_adsonm
            i_objvers       = rs_c_objvers-active
            i_only_existing = rs_c_true
        ).
      CATCH cx_rs_not_found.    "
        EXIT.
    ENDTRY.

    TRY .
        e_tablnm = lt_tablnm[ dsotabtype = 'AQ' ]-name.
      CATCH cx_sy_itab_line_not_found.
        EXIT.
    ENDTRY.


    " Get Strucdescr and "relevant" components for writing
    TRY.
        eo_strucdescr ?= cl_abap_typedescr=>describe_by_name( e_tablnm ).
        DATA(lt_components) = eo_strucdescr->components.
        " remove technical components (not needed for data retrieval)
        DELETE lt_components WHERE name = 'REQTSN' OR name = 'DATAPAKID' OR name = 'RECORD' OR name = 'RECORDMODE'.
        et_components = lt_components.
        FREE: lt_components.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    CREATE DATA eo_table TYPE STANDARD TABLE OF (e_tablnm).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
" Main part of the program

START-OF-SELECTION.
  DATA: lt_filetable TYPE filetable.
  DATA: lv_rc TYPE sysubrc.
  DATA: lt_filedata TYPE stringtab.
  DATA: lv_filename TYPE string.
  DATA: lv_string TYPE string.

*** Screen
*  SELECTION-SCREEN BEGIN OF BLOCK todo WITH FRAME TITLE TEXT-000.
*  PARAMETERS: p_gen TYPE abap_bool AS CHECKBOX DEFAULT abap_true.
*  PARAMETERS: p_write TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
*  SELECTION-SCREEN END OF BLOCK todo.

  SELECTION-SCREEN BEGIN OF BLOCK source WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_del(1) TYPE c DEFAULT ','.
  PARAMETERS: p_esc(1) TYPE c DEFAULT '#'.

  SELECTION-SCREEN END OF BLOCK source.
  SELECTION-SCREEN BEGIN OF BLOCK target WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_inarea TYPE rsinfoarea DEFAULT 'NODESNOTCONNECTED'.
  PARAMETERS: p_adso   TYPE rsdodsobject DEFAULT 'ZPATEST'.
  PARAMETERS: p_genhan TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
  SELECTION-SCREEN END OF BLOCK target.

  " Get File from Workstation
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select upload file'
      file_filter             = '*.csv'
      multiselection          = abap_false " only one file
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK lt_filetable IS NOT INITIAL.
  lv_filename = lt_filetable[ 1 ]. " retrieve path
  CHECK lv_filename IS NOT INITIAL.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = lt_filedata
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK lt_filedata IS NOT INITIAL.
  " build one string so the methods can later one be reused more easily.
  " in most cases (e.g. csv from web etc.) we will get one large string
  LOOP AT lt_filedata ASSIGNING FIELD-SYMBOL(<line>).
    IF lv_string IS INITIAL.
      lv_string = |{ <line> }{ cl_abap_char_utilities=>cr_lf }|.
    ELSE.
      lv_string = |{ lv_string }{ <line> }{ cl_abap_char_utilities=>cr_lf }|.
    ENDIF.
  ENDLOOP.
  FREE: lt_filedata.

  DATA(lo_adsogen) = NEW lcl_zpa_ut_cl_adsogen( ).


    lo_adsogen->process_csv(
      EXPORTING
        iv_delimiter   = p_del
        iv_escape_char = p_esc
        iv_adsonm      = p_adso
        iv_infoarea    = p_inarea
        iv_genhanaview = p_genhan
      IMPORTING
        et_adso_msg    = DATA(lt_adso_msg)
      CHANGING
        cv_string      = lv_string  ).

    IF lt_adso_msg IS NOT INITIAL.
      CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
        EXPORTING
          i_t_msg = lt_adso_msg
          i_txt   = 'ADSO Generation Log - Please check the generated ADSO'.
      EXIT.
    ENDIF.

    IF lo_adsogen->mt_msg IS NOT INITIAL.
      LOOP AT lo_adsogen->mt_msg ASSIGNING FIELD-SYMBOL(<msg>).
        WRITE: / <msg>.
      ENDLOOP.
    ENDIF.
