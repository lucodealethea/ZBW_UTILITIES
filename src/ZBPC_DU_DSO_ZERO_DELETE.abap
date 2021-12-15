*&---------------------------------------------------------------------*
*& Report ZBPC_DU_DSO_ZERO_DELETE
*&---------------------------------------------------------------------*
*& Direct Update DSO cannot be compressed with 0 KYF records           *
*& Used usualy as Parameter Model with KYF = 1 - requires to remove 0  *
*&---------------------------------------------------------------------*
REPORT zbpc_du_dso_zero_delete.

CLASS lcl_zpa_ut_cl_adsogen DEFINITION.
  PUBLIC SECTION.
    DATA: mt_msg TYPE string_table.


    METHODS:
      get_adso_info
        IMPORTING iv_adsonm     TYPE rsdodsobject
        EXPORTING e_tablnm      TYPE rsoadsonm
                  et_components TYPE abap_compdescr_tab
                  eo_strucdescr TYPE REF TO cl_abap_structdescr
                  eo_table      TYPE REF TO data.
ENDCLASS.

CLASS lcl_zpa_ut_cl_adsogen IMPLEMENTATION.
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
        e_tablnm = lt_tablnm[ dsotabtype = 'AT' ]-name.
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
PARAMETERS: p_adso   TYPE rsdodsobject DEFAULT 'D_CR_CEPC'.

DATA : iv_adsonm   TYPE rsoadsonm,
       lt_fields   TYPE string_table,
       lv_float    TYPE f,
       lv_pack(16) TYPE p DECIMALS 6.
DATA : wa_string TYPE string,
       m3        TYPE string,

       l_title   TYPE sy-title.
FIELD-SYMBOLS: <lt_target> TYPE STANDARD TABLE.

START-OF-SELECTION.
  iv_adsonm = p_adso.
  DATA(lo_adsogen) = NEW lcl_zpa_ut_cl_adsogen( ).


  lo_adsogen->get_adso_info(
          EXPORTING
            iv_adsonm     = iv_adsonm
          IMPORTING
            e_tablnm      = DATA(lv_tablnm)
            et_components = DATA(lt_components)
            eo_strucdescr = DATA(lo_structdescr)
            eo_table      = DATA(lo_table) ).

  IF lo_adsogen->mt_msg IS NOT INITIAL.
    LOOP AT lo_adsogen->mt_msg ASSIGNING FIELD-SYMBOL(<msg>).
      WRITE: / <msg>.
    ENDLOOP.
    EXIT.
  ELSE.

    DATA: ls_components LIKE LINE OF lt_components.

    ASSIGN lo_table->*  TO <lt_target>.
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
    REFRESH : <lt_target>.
    DATA: ctr  TYPE i, ctrx TYPE numc10.
* find the key figures only Packed Number in BCD Format 'P' are considered
* ie. Currency Field in BCD Format not considered
* delete from DU dso any value in key figures different from 1 in steps
    LOOP AT lt_components INTO ls_components WHERE type_kind = 'P'.

      CLEAR wa_string.
      CONCATENATE ls_components-name 'NE 1' INTO wa_string SEPARATED BY space.
      SELECT * FROM (lv_tablnm) APPENDING CORRESPONDING FIELDS OF TABLE <lt_target> WHERE (wa_string).
      DESCRIBE TABLE <lt_target> LINES ctr.       ctrx = ctr.
      IF ctr <> 0.
        DELETE FROM (lv_tablnm) WHERE (wa_string).
        COMMIT WORK.
        CLEAR wa_string.
        CONCATENATE 'Deleted ' ctrx 'records from ' lv_tablnm INTO wa_string SEPARATED BY '-'.
        WRITE: / wa_string.
        IF sy-batch = abap_false.
          PERFORM callback_alv
          USING l_title abap_true <lt_target>.
        ENDIF.
      ELSE.
        CLEAR wa_string.
        CONCATENATE 'No records needed to be deleted from ' lv_tablnm INTO wa_string SEPARATED BY '-'.
        WRITE: / wa_string.
      ENDIF.
    ENDLOOP.


  ENDIF.
FORM callback_alv USING
VALUE(i_title) TYPE sy-title
VALUE(i_sort)  TYPE abap_bool
it_data        TYPE ANY TABLE.

  IF it_data IS INITIAL.
    CONCATENATE 'No data found with : ' lv_tablnm '/' ctrx '/records, KYF ' ls_components-name INTO m3.
    MESSAGE i799(rsm1) WITH m3.

  ELSE.

    IF i_sort = abap_true.
      SORT it_data.
    ENDIF.

    CALL FUNCTION 'RSDU_CALL_ALV_TABLE'
      EXPORTING
        i_title   = i_title
        i_ta_data = it_data.

  ENDIF.


ENDFORM.
