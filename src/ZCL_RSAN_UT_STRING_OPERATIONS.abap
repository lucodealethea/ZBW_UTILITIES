class ZCL_RSAN_UT_STRING_OPERATIONS definition
  public
  final
  create public .

public section.

  constants C_EMPTY_STRING type STRING value `` ##NO_TEXT.
  constants C_SPACE_STRING type STRING value ` ` ##NO_TEXT.

  class-methods TRIM
    importing
      value(I_INPUT) type STRING
    returning
      value(R_OUTPUT) type STRING .
  class-methods LTRIM
    importing
      value(I_INPUT) type STRING
    returning
      value(R_OUTPUT) type STRING .
  class-methods RTRIM
    importing
      value(I_INPUT) type STRING
    returning
      value(R_OUTPUT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_RSAN_UT_STRING_OPERATIONS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RSAN_UT_STRING_OPERATIONS=>LTRIM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INPUT                        TYPE        STRING
* | [<-()] R_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD ltrim.

  IF i_input IS NOT INITIAL.
    SHIFT i_input LEFT  DELETING LEADING  space.
  ENDIF.
  r_output = i_input.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RSAN_UT_STRING_OPERATIONS=>RTRIM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INPUT                        TYPE        STRING
* | [<-()] R_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD rtrim.
  CONSTANTS c_char_buffer_legth TYPE i VALUE 4096.

  IF i_input IS NOT INITIAL.
    DATA l_len TYPE i.
    DATA l_char_buffer(c_char_buffer_legth) TYPE c.
    l_len = STRLEN( i_input ).

* enough short strings move through character field -> this will do rtrim automatically
    IF l_len <= c_char_buffer_legth.
      l_char_buffer = i_input.
      i_input       = l_char_buffer.
    ELSE.
*rc - comment - not possible to use: SHIFT i_input RIGHT DELETING TRAILING space.
      DATA l_idx TYPE i.
      l_idx = l_len - 1.
      DO.
        IF    l_idx <= 0
           OR i_input+l_idx(1) NE c_space_string.
*
          EXIT.
        ENDIF.
        SUBTRACT 1 FROM l_idx.
      ENDDO.
      ADD 1 TO l_idx.
      i_input = i_input(l_idx).
    ENDIF.
  ENDIF.
  r_output = i_input.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RSAN_UT_STRING_OPERATIONS=>TRIM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INPUT                        TYPE        STRING
* | [<-()] R_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD trim.

  IF i_input IS NOT INITIAL.
    SHIFT i_input RIGHT DELETING TRAILING space.
    SHIFT i_input LEFT  DELETING LEADING  space.
  ENDIF.
  r_output = i_input.

ENDMETHOD.
ENDCLASS.
