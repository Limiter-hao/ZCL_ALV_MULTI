class ZCX_ALV_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

*"* public components of class ZCX_ALV_ERROR
*"* do not include other source files here!!!
*"* protected components of class ZCX_ALV_ERROR
*"* do not include other source files here!!!
*"* protected components of class ZCX_ALV_ERROR
*"* do not include other source files here!!!
public section.

  constants ZCX_ALV_ERROR type SOTR_CONC value 'FA218DFC22001EDC95E4A396FA578961' ##NO_TEXT.
  data ERROR type STRING .
  data SYST_AT_RAISE type SYST .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ERROR type STRING optional
      !SYST_AT_RAISE type SYST optional .
  class-methods RAISE_TEXT
    importing
      !IV_TEXT type CLIKE
    raising
      ZCX_ALV_ERROR .
  class-methods RAISE_SYMSG
    raising
      ZCX_ALV_ERROR .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
*"* private components of class ZCX_ALV_ERROR
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCX_ALV_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_ALV_ERROR .
 ENDIF.
me->ERROR = ERROR .
me->SYST_AT_RAISE = SYST_AT_RAISE .
  endmethod.


method IF_MESSAGE~GET_LONGTEXT.

  IF   me->error         IS NOT INITIAL
    OR me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this as longtext as well
*--------------------------------------------------------------------*
    result = me->get_text( ).
  ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
    super->if_message~get_longtext( EXPORTING
                                      preserve_newlines = preserve_newlines
                                    RECEIVING
                                      result            = result ).
  ENDIF.
  endmethod.


method IF_MESSAGE~GET_TEXT.

  IF me->error IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this
*--------------------------------------------------------------------*
    result = me->error .
  ELSEIF me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied by syst create messagetext now
*--------------------------------------------------------------------*
    MESSAGE ID syst_at_raise-msgid TYPE syst_at_raise-msgty NUMBER syst_at_raise-msgno
         WITH  syst_at_raise-msgv1 syst_at_raise-msgv2 syst_at_raise-msgv3 syst_at_raise-msgv4
         INTO  result.
  ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
    CALL METHOD super->if_message~get_text
      RECEIVING
        result = result.
  ENDIF.
  endmethod.


  method RAISE_SYMSG.
    raise exception type zcx_excel
      exporting
        syst_at_raise = syst.
  endmethod.


  METHOD raise_text.
    RAISE EXCEPTION TYPE zcx_alv_error
      EXPORTING
        error = iv_text.
  ENDMETHOD.
ENDCLASS.
