FUNCTION zalv_call_screen.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(CL_MULTI) TYPE REF TO  ZCL_ALV_MULTI
*"----------------------------------------------------------------------

  gcl_alv_multi = cl_multi.

  CALL SCREEN 100.

ENDFUNCTION.
