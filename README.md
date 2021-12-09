# ZCL_ALV_MULTI
多ALV展示类的demo程序

[多屏幕ALV类.zip](https://github.com/Limiter-hao/ZCL_ALV_MULTI/files/7683254/ALV.zip)

REPORT zalv_multi_demo.

PARAMETERS: p_1 TYPE c RADIOBUTTON GROUP rg1,
            p_2 TYPE c RADIOBUTTON GROUP rg1.

DATA: gt_vbak TYPE TABLE OF vbak,
      gt_vbap TYPE TABLE OF vbap.

"事件类，只能使用cl_alv_event 来命名
"比较重要的一个参数就是 objid objid用来区分是第几个alv
CLASS cl_alv_event DEFINITION INHERITING FROM zcl_alv_event_receiver.
  PUBLIC SECTION.

    METHODS double_click REDEFINITION."双击
    METHODS user_command REDEFINITION."点击按钮后
    METHODS toolbar REDEFINITION."按钮
    METHODS hotspot_click REDEFINITION."单击
    METHODS change_fieldcat REDEFINITION."更改fieldcat

ENDCLASS.

CLASS cl_alv_event IMPLEMENTATION.

  METHOD toolbar.
    DATA:ls_toolbar TYPE stb_button.

    DEFINE mar_toolbar.
      ls_toolbar-function   = &1.
      ls_toolbar-text       = &2.
      ls_toolbar-quickinfo  = &3.
      ls_toolbar-icon       = &4.
      APPEND ls_toolbar TO e_object->mt_toolbar .
      CLEAR ls_toolbar.
    END-OF-DEFINITION.

    CASE objid.
      WHEN '1'.
        mar_toolbar 'SAVE' '' '保存' icon_system_save.
      WHEN '2'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD user_command.
    CASE e_ucomm.
      WHEN 'SAVE'.

        MESSAGE '保存成功' TYPE 'I' DISPLAY LIKE 'S'.

        "如果是刷新当前的alv,只需要使用get_alv_grid即可
        TRY.
            refresh( get_alv_grid( ) ).
          CATCH zcx_alv_error INTO DATA(cx).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD double_click.

    CASE objid.
      WHEN '1'.
        "按照销售订单来过滤第二个alv
        ASSIGN e_data->* TO FIELD-SYMBOL(<ls_data>).

        DATA(lt_filter) = set_filter( EXPORTING i_fields = VALUE #( ( name = 'VBELN' ) )
                                                i_data   = e_data ).

        "双击的话是要刷新第二个，那么get_alv_grid的时候传入参数2
        TRY.
            get_alv_grid( '2' )->set_filter_criteria( lt_filter ).

            refresh( get_alv_grid( '2' ) ).
          CATCH zcx_alv_error INTO DATA(cx).
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD hotspot_click.

    CASE objid.
      WHEN '1'.
        "单击vbeln来跳转到va03
        ASSIGN e_data->* TO FIELD-SYMBOL(<ls_data>).

        ASSIGN COMPONENT e_column_id-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<value>).
        CASE e_column_id-fieldname.
          WHEN 'VBELN'.
            SET PARAMETER ID 'AUN' FIELD <value>.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDCASE.

    ENDCASE.

  ENDMETHOD.
  METHOD change_fieldcat.

    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CASE <ls_fcat>-fieldname.
        WHEN 'VBELN'.
          <ls_fcat>-hotspot = abap_true.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  gt_vbak = VALUE #( ( vbeln = '1' erdat = sy-datum )
  ( vbeln = '2' erdat = sy-datum )
  ( vbeln = '3' erdat = sy-datum ) ).

  gt_vbap = VALUE #(
  ( vbeln = '1' posnr = '1' )
  ( vbeln = '1' posnr = '2' )
  ( vbeln = '1' posnr = '3' )
  ( vbeln = '2' posnr = '1' )
  ( vbeln = '2' posnr = '2' )
  ( vbeln = '3' posnr = '1' )
  ( vbeln = '3' posnr = '2' )
  ( vbeln = '3' posnr = '3' )
  ( vbeln = '3' posnr = '4' ) ).

  "调用alv展示

  " iv_mode  12  13代表 竖向 2/3 个alv， 21  31代表横向 2/3 个alv

  "系统会根据tcode自动获取tcode的描述作为标题，如果想指定，iv_title 给值即可

  CASE abap_true.
    WHEN p_1."单个屏幕

      TRY.

          NEW zcl_alv_multi(
            it_mapping = VALUE #( ( objid = '1' tabname = 'GT_VBAK' ) ) )->call_screen( ).

        CATCH zcx_alv_error INTO DATA(lcx).
      ENDTRY.

      "单个屏幕的也推荐使用falv

    WHEN p_2."多个屏幕
      TRY.

          NEW zcl_alv_multi(
            it_mapping = VALUE #( ( objid = '1' tabname = 'GT_VBAK' )
                                  ( objid = '2' tabname = 'GT_VBAP' ) )
            iv_mode = '21' )->call_screen( ).

        CATCH zcx_alv_error INTO lcx.
      ENDTRY.

    WHEN OTHERS.
  ENDCASE.
