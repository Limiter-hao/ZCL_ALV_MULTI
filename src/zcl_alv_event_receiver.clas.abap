"! alv事件类，用于配合zcl_alv_multi使用
"! Author  :郝光源
"! Req Date:20211207
"! 重构了之前版本的代码，解决了能够按照程序中的子类来实例化的问题
"! 基本上满足了常用alv事件的使用
CLASS zcl_alv_event_receiver DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        VALUE(i_subclass) TYPE REF TO cl_abap_typedescr OPTIONAL
        VALUE(i_objid)    TYPE bu_object_id OPTIONAL
        !i_alvmulti       TYPE REF TO zcl_alv_multi
        !i_mainprogram    TYPE sy-repid
      RETURNING
        VALUE(ro_event)   TYPE REF TO zcl_alv_event_receiver .
    METHODS constructor
      IMPORTING
        VALUE(i_objid) TYPE bu_object_id OPTIONAL
        !i_alvmulti    TYPE REF TO zcl_alv_multi OPTIONAL
        !i_mainprogram TYPE sy-repid OPTIONAL .
    METHODS get_alv_grid
      IMPORTING
        !i_objid        TYPE bu_object_id OPTIONAL
      RETURNING
        VALUE(alv_grid) TYPE REF TO cl_gui_alv_grid
      RAISING
        zcx_alv_error .
    METHODS get_split_container
      RETURNING
        VALUE(split_container) TYPE REF TO cl_gui_splitter_container .
    METHODS handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        !e_object .
    METHODS handle_user_command FINAL
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        !e_ucomm .
    METHODS handle_double_click FINAL
        FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        !e_row
        !e_column
        !es_row_no .
    METHODS menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING
        !e_object
        !e_ucomm .
    METHODS handle_hotspot_click FINAL
        FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        !e_row_id
        !e_column_id
        !es_row_no .
    METHODS data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed .
    METHODS data_changed_finished
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !e_modified
        !et_good_cells .
    METHODS on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING
        !e_fieldname
        !e_fieldvalue
        !es_row_no
        !er_event_data
        !et_bad_cells
        !e_display .
    METHODS on_f1
        FOR EVENT onf1 OF cl_gui_alv_grid
      IMPORTING
        !e_fieldname
        !es_row_no
        !er_event_data .
    METHODS user_command
      IMPORTING
        !e_ucomm TYPE sy-ucomm .
    METHODS toolbar
      IMPORTING
        !e_object TYPE REF TO cl_alv_event_toolbar_set .
    METHODS hotspot_click
      IMPORTING
        !e_row_id    TYPE lvc_s_row
        !e_column_id TYPE lvc_s_col
        !es_row_no   TYPE lvc_s_roid
        !e_data      TYPE REF TO data .
    METHODS double_click
      IMPORTING
        !e_row     TYPE lvc_s_row OPTIONAL
        !e_column  TYPE lvc_s_col OPTIONAL
        !es_row_no TYPE lvc_s_roid OPTIONAL
        !e_data    TYPE REF TO data .
    METHODS set_fieldcat FINAL
      IMPORTING
        !i_objid       TYPE bu_object_id
        !table         TYPE table
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
    METHODS change_fieldcat
      IMPORTING
        !i_objid TYPE bu_object_id
      CHANGING
        !ct_fcat TYPE lvc_t_fcat .
    METHODS set_layout
      IMPORTING
        !i_objid         TYPE bu_object_id
      RETURNING
        VALUE(rs_layout) TYPE lvc_s_layo .
    CLASS-METHODS check_if_called_from_subclass
      RETURNING
        VALUE(ro_subclass) TYPE REF TO object .
    METHODS refresh
      IMPORTING
        !alv_grid TYPE REF TO cl_gui_alv_grid .
  PROTECTED SECTION.

    DATA mainprogram TYPE sy-repid.
    DATA subclass_type TYPE REF TO cl_abap_typedescr .
    DATA objid TYPE bu_object_id .

    DATA error_in_data TYPE char1 .
    DATA alv_multi TYPE REF TO zcl_alv_multi .

    TYPES:BEGIN OF ts_filter_field,
            name TYPE fieldname,
          END OF ts_filter_field.
    TYPES tt_filter_field TYPE  TABLE OF ts_filter_field.

    METHODS check_data
      IMPORTING
        !ps_mat          TYPE lvc_s_modi
        !pr_data_changed TYPE REF TO cl_alv_changed_data_protocol .

    METHODS set_filter IMPORTING i_fields      TYPE tt_filter_field
                                 i_data        TYPE REF TO data
                       RETURNING VALUE(filter) TYPE lvc_t_filt.

  PRIVATE SECTION.

    METHODS default_toolbar
      IMPORTING
        !e_object TYPE REF TO cl_alv_event_toolbar_set .

    METHODS default_user_command
      IMPORTING
        !e_ucomm TYPE sy-ucomm .

    METHODS default_hotspot_click
      IMPORTING
        !sdata       TYPE REF TO data
        !e_column_id TYPE lvc_s_col .

    METHODS get_fieldcat_from_table
      IMPORTING
        !table         TYPE table
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .

    METHODS get_single_data
      IMPORTING
                !tabix        TYPE sy-tabix
      RETURNING VALUE(r_data) TYPE REF TO data.

ENDCLASS.



CLASS ZCL_ALV_EVENT_RECEIVER IMPLEMENTATION.


  METHOD change_fieldcat.

  ENDMETHOD.


  METHOD check_data.

*    read table gt_head  into gs_head index ps_mat-row_id.
*
*    DATA ls_head LIKE gs_head.
*
*    FIELD-SYMBOLS <fs> TYPE any.
*
*    ASSIGN COMPONENT  ps_mat-fieldname OF STRUCTURE ls_head TO <fs>.
*
*    CALL METHOD pr_data_changed->get_cell_value
*      EXPORTING
*        i_row_id    = ps_mat-row_id
*        i_fieldname = ps_mat-fieldname
*      IMPORTING
*        e_value     = <fs>.
*
*    DATA lv_error TYPE c.
*
*    CASE ps_mat-fieldname.
*
*      WHEN 'ZHSL'.
*
*
*        IF <fs> > gs_head-labst.
*
*          lv_error = 'X'.
*          RAISE EVENT zif_msg~display_msg EXPORTING iv_type = 'W' iv_msg = '转换数量不允许大于库存数量'.
*        ELSE.
*
*        ENDIF.
*
*    ENDCASE.
*
*    IF lv_error = 'X'.
*
*      CALL METHOD pr_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ps_mat-row_id
*          i_fieldname = ps_mat-fieldname
*          i_value     = <fs>.
*
*    ELSE.
*
*      MODIFY gt_head FROM gs_head INDEX ps_mat-row_id.
*
*    ENDIF.
*
*    CLEAR gs_head.
  ENDMETHOD.


  METHOD check_if_called_from_subclass.

    DATA: callstack TYPE abap_callstack,
          src       TYPE TABLE OF string.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    ASSIGN callstack[ 8 ] TO FIELD-SYMBOL(<stack>).
    CHECK sy-subrc EQ 0.

    DATA(compiler) = cl_abap_compiler=>create(
      p_name             = <stack>-mainprogram
      p_include          = <stack>-include
      p_no_package_check = abap_true ).

    compiler->get_all( IMPORTING p_result = DATA(result_all) ).

    DATA(lv_full_name) = '\PR:' && <stack>-mainprogram && '\TY:CL_ALV_EVENT'.

    compiler->get_single_ref(
      EXPORTING
        p_full_name = lv_full_name
      IMPORTING
        p_result    = DATA(result) " Where-Used List
      EXCEPTIONS
        OTHERS      = 5 ).
    IF sy-subrc EQ 0 AND result IS NOT INITIAL.

      DATA(ls_result) = VALUE #( result[ 1 ]  OPTIONAL ).

      DATA(subclass_name) = |\\PROGRAM={ <stack>-mainprogram }\\CLASS={ ls_result-name }|.
      cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = to_upper( subclass_name )
                                            RECEIVING  p_descr_ref    = ro_subclass
                                            EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    objid = i_objid.
    alv_multi = i_alvmulti.
    mainprogram = i_mainprogram.

  ENDMETHOD.


  METHOD create.

    IF i_subclass IS INITIAL.
      i_subclass ?= check_if_called_from_subclass( ).
    ENDIF.

    IF i_subclass IS NOT INITIAL.
      DATA subclass TYPE REF TO object.
      DATA(sublcass_abs_name) = i_subclass->absolute_name.
      CREATE OBJECT subclass TYPE (sublcass_abs_name)
        EXPORTING i_objid = i_objid
                  i_alvmulti = i_alvmulti
                  i_mainprogram = i_mainprogram.

      ro_event ?= subclass.
      ro_event->subclass_type = i_subclass.

    ENDIF.

  ENDMETHOD.


  METHOD data_changed.

    DATA: ls_good TYPE lvc_s_modi.

    error_in_data = space.

    LOOP AT er_data_changed->mt_mod_cells INTO ls_good.

      CALL METHOD check_data
        EXPORTING
          ps_mat          = ls_good
          pr_data_changed = er_data_changed.

    ENDLOOP.

    refresh( get_alv_grid( ) ).
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.


  METHOD data_changed_finished.
  ENDMETHOD.


  METHOD default_hotspot_click.


  ENDMETHOD.


  METHOD default_toolbar.

    DELETE e_object->mt_toolbar WHERE quickinfo = '插入行'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '删除行'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '剪切'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '复制文本'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '插入总览'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '附加行'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '复制行'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '视图'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '显示图形'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '撤销'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '明细'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '打印'.
    DELETE e_object->mt_toolbar WHERE quickinfo = '最终用户文档'.

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
        "HGY 20200520 增加按钮的折叠
        IF alv_multi->get_evas( )-mode NE '11'.
          mar_toolbar 'EXPAND' '' '展开' icon_expand.
          mar_toolbar 'COLLPASE'  '' '折叠' icon_collapse.
        ENDIF.
        IF alv_multi->get_evas( )-tree = abap_true.
          mar_toolbar 'TREE' '' '层次结构展示' icon_tree.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD default_user_command.

    CASE e_ucomm.
      WHEN 'EXPAND'.
        IF alv_multi->get_evas( )-horizontal = abap_true.
          get_split_container( )->set_column_width( EXPORTING id = 1 width = 600 ).
        ELSE.
          get_split_container( )->set_row_height( EXPORTING id = 1 height = 30 ).
        ENDIF.
      WHEN 'COLLPASE'.
        IF alv_multi->get_evas( )-horizontal = abap_true.
          get_split_container( )->set_column_width( EXPORTING id = 1 width = 2050 ).
        ELSE.
          get_split_container( )->set_row_height( EXPORTING id = 1 height = 100 ).
        ENDIF.
      WHEN 'TREE'.
        alv_multi->get_containers( )-tree_container->set_column_width( EXPORTING id = 1 width = 15 ).
    ENDCASE.

  ENDMETHOD.


  METHOD double_click.

*    CASE objid.
*      WHEN '1'."双击
*    ASSIGN e_data->* TO FIELD-SYMBOL(<ls_data>).
*
*    get_alv_grid( '2' )->set_filter_criteria(
*      set_filter( EXPORTING i_fields = VALUE #( ( name = 'PERNR' )
*                                                ( name = 'SAKNR' ) )
*                            i_data = e_data ) ).
*
*    refresh( get_alv_grid( '2' ) ).

*      WHEN '2'.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD get_alv_grid.
    DATA lv_objid TYPE bu_object_id.
    IF i_objid IS INITIAL.
      lv_objid = objid.
    ELSE.
      lv_objid = i_objid.
    ENDIF.
    alv_grid = alv_multi->get_single_mapping( lv_objid )-alv_grid.

    IF alv_grid IS NOT BOUND.
      zcx_alv_error=>raise_text( |未查询到标识{ lv_objid }的alv| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_fieldcat_from_table.

    DATA: lcl_table TYPE REF TO data.
    CREATE DATA lcl_table LIKE table.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN lcl_table->* TO <table>.

    DATA salv_table TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = salv_table
                                CHANGING  t_table      = <table> ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = salv_table->get_columns( ) " ALV Filter
            r_aggregations = salv_table->get_aggregations( ) )." ALV Aggregations
      CATCH cx_root.
    ENDTRY.

    DELETE rt_fcat WHERE fieldname = 'MANDT'.

  ENDMETHOD.


  METHOD get_single_data.

    FIELD-SYMBOLS <lt_data> TYPE  table.

    "获取当前程序的内表
    DATA(tabname) = |({ mainprogram }){ alv_multi->get_single_mapping( objid )-tabname }|.
    ASSIGN (tabname) TO <lt_data>.
    r_data = REF #( <lt_data>[ tabix ] OPTIONAL ).

  ENDMETHOD.


  METHOD get_split_container.

    split_container = alv_multi->get_containers( )-split_container.

  ENDMETHOD.


  METHOD handle_double_click.

    "设置上屏幕高度变为30
    get_split_container( )->set_row_height( EXPORTING id = 1 height = 30 ).

    double_click( EXPORTING e_row     = e_row
                            e_column  = e_column
                            es_row_no = es_row_no
                            e_data    = get_single_data( es_row_no-row_id ) ).

  ENDMETHOD.


  METHOD handle_hotspot_click.

    hotspot_click( EXPORTING e_row_id    = e_row_id
                             e_column_id = e_column_id
                             es_row_no   = es_row_no
                             e_data      = get_single_data( es_row_no-row_id ) ).

  ENDMETHOD.


  METHOD handle_toolbar.

    "默认按钮设置
    default_toolbar( e_object = e_object ).
    "子类方式个性化按钮
    toolbar( e_object = e_object ).

  ENDMETHOD.


  METHOD handle_user_command.

    "设置默认按钮功能
    default_user_command( e_ucomm = e_ucomm ).
    "子类个性化按钮定制
    user_command( e_ucomm = e_ucomm ).

  ENDMETHOD.


  METHOD hotspot_click.

*    "例子，调用va03查看销售订单
*    ASSIGN e_data->* TO FIELD-SYMBOL(<ls_data>).
*    ASSIGN COMPONENT e_column_id-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<value>).
*    CASE e_column_id-fieldname.
*      WHEN 'VBELN'.
*        SET PARAMETER ID 'AUN' FIELD <value>.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*    ENDCASE.

  ENDMETHOD.


  METHOD menu_button.
  ENDMETHOD.


  METHOD on_f1.
  ENDMETHOD.


  METHOD on_f4.

  ENDMETHOD.


  METHOD refresh.

    "获取当前alv的layout
    DATA ls_layout TYPE lvc_s_layo.
    alv_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).

    "SAPBUG,获取到的layout的CWIDTH_OPT 参数，赋值X，但是获取到的是1
    ls_layout-cwidth_opt = abap_true.

    "给alv类重置layout
    alv_grid->set_frontend_layout( ls_layout ).
    "刷新
    alv_grid->refresh_table_display(
      is_stable = VALUE #( row = abap_true col = abap_true ) ).

  ENDMETHOD.


  METHOD set_fieldcat.

    "根据内表获取alv的字段结构
    rt_fcat = get_fieldcat_from_table( table = table ).
    "针对alv的字段处理
    change_fieldcat( EXPORTING i_objid = i_objid
                     CHANGING  ct_fcat = rt_fcat ).

  ENDMETHOD.


  METHOD set_filter.

    ASSIGN i_data->* TO FIELD-SYMBOL(<data>).

    LOOP AT i_fields INTO DATA(ls_fields).
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE <data> TO FIELD-SYMBOL(<value>).
      IF sy-subrc = 0.
        APPEND VALUE #( fieldname = ls_fields-name sign = 'I' option = 'EQ'  low = <value> )
        TO filter.
      ENDIF.
      CLEAR ls_fields.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_layout.
    rs_layout-cwidth_opt = abap_true.
    rs_layout-zebra = abap_true.
    rs_layout-sel_mode = 'D'.
  ENDMETHOD.


  METHOD toolbar.


  ENDMETHOD.


  METHOD user_command.

  ENDMETHOD.
ENDCLASS.
