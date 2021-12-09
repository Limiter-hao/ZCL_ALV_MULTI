"! alv事件类，能够实现多个alv的实现
"! Author  :郝光源
"! Req Date:20211207
class ZCL_ALV_MULTI definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_mapping,
        objid    TYPE bu_object_id,
        tabname  TYPE tabnam,
        alv_grid TYPE REF TO cl_gui_alv_grid,
      END OF ts_mapping .
  types:
    tt_mapping TYPE TABLE OF ts_mapping .
  types:
    BEGIN OF  ts_containers,

        container       TYPE REF TO cl_gui_docking_container,
        tree_container  TYPE REF TO cl_gui_splitter_container,
        split_container TYPE REF TO cl_gui_splitter_container,
      END OF ts_containers .
  types:
    BEGIN OF ts_evas,
        title      TYPE ttext,
        mode       TYPE char2,
        tree       TYPE abap_bool,
        horizontal TYPE abap_bool,
        numbers    TYPE i,
      END OF ts_evas .

  methods GET_SINGLE_MAPPING
    importing
      !I_OBJID type BU_OBJECT_ID
    returning
      value(RS_MAPPING) type TS_MAPPING .
  methods GET_CONTAINERS
    returning
      value(RS_CONTAINERS) type TS_CONTAINERS .
  methods CONSTRUCTOR
    importing
      !IV_MODE type CHAR2 optional
      !IV_TREE type ABAP_BOOL optional
      !IT_MAPPING type TT_MAPPING
      !IV_TITLE type TTEXT optional
    raising
      ZCX_ALV_ERROR .
  methods CALL_SCREEN .
  methods INIT_ALV .
  methods GET_EVAS
    returning
      value(RS_EVAS) type TS_EVAS .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mapping TYPE tt_mapping .
    DATA mainprogram TYPE repid .
    DATA numbers TYPE i .
    DATA containers TYPE ts_containers .
    DATA evas TYPE ts_evas .

    METHODS set_alv_grid
      IMPORTING
        !i_container TYPE REF TO cl_gui_container
        !i_objid     TYPE bu_object_id
      CHANGING
        !alv_grid    TYPE REF TO cl_gui_alv_grid
        !table       TYPE table .

    METHODS split_container .
    METHODS refresh
      IMPORTING
        !i_objid  TYPE bu_object_id
        !alv_grid TYPE REF TO cl_gui_alv_grid .
ENDCLASS.



CLASS ZCL_ALV_MULTI IMPLEMENTATION.


  METHOD call_screen.

    CALL FUNCTION 'ZALV_CALL_SCREEN'
      EXPORTING
        cl_multi = me.

  ENDMETHOD.


  METHOD constructor.

    IF it_mapping IS INITIAL.
      zcx_alv_error=>raise_text( '至少传入一个alv内表!' ).
    ENDIF.

    evas-mode = iv_mode.

    IF evas-mode IS INITIAL.
      evas-mode = '11'.
    ENDIF.

    evas-tree = iv_tree.
    mapping = it_mapping.

    IF iv_title IS NOT INITIAL.
      evas-title  = iv_title.
    ELSE.
      SELECT SINGLE ttext
      FROM tstct INTO @evas-title
     WHERE tcode = @sy-tcode.
    ENDIF.
  ENDMETHOD.


  METHOD get_containers.
    rs_containers = containers.
  ENDMETHOD.


  METHOD get_evas.

    rs_evas = evas.

  ENDMETHOD.


  METHOD get_single_mapping.

    rs_mapping = VALUE #( mapping[ objid = i_objid ] OPTIONAL ).

  ENDMETHOD.


  METHOD init_alv.

    DATA: callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    ASSIGN callstack[ 5 ] TO FIELD-SYMBOL(<stack>).
    IF sy-subrc = 0.
      mainprogram = <stack>-mainprogram.
    ENDIF.

    split_container( ).
*------------------循环构造alv--------------------------------------------
    DATA lv_objid TYPE bu_object_id.
    DO numbers TIMES.
      "alv 标识
      lv_objid = sy-index.
      CONDENSE lv_objid NO-GAPS.
      "构造alv
      IF evas-horizontal = abap_true."横向多个alv
        DATA(lcl_split) = containers-split_container->get_container( row = 1 column = sy-index ).
      ELSE.
        lcl_split = containers-split_container->get_container( row = sy-index column = 1 ).
      ENDIF.
      "动态化alv类
      IF numbers > 1.
        DATA(lv_tbnam) = 'GT_DATA' && sy-index.
      ELSE.
        lv_tbnam = 'GT_DATA'.
      ENDIF.
      "动态化内表
      "先从配置表中判断是否有特殊对应的内表
      READ TABLE mapping ASSIGNING FIELD-SYMBOL(<ls_mapping>) WITH KEY objid = lv_objid.
      IF sy-subrc = 0.
        lv_tbnam = <ls_mapping>-tabname.
      ELSE.
        APPEND INITIAL LINE TO mapping ASSIGNING <ls_mapping>.
        <ls_mapping>-objid = lv_objid.
        <ls_mapping>-tabname = lv_tbnam.
      ENDIF.

      lv_tbnam = |({ mainprogram }){ lv_tbnam }|.

      ASSIGN (lv_tbnam) TO FIELD-SYMBOL(<lt_data>).
      CLEAR:lv_tbnam.

      set_alv_grid( EXPORTING i_container = lcl_split
                              i_objid     = lv_objid
                    CHANGING  alv_grid    = <ls_mapping>-alv_grid
                              table       = <lt_data> ).


    ENDDO.
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


  METHOD set_alv_grid.

    DATA:lt_fcat    TYPE lvc_t_fcat,
         ls_layout  TYPE lvc_s_layo,
         ls_variant TYPE disvariant.

    IF   alv_grid IS NOT BOUND.

      alv_grid = NEW #( i_parent = i_container ).

      "实例化事件处理类
      DATA evf_cls TYPE REF TO zcl_alv_event_receiver.
      evf_cls ?= zcl_alv_event_receiver=>create( i_objid       = i_objid
                                                 i_alvmulti    = me
                                                 i_mainprogram = mainprogram ).

      IF evf_cls IS BOUND.
        lt_fcat = evf_cls->set_fieldcat( i_objid = i_objid
                                         table   = table ).
        ls_layout = evf_cls->set_layout( i_objid ).
      ENDIF.

      ls_variant-report = mainprogram.
      ls_variant-handle = i_objid.

      CALL METHOD alv_grid->set_table_for_first_display
        EXPORTING
          is_layout       = ls_layout
          i_save          = 'A'
          is_variant      = ls_variant
        CHANGING
          it_outtab       = table
          it_fieldcatalog = lt_fcat.

      IF evf_cls IS BOUND.

        "数据修改中事件
        SET HANDLER evf_cls->data_changed       FOR alv_grid.
        "数据修改后事件
        SET HANDLER evf_cls->data_changed_finished FOR alv_grid.
        "光标移动事件
        CALL METHOD alv_grid->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.
        "回车事件
        CALL METHOD alv_grid->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter.
        SET HANDLER evf_cls->handle_user_command FOR alv_grid."用户命令
        SET HANDLER evf_cls->handle_toolbar      FOR alv_grid."状态栏
        SET HANDLER evf_cls->menu_button         FOR alv_grid."状态栏下拉
        CALL METHOD alv_grid->set_toolbar_interactive."激活状态栏
        SET HANDLER evf_cls->handle_hotspot_click FOR alv_grid."热点事件
        SET HANDLER evf_cls->handle_double_click  FOR alv_grid."双击

      ENDIF.

    ELSE.
      refresh( i_objid  = i_objid
               alv_grid = alv_grid ).
    ENDIF.

  ENDMETHOD.


  METHOD split_container.

    IF containers-container IS NOT BOUND.

      "将容器与屏幕做关联
      containers-container = NEW cl_gui_docking_container(
        repid     = 'SAPLZALV_MULTI'
        dynnr     = '0100'
        extension = 2050 ).

    ELSE.
      RETURN.
    ENDIF.

    IF evas-tree = abap_true."先将容器划分为左右两个

      IF containers-tree_container IS NOT BOUND.
        containers-tree_container = NEW cl_gui_splitter_container(
          parent  = containers-container
          rows    = 1
          columns = 2 ).

        "设置边框
        containers-tree_container->set_border( cl_gui_cfw=>false ).
        "设置为相对模式
        containers-tree_container->set_column_mode( containers-tree_container->mode_relative ).
        "将分割的左面屏幕进行隐藏
        containers-tree_container->set_column_width( EXPORTING id = 1 width = 0 ).

      ENDIF.

    ENDIF.

*----------------------将容器进行动态分割----------------------------------
    DATA: lv_rows    TYPE i,
          lv_columns TYPE i.

    lv_rows = evas-mode(1).
    lv_columns = evas-mode+1(1).

    IF containers-split_container IS NOT BOUND.

      "如果要显示tree,在那么alv在tree的右面容器再进行分割处理
      IF evas-tree = abap_true.

        containers-split_container = NEW cl_gui_splitter_container(
          parent  = containers-tree_container->get_container( row = 1 column = 2 )
          rows    = lv_rows
          columns = lv_columns ).

      ELSE.

        containers-split_container = NEW cl_gui_splitter_container(
          parent  = containers-container
          rows    = lv_rows
          columns = lv_columns ).

      ENDIF.

      "设置边框
      containers-split_container->set_border( cl_gui_cfw=>true ).
      "设置为相对模式
      containers-split_container->set_column_mode( containers-split_container->mode_absolute ).

    ENDIF.

    " 控制横屏跟竖屏的时候，上面跟左面的alv占满全屏,并输出分割的数量

    IF lv_rows > 1."纵向多个alv

      containers-split_container->set_row_height( EXPORTING id = 1 height = 100 ).
      numbers = lv_rows.

    ELSEIF lv_columns > 1."横向多个alv

      containers-split_container->set_column_width( EXPORTING id = 1 width = 2050 ).
      numbers = lv_columns.
      evas-horizontal = abap_true.

    ELSE."一个alv

      containers-split_container->set_row_height( EXPORTING id = 1 height = 100 ).
      numbers = lv_rows.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
