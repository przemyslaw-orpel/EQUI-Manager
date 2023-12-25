*&---------------------------------------------------------------------*
*& Report  ZPM_ME
*&
*&---------------------------------------------------------------------*
*& Equipment management gui
*& Author: Przemyslaw Orpel
*&---------------------------------------------------------------------*
report zpm_me.

data: s100_ok   type syst_ucomm. "screen 100 element

class lc_gui_screen definition.
  public section.
    class-data:
    screen type ref to lc_gui_screen. "Singleton
    class-methods:
      create_screen.
    methods:
      constructor.
  private section.
    methods:
      create_splitter,
      create_tplnr_alv_tree,
      select_tplnr_data,
      init_alv_tree,
      create_nodes,
      hide_column_tree,
      set_tree_col_settings,
      set_tree_handler,
      create_equi_alv,
      create_alv,
      set_alv_handler,
      set_color_col,
      refresh_equi_alv,
      select_equi_data,
      add_buttons,
      show_bom importing iv_equnr type equnr,
      on_tree_click  for event double_click of cl_salv_events_tree importing node_key,
      on_equnr_click for event link_click of cl_salv_events_table importing row sender,
      on_user_command for event added_function of  cl_salv_events importing e_salv_function.

    types: begin of ty_tree_tab,
             node_key    type i,  "tplnr key
             node_parent type i,  "tplma key
             tplnr       type tplnr,
             pltxt       type pltxt,
           end of ty_tree_tab,
           begin of ty_equi_view,
             equnr type equnr,
             eqktx type ktx01,
             tplnr type tplnr,
             tplma type tplma,
             ernam type ernam,
             erdat type erdat,
           end of ty_equi_view.

    data:
      gt_equi          type table of ty_equi_view,
      gt_tree_init     type table of ty_tree_tab,
      gt_tree          type table of ty_tree_tab,
      gr_alv           type ref to cl_salv_table,
      gr_left_cont     type ref to cl_gui_container,
      gr_right_cont    type ref to cl_gui_container,
      gr_splitter      type ref to cl_gui_splitter_container,
      gr_alv_tree      type ref to cl_salv_tree,
      gv_sel_tplnr     type string,
      gv_expand_icon   type salv_de_tree_image,
      gv_collapse_icon type salv_de_tree_image,
      gv_hier_icon     type salv_de_tree_image.

endclass.

class lc_gui_screen implementation.
  method constructor.
    me->create_splitter( ).
    me->create_tplnr_alv_tree( ).
    me->create_equi_alv( ).

  endmethod.

  method create_screen.
    if screen is initial.
      screen = new #( ).
    endif.
  endmethod.

  method create_splitter.
    " Create splitter instance
    gr_splitter = new #( parent = cl_gui_container=>default_screen
                         rows = 1
                         columns = 2 ).
    " Set left side container
    gr_left_cont = gr_splitter->get_container( row = 1
                                               column = 1 ).
    "Set right side container
    gr_right_cont = gr_splitter->get_container( row = 1
                                                column = 2 ).
  endmethod.

  method create_tplnr_alv_tree.
    me->select_tplnr_data( ).
    me->init_alv_tree( ).
    me->create_nodes( ).
    me->hide_column_tree( ).
    me->set_tree_col_settings( ).
    me->set_tree_handler( ).

    " Set toolbar
    gr_alv_tree->get_functions( )->set_all( ).
    " Show alv tree
    gr_alv_tree->display( ).
  endmethod.

  method create_equi_alv.
    me->create_alv( ).
    me->set_color_col( ).
    me->set_alv_handler( ).
    me->add_buttons( ).

    " Set toolbar
    gr_alv->get_functions( )->set_all( ).
    " Show alv
    gr_alv->display( ).
  endmethod.

  method select_tplnr_data.
    types: begin of ty_tplnr_key,
             tplma    type tplma,
             node_key type i,
           end of ty_tplnr_key.

    data: lt_tplnr_key type table of  ty_tplnr_key,
          lt_iflot     type table of iflot,
          lv_node_key  type i,
          lv_parent    type i.

    " Select data from DB
    select distinct * from iflot
      into table lt_iflot order by tplma tplnr.

    "Set node key to tplnr
    loop at lt_iflot into data(lv_tplma).
      append value ty_tplnr_key( tplma = lv_tplma-tplnr
                                 node_key = sy-tabix ) to lt_tplnr_key.
    endloop.

    "Set node_key and parent for tplnr
    loop at lt_iflot into data(lv_iflot).
      lv_node_key = 0.
      lv_parent = 0.

      "Find node key (tplnr)
      read table lt_tplnr_key into data(wa_tplnr_key)
         with table key tplma = lv_iflot-tplnr.
      if sy-subrc eq 0.
        lv_parent = wa_tplnr_key-node_key.
      endif.

      "Find parent key (tplma)
      read table lt_tplnr_key into wa_tplnr_key
         with table key tplma = lv_iflot-tplma.
      if sy-subrc eq 0.
        lv_node_key = wa_tplnr_key-node_key.
      endif.

      " Add row to tree table
      append value ty_tree_tab(
          node_key    =  lv_parent
          node_parent = lv_node_key
          tplnr       = lv_iflot-tplnr ) to gt_tree.
    endloop.

    " Fill short texts (pltxt)
    loop at gt_tree  assigning field-symbol(<fs_tree_tab>).
      select single pltxt from iflotx
        into <fs_tree_tab>-pltxt
        where tplnr = <fs_tree_tab>-tplnr.
    endloop.

    " Important! Sort tree tabel by node_key and parent
    sort gt_tree by node_key node_parent.
  endmethod.

  method init_alv_tree.
    me->create_alv( ).
    " Create ALV Tree instance
    try.
        cl_salv_tree=>factory(
        exporting
          r_container = gr_left_cont
          importing
            r_salv_tree = gr_alv_tree
            changing
              t_table = gt_tree_init  " must be empty
              ).
      catch cx_salv_error into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method create_nodes.
    field-symbols: <fs_line> type ty_tree_tab.
    data: lr_node type ref to cl_salv_node.
    " Set icon
    gv_expand_icon = icon_expand_all.
    gv_collapse_icon = icon_collapse_all.
    gv_hier_icon = icon_tree.

    " Get nodes from alv tree
    data(lr_nodes) = gr_alv_tree->get_nodes( ).

    " Filling the tree
    try.
        loop at gt_tree assigning <fs_line>.
          if <fs_line>-node_parent eq 0.
            " Add first nodes
            lr_node = lr_nodes->add_node(
                                  related_node   = ''
                                  relationship   = cl_gui_column_tree=>relat_last_child
                                  collapsed_icon = gv_expand_icon
                                  expanded_icon  = gv_collapse_icon
                                  data_row       = <fs_line>
                                  row_style      = if_salv_c_tree_style=>emphasized_a
                                  text           = | { <fs_line>-tplnr }| ).
          else.
            lr_node = lr_nodes->add_node(
                                   related_node   = conv #( <fs_line>-node_parent )
                                   relationship   = cl_gui_column_tree=>relat_last_child
                                   collapsed_icon = gv_expand_icon
                                   expanded_icon  = gv_collapse_icon
                                   data_row       = <fs_line>
                                   row_style      = if_salv_c_tree_style=>emphasized_a
                                   text           = | { <fs_line>-tplnr }| ).
          endif.
        endloop.
      catch cx_salv_msg into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method hide_column_tree.
    try.
        data(lo_tree_col) = gr_alv_tree->get_columns( ).
        lo_tree_col->get_column( 'NODE_KEY' )->set_visible( abap_false ).
        lo_tree_col->get_column( 'NODE_PARENT' )->set_visible( abap_false ).
        lo_tree_col->get_column( 'TPLNR' )->set_visible( abap_false ).
      catch cx_salv_not_found into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method set_tree_col_settings.
    data(lr_setting) = gr_alv_tree->get_tree_settings( ).
    lr_setting->set_hierarchy_header( 'TPLNR TREE' ).
    lr_setting->set_hierarchy_size( 40 ).
    lr_setting->set_hierarchy_icon( gv_hier_icon ).
  endmethod.

  method set_tree_handler.
    "Set double click handler
    data(lo_event) = gr_alv_tree->get_event( ).
    set handler me->on_tree_click for lo_event.
  endmethod.

  method on_tree_click.
    " Rename parametr node key
    data(lv_key) = node_key.
    " Read tplnr from node
    read table gt_tree with key node_key = lv_key into data(lv_tree_row).
    " Serch tplnr
    gv_sel_tplnr =  |{ lv_tree_row-tplnr }{ '%' } |.
    " Set alv equi header
    gr_alv->get_display_settings( )->set_list_header( | { lv_tree_row-tplnr } - { lv_tree_row-pltxt } | ).

    me->refresh_equi_alv( ).
  endmethod.

  method refresh_equi_alv.
    clear gt_equi.
    " Select alv data
    me->select_equi_data( ).
    " Refresh alv
    gr_alv->refresh( ).
    " Optimize column width
    gr_alv->get_columns( )->set_optimize( ).
  endmethod.

  method select_equi_data.
    " Fill equipment table
    select * from equi
      join equz on equz~equnr = equi~equnr
      join iloa on iloa~iloan = equz~iloan
      join iflot on iloa~tplnr = iflot~tplnr
      join eqkt on equi~equnr = eqkt~equnr
      where iloa~tplnr like @gv_sel_tplnr
      order by equi~equnr
      into corresponding fields of table @gt_equi.
  endmethod.

  method create_alv.
    " Create ALV instance
    try.
        cl_salv_table=>factory(
          exporting
         r_container = gr_right_cont
         importing
           r_salv_table = gr_alv
           changing
             t_table = gt_equi
        ).
      catch cx_salv_error into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method set_color_col.
    try.
        data(lr_equnr_col) = cast cl_salv_column_table(
        gr_alv->get_columns( )->get_column( 'EQUNR' ) ).

        lr_equnr_col->set_key( abap_false ).
        lr_equnr_col->set_color( value #( col = col_total ) ). "Like yellow"

      catch cx_salv_not_found into data(lx_column_error).
        message lx_column_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method set_alv_handler.
    "Set column hotspot
    try.
        data(lr_equnr_col) = cast cl_salv_column_table(
         gr_alv->get_columns( )->get_column( 'EQUNR' ) ).

        lr_equnr_col->set_cell_type( if_salv_c_cell_type=>hotspot ).

      catch cx_salv_not_found into data(lx_column_error).
        message lx_column_error->get_text( ) type 'E'.
    endtry.

    "Register handler
    data(lr_event) = gr_alv->get_event( ).
    set handler me->on_equnr_click for lr_event.
    set handler me->on_user_command for lr_event.
  endmethod.

  method on_equnr_click.
    "Read equnr
    read table gt_equi into data(lv_row_equi) index row.

    " Open IE03 transaction with EQUNR parametr
    set parameter id: 'EQN' field lv_row_equi-equnr.
    call transaction 'IE03' and skip first screen.

    me->refresh_equi_alv( ).
  endmethod.

  method add_buttons.
    try.
        " Add printer buton
        gr_alv->get_functions( )->add_function(
               name = 'PRINTEQUI'
               icon =  conv string( icon_print )
               text = 'Print equi'
               tooltip = 'Print equipment'
               position = if_salv_c_function_position=>right_of_salv_functions ).
        " Add bom buton
        gr_alv->get_functions( )->add_function(
             name = 'SHOWBOM'
             icon =  conv string( icon_bom )
             text = 'BOM'
             tooltip = 'Equipment BOMs'
             position = if_salv_c_function_position=>right_of_salv_functions ).
      catch cx_salv_existing cx_salv_wrong_call into data(lx_alv_error).
        message lx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method show_bom.
    constants: lc_spras type spras value 'L', "Polish
               lc_stlty type stlty value 'E'. "Equipment material specification
    types: begin of ty_bom_view,
             posnr type sposn,
             postp type postp,
             idnrk type idnrk,
             maktx type maktx,
             datuv type datuv,
             meins type kmpme,
             menge type kmpmg,
           end of ty_bom_view.
    data: lt_bom type standard table of ty_bom_view,
          lo_alv type ref to cl_salv_table.

    " Select BOM data
    select * from eqst
    join stpo on stpo~stlnr = eqst~stlnr
    join makt on makt~matnr = stpo~idnrk
      where eqst~equnr = @iv_equnr
      and makt~spras = @lc_spras
      and stpo~stlty = @lc_stlty
      into corresponding fields of table @lt_bom.

    "  Create ALV instance
    try.
        cl_salv_table=>factory(
          importing
             r_salv_table = lo_alv
           changing
             t_table = lt_bom  ).
      catch cx_salv_msg into data(lx_error).
        message lx_error->get_text( ) type 'E'.
    endtry.

    " Set ALV Popup
    lo_alv->set_screen_popup( start_column = 30 start_line = 1 end_column = 130 end_line = 30 ).
    " Display popup
    lo_alv->display( ).
  endmethod.

  method on_user_command.
    data(lv_sel_row) = gr_alv->get_selections( )->get_selected_rows( ).
    " Check user select equi row
    if lv_sel_row is initial.
      message 'Please selecet equimpnet row' type 'I'.
    else.
      " Read selected row index
      read table lv_sel_row into data(lv_row) index 1.
      " Read equi line
      read table gt_equi into data(ls_equi) index lv_row.

      case e_salv_function.
        when 'PRINTEQUI'.
          " In my case smart form impor equnr and print details about equimpent
          call function '' "Set smart forms function
            exporting
              lv_equnr         = ls_equi-equnr
            exceptions
              formatting_error = 1
              internal_error   = 2
              send_error       = 3
              user_canceled    = 4
              others           = 5.
          if sy-subrc <> 0.
            message 'Printer error' type 'E'.
          endif.

        when 'SHOWBOM'.
          me->show_bom( iv_equnr = ls_equi-equnr ).
      endcase.
    endif.
  endmethod.
endclass.


start-of-selection.
  call screen 100.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       PBO
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS_100'.
  set titlebar 'TITLE_100'. " Equipment management
  lc_gui_screen=>create_screen( ).
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*      PAI
*----------------------------------------------------------------------*
module user_command_0100 input.
  case s100_ok.
    when 'EXIT'.
      leave program.
  endcase.
endmodule.