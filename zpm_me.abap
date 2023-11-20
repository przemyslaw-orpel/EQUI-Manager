*&---------------------------------------------------------------------*
*& Report  ZPM_ME
*&
*&---------------------------------------------------------------------*
*& Equipment management gui
*& Author: Przemyslaw Orpel
*&---------------------------------------------------------------------*
report zpm_me.

data: s100_ok   type syst_ucomm.

class lc_gui_screen definition.
  public section.
    class-data:
    screen type ref to lc_gui_screen.
    class-methods:
      create_screen.
    methods:
      constructor,
      create_tplnr_alv_tree,
      select_tplnr_data,
      init_alv_tree,
      create_nodes,
      hide_column_tree,
      set_column_settings,
      create_equi_alv,
      on_link_click  for event double_click of cl_salv_events_tree importing node_key.
  private section.

    types: begin of ty_tree_tab,
             node_key    type i,  "tplnr
             node_parent type i,  "tplma
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
      gt_equi          type standard table of ty_equi_view,
      gr_alv           type ref to cl_salv_table,
      gr_left_cont     type ref to cl_gui_container,
      gr_right_cont    type ref to cl_gui_container,
      gr_splitter      type ref to cl_gui_splitter_container,
      gr_alv_tree      type ref to cl_salv_tree,
      gt_tree_init     type table of ty_tree_tab,
      gt_tree          type table of ty_tree_tab,
      gv_expand_icon   type salv_de_tree_image,
      gv_collapse_icon type salv_de_tree_image,
      gv_hier_icon     type salv_de_tree_image.

endclass.

class lc_gui_screen implementation.
  method constructor.
    " Set splitter
    gr_splitter = new #( parent = cl_gui_container=>default_screen
                         rows = 1
                         columns = 2 ).

    " Set left side container
    gr_left_cont = gr_splitter->get_container( row = 1
                                               column = 1 ).
    "Set right side container
    gr_right_cont = gr_splitter->get_container( row = 1
                                                column = 2 ).
    me->create_tplnr_alv_tree( ).
    me->create_equi_alv( ).

  endmethod.

  method create_screen.
    if screen is initial.
      screen = new lc_gui_screen( ).
    endif.
  endmethod.

  method create_tplnr_alv_tree.
    me->select_tplnr_data( ).
    me->init_alv_tree( ).
    me->create_nodes( ).
    me->hide_column_tree( ).
    me->set_column_settings( ).

    data(lo_event) = gr_alv_tree->get_event( ).
    set handler me->on_link_click for lo_event.

    " Set toolbar
    gr_alv_tree->get_functions( )->set_all( ).

    " Show alv tree
    gr_alv_tree->display( ).

  endmethod.

  method create_equi_alv.
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
      catch cx_salv_error into data(gx_alv_error).
        message gx_alv_error->get_text( ) type 'E'.
    endtry.

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
    " Create ALV Tree instance
    try.
        cl_salv_tree=>factory(
        exporting
          r_container = gr_left_cont
          importing
            r_salv_tree = gr_alv_tree
            changing
              t_table = gt_tree_init
              ).
      catch cx_salv_error into data(gx_alv_error).
        message gx_alv_error->get_text( ) type 'E'.
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
      catch cx_salv_msg into data(gx_alv_error).
        message gx_alv_error->get_text( ) type 'E'.
    endtry.

  endmethod.

  method hide_column_tree.
    try.
        data(lo_tree_col) = gr_alv_tree->get_columns( ).
        lo_tree_col->get_column( 'NODE_KEY' )->set_visible( abap_false ).
        lo_tree_col->get_column( 'NODE_PARENT' )->set_visible( abap_false ).
        lo_tree_col->get_column( 'TPLNR' )->set_visible( abap_false ).
      catch cx_salv_not_found into data(gx_alv_error).
        message gx_alv_error->get_text( ) type 'E'.
    endtry.
  endmethod.

  method set_column_settings.
    data(lr_setting) = gr_alv_tree->get_tree_settings( ).
    lr_setting->set_hierarchy_header( 'TPLNR TREE' ).
    lr_setting->set_hierarchy_size( 40 ).
    lr_setting->set_hierarchy_icon( gv_hier_icon ).
  endmethod.

  method on_link_click.
    " Rename parametr node key
    data(key) = node_key.

    " Read tplnr from node
    read table gt_tree with key node_key = key into data(lv_tree_row).

    " Serch tplnr
    data(lv_stplnr) =  |{ lv_tree_row-tplnr }{ '%' } |.

    " Fill equipment table
    select * from equi
      join equz on equz~equnr = equi~equnr
      join iloa on iloa~iloan = equz~iloan
      join iflot on iloa~tplnr = iflot~tplnr
      join eqkt on equi~equnr = eqkt~equnr
      where iloa~tplnr like @lv_stplnr
      into corresponding fields of table @gt_equi.

    " Refresh alv
    gr_alv->refresh( ).

    " Optimize column width
    gr_alv->get_columns( )->set_optimize( ).
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