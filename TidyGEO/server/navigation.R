

# Column navigation -------------------------------------------------------


# ** helper functions -----------------------------------------------------

col_navigation_set_server <- function(datatype, extra_tag = NULL) {
  observe({
    n_cols <- ncol(get_data_member(datatype, dataname(datatype)))
    this_viewing_min <- get_data_member(datatype, "viewing_min")
    set_x_equalto_y("use_viewing_subset", !is.null(n_cols) && n_cols > 19, datatype)
    set_x_equalto_y("viewing_subset", c(this_viewing_min, min(this_viewing_min + 5, n_cols)), datatype)
  }, priority = 1)
  
  assign(view(datatype), reactive({
    if (get_data_member(datatype, "use_viewing_subset")) {
      this_viewing_subset <- get_data_member(datatype, "viewing_subset")
      cols_to_view <- this_viewing_subset[1]:this_viewing_subset[2]
      if (get_data_member(datatype, "viewing_min") == 2) {
        cols_to_view <- c(1, cols_to_view)
      }
      get_data_member(datatype, dataname(datatype))[cols_to_view]
    } else {
      get_data_member(datatype, dataname(datatype))
    }
  }), pos = parent.frame())
  
  output[[visible(datatype, extra_tag)]] <- renderText({
    paste("Showing", 
          get_data_member(datatype, "viewing_subset")[1], "to", 
          get_data_member(datatype, "viewing_subset")[2], "of", 
          ncol(get_data_member(datatype, dataname(datatype))), 
          "columns")
  })
}

table_for_col_navigation <- function(datatype, extra_tag = NULL, show_rownames = FALSE, show_filters = FALSE) {
  filter <- if (show_filters) list(position = "top", clear = FALSE) else c("none")
  
  output[[display(datatype, extra_tag)]] <- DT::renderDT({
    if (!is.null(get_data_member(datatype, dataname(datatype)))) {
      datatable(do.call(view(datatype), list()), filter = filter, rownames = show_rownames, 
                options = c(BASIC_TABLE_OPTIONS, pageLength = get_data_member(datatype, "user_pagelen")))
    }
    else {
      empty_table(get_data_member(datatype, display_default))
    }
  })
}

# ** observers ------------------------------------------------------------

observeEvent(input$next_cols_clicked, {
  to_move <- allowed_datatypes[which(str_detect(input$next_cols_clicked, allowed_datatypes))]
  if (identical(to_move, character(0))) {
    stop("Error in clicking next cols. The button that called next cols is not tagged with a valid datatype.")
  } else {
    ncol_data_to_move <- ncol(get_data_member(to_move, dataname(to_move)))
    #move_by <- floor(ncol_data_to_move / 5)
    current_subset <- get_data_member(to_move, "viewing_subset")[1]
    
    start <- min(ncol_data_to_move, current_subset + MOVE_BY + 1)
    end <- min(ncol_data_to_move, start + MOVE_BY)
    eval(
      expr(
        `<-`(
          !!get_data_member_expr(to_move, "user_pagelen"),
          input[[paste0(display(next_col_source(input$next_cols_clicked)), "_state")]][["length"]]
        )
      )
    )
    
    eval(
      expr(`<-`(!!get_data_member_expr(to_move, "viewing_subset"), c(start, end)))
    )
  }
  session$sendCustomMessage("resetValue", "next_cols_clicked")
})

observeEvent(input$prev_cols_clicked, {
  to_move <- allowed_datatypes[which(str_detect(input$prev_cols_clicked, allowed_datatypes))]
  if (identical(to_move, character(0))) {
    stop("Error in clicking next cols. The button that called prev cols is not tagged with a valid datatype.")
  } else {
    ncol_data_to_move <- ncol(get_data_member(to_move, dataname(to_move)))
    #move_by <- floor(ncol_data_to_move / 5)
    viewing_min <- get_data_member(to_move, "viewing_min")
    current_subset <- get_data_member(to_move, "viewing_subset")[2]
    
    end <- max(viewing_min, current_subset - MOVE_BY - 1)
    start <- max(viewing_min, end - MOVE_BY)
    
    eval(
      expr(
        `<-`(
          !!get_data_member_expr(to_move, "user_pagelen"),
          input[[paste0(display(prev_col_source(input$prev_cols_clicked)), "_state")]][["length"]]
        )
      )
    )
    
    eval(
      expr(`<-`(!!get_data_member_expr(to_move, "viewing_subset"), c(start, end)))
    )
  }
  session$sendCustomMessage("resetValue", "prev_cols_clicked")
})

# Tab navigation ----------------------------------------------------------


# ** helper functions -----------------------------------------------------

# creates observers for a navigation set
navigation_set_server <- function(prev, from, to, section_prev = NULL, section_to = NULL) {
  observeEvent(get_input(nav(from, prev, section_prev)), {
    updateTabsetPanel(session, section_prev, selected = prev)
  })
  observeEvent(get_input(nav(from, to, section_to)), {
    updateTabsetPanel(session, section_to, selected = to)
  })
}

# ** observers ------------------------------------------------------------


# ** ** Clinical side panel -----------------------------------------------

observeEvent(get_input(nav("clinical", "choose")), {
  updateTabItems(session, "top_level", "choose_dataset")
})
observeEvent(get_input(nav("1", "2", "clinical")), {
  updateTabsetPanel(session, "clinical_side_panel", selected = "2")
})
navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("3", "4", "5", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("4", "5", "6", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("6", "7", "8", "clinical_side_panel", "clinical_side_panel")
observeEvent(get_input(nav("8", "7", "clinical")), {
  updateTabsetPanel(session, "clinical_side_panel", selected = "7")
})
observeEvent(get_input(nav("clinical", "assay")), {
  updateTabItems(session, "top_level", "assay_data")
})

# ** ** Expression side panel ---------------------------------------------

observeEvent(get_input(nav("assay", "clinical")), {
  updateTabItems(session, "top_level", "clinical_data")
})
observeEvent(get_input(nav("1", "2", "assay")), {
  updateTabsetPanel(session, "expression_side_panel", selected = "2")
})
observeEvent(get_input(nav("2", "1", "assay")), {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})
observeEvent(get_input(nav("assay", "all")), {
  updateTabItems(session, "top_level", "all_data")
})

# ** ** Feature side panel ------------------------------------------------

navigation_set_server("1", "2", "3", "feature_side_panel", "feature_side_panel")
navigation_set_server("2", "3", "4", "feature_side_panel", "feature_side_panel")
navigation_set_server("3", "4", "5", "feature_side_panel", "feature_side_panel")
observeEvent(get_input(nav("1", "2", "feature")), {
  updateTabsetPanel(session, "feature_side_panel", selected = "2")
})
observeEvent(get_input(nav("1", "assay", "feature")), {
  updateTabItems(session, "top_level", "assay_data")
})
observeEvent(get_input(nav("5", "4", "feature")), {
  updateTabsetPanel(session, "feature_side_panel", selected = "4")
})
observeEvent(get_input(nav("5", "assay", "feature")), {
  updateTabItems(session, "top_level", "assay_data")
})

# ** ** All side panel ----------------------------------------------------

navigation_set_server("1", "2", "3", "all_data_options", "all_data_options")
observeEvent(get_input(nav("all", "assay")), {
  updateTabsetPanel(session, "top_level", "assay_data")
})
observeEvent(get_input(nav("1", "2", "all")), {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})
observeEvent(get_input(nav("3", "2", "all")), {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})
