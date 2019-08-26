

# Column navigation -------------------------------------------------------


# ** helper functions -----------------------------------------------------



# ** observers ------------------------------------------------------------

observeEvent(input$next_cols_clicked, {
  to_move <- ALLOWED_DATATYPES[which(str_detect(input$next_cols_clicked, ALLOWED_DATATYPES))]
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
  to_move <- ALLOWED_DATATYPES[which(str_detect(input$prev_cols_clicked, ALLOWED_DATATYPES))]
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
