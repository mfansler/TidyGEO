output$input_sub_range <- renderUI({
  this_data <- get_data_member("clinical", dataname("clinical"))
  if (isAllNum(this_data[input$colsToSub])) {
    output = tagList()
    currCol <- as.numeric(as.character(this_data[!is.na(this_data[,input$colsToSub]),input$colsToSub]))
    this_min <- min(currCol)
    this_max <- max(currCol)
    this_quantiles <- c(quantile(currCol)[2], quantile(currCol)[3])
    if (this_quantiles[1] == this_quantiles[2]) {
      this_quantiles <- c(this_min, this_max)
    }
    output[[1]] <- sliderInput(inputId = "slideInSub", label = "Please choose a range of values (inclusive)", min = this_min, max = this_max, value = this_quantiles)
    output[[2]] <- textInput("newRangeVal", label = "Please enter a value to replace all values in the range:")
    output[[3]] <- tertiary_button("add_val_to_sub", "Add range to table")
    #output[[4]] <- tertiary_button("remove_val_to_sub", "Remove")
    output
  }
  else {
    tags$b("Please enter a value and its replacement in the table below.")
  }
})

output$display_cols_to_sub <- renderUI({
  selectInput(inputId = "colsToSub", label = div("Please select a column with values to substitute: ", 
                                                 help_link("clinical", "substitute_help")), 
              choices = clinical_colnames(),
              selected = get_data_member("clinical", "last_selected_substitute"))
})


suggestions <- reactive({ 
  if (data_loaded("clinical") && !is.null(input$colsToSub)) {
    unique(as.character(get_data_member("clinical", dataname("clinical"))[,input$colsToSub]))
  }
})

output$input_subs_table <- renderRHandsontable({
  rhandsontable(get_data_member("clinical", "subs_display"), height = 250, rowHeaders = FALSE, stretchH = "all") %>% 
    hot_col(col = "To_Replace", type = "autocomplete", source = suggestions(), strict = FALSE) #%>%
  #hot_col(col = "New_Val", type = "autocomplete", source = values$thes_suggest_vals, strict = FALSE)
})

observeEvent(input$input_subs_table, {
  set_x_equalto_y("subs_input", hot_to_r(input$input_subs_table), "clinical")
})

observeEvent(input$add_val_to_sub, {
  if (!is.null(input$colsToSub) && input$colsToSub != "") {
    this_subs <- get_data_member("clinical", "subs_input")
    if (
      is.na(this_subs[nrow(this_subs), "To_Replace"]) |
      this_subs[nrow(this_subs), "To_Replace"] == ""
      ) {
      this_subs[nrow(clinical_vals$subs_input),] <- c(paste("RANGE:", paste(input$slideInSub, collapse = " - ")), input$newRangeVal)
    } else {
      this_subs <- rbind(this_subs, c(paste("RANGE:", paste(input$slideInSub, collapse = " - ")), input$newRangeVal))
    }
    set_x_equalto_y("subs_input", this_subs, "clinical")
    set_x_equalto_y("subs_display", this_subs, "clinical")
  }
})

observeEvent(input$evaluate_subs, {
  set_x_equalto_y("last_selected_substitute", input$colsToSub, "clinical")
  sub_specs <- list(get_data_member("clinical", "subs_input"))
  names(sub_specs) <- input$colsToSub
  status <- withProgress(
    eval_function("clinical", "substitute_vals", list(sub_specs, input$sub_w_regex), "substitute values"), 
    message = "Substituting values")
  if (status != SUCCESS) {
    showModal(
      error_modal("Error in substitute values", "No values substituted.", status)
    )
  }
  set_x_equalto_y("subs_display", data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE), "clinical")
})

observeEvent(input$undo_subs, {
  undo_last_action("clinical")
})

navigation_set_server("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
