output$input_sub_range <- renderUI({
  
  if (isAllNum(clinical_vals$clinical_data[input$colsToSub])) {
    output = tagList()
    currCol <- as.numeric(as.character(clinical_vals$clinical_data[!is.na(clinical_vals$clinical_data[,input$colsToSub]),input$colsToSub]))
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
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  setNames(colNames, colNames)
  selectInput(inputId = "colsToSub", label = div("Please select a column with values to substitute: ", 
                                                 help_link("clinical", "substitute_help")), 
              choices = colNames,
              selected = clinical_vals$last_selected_substitute)
})


suggestions <- reactive({ 
  if (!is.null(clinical_vals$clinical_data) && !is.null(input$colsToSub)) {
    unique(as.character(clinical_vals$clinical_data[,input$colsToSub]))
  }
})

output$input_subs_table <- renderRHandsontable({
  rhandsontable(clinical_vals$subs_display, height = 250, rowHeaders = FALSE, stretchH = "all") %>% 
    hot_col(col = "To_Replace", type = "autocomplete", source = suggestions(), strict = FALSE) #%>%
  #hot_col(col = "New_Val", type = "autocomplete", source = values$thes_suggest_vals, strict = FALSE)
})

observeEvent(input$input_subs_table, {
  clinical_vals$subs_input <- hot_to_r(input$input_subs_table)
})

observeEvent(input$add_val_to_sub, {
  if (!is.null(input$colsToSub) && input$colsToSub != "") {
    if (
      is.na(clinical_vals$subs_input[nrow(clinical_vals$subs_input), "To_Replace"]) |
      clinical_vals$subs_input[nrow(clinical_vals$subs_input), "To_Replace"] == ""
      ) {
      clinical_vals$subs_input[nrow(clinical_vals$subs_input),] <- c(paste("RANGE:", paste(input$slideInSub, collapse = " - ")), input$newRangeVal)
    } else {
      clinical_vals$subs_input <- rbind(clinical_vals$subs_input, c(paste("RANGE:", paste(input$slideInSub, collapse = " - ")), input$newRangeVal))
    }
    clinical_vals$subs_display <- clinical_vals$subs_input
  }
})

observeEvent(input$evaluate_subs, {
  clinical_vals$last_selected_substitute <- input$colsToSub
  sub_specs <- list(clinical_vals$subs_input)
  names(sub_specs) <- input$colsToSub
  status <- withProgress(
    eval_function("clinical", "substitute_vals", list(sub_specs, input$sub_w_regex), "substitute values"), 
    message = "Substituting values")
  if (status != "completed") {
    showModal(
      error_modal("Error in substitute values", "No values substituted.", status)
    )
  }
  clinical_vals$subs_display <- data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE)
})

observeEvent(input$undo_subs, {
  undo_last_action("clinical")
})