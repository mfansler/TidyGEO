output$sliderExclude <- renderUI({
  
  output <- tagList()
  #browser()
  if (!is.null(input$col_valsToExclude) && input$col_valsToExclude %in% colnames(clinical_vals$clinical_data)) {
        if (isAllNum(clinical_vals$clinical_data[input$col_valsToExclude])) {
          currCol <- as.numeric(as.character(clinical_vals$clinical_data[!is.na(clinical_vals$clinical_data[,input$col_valsToExclude]),input$col_valsToExclude]))
          output[[1]] <- radioButtons("excludeToKeep", label = "I would like to:", 
                                      choices = list("exclude the values within the range." = "exclude", 
                                                     "include the values within the range." = "keep"))
          output[[2]] <- sliderInput(inputId = "sliderExclude", label = "Please choose a range of values (inclusive)", min = min(currCol), max = max(currCol), value = c(quantile(currCol)[2], quantile(currCol)[3]))
        }
        else {
          #p(style = "color:red", "Looks like this column isn't numeric!")
          output[[1]] <- div(tags$b("Which values would you like to exclude?"), 
                             help_button("Excluding a value will remove the entire row that contains that value."))
          output[[2]] <- checkboxInput(inputId = "select_all_exclude", label = tags$i("Select all"))
          #browser()
          #if (!is.null(input$col_valsToExclude)) {
            #valNames <- unique(as.character(clinical_vals$clinical_data[,input$col_valsToExclude]))
            #valNames[which(is.na(valNames))] <- "NA"
          output[[3]] <- checkboxGroupInput(inputId = "valsToExclude", 
                                            label = NULL,
                                            choices = to_exclude_options())
          #}
        }
    output
  }
  
})

output$display_cols_for_exclude <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  setNames(colNames, colNames)
  selectInput(inputId = "col_valsToExclude", label = div("Column to use as filtering criteria: ", 
                                                         help_link(id = "exclude_help")), 
              choices = colNames,
              selected = clinical_vals$last_selected_exclude)
})

to_exclude_options <- reactive({
  if (!is.null(input$col_valsToExclude) && input$col_valsToExclude %in% colnames(clinical_vals$clinical_data)) {
    valNames <- unique(as.character(clinical_vals$clinical_data[,input$col_valsToExclude]))
    valNames[which(is.na(valNames))] <- "NA"
    valNames
    
  }
})

observe({
  if (!is.null(input$select_all_exclude)) {
    updateCheckboxGroupInput(
      session, 'valsToExclude', choices = to_exclude_options(),
      selected = if (input$select_all_exclude) to_exclude_options()
    )
  }
})

#output$display_vals_to_exclude <- renderUI({
#  if (!is.null(input$col_valsToExclude)) {
#    valNames <- unique(as.character(clinical_vals$clinical_data[,input$col_valsToExclude]))
#    valNames[which(is.na(valNames))] <- "NA"
#    checkboxGroupInput(inputId = "valsToExclude", 
#                       label = NULL,
#                       choices = to_exclude_options())
#  }
#})

observeEvent(input$clinical_evaluate_exclude, {
  
  if (!is.null(input$col_valsToExclude) && (!is.null(input$valsToExclude) || !is.null(input$sliderExclude))) {
    #if (input$exclude_isrange && isAllNum(clinical_vals$clinical_data[input$col_valsToExclude])) {
    if (isAllNum(clinical_vals$clinical_data[input$col_valsToExclude])) {
      to_exclude <-  paste(input$excludeToKeep, paste(input$sliderExclude, collapse = " - "), sep = ": ")
    } 
    else {
      to_exclude <- input$valsToExclude
    }
    clinical_vals$last_data <- clinical_vals$clinical_data
    clinical_vals$last_selected_exclude <- input$col_valsToExclude
    clinical_vals$clinical_data <- withProgress(excludeVars(clinical_vals$clinical_data, input$col_valsToExclude, to_exclude), 
                                    message = "Filtering rows")
    
    #WRITING COMMANDS TO R SCRIPT
    #before <- length(clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(commentify("exclude undesired samples"), clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(c(paste0("variable <- ", format_string(input$col_valsToExclude)),
    #                            paste0("values <- ", format_string(to_exclude))), clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines("clinical_data <- excludeVars(clinical_data, variable, values)", 
    #                          clinical_vals$oFile)
    #clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
    
    set_undo_point_script("clinical")
    save_lines(commentify("exclude undesired samples"), "clinical", "body")
    add_function("excludeVars", "clinical")
    save_lines(c(paste0("variable <- ", format_string(input$col_valsToExclude)),
                 paste0("values <- ", format_string(to_exclude))), "clinical", "body")
    save_lines("clinical_data <- excludeVars(clinical_data, variable, values)", 
                                     "clinical", "body")
  }
})

observeEvent(input$undo_filter, {
  undo_last_action()
})