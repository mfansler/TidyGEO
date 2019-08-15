observeEvent(input$show_broken_cols_example_feature, {
  showModal(
    modalDialog(title = "Data with missing values",
                p('In this example, the values for "Hair" and "Eye" are missing in the second and third rows,
                  which has caused "Sex" and "Freq" to shift over into the wrong columns.'),
                DTOutput("broken_cols_example"),
                footer = modalButton("Got it")
                )
  )
})

#output$broken_cols_example <- renderDT({
#  test_data <- head(as.data.frame(HairEyeColor, stringsAsFactors = FALSE), 5)
#  test_data <- mapply(function(x, y) {
#    paste(x, y, sep = ": ")
#  }, colnames(test_data), test_data)
#  test_data[2:3,1:2] <- test_data[2:3, 3:4]
#  test_data[2:3, 3:4] <- c(NA, NA)
#  datatable(test_data, rownames = FALSE, options = list(dom = "t"))
#})

current_colnames_feature <- reactive({
  if (!is.null(feature_vals$feature_data)) {
    colnames(feature_vals$feature_data)[-which(colnames(feature_vals$feature_data) == "ID")]
  } 
})

output$display_cols_to_shift_feature <- renderUI({
  selectInput(inputId = "col_to_shift_feature", label = div("Shift the values from:"), choices = current_colnames_feature())
})

output$display_destination_cols_feature <- renderUI({
  if (!is.null(current_colnames_feature()) && !is.null(input$col_to_shift_feature)) {
    into_cols <- current_colnames_feature()[-which(current_colnames_feature() %in% input$col_to_shift_feature)]
    selectInput(inputId = "destination_col_feature", label = div("into:"), choices = into_cols)
  }  
})

shift_preview_feature <- reactive({ 
  if (!is.null(feature_vals$feature_data) &&
      !is.null(input$col_to_shift_feature) &&
      !is.null(input$destination_col_feature) &&
      input$col_to_shift_feature != "" && 
      input$destination_col_feature != "") {
    #shift_cells(head(feature_vals$feature_data, 5), input$col_to_shift_feature, input$destination_col_feature)
    preview_data <- head(feature_vals$feature_data, 5)
    preview_data <- cbind(sapply(preview_data[,input$col_to_shift_feature], shorten_labels, 20), 
                          rep(as.character(icon("arrow-right")), nrow(preview_data)), 
                          sapply(preview_data[,input$destination_col_feature], shorten_labels, 20),
                          !is.na(preview_data[,input$col_to_shift_feature]) & !is.na(preview_data[,input$destination_col_feature]))
    colnames(preview_data) <- c(shorten_labels(input$col_to_shift_feature, 20), " ", shorten_labels(input$destination_col_feature, 20), "conflict")
    preview_data
  }
})

output$shift_preview_table_feature <- DT::renderDT({
  if (!is.null(feature_vals$feature_data) &&
      !is.null(input$col_to_shift_feature) &&
      !is.null(input$destination_col_feature) &&
      input$col_to_shift_feature != "" && 
      input$destination_col_feature != "") {
    #datatable(shift_preview()[["result"]][input$destination_col_feature], options = list(dom = "t"))
    datatable(shift_preview_feature(), rownames = FALSE, 
              options = list(dom = "t", columnDefs = list(list(targets = 3, visible = FALSE))), 
              escape = c(1, 3)) %>%
      formatStyle(
        columns = 4,
        valueColumns = 4,
        target = 'row',
        backgroundColor = styleEqual(TRUE, "#fee0d2")
      )
  }
})

output$conflict_actions_feature <- renderUI({
  radioButtons("conflict_option_feature", label = "What would you like to do for these conflicts?", 
               choiceNames = c(paste("Keep the values from", input$col_to_shift_feature),
                               paste("Keep the values from", input$destination_col_feature),
                               "Keep values from both columns, separated by a delimiter",
                               "Keep values from both columns, repeating rows for each conflict"),
               choiceValues = c(input$col_to_shift_feature, input$destination_col_feature, "delim", "keepall"),
               selected = input$destination_col_feature)
})

output$conflict_delimiter_option_feature <- renderUI({
  if (!is.null(input$conflict_option_feature) && input$conflict_option_feature == "delim") {
    textInput("conflict_delimiter_feature", "Please enter a delimiter to separate the values in the new column.")
  }
})

output$shift_conflicts_table_feature <- DT::renderDT({
  if (!is.null(feature_vals$shift_results[["conflicts"]])) {
    datatable(feature_vals$shift_results[["conflicts"]], options = list(dom = "t", scrollY = 300, paging = FALSE))
  }
})

observeEvent(input$evaluate_shift_feature, {
  feature_vals$shift_results <- shift_cells(feature_vals$feature_data, input$col_to_shift_feature, input$destination_col_feature)
  if (!is.null(feature_vals$shift_results[["conflicts"]])) {
    showModal(modalDialog( title = div(HTML('<font color="red">Whoops!</font>'), 
                                       tertiary_button("cancel_shift_feature", "Cancel", class = "right_align")),
                           HTML(paste0('<font color="red">Looks like we\'re trying to overwrite some of the values in ',
                                       input$destination_col_feature, '. Please see the conflicts in the table below, and indicate how you
                                       would like to resolve the conflicts.</font>')),
                           DTOutput("shift_conflicts_table_feature"),
                           br(),
                           uiOutput("conflict_actions_feature"),
                           uiOutput("conflict_delimiter_option_feature"),
                           footer = primary_button("evaluate_conflicts_feature", "Resolve")
                           ))
  } else {
    feature_vals$last_data <- feature_vals$feature_data
    
    feature_vals$feature_data <- feature_vals$shift_results[["result"]]
    
    #WRITING COMMANDS TO R SCRIPT
    set_undo_point_script("feature")
    save_lines(commentify("shift cells"), "feature", "body")
    add_function("shift_cells", "feature")
    save_lines(paste0("feature_data <- shift_cells(feature_data, ", 
                                            format_string(input$col_to_shift_feature), ", ",
                                            format_string(input$destination_col_feature), ")"), "feature", "body")
  }
})

observeEvent(input$cancel_shift_feature, {
  removeModal()
})

observeEvent(input$evaluate_conflicts_feature, {
  removeModal()
  results <- shift_cells(feature_vals$feature_data, input$col_to_shift_feature, input$destination_col_feature, 
                         conflicts = if (input$conflict_option_feature == "delim") input$conflict_delimiter_feature else input$conflict_option_feature)
  feature_vals$last_data <- feature_vals$feature_data
  feature_vals$feature_data <- results[["result"]]
  
  set_undo_point_script("feature")
  save_lines(commentify("shift cells"), "feature", "body")
  add_function("shift_cells", "feature")
  save_lines(paste0("feature_data <- shift_cells(feature_data, ", 
                    format_string(input$col_to_shift_feature), ", ",
                    format_string(input$destination_col_feature), ", ",
                    format_string(if (input$conflict_option_feature == "delim") input$conflict_delimiter_feature else input$conflict_option_feature), ")"), 
             "feature", "body")
})

observeEvent(input$undo_shift_feature, {
  undo_last_action("feature")
})