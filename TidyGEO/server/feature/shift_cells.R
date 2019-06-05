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

output$broken_cols_example <- renderDT({
  test_data <- head(as.data.frame(HairEyeColor, stringsAsFactors = FALSE), 5)
  test_data <- mapply(function(x, y) {
    paste(x, y, sep = ": ")
  }, colnames(test_data), test_data)
  test_data[2:3,1:2] <- test_data[2:3, 3:4]
  test_data[2:3, 3:4] <- c(NA, NA)
  datatable(test_data, rownames = FALSE, options = list(dom = "t"))
})

current_colnames <- reactive({
  if (!is.null(assay_vals$feature_data)) {
    colnames(assay_vals$feature_data)[-which(colnames(assay_vals$feature_data) == "ID")]
  } 
})

output$display_cols_to_shift_feature <- renderUI({
  selectInput(inputId = "col_to_shift_feature", label = div("Shift the values from:"), choices = current_colnames())
})

output$display_destination_cols_feature <- renderUI({
  if (!is.null(current_colnames()) && !is.null(input$col_to_shift_feature)) {
    into_cols <- current_colnames()[-which(current_colnames() %in% input$col_to_shift_feature)]
    selectInput(inputId = "destination_col_feature", label = div("into:"), choices = into_cols)
  }  
})

shift_preview <- reactive({ 
  if (!is.null(assay_vals$feature_data) &&
      !is.null(input$col_to_shift_feature) &&
      !is.null(input$destination_col_feature) &&
      input$col_to_shift_feature != "" && 
      input$destination_col_feature != "") {
    #shift_cells(head(assay_vals$feature_data, 5), input$col_to_shift_feature, input$destination_col_feature)
    preview_data <- head(assay_vals$feature_data, 5)
    preview_data <- cbind(sapply(preview_data[,input$col_to_shift_feature], shorten_labels, 20), 
                          rep(as.character(icon("arrow-right")), nrow(preview_data)), 
                          sapply(preview_data[,input$destination_col_feature], shorten_labels, 20),
                          !is.na(preview_data[,input$col_to_shift_feature]) & !is.na(preview_data[,input$destination_col_feature]))
    colnames(preview_data) <- c(shorten_labels(input$col_to_shift_feature, 20), " ", shorten_labels(input$destination_col_feature, 20), "conflict")
    preview_data
  }
})

output$shift_preview_table_feature <- DT::renderDT({
  if (!is.null(assay_vals$feature_data) &&
      !is.null(input$col_to_shift_feature) &&
      !is.null(input$destination_col_feature) &&
      input$col_to_shift_feature != "" && 
      input$destination_col_feature != "") {
    #datatable(shift_preview()[["result"]][input$destination_col_feature], options = list(dom = "t"))
    datatable(shift_preview(), rownames = FALSE, 
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
  if (!is.null(assay_vals$shift_results[["conflicts"]])) {
    datatable(assay_vals$shift_results[["conflicts"]], options = list(dom = "t", scrollY = 300, paging = FALSE))
  }
})

observeEvent(input$evaluate_shift_feature, {
  assay_vals$shift_results <- shift_cells(assay_vals$feature_data, input$col_to_shift_feature, input$destination_col_feature)
  if (!is.null(assay_vals$shift_results[["conflicts"]])) {
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
    assay_vals$last_feature <- assay_vals$feature_data
    
    assay_vals$feature_data <- assay_vals$shift_results[["result"]]
    
    assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                       start = 1, 
                                                       forward_distance = 4, 
                                                       previous_view = assay_vals$feature_data)
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(assay_vals$oFile)
    assay_vals$oFile <- saveLines(commentify("shift cells"), assay_vals$oFile)
    assay_vals$oFile <- saveLines(paste0("featureData <- shift_cells(featureData, ", 
                                            format_string(input$col_to_shift_feature), ", ",
                                            format_string(input$destination_col_feature), ")"), assay_vals$oFile)
    assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
  }
})

observeEvent(input$cancel_shift_feature, {
  removeModal()
})

observeEvent(input$evaluate_conflicts_feature, {
  removeModal()
  results <- shift_cells(assay_vals$feature_data, input$col_to_shift_feature, input$destination_col_feature, 
                         conflicts = if (input$conflict_option_feature == "delim") input$conflict_delimiter_feature else input$conflict_option_feature)
  assay_vals$last_feature <- assay_vals$feature_data
  assay_vals$feature_data <- results[["result"]]
  
  assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                     start = 1, 
                                                     forward_distance = 4, 
                                                     previous_view = assay_vals$feature_data)
  
  #WRITING COMMANDS TO R SCRIPT
  before <- length(assay_vals$oFile)
  assay_vals$oFile <- saveLines(commentify("shift cells"), assay_vals$oFile)
  assay_vals$oFile <- saveLines(paste0("featureData <- shift_cells(featureData, ", 
                                          format_string(input$col_to_shift_feature), ", ",
                                          format_string(input$destination_col_feature), ", ",
                                          format_string(if (input$conflict_option_feature == "delim") input$conflict_delimiter_feature else input$conflict_option_feature), ")"), 
                                   assay_vals$oFile)
  assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
})

observeEvent(input$undo_shift_feature, {
  assay_vals$feature_data <- assay_vals$last_feature
  assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                     start = 1, 
                                                     forward_distance = 4, 
                                                     previous_view = assay_vals$feature_data)
  assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$current_chunk_len)
  assay_vals$current_chunk_len <- 0
})