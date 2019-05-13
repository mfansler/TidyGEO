observeEvent(input$show_broken_cols_example, {
  showModal(
    modalDialog(
      DTOutput("broken_cols_example")
    )
  )
})

output$broken_cols_example <- renderDT({
  test_data <- head(as.data.frame(HairEyeColor, stringsAsFactors = FALSE), 5)
  test_data[2:3,1:2] <- test_data[2:3, 3:4]
  test_data[2:3, 3:4] <- c(NA, NA)
  datatable(test_data, rownames = FALSE, options = list(dom = "t"))
})

current_colnames <- reactive({ colnames(clinical_vals$clinical_data) })

output$display_cols_to_shift <- renderUI({
  selectInput(inputId = "col_to_shift", label = div("Shift the values from:"), choices = current_colnames())
})

output$display_destination_cols <- renderUI({
  into_cols <- current_colnames()[-which(input$col_to_shift %in% current_colnames())]
  selectInput(inputId = "destination_col", label = div("into:"), choices = into_cols)  
})

shift_preview <- reactive({ 
  if (!is.null(clinical_vals$clinical_data) &&
      !is.null(input$col_to_shift) &&
      !is.null(input$destination_col) &&
      input$col_to_shift != "" && 
      input$destination_col != "") {
    #shift_cells(head(clinical_vals$clinical_data, 5), input$col_to_shift, input$destination_col)
    preview_data <- head(clinical_vals$clinical_data, 5)
    preview_data <- cbind(sapply(preview_data[,input$col_to_shift], shorten_labels, 20), 
                          rep(as.character(icon("arrow-right")), nrow(preview_data)), 
                          sapply(preview_data[,input$destination_col], shorten_labels, 20))
    colnames(preview_data) <- c(shorten_labels(input$col_to_shift, 20), " ", shorten_labels(input$destination_col, 20))
    preview_data
  }
})

output$shift_preview_table <- DT::renderDT({
  #datatable(shift_preview()[["result"]][input$destination_col], options = list(dom = "t"))
  datatable(shift_preview(), rownames = FALSE, options = list(dom = "t"), escape = c(1, 3))
})

output$conflict_actions <- renderUI({
  radioButtons("conflict_option", label = "What would you like to do for these conflicts?", 
               choiceNames = c(paste("Keep the values from", input$col_to_shift),
                               paste("Keep the values from", input$destination_col),
                               "Keep values from both columns, separated by a delimiter"),
               choiceValues = c(input$col_to_shift, input$destination_col, "delim"))
})

output$conflict_delimiter_option <- renderUI({
  if (!is.null(input$conflict_option) && input$conflict_option == "delim") {
    textInput("conflict_delimiter", "Please enter a delimiter to separate the values in the new column.")
  }
})

output$shift_conflicts_table <- DT::renderDT({
    datatable(shift_preview()[["conflicts"]], options = list(dom = "t"))
})

observeEvent(input$evaluate_shift, {
  results <- shift_cells(clinical_vals$clinical_data, input$col_to_shift, input$destination_col)
  if (!is.null(results[["conflicts"]])) {
    showModal(modalDialog( title = div(HTML('<font color="red">Whoops!</font>'), 
                                       tertiary_button("cancel_shift", "Cancel", class = "right_align")),
      HTML(paste0('<font color="red">Looks like we\'re trying to overwrite some of the values in ',
               input$destination_col, '. Please see the conflicts in the table below.</font>')),
      DTOutput("shift_conflicts_table"),
      uiOutput("conflict_actions"),
      uiOutput("conflict_delimiter_option"),
      footer = primary_button("evaluate_conflicts", "Try again")
    ))
  } else {
    clinical_vals$clinical_data <- results[["result"]]
  }
})

observeEvent(input$cancel_shift, {
  removeModal()
})

observeEvent(input$evaluate_conflicts, {
  removeModal()
  results <- shift_cells(clinical_vals$clinical_data[1:5,], input$col_to_shift, input$destination_col, 
                         conflicts = if (input$conflict_option == "delim") input$conflict_delimiter else input$conflict_option)
  clinical_vals$clinical_data <- results[["result"]]
})

observeEvent(input$undo_shift, {
  undo_last_action()
})