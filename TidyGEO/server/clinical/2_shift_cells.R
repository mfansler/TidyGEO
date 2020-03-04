observeEvent(input$show_broken_cols_example, {
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

output$display_cols_to_shift <- renderUI({
  selectInput(inputId = "col_to_shift", label = div("Shift the values from:"), choices = clinical_colnames(), 
              options(list(placeholder = "Please load a dataset...")))
})

output$display_destination_cols <- renderUI({
  into_cols <- clinical_colnames()[-which(clinical_colnames() %in% input$col_to_shift)]
  selectInput(inputId = "destination_col", label = div("into:"), choices = into_cols,
              options(list(placeholder = "Please load a dataset...")))
})

shift_preview <- reactive({ 
  if (data_loaded("clinical") &&
      !is.null(input$col_to_shift) &&
      !is.null(input$destination_col) &&
      input$col_to_shift != "" && 
      input$destination_col != "") {
    #shift_cells(head(clinical_vals[[dataname("clinical")]], 5), input$col_to_shift, input$destination_col)
    preview_data <- head(get_data_member("clinical", dataname("clinical")), 5)
    preview_data <- cbind(sapply(preview_data[,input$col_to_shift], shorten_labels, 20), 
                          rep(as.character(icon("arrow-right")), nrow(preview_data)), 
                          sapply(preview_data[,input$destination_col], shorten_labels, 20),
                          !is.na(preview_data[,input$col_to_shift]) & !is.na(preview_data[,input$destination_col]))
    colnames(preview_data) <- c(shorten_labels(input$col_to_shift, 20), " ", shorten_labels(input$destination_col, 20), "conflict")
    preview_data
  }
})

output$shift_preview_table <- DT::renderDT({
  if (data_loaded("clinical") &&
      !is.null(input$col_to_shift) &&
      !is.null(input$destination_col) &&
      input$col_to_shift != "" && 
      input$destination_col != "") {
  datatable(shift_preview(), rownames = FALSE, 
            options = list(dom = "t", columnDefs = list(list(targets = 3, visible = FALSE))), 
            escape = c(1, 3)) %>%
    formatStyle(
      columns = 4,
      valueColumns = 4,
      target = 'row',
      backgroundColor = styleEqual("TRUE", "#fee0d2")
    )
  }
})

output$conflict_actions <- renderUI({
  radioButtons("conflict_option", label = "What would you like to do for these conflicts?", 
               choiceNames = c(paste("Keep the values from", input$col_to_shift),
                               paste("Keep the values from", input$destination_col),
                               "Keep values from both columns, separated by a delimiter"),
               choiceValues = c(input$col_to_shift, input$destination_col, "delim"),
               selected = input$destination_col)
})

output$conflict_delimiter_option <- renderUI({
  if (!is.null(input$conflict_option) && input$conflict_option == "delim") {
    textInput("conflict_delimiter", "Please enter a delimiter to separate the values in the new column.")
  }
})

output$shift_conflicts_table <- DT::renderDT({
  if (!is.null(get_data_member("clinical", "shift_results")[["conflicts"]])) {
    datatable(get_data_member("clinical", "shift_results")[["conflicts"]], options = list(dom = "t", scrollY = 300, paging = FALSE))
  }
})

observeEvent(input$evaluate_shift, {
  set_x_equalto_y("shift_results", shift_cells(get_data_member("clinical", dataname("clinical")), input$col_to_shift, input$destination_col), "clinical")
  if (!is.null(get_data_member("clinical", "shift_results")[["conflicts"]])) {
    showModal(modalDialog( title = div(HTML('<font color="red">Whoops!</font>'), 
                                       tertiary_button("cancel_shift", "Cancel", class = "right_align")),
      HTML(paste0('<font color="red">Looks like we\'re trying to overwrite some of the values in ',
               input$destination_col, '. Please see the conflicts in the table below, and indicate how you
               would like to resolve the conflicts.</font>')),
      DTOutput("shift_conflicts_table"),
      br(),
      uiOutput("conflict_actions"),
      uiOutput("conflict_delimiter_option"),
      footer = primary_button("evaluate_conflicts", "Resolve")
    ))
  } else {
    set_x_equalto_y("last_data", get_data_member("clinical", dataname("clinical")), "clinical")
    set_x_equalto_y(dataname("clinical"), get_data_member("clinical", "shift_results")[["result"]], "clinical")
                    
    set_undo_point_script("clinical")
    save_lines(commentify("shift cells"), "clinical", "body")
    add_function("shift_cells", "clinical")
    save_lines(paste0(dataname("clinical"), " <- shift_cells(", dataname("clinical"), ", ", 
                                            format_string(input$col_to_shift), ", ",
                                            format_string(input$destination_col), ")[['result']]"), "clinical", "body")
  }
})

observeEvent(input$cancel_shift, {
  removeModal()
})

observeEvent(input$evaluate_conflicts, {
  removeModal()
  results <- shift_cells(get_data_member("clinical", dataname("clinical")), input$col_to_shift, input$destination_col, 
                         conflicts = if (input$conflict_option == "delim") input$conflict_delimiter else input$conflict_option)
  set_x_equalto_y("last_data", get_data_member("clinical", dataname("clinical")), "clinical")
  set_x_equalto_y(dataname("clinical"), results[["result"]], "clinical")
  
  set_undo_point_script("clinical")
  save_lines(commentify("shift cells"), "clinical", "body")
  add_function("shift_cells", "clinical")
  save_lines(paste0(dataname("clinical"), " <- shift_cells(", dataname("clinical"), ", ", 
                                          format_string(input$col_to_shift), ", ",
                                          format_string(input$destination_col), ", ",
                                          format_string(if (input$conflict_option == "delim") input$conflict_delimiter else input$conflict_option), ")[['result']]"), 
                                   "clinical", "body")
})

observeEvent(input$undo_shift, {
  undo_last_action("clinical")
})

navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")