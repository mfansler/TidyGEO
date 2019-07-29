output$col_to_match1_selector <- renderUI({
  if (!is.null(input$data_to_match1)) {
    data_to_match <- paste0(input$data_to_match1, "_vals$", input$data_to_match1, "_data")
    if (!is.null(eval(parse(text = data_to_match)))) {
      selectInput("col_to_match1", 
                  paste("Choose a column from the", input$data_to_match1, "data to match:"), 
                  choices = eval(parse(text = paste0("colnames(", data_to_match, ")"))))
    } else {
      HTML(paste0(
        '<p style="color:red">There is no ', input$data_to_match1, ' data loaded. Please load data in the "',
        toupper(substring(input$data_to_match1, 1, 1)), substring(input$data_to_match1, 2, nchar(input$data_to_match1)), 
        ' data" tab.</p>'))
    }
  }
})

output$col_to_match2_selector <- renderUI({
  if (!is.null(input$data_to_match2)) {
    data_to_match <- paste0(input$data_to_match2, "_vals$", input$data_to_match2, "_data")
    if (!is.null(eval(parse(text = data_to_match)))) {
      selectInput("col_to_match2", 
                  paste("Choose a column from the", input$data_to_match2, "data to match:"), 
                  choices = eval(parse(text = paste0("colnames(", data_to_match, ")"))))
    } else {
      HTML(paste0(
        '<p style="color:red">There is no ', input$data_to_match2, ' data loaded. Please load data in the "',
        toupper(substring(input$data_to_match2, 1, 1)), substring(input$data_to_match2, 2, nchar(input$data_to_match2)), 
        ' data" tab.</p>'))
    }
  }
})