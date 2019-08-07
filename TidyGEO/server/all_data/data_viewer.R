data_to_view <- reactive({
  eval(parse(text = paste0(input$data_to_view, "_vals$", input$data_to_view, "_data")))
})

output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    if (!is.null(data_to_view())) {
      # This acts screwy if you try to use one DTOutput for all the datatypes. Don't know why.
      # Best to keep them all separate for now.
      DTOutput(paste0(input$data_to_view, "_view"))
    } else if (input$data_to_view == "all") {
      HTML('<p style="color:red">No datasets have been joined yet. Please join datasets to view this data.</p>')
    } else {
      HTML(paste0(
        '<p style="color:red">There is no ', input$data_to_view, ' data loaded. Please load data in the "',
        toupper(substring(input$data_to_view, 1, 1)), substring(input$data_to_view, 2, nchar(input$data_to_view)), 
        ' data" tab.</p>'))
    }
  }
})

dt_opts <- list(scrollX = TRUE,
                columnDefs = list(list(
                  targets = "_all",
                  ##Makes it so that the table will only display the first 50 chars.
                  ##See https://rstudio.github.io/DT/options.html
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && typeof data === 'string' && data.length > 50 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                    "}"
                  )
                )))

output$clinical_view <- renderDT({
  clinical_vals$clinical_data
}, options = dt_opts)

output$assay_view <- renderDT({
  assay_vals$assay_data
}, options = dt_opts)

output$feature_view <- renderDT({
  feature_vals$feature_data
}, options = dt_opts)

output$all_view <- renderDT({
  all_vals$all_data
}, options = dt_opts)