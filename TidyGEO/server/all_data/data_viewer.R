data_to_view_is_null <- reactive({
  eval(parse(text = paste0("is.null(", input$data_to_view, "_vals$", input$data_to_view, "_data)")))
})

output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    if (data_to_view_is_null()) {
      get_null_error_message(input$data_to_view)
    } else {
      # This acts screwy if you try to use one DTOutput for all the datatypes. Don't know why.
      # Best to keep them all separate for now.
      DTOutput(paste0(input$data_to_view, "_view"))
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