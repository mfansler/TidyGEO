data_to_view_is_null <- reactive({
  eval(parse(text = paste0("is.null(", input$data_to_view, "_vals$", input$data_to_view, "_data)")))
})

observe({
  all_vals$use_viewing_subset <- !is.null(ncol(all_vals$all_data)) && ncol(all_vals$all_data) > 19
  all_vals$viewing_subset <- c(all_vals$viewing_min, min(all_vals$viewing_min + 5, ncol(all_vals$all_data)))
}, priority = 1)

all_in_view <- reactive({
  if (all_vals$use_viewing_subset) {
    all_vals$all_data[all_vals$viewing_subset[1]:all_vals$viewing_subset[2]]
  } else {
    all_vals$all_data
  }
})

output$all_vals_viewing_subset <- renderUI({
  if (eval(expr(`$`(!!sym(paste0(input$data_to_view, "_vals")), "use_viewing_subset")))) {
    col_navigation_set(input$data_to_view, "data_viewer")
  }
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
  clinical_in_view()
}, options = dt_opts)

output$assay_view <- renderDT({
  assay_in_view()
}, options = dt_opts)

output$feature_view <- renderDT({
  feature_in_view()
}, options = dt_opts)

output$all_view <- renderDT({
  all_in_view()
}, options = dt_opts)