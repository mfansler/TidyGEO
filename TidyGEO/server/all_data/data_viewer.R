#data_to_view_is_null <- reactive({
#  eval(parse(text = paste0("is.null(", input$data_to_view, "_vals$", input$data_to_view, "_data)")))
#})

col_navigation_set_server("all", "data_viewer")

output$all_vals_viewing_subset <- renderUI({
  if (eval(expr(`$`(!!sym(paste0(input$data_to_view, "_vals")), "use_viewing_subset")))) {
    col_navigation_set(input$data_to_view, "data_viewer")
  }
})

output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    #if (data_to_view_is_null()) {
    #  get_null_error_message(input$data_to_view)
    #} else {
      # This acts screwy if you try to use one DTOutput for all the datatypes. Don't know why.
      # Best to keep them all separate for now.
      tagList(
        uiOutput("all_vals_viewing_subset"),
        DTOutput(paste0(input$data_to_view, "_view"))
      )
    #}
  }
})

dt_opts <- list(scrollX = TRUE,
                stateSave = TRUE,
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
  if (!is.null(clinical_vals$clinical_data)) {
    datatable(clinical_in_view(), rownames = TRUE, 
              options = c(basic_table_options, pageLength = clinical_vals$user_pagelen))
  }
  else {
    empty_table(clinical_vals$display_default)
  }
})

output$assay_view <- renderDT({
  if (!is.null(assay_vals$assay_data)) {
    datatable(assay_in_view(), rownames = FALSE, 
              options = c(basic_table_options, pageLength = assay_vals$user_pagelen))
  } else {
    empty_table(assay_vals$display_default)
  }
})

output$feature_view <- renderDT({
  if (!is.null(feature_vals$feature_data)) {
    datatable(feature_in_view(), rownames = FALSE, 
              options = c(basic_table_options, pageLength = feature_vals$user_pagelen))
  } else {
    empty_table(feature_vals$display_default)
  }
})

output$all_view <- renderDT({
  if (!is.null(all_vals$all_data)) {
    datatable(all_in_view(), options = c(basic_table_options, pageLength = all_vals$user_pagelen))
  } else {
    empty_table(all_vals$display_default)
  }
})