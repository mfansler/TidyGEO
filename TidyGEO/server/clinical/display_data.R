observeEvent(input$reset, {
  reset_datatype("clinical")
})

observe({
  clinical_vals$use_viewing_subset <- !is.null(ncol(clinical_vals$clinical_data)) && ncol(clinical_vals$clinical_data) > 19
  clinical_vals$viewing_subset <- c(clinical_vals$viewing_min, min(clinical_vals$viewing_min + 5, ncol(clinical_vals$clinical_data)))
}, priority = 1)

clinical_in_view <- reactive({
  if (clinical_vals$use_viewing_subset) {
    clinical_vals$clinical_data[clinical_vals$viewing_subset[1]:clinical_vals$viewing_subset[2]]
  } else {
    clinical_vals$clinical_data
  }
})

output$clinical_vals_viewing_subset <- renderUI({
  if (clinical_vals$use_viewing_subset) {
    col_navigation_set("clinical")
  }
})

output$dataset <- DT::renderDT({
  if (!is.null(clinical_vals$clinical_data)) {
    datatable(clinical_in_view(), rownames = TRUE, options = list(
      scrollX = TRUE,
      columnDefs = list(list(
        targets = "_all",
        ##Makes it so that the table will only display the first 50 chars.
        ##See https://rstudio.github.io/DT/options.html
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && typeof data === 'string' && data.length > 50 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
          "}")
      ))))
  }
  else {
    datatable(clinical_vals$display_default, rownames = FALSE, colnames = "NO DATA")
  }
})