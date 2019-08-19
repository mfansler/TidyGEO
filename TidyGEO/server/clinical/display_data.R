observeEvent(input$reset, {
  reset_datatype("clinical")
})

col_navigation_set_server("clinical")

output$clinical_vals_viewing_subset <- renderUI({
  if (clinical_vals$use_viewing_subset) {
    col_navigation_set("clinical")
  }
})

output$clinical_display <- DT::renderDT({
  if (!is.null(clinical_vals$clinical_data)) {
    datatable(clinical_in_view(), rownames = TRUE, 
              options = c(basic_table_options, pageLength = clinical_vals$user_pagelen))
  }
  else {
    empty_table(clinical_vals$display_default)
  }
})