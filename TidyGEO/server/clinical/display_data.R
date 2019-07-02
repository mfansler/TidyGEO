output$dataset <- DT::renderDT({
  if (!is.null(clinical_vals$clinical_data)) {
    datatable(clinical_vals$clinical_data, rownames = TRUE, options = list(
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