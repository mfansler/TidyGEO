output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    data_to_view <- paste0(input$data_to_view, "_vals$", input$data_to_view, "_data")
    if (!is.null(eval(parse(text = data_to_view)))) {
      DTOutput("data_view")
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

output$data_view <- renderDT({
  browser()
  datatable(eval(parse(
    text = paste0(input$data_to_view, "_vals$", input$data_to_view, "_data")
  )),
  colnames = eval(parse(
    text = paste0("colnames(", input$data_to_view, "_vals$", input$data_to_view, "_data)")
  )),
  options = list(scrollX = TRUE,
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
  ))))
})