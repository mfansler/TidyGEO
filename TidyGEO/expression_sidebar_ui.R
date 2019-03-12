sidebarPanel(
  h4("Formatting the expression data"),
  p("This portion of the application can reformat the expression (assay) data
    associated with the clinical data for the specified GEO ID. If you have already
    loaded clinical data, please start by clicking the button below."),
  primary_button("download_expr", "Load Expression Data", 
                 icon = help_button("Please make sure to download some clinical data first.")),
  hr(),
  tags$b("Options:"),
  fluidRow(
    column(2, primary_button("expression_replace_id", label = div("Use a different column as ID", 
                                                                  help_button('Search the feature data for different values to use as the "ID" column.'))))
    
  ),
  fluidRow(
    column(2, primary_button(id = "expression_transpose", 
                             label = div("Transpose", 
                                         help_button("Values in the ID column become the column names and column names become the ID column")
                             )))
  ),
  primary_button("expression_evaluate_filters", label = div("Apply filters", help_button("The filters below the column names are just previews for now. Clicking this drops the rows from the table."))),
  tags$style(type = 'text/css', '#expression_replace_id { margin-top: 3px; }'),
  tags$style(type = 'text/css', '#expression_transpose { margin-top: 3px; }'),
  tags$style(type = 'text/css', '#expression_evaluate_filters { margin-top: 3px; margin-bottom: 9px; }'),
  fluidRow(
    column(1, tertiary_button(id = "undoEvalExpr", label = "Undo")),
    column(1, offset = 5, tertiary_button(id = "resetExpr", label = "Reset"))
  ),
  hr(),
  radioButtons("expression_fileType", "File type:", 
               choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                           "JSON" = "JSON", "Excel" = "xlsx")),
  uiOutput("expression_nameFile"),
  tags$b("Download:"),
  fluidRow(
    column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    column(7, offset = 3, div(downloadButton("expression_downloadRscript", "R script", 
                                             style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                              help_link(id = "expression_r_help")))
  )
  )