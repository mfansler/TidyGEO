tabPanel(title = icon("download"), value = "5",
  h4("Saving the feature data"),
  p("Here is where you can download the feature data to your computer."), 
  p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
  radioButtons("feature_fileType", div("File type:", help_link("feature", "files_help")), 
               choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                           "JSON" = "JSON", "Excel" = "xlsx", "Formatted for BRB Array Tools" = "txt")),
  uiOutput("feature_nameFile"),
  tags$b("Download:"),
  fluidRow(
    column(1, downloadButton("feature_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    column(7, offset = 3, div(downloadButton("feature_downloadRscript", "R script", 
                                             style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                              help_link("feature", "r_help")))
  ),
  hr(), tertiary_button('feature_nav_5_to_4_button', div(icon('arrow-left'), 'Back')),
  secondary_button('feature_5_to_expression_button', div('Finish assay data', icon('arrow-right')), class = "right_align")
)