tabPanel(title = icon("download"), value = "5",
  h4("Saving the feature data"),
  p("Here is where you can download the feature data to your computer."), 
  p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
  radioButtons("feature_fileType", div("File type:", help_link("feature", "files_help")), 
               choices = SUPPORTED_FILE_TYPES),
  uiOutput("feature_nameFile"),
  tags$b("Download:"),
  fluidRow(
    column(1, downloadButton("feature_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    column(7, offset = 3, div(downloadButton("feature_downloadRscript", "R script", 
                                             style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                              help_link("feature", "r_help")))
  ),
  hr(), tertiary_button(nav("5", "4", "feature"), div(PREV_ICON, 'Back')),
  secondary_button(nav("5", "assay", "feature"), div('Finish assay data', NEXT_ICON), class = "right_align")
)