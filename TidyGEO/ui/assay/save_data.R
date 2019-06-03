tabPanel(title = icon("download"), value = "2",
         h4("Saving the data"),
         p("Here is where you can download the assay data to your computer."), 
         p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
         radioButtons("expression_fileType", div("File type:", help_link("expression_files_help")), 
                      choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                  "JSON" = "JSON", "Excel" = "xlsx", "Formatted for BRB Array Tools" = "zip")),
         uiOutput("expression_nameFile"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("expression_downloadRscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link(id = "expression_r_help")))
         ),
         hr(), uiOutput("expression_nav_2_ui")
)