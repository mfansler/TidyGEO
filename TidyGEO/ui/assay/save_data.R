tabPanel(title = icon("download"), value = "2",
         h4("Saving the data"),
         p("Here is where you can download the assay data to your computer."), 
         p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
         radioButtons("expression_fileType", div("File type:", help_link("assay", "files_help")), 
                      choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                  "JSON" = "JSON", "Excel" = "xlsx", "Formatted for BRB Array Tools" = "txt")),
         uiOutput("expression_nameFile"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("expression_downloadRscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link("assay", "r_help")))
         ),
         hr(), tertiary_button('expression_nav_2_to_1_button', div(icon('arrow-left'), 'Back')),
         secondary_button("expression_to_all", div("Continue to all data", icon("arrow-right")), class = "right_align")
)