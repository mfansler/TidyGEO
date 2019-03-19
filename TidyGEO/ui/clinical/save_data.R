tabPanel("6",
         h4("Saving the data"),
         p("Here is where you can download the clinical data to your computer."),
         p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
         radioButtons("clinical_file_type", div("File type:", help_link("clinical_files_help")), 
                      choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                  "JSON" = "JSON", "Excel" = "xlsx")),
         uiOutput("clinical_display_filename"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("clinical_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("clinical_save_rscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link(id = "clinical_r_help")))
         ),
         hr(), uiOutput("nav_6_ui")
)