tabPanel(title = icon("download"), value = "3",
         h4("Saving the data"),
         selectInput("which_data_to_save", 
                     "Which data would you like to download to your computer?", 
                     c("clinical data" = "clinical", "assay data" = "assay", "feature data" = "feature", "all (zipped)" = "zip", "joined data" = "all")),
         radioButtons("all_file_type", div("File type:", help_link("all_files_help")), c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                                                                "JSON" = "JSON", "Excel" = "xlsx", "Formatted for BRB Array Tools" = "txt")),
         p("BRB Array Tools", help_link("brb_help_all")),
         uiOutput("all_display_filename"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("all_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("all_save_rscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link(id = "all_r_help")))
         )
         
         
         
         )