tabPanel(title = SAVE_ICON, value = "8",
         h4("Saving the data"),
         p("Here is where you can download the clinical data to your computer."),
         p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
         radioButtons("clinical_file_type", div("File type:", help_link("clinical", "files_help")), 
                      choices = SUPPORTED_FILE_TYPES),
         uiOutput("clinical_display_filename"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("clinical_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("clinical_save_rscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link("clinical", "r_help")))
         ),
         hr(), tertiary_button(nav("8", "7", "clinical"), div(PREV_ICON, "Back")),
         secondary_button(nav("clinical", "assay"), div("Continue to assay data", NEXT_ICON), class = "right_align")
)