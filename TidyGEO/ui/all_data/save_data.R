tabPanel(title = icon("download"), value = "3",
         h4("Saving the data"),
         selectInput("which_data_to_save", 
                     "Which data would you like to download to your computer?", 
                     c("phenotype data" = "clinical", "assay data" = "assay", "feature data" = "feature", "all (zipped)" = "zip", "joined data" = "all")),
         conditionalPanel(condition = "input.which_data_to_save === 'zip'",
                          radioButtons("zipped_filetype", div("Zip file type:", help_link("all", "zipfiles_help")),
                                       c("Zip" = "zip", "Gzipped TAR" = "tgz"))),
         uiOutput("all_file_type_select"),
         textInput("all_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
                   placeholder = "Start typing..."),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("all_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("all_save_rscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link("all", "r_help")))
         ),
         hr(), tertiary_button(nav("3", "2", "all"), div(PREV_ICON, "Back"))
         )
