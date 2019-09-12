tabPanel(title = SAVE_ICON, value = "2",
         h4("Saving the data"),
         p("Here is where you can download the assay data to your computer."), 
         p("You can also download the R script that produced this data. The R script allows you
           to replicate the steps you took so you can see how the data was obtained."),
         uiOutput("assay_file_type_select"),
         uiOutput("expression_nameFile"),
         tags$b("Download:"),
         fluidRow(
           column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           column(7, offset = 3, div(downloadButton("expression_downloadRscript", "R script", 
                                                    style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                     help_link("assay", "r_help")))
         ),
         hr(), tertiary_button(nav("2", "1", "assay"), div(PREV_ICON, 'Back')),
         secondary_button(nav("assay", "all"), div("Continue to all data", NEXT_ICON), class = "right_align")
)