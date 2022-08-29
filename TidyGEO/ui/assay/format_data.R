tabPanel(title = icon("pen-to-square"), value = "1",
         h4("Formatting the assay data"),
         p("This portion of the application can reformat the assay data
           associated with the specified GEO ID."), 
         p("Please start by clicking the help icons or the option buttons below."),
         tags$b("Options:"),
         fluidRow(
           column(12, 
                  primary_button("expression_replace_id", 
                                 label = div(REPLACE_ID_ICON,
                                             "Use different ID column"),
                                 width = '200px', class = "indent"), 
                  help_link("assay", "replace_id_help"))
           
         ),
         fluidRow(
           column(12, 
                  primary_button(id = "expression_transpose",
                                 label = div(TRANSPOSE_ICON,
                                             "Transpose"),
                                 width = '200px', class = "indent"),
                  help_link("assay", "transpose_help"))
         ),
         div(
           undo_button("undoEvalExpr"),
           br()
         ),
         hr(), tertiary_button(nav("assay", "clinical"), div(PREV_ICON, "Back to phenotype data")),
         secondary_button(nav("1", "2", "assay"), div("Next", NEXT_ICON), class = "right_align")
)
