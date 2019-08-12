tabPanel(title = icon("edit"), value = "1",
         h4("Formatting the assay data"),
         p("This portion of the application can reformat the assay data
           associated with the specified GEO ID."), 
         p("Please start by clicking the help icons or the option buttons below."),
         tags$b("Options:"),
         fluidRow(
           column(12, 
                  primary_button("expression_replace_id", 
                                 label = div(icon("exchange-alt"),
                                             "Use different ID column"),
                                 width = '200px', class = "indent"), 
                  help_link("assay", "replace_id_help"))
           
         ),
         fluidRow(
           column(12, 
                  primary_button(id = "expression_transpose",
                                 label = div(icon("retweet"),
                                             "Transpose"),
                                 width = '200px', class = "indent"),
                  help_link("assay", "transpose_help"))
         ),
         #primary_button("expression_evaluate_filters", 
          #              label = div(icon("filter"),
          #                          "Apply filters"),
          #              width = '200px', class = "indent"),
         #help_link(id = "evaluate_filters_help"),
         div(
           undo_button("undoEvalExpr"),
           br()
         ),
         hr(), tertiary_button("assay_to_clinical", div(icon("arrow-left"), "Back to clinical data")),
         secondary_button("expression_nav_1_to_2_button", div("Next", icon("arrow-right")), class = "right_align")
)