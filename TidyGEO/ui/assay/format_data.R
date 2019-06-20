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
                  help_link(id = "replace_id_help"))
           
         ),
         fluidRow(
           column(12, 
                  primary_button(id = "expression_transpose",
                                 label = div(icon("retweet"),
                                             "Transpose"),
                                 width = '200px', class = "indent"),
                  help_link(id = "transpose_help"))
         ),
         #primary_button("expression_evaluate_filters", 
          #              label = div(icon("filter"),
          #                          "Apply filters"),
          #              width = '200px', class = "indent"),
         #help_link(id = "evaluate_filters_help"),
         div(
           tipify(tertiary_button(id = "undoEvalExpr", label = div(icon("undo"), "Undo"), class = "right_align"), 
                  title = "Undo the last action.", placement = "bottom", trigger = "hover"),
           br()
         ),
         hr(), navigation_set("clinical_data", "1", "2", "top_level", "expression_side_panel")
)