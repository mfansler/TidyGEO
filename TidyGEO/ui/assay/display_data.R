tabPanel("Assay data",
         h4("Assay Data"),
         p("The abundance measurement of each element derived from each sample."),
         br(),
         div(
           secondary_button(id = "expression_prev_cols", label = div(icon("arrow-left"), "Previous columns")),
           secondary_button(id = "expression_next_cols", label = "Next columns", icon = icon("arrow-right"), class = "right_align")
         ),
         bsAlert("alpha_alert"),
         withSpinner(dataTableOutput("exprPreview"), type = 5)#,
         #primary_button("expression_evaluate_filters", label = "Evaluate filters")
)