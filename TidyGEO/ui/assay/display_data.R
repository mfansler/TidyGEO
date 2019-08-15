tabPanel("Data",
         fluidRow(
           column(7,
                  h4("Assay Data"),
                  div(em("The abundance measurement of each element derived from each sample."),
                      a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/overview.html", "(Read more)"))),
           column(2, offset = 1,
                  br(),
                  reset_button("resetExpr")
           )
         ),
         br(),
         uiOutput("assay_vals_viewing_subset"),
         br(),
         bsAlert("alpha_alert"),
         withSpinner(dataTableOutput("exprPreview"), type = 5),
         uiOutput("evaluate_filters_button")
)