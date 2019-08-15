tabPanel("Feature Data",
         fluidRow(
           column(7,
                  h4("Feature Data"),
                  div(em("Expression profiling analysis usually generates quantitative data for features of interest. 
             Features of interest may be genes, transcripts, exons, miRNA, or some other genetic entity."),
                      a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/seq.html", "(Read more)"))),
           column(2, offset = 1,
                  br(),
                  reset_button("reset_feature")
           )
         ),
         br(),
         uiOutput("feature_vals_viewing_subset"),
         br(),
         withSpinner(dataTableOutput("feature_preview"), type = 5),
         uiOutput("evaluate_filters_button_feature")
)