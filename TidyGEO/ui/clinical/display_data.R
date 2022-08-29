tabPanel(title = "Phenotype Data",
         fluidRow(
           column(8, 
                  h4("Phenotype Data"),
                  div(em("A description of the biological samples and protocols to which they were subjected."),
                      a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/overview.html", "(Read more)"))
           ),
           column(2, offset = 1, 
                  br(),
                  reset_button("reset")
           )
         ),
         br(),
         uiOutput("clinical_vals_viewing_subset"),
         br(),
         bsAlert("parseError"),
         table_for_col_navigation("clinical")
)
