tabPanel(title = "Clinical Data",
         fluidRow(
           #div(style = "display:inline-block;vertical-align:top;",
           
           
               column(8, 
                      h4("Clinical Data"),
                      div(em("A description of the biological samples and protocols to which they were subjected."),
                          a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/overview.html", "(Read more)"))
               ),
               column(2, offset = 1, 
                      br(),
                      tipify(tertiary_button("reset", div(icon("history"), "Reset")), 
                             title = "Reset the dataset to its original downloaded state.", placement = "bottom", trigger = "hover")
               )
               #)
         ),
         br(), br(), 
         bsAlert("parseError"),
         withSpinner(DTOutput("dataset"), type = 5)
)