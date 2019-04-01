tabPanel(title = "Clinical Data",
         fluidRow(
           #div(style = "display:inline-block;vertical-align:top;",
           
           column(2, 
                  br(),
                  tertiary_button("reset", div(icon("sticky-note"), "Reset"))
           ),
               column(6, 
                      h4("Clinical Data"),
                      p("A description of the biological samples and protocols to which they were subjected.")
               )
               #)
         ),
         #, help_button("Reset the dataset to its original downloaded state.", placement = "bottom")
         br(), 
         fluidRow(
           column(2, offset = 0, tertiary_button("undo", div("Undo", help_button("Undo the last action.", placement = "bottom")))),
           #warning about merged datasets using dates as the criteria
           column(6, offset = 2, uiOutput("mergedWarning"))
         ), 
         br(), br(), 
         bsAlert("parseError"),
         withSpinner(DTOutput("dataset"), type = 5)
)