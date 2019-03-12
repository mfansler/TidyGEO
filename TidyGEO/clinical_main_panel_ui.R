mainPanel(
  tabsetPanel(
    tabPanel(title = "Clinical Data", 
             br(), 
             fluidRow(
               column(2, tertiary_button("reset", div("Reset", help_button("Reset the dataset to its original downloaded state.", placement = "bottom")))),
               column(2, offset = 0, tertiary_button("undo", div("Undo", help_button("Undo the last action.", placement = "bottom"))))
             ), 
             br(), br(), 
             bsAlert("alert"), withSpinner(DTOutput("dataset"), type = 5)
    ),
    tabPanel("Graphical Summary",
             colorSelectorInput("clinical_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
             sliderInput("clinical_binwidths", "Width of bars (for numeric):", min = 0, max = 60, value = 30),
             uiOutput("plots")
    )
  )
)