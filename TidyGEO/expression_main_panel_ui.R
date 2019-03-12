mainPanel(
  tabsetPanel(
    tabPanel("Assay data",
             fluidRow(
               column(1, secondary_button(id = "expression_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
               column(1, offset = 8, secondary_button(id = "expression_next_cols", label = div("Next columns", icon("arrow-right"))))
             ),
             bsAlert("alpha_alert"),
             withSpinner(dataTableOutput("exprPreview"), type = 5)
    ),
    tabPanel("Graphical summary",
             colorSelectorInput("expr_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
             uiOutput("expr_select_binwidths"),
             uiOutput("histograms_expression")
    )
  )
) #main panel