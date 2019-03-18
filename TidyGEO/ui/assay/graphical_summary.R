tabPanel("Graphical summary",
         colorSelectorInput("expr_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
         uiOutput("expr_select_binwidths"),
         #checkboxInput("expr_display_labels", "Display labels above columns?"),
         uiOutput("histograms_expression")
)