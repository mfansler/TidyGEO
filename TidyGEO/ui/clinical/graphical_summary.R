tabPanel("Graphical Summary",
         colorSelectorInput("clinical_plot_color", "Color of bars:", choices = c(brewer.pal(11, PLOT_COLORS), "#808080", "#000000"), ncol = 13),
         uiOutput("choose_variable_to_view"),
         sliderInput("clinical_binwidths", "Number of bins (for numeric):", min = 1, max = MAX_BINS, value = MAX_BINS / 2, width = '100%'),
         uiOutput("plots")
)