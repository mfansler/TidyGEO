tabPanel("Graphical summary",
         colorSelectorInput("feature_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
         sliderInput("feature_binwidths", "Number of bins (for numeric):", min = 1, max = 80, value = 40, width = '100%'),
         uiOutput("histograms_feature")
)