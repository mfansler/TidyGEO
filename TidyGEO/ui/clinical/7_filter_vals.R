tabPanel(title = EXCLUDE_ICON, value = "7",
         h4("Filtering samples"),
         p("You may want to remove some of the samples in the data, for example, if you have samples with missing (NA) values."), 
         p("Here, you can specify values to use as filtering criteria.
           Rows that match the filtering criteria for the specified column will be removed."),
         uiOutput("display_cols_for_exclude"),
         uiOutput("sliderExclude"),
         div(
           primary_button("clinical_evaluate_exclude", div(EXCLUDE_ICON, "Exclude")),
           undo_button("undo_filter")
         ),
         hr(), navigation_set("6", "7", "8", "clinical_side_panel", "clinical_side_panel")
)