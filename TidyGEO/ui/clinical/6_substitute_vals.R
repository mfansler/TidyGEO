tabPanel(title = SUBSTITUTE_ICON, value = "6",
         h4("Substitute values"),
         p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
           some of the values in the data for other values."), 
         p("Here, you can identify values you would like to replace and an alternative to replace them with."),
         uiOutput("display_cols_to_sub"),
         uiOutput("input_sub_range"),
         h5(paste0('Right click to add or remove rows. To delete a word or phrase, ',
                     'enter that word or phrase into the column on the left and leave the column on the right empty.')),
        checkboxInput("sub_w_regex", div("Use regex", regex_help_link("clinical", "substitute"))),
        rHandsontableOutput("input_subs_table"),
         div(
           primary_button("evaluate_subs", div(SUBSTITUTE_ICON, "Substitute")),
           undo_button("undo_subs")
         ),
         hr(), navigation_set("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
)
