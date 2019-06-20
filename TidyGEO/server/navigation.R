# creates observers for a navigation set
navigation_set_server <- function(prev, from, to, section_prev = NULL, section_to = NULL) {
  observeEvent(eval(parse(text = paste0("input$nav_", from, "_to_", prev, "_", section_prev))), {
    updateTabsetPanel(session, section_prev, selected = prev)
  })
  observeEvent(eval(parse(text = paste0("input$nav_", from, "_to_", to, "_", section_to))), {
    updateTabsetPanel(session, section_to, selected = to)
  })
}


# Clinical side panel -----------------------------------------------------


navigation_set_server(prev = "choose_dataset", from = "1", to = "2", section_prev = "top_level", section_to = "clinical_side_panel")
navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("3", "4", "5", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("4", "5", "6", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("6", "7", "8", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("7", "8", "assay_data","clinical_side_panel", "top_level")

# Expression side panel ---------------------------------------------------

navigation_set_server("clinical_data", "1", "2", "top_level", "expression_side_panel")
observeEvent(input$expression_nav_2_to_1_button, {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})