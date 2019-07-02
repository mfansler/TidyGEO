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

observeEvent(input$clinical_to_choose_button, {
  updateTabItems(session, "top_level", "choose_dataset")
})
observeEvent(input$clinical_1_to_2_button, {
  updateTabsetPanel(session, "clinical_side_panel", selected = "2")
})
navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("3", "4", "5", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("4", "5", "6", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("6", "7", "8", "clinical_side_panel", "clinical_side_panel")
observeEvent(input$clinical_8_to_7_button, {
  updateTabsetPanel(session, "clinical_side_panel", selected = "7")
})
observeEvent(input$clinical_to_assay_data, {
  updateTabItems(session, "top_level", "assay_data")
})

# Expression side panel ---------------------------------------------------

observeEvent(input$assay_to_clinical, {
  updateTabItems(session, "top_level", "clinical_data")
})
observeEvent(input$expression_nav_1_to_2_button, {
  updateTabsetPanel(session, "expression_side_panel", selected = "2")
})
observeEvent(input$expression_nav_2_to_1_button, {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})
observeEvent(input$expression_to_all, {
  updateTabItems(session, "top_level", "all_data")
})

# Feature side panel ------------------------------------------------------

navigation_set_server("1", "2", "3", "feature_side_panel", "feature_side_panel")
navigation_set_server("2", "3", "4", "feature_side_panel", "feature_side_panel")
navigation_set_server("3", "4", "5", "feature_side_panel", "feature_side_panel")
observeEvent(input$feature_nav_5_to_4_button, {
  updateTabsetPanel(session, "feature_side_panel", selected = "4")
})
observeEvent(input$feature_5_to_expression_button, {
  updateTabItems(session, "top_level", "assay_data")
})
