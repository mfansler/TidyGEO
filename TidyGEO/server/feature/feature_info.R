observeEvent(input$start_feature_format, {
  updateTabsetPanel(session, "expression_side_panel", selected = "2")
})
observeEvent(input$back_to_assay, {
  updatePrettySwitch(session, "display_assay_or_feature", value = FALSE)
})