observeEvent(input$start_feature_format, {
  updateTabsetPanel(session, "feature_side_panel", selected = "2")
})
observeEvent(input$back_to_assay, {
  updateTabItems(session, "top_level", "assay_data")
})