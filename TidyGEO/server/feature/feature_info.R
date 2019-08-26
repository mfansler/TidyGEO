observeEvent(get_input(nav("1", "2", "feature")), {
  updateTabsetPanel(session, "feature_side_panel", selected = "2")
})
observeEvent(get_input(nav("1", "assay", "feature")), {
  updateTabItems(session, "top_level", "assay_data")
})