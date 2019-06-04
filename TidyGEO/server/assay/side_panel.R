output$expression_side_panel_contents <- renderUI({
  if (!is.null(input$display_assay_or_feature)) {
    if (input$display_assay_or_feature) {
      source(file.path("ui", "feature", "side_panel_feature.R"), local = TRUE)$value
    } else {
      source(file.path("ui", "assay", "side_panel_assay.R"), local = TRUE)$value
    }
  }
})