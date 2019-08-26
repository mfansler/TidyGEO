output$feature_nameFile <- renderUI({
  textInput("feature_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = get_filename("feature", input$geoID, input$feature_fileType))
})

output$feature_downloadData <- downloadHandler(
  filename = function() {
    input$feature_userFileName
  },
  content = function(file) {
    
    save_data(get_data_member("feature", dataname("feature")), file, input$feature_fileType)
  }
)

output$feature_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$feature_userFileName, ".R")
  },
  content = function(file) {
    save_rscript("feature", file, input$feature_userFileName, input$feature_fileType)
  }
)

observeEvent(get_input(nav("5", "4", "feature")), {
  updateTabsetPanel(session, "feature_side_panel", selected = "4")
})
observeEvent(get_input(nav("5", "assay", "feature")), {
  updateTabItems(session, "top_level", "assay_data")
})