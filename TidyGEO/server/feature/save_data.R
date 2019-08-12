output$feature_nameFile <- renderUI({
  textInput("feature_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = get_filename("feature", input$geoID, input$feature_fileType))
})

output$feature_downloadData <- downloadHandler(
  filename = function() {
    input$feature_userFileName
  },
  content = function(file) {
    
    save_data(feature_vals$feature_data, file, input$feature_fileType)
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