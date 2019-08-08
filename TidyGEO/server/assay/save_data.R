output$expression_nameFile <- renderUI({
  textInput("expression_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Data.", input$expression_fileType))
})

output$expression_downloadData <- downloadHandler(
  filename = function() {
    input$expression_userFileName
  },
  content = function(file) {
    save_data(assay_vals$assay_data, file, input$expression_fileType)
  }
)

output$expression_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$expression_userFileName, ".R")
  },
  content = function(file) {
    save_rscript("assay", file, input$expression_userFileName, input$expression_fileType)
  }
)