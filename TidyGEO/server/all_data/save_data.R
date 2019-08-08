output$all_display_filename <- renderUI({
  extension <- if (input$which_data_to_save == "all") "gzip" else input$all_file_type
  textInput("all_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Annotations.", extension))
})

output$all_evaluate_save <- downloadHandler(
  filename = function() {
    input$all_user_filename
  },
  content = function(file) {
    if (input$which_data_to_save == "clinical") {
      myData <- clinical_vals$clinical_data
      myData <- cbind(rownames(myData), myData)
      colnames(myData)[1] <- if (input$all_file_type == "txt") "ExpId" else ""
    } else if (input$which_data_to_save == "zip") {
      myData <- list(clinical_vals$clinical_data, clinical)
    } else {
      myData <- eval(parse(text = paste0(input$which_data_to_save, "_vals$", clinical, "_data")))
    }
    save_data(myData, file, input$all_file_type)
  }
)

output$all_save_rscript <- downloadHandler(
  filename = function() {
    paste0(input$all_user_filename, ".R")
  },
  content = function(file) {
    save_rscript(input$which_data_to_save, file, input$all_user_filename, input_all_file_type)
  }
)