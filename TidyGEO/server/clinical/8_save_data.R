output$clinical_display_filename <- renderUI({
  textInput("clinical_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = get_filename("clinical", input$geoID, input$clinical_file_type))
})

output$clinical_evaluate_save <- downloadHandler(
  filename = function() {
    input$clinical_user_filename
  },
  content = function(file) {
    myData <- clinical_vals[[dataname("clinical")]]
    myData <- cbind(rownames(myData), myData)
    colnames(myData)[1] <- if (input$clinical_file_type == "txt") "ExpId" else ""
    save_data(clinical_vals[[dataname("clinical")]], file, input$clinical_file_type)
  }
)

output$clinical_save_rscript <- downloadHandler(
  filename = function() {
    paste0(input$clinical_user_filename, ".R")
  },
  content = function(file) {
    save_rscript("clinical", file, input$clinical_user_filename, input$clinical_file_type)
  }
)