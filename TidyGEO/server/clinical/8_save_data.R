output$clinical_display_filename <- renderUI({
  textInput("clinical_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = get_filename("clinical", input$geoID, input$clinical_file_type))
})

output$clinical_evaluate_save <- downloadHandler(
  filename = function() {
    input$clinical_user_filename
  },
  content = function(file) {
    myData <- get_data_member("clinical", dataname("clinical"))
    myData <- cbind(rownames(myData), myData)
    colnames(myData)[1] <- if (input$clinical_file_type == "txt") "ExpId" else ""
    save_data(myData, file, input$clinical_file_type)
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

observeEvent(get_input(nav("8", "7", "clinical")), {
  updateTabsetPanel(session, "clinical_side_panel", selected = "7")
})
observeEvent(get_input(nav("clinical", "assay")), {
  updateTabItems(session, "top_level", "assay_data")
})