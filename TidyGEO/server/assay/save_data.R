output$expression_nameFile <- renderUI({
  textInput("expression_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = get_filename("assay", input$geoID, input$expression_fileType))
})

assay_file_options <- reactive({
  if (get_data_member("assay", "id_col") == "colnames") SUPPORTED_FILE_TYPES[which(SUPPORTED_FILE_TYPES != "txt")] else SUPPORTED_FILE_TYPES
})

output$assay_file_type_select <- renderUI({
  radioButtons("expression_fileType", div("File type:", help_link("assay", "files_help")), 
               choices = assay_file_options())
})

output$expression_downloadData <- downloadHandler(
  filename = function() {
    input$expression_userFileName
  },
  content = function(file) {
    save_data(get_data_member("assay", dataname("assay")), file, input$expression_fileType)
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

observeEvent(get_input(nav("2", "1", "assay")), {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})
observeEvent(get_input(nav("assay", "all")), {
  updateTabItems(session, "top_level", "all_data")
})