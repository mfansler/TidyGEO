observeEvent(input$split_help, {
  images <- c("separate_example.gif")
  image_names <- c("Demo - Separate Columns")
  
  help_modal("help_docs/Split_Vars_Documentation.md", images, image_names)
})

observeEvent(input$divide_help, {
  images <- c("divide_example.gif")
  image_names <- c("Demo - Divide Columns")
  
  help_modal("help_docs/Divide_Vars_Documentation.md", images, image_names)
})

observeEvent(input$substitute_help, {
  images <- c("substitute_example.gif", "substitute_numeric_example.gif")
  image_names <- c("Demo - Substitute Values", "Demo - Substitute Range")
  
  help_modal("help_docs/Substitute_Vals_Documentation.md", images, image_names)
})

observeEvent(input$regex_help, {
  help_modal("help_docs/Regular_Expressions_Documentation.md")
})

observeEvent(input$exclude_help, {
  images <- c("exclude_example.gif", "exclude_numeric_example.gif")
  image_names <- c("Demo - Exclude Values", "Demo - Exclude Range")
  
  help_modal("help_docs/Exclude_Vals_Documentation.md", images, image_names)
})

observeEvent(input$download_help, {
  images <- c("download_example.gif")
  image_names <- c("Demo - Load Series")
  
  help_modal("help_docs/Download_Data_Documentation.md", images, image_names)
})

observeEvent(input$clinical_r_help, {
  help_modal("help_docs/R_Help_Documentation.md")
})

observeEvent(input$replace_id_help, {
  images <- c("different_id_example.gif")
  image_names <- c("Demo - Replacing the ID Column")
  
  help_modal("help_docs/Different_ID_Documentation.md", images, image_names)
})

observeEvent(input$transpose_help, {
  images <- c("transpose_example.gif")
  image_names <- c("Demo - Transpose Columns and Rows")
  
  help_modal("help_docs/Transpose_Documentation.md", images, image_names)
})
observeEvent(input$filter_help, {
  images <- c("filter_presets_example.gif", "filter_by_name_example.gif")
  image_names <- c("Demo - Filter using Presets", "Demo - Filter by Name")
  
  help_modal("help_docs/Filter_Data_Documentation.md", images, image_names)
})

observeEvent(input$evaluate_filters_help, {
  images <- c("apply_filters_example.gif")
  image_names <- c("Demo - Apply Filters")
  
  help_modal("help_docs/Apply_Filters_Documentation.md", images, image_names)
})

observeEvent(input$expression_r_help, {
  help_modal("help_docs/R_Help_Documentation.md")
})

observeEvent(input$clinical_files_help, {
  help_modal("help_docs/File_Types_Documentation.md")
})

observeEvent(input$expression_files_help, {
  help_modal("help_docs/File_Types_Documentation.md")
})

observeEvent(input$clickimg, {
  showModal(
    modalDialog(
      tags$img(src = input$clickimg, width = "100%", height = "100%"),
      size = "l"
      )
    )
}, ignoreInit = TRUE)