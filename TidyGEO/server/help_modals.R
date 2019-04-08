observeEvent(input$split_help, {
  help_modal("help_docs/Split_Vars_Documentation.md", "split_images")
})
output$split_images <- renderUI({
  images <- c("separate_example.gif")
  image_names <- c("Demo - Separate Columns")
  
  create_image_grid(images, image_names)
})

observeEvent(input$divide_help, {
  help_modal("help_docs/Divide_Vars_Documentation.md", "divide_images")
})
output$divide_images <- renderUI({
  images <- c("divide_example.gif")
  image_names <- c("Demo - Divide Columns")
  
  create_image_grid(images, image_names)
})

observeEvent(input$substitute_help, {
  help_modal("help_docs/Substitute_Vals_Documentation.md", "substitute_images")
})
output$substitute_images <- renderUI({
  images <- c("substitute_example.gif", "substitute_numeric_example.gif")
  image_names <- c("Demo - Substitute Values", "Demo - Substitute Range")
  
  create_image_grid(images, image_names)
})

observeEvent(input$regex_help, {
  help_modal("help_docs/Regular_Expressions_Documentation.md")
})

observeEvent(input$exclude_help, {
  help_modal("help_docs/Exclude_Vals_Documentation.md", "exclude_images")
})
output$exclude_images <- renderUI({
  images <- c("exclude_example.gif", "exclude_numeric_example.gif")
  image_names <- c("Demo - Exclude Values", "Demo - Exclude Range")
  
  create_image_grid(images, image_names)
})

observeEvent(input$download_help, {
  help_modal("help_docs/Download_Data_Documentation.md", "download_images")
})
output$download_images <- renderUI({
  images <- c("download_example.gif")
  image_names <- c("Demo - Load Series")
  
  create_image_grid(images, image_names)
})

observeEvent(input$clinical_r_help, {
  help_modal("help_docs/R_Help_Documentation.md")
})

observeEvent(input$replace_id_help, {
  help_modal("help_docs/Different_ID_Documentation.md", "different_id_images")
})

output$different_id_images <- renderUI({
  images <- c("different_id_example.gif")
  image_names <- c("Demo - Replacing the ID Column")
  
  create_image_grid(images, image_names)
})

observeEvent(input$transpose_help, {
  help_modal("help_docs/Transpose_Documentation.md", "transpose_images")
})

output$transpose_images <- renderUI({
  images <- c("transpose_example.gif")
  image_names <- c("Demo - Transpose Columns and Rows")
  
  create_image_grid(images, image_names)
})

observeEvent(input$filter_help, {
  help_modal("help_docs/Filter_Data_Documentation.md", "select_cols_images")
})

output$select_cols_images <- renderUI({
  images <- c("filter_presets_example.gif", "filter_by_name_example.gif")
  image_names <- c("Demo - Filter using Presets", "Demo - Filter by Name")
  
  create_image_grid(images, image_names)
})

observeEvent(input$evaluate_filters_help, {
  help_modal("help_docs/Apply_Filters_Documentation.md", "apply_filters_images")
})

output$apply_filters_images <- renderUI({
  images <- c("apply_filters_example.gif")
  image_names <- c("Demo - Apply Filters")
  
  create_image_grid(images, image_names)
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
  showModal(modalDialog({
    tags$img(src = input$clickimg, width = "100%", height = "100%")
  },
  size = "l"
  ))
}, ignoreInit = TRUE)