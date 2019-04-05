observeEvent(input$split_help, {
  help_modal("help_docs/Split_Vars_Documentation.md", "split_images")
})
output$split_images <- renderUI({
  images <- c("separate_example.gif")
  image_names <- c("Separate Columns Demo")
  
  create_image_grid(images, image_names)
})

observeEvent(input$divide_help, {
  help_modal("help_docs/Divide_Vars_Documentation.md", "divide_images")
})
output$divide_images <- renderUI({
  images <- c("divide_example.gif")
  image_names <- c("Divide Columns Demo")
  
  create_image_grid(images, image_names)
})

observeEvent(input$substitute_help, {
  help_modal("help_docs/Substitute_Vals_Documentation.md", "substitute_images")
})
output$substitute_images <- renderUI({
  images <- c("substitute_example.gif")
  image_names <- c("Substitute Values Demo")
  
  create_image_grid(images, image_names)
})

observeEvent(input$regex_help, {
  help_modal("help_docs/Regular_Expressions_Documentation.md")
})

observeEvent(input$exclude_help, {
  help_modal("help_docs/Exclude_Vals_Documentation.md", "exclude_images")
})
output$exclude_images <- renderUI({
  images <- c("exclude_example.gif")
  image_names <- c("Exclude Values Demo")
  
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
  help_modal("help_docs/Different_ID_Documentation.md")
})

observeEvent(input$transpose_help, {
  help_modal("help_docs/Transpose_Documentation.md")
})

observeEvent(input$filter_help, {
  help_modal("help_docs/Filter_Data_Documentation.md")
})

observeEvent(input$evaluate_filters_help, {
  help_modal("help_docs/Apply_Filters_Documentation.md")
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