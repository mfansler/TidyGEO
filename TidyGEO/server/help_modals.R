# helper functions --------------------------------------------------------


#' Create a pop-up box with instructions.
#'
#' @param help_file .
#' @param images_id .
#' @return .
#' @examples
#' help_modal("My_Help_File.md", "my_images_id")
help_modal <- function(help_file, images = NULL, image_names = NULL) {
  #start_time <- Sys.time()
  showModal(
    modalDialog(
      includeMarkdown(help_file),
      conditionalPanel(
        condition = !is.null(images),
        #uiOutput(images_id)
        create_image_grid(images, image_names)
      ),
      footer = modalButton("Close"),
      size = "l"
    )
  )
  #end_time <- Sys.time()
  #print(paste("Creating help modal", end_time - start_time))
}

#' .
#'
#' @param images .
#' @param image_names .
#' @return .
#' @examples
#' create_image_grid(c("image.gif", "image2.gif"), c("Image", "Image 2"))
create_image_grid <- function(images, image_names) {
  #start_time <- Sys.time()
  image_grid <- fluidRow(
    mapply(function(my_image, img_name) {
      column(3, 
             div(tags$img(src = my_image, width = "200px", class = "clickimg", "data-value" = my_image), img_name)
      )
    }, images, image_names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
  #end_time <- Sys.time()
  #print(paste("Creating image grid", end_time - start_time))
  return(image_grid)
}

# listeners ---------------------------------------------------------------


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
  source(file.path("server", "regex_modal.R"), local = TRUE)$value
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

observeEvent(input$regex_help_split_pairs, {
  source(file.path("server", "regex_modal.R"), local = TRUE)$value
})

observeEvent(input$regex_help_split_cols, {
  source(file.path("server", "regex_modal.R"), local = TRUE)$value
})

# enlarge image listener --------------------------------------------------


observeEvent(input$clickimg, {
  #start_time <- Sys.time()
  showModal(
    modalDialog(
      tags$img(src = input$clickimg, width = "100%", height = "100%"),
      size = "l"
      )
    )
  #end_time <- Sys.time()
  #print(paste("Creating enlarged image modal", end_time - start_time))
  session$sendCustomMessage("resetValue", "clickimg")
}, ignoreInit = TRUE)