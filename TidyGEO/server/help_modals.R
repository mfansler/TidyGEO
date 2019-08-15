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
      size = "l",
      easyClose = TRUE
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

observeEvent(input$shift_help_clicked, {
  images <- c("shift_cells_example.gif")
  image_names <- c("Demo - Shift Cells")
  
  help_modal("help_docs/Shift_Cells_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "shift_help_clicked")
})

observeEvent(input$split_help_clicked, {
  images <- c("separate_example.gif")
  image_names <- c("Demo - Separate Columns")
  
  help_modal("help_docs/Split_Vars_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "split_help_clicked")
})

observeEvent(input$divide_help_clicked, {
  images <- c("divide_example.gif")
  image_names <- c("Demo - Divide Columns")
  
  help_modal("help_docs/Divide_Vars_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "divide_help_clicked")
})

observeEvent(input$substitute_help_clicked, {
  images <- c("substitute_example.gif", "substitute_numeric_example.gif")
  image_names <- c("Demo - Substitute Values", "Demo - Substitute Range")
  
  help_modal("help_docs/Substitute_Vals_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "substitute_help_clicked")
})

observeEvent(input$regex_help_clicked, {
  values$regex_dt <- allowed_datatypes[which(str_detect(input$regex_help_clicked, allowed_datatypes))]
  if (identical(values$regex_dt, character(0))) {
    stop("Error in opening regex modal. The button that called the modal is not tagged with a valid datatype.")
  } else {
    source(file.path("server", "regex_modal.R"), local = TRUE)$value
  }
  session$sendCustomMessage("resetValue", "regex_help_clicked")
})

observeEvent(input$exclude_help_clicked, {
  images <- c("exclude_example.gif", "exclude_numeric_example.gif")
  image_names <- c("Demo - Exclude Values", "Demo - Exclude Range")
  
  help_modal("help_docs/Exclude_Vals_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "exclude_help_clicked")
})

observeEvent(input$download_help_clicked, {
  images <- c("download_example.gif")
  image_names <- c("Demo - Load Series")
  
  help_modal("help_docs/Download_Data_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "download_help_clicked")
})

observeEvent(input$r_help_clicked, {
  help_modal("help_docs/R_Help_Documentation.md")
  session$sendCustomMessage("resetValue", "r_help_clicked")
})

observeEvent(input$replace_id_help_clicked, {
  images <- c("different_id_example.gif")
  image_names <- c("Demo - Replacing the ID Column")
  
  help_modal("help_docs/Different_ID_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "replace_id_help_clicked")
})

observeEvent(input$transpose_help_clicked, {
  images <- c("transpose_example.gif")
  image_names <- c("Demo - Transpose Columns and Rows")
  
  help_modal("help_docs/Transpose_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "transpose_help_clicked")
})
observeEvent(input$filter_help_clicked, {
  images <- c("filter_presets_example.gif", "filter_by_name_example.gif")
  image_names <- c("Demo - Filter using Presets", "Demo - Filter by Name")
  
  help_modal("help_docs/Filter_Data_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "filter_help_clicked")
})

observeEvent(input$evaluate_filters_help_clicked, {
  images <- c("apply_filters_example.gif")
  image_names <- c("Demo - Apply Filters")
  
  help_modal("help_docs/Apply_Filters_Documentation.md", images, image_names)
  session$sendCustomMessage("resetValue", "evaluate_filters_help_clicked")
})

observeEvent(input$files_help_clicked, {
  help_modal("help_docs/File_Types_Documentation.md")
  session$sendCustomMessage("resetValue", "files_help_clicked")
})

observeEvent(input$zip_files_help_clicked, {
  # https://file.org/extension/gz
  # https://www.google.com/search?q=how+to+open+tar.gz+on+windows+10&rlz=1C1SQJL_enUS777US777&oq=how+to+open+tar.gz+&aqs=chrome.3.0l2j69i57j0l3.7999j0j7&sourceid=chrome&ie=UTF-8
  # https://stackoverflow.com/questions/20762094/how-are-zlib-gzip-and-zip-related-what-do-they-have-in-common-and-how-are-they
})

# enlarge image listener --------------------------------------------------


observeEvent(input$clickimg, {
  #start_time <- Sys.time()
  showModal(
    modalDialog(
      tags$img(src = input$clickimg, width = "100%", height = "100%"),
      size = "l",
      easyClose = TRUE
      )
    )
  #end_time <- Sys.time()
  #print(paste("Creating enlarged image modal", end_time - start_time))
  session$sendCustomMessage("resetValue", "clickimg")
}, ignoreInit = TRUE)