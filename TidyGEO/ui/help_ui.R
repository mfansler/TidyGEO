# button icons ------------------------------------------------------------

#' Create a question icon with a tooltip.
#'
#' @param message The content of the tooltip.
#' @param placement Relative to the question icon: "right", "left", "bottom", or "top".
#' @return A question icon with a tooltip to use next to buttons or labels.
#' @examples
#' help_button("Here is some extra help.", "top")
help_button <- function(message = "content", placement = "right") {
  tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover")
}

#' Create a clickable question icon.
#' This icon leads to a help modal in a listener with the same ID.
#' @param id The ID for the input object to which the observeEvent for the modal is listening.
#' @return A clickable question icon that leads to a help modal.
#' @examples
#' help_link("my_help_link")
help_link <- function(id) {
  tipify(actionLink(inputId = id, label = icon("question-circle")), title = "Click for help", placement = "right", trigger = "hover")
}

# help modal --------------------------------------------------------------

#' Create a pop-up box with instructions.
#'
#' @param help_file .
#' @param images_id .
#' @return .
#' @examples
#' help_modal("My_Help_File.md", "my_images_id")
help_modal <- function(help_file, images = NULL, image_names = NULL) {
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
}


# creating an image grid for help modals ----------------------------------

#' .
#'
#' @param images .
#' @param image_names .
#' @return .
#' @examples
#' create_image_grid(c("image.gif", "image2.gif"), c("Image", "Image 2"))
create_image_grid <- function(images, image_names) {
  fluidRow(
    mapply(function(my_image, img_name) {
      column(3, 
             div(tags$img(src = my_image, width = "200px", class = "clickimg", "data-value" = my_image), img_name)
      )
    }, images, image_names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
}