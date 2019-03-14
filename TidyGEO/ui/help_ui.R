# button icons ------------------------------------------------------------


help_button <- function(message = "content", placement = "right") {
  tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover")
}

help_link <- function(id) {
  tipify(actionLink(inputId = id, label = icon("question-circle")), title = "Click for help", placement = "right", trigger = "hover")
}

# help modal --------------------------------------------------------------


help_modal <- function(help_file, images_id = NULL) {
  showModal(
    modalDialog(
      includeMarkdown(help_file),
      conditionalPanel(
        condition = !is.null(images_id),
        uiOutput(images_id)
      ),
      footer = modalButton("Close"),
      size = "l"
    )
  )
}


# creating an image grid for help modals ----------------------------------


create_image_grid <- function(images, image_names) {
  fluidRow(
    mapply(function(my_image, img_name) {
      column(3, 
             div(tags$img(src = my_image, width = "200px", class = "clickimg", "data-value" = my_image), img_name)
      )
    }, images, image_names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
}