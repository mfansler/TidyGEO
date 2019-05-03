# help buttons ------------------------------------------------------------


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

# action buttons ----------------------------------------------------------


primary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", class = class)
}

secondary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
                      style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f", class = class)
}

tertiary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
                      style = "color: #fff; background-color: #6baed6; border-color: #6baed6", class = class)
}