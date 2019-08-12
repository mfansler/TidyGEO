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
help_link <- function(section, id) {
  tipify(actionLink(inputId = paste0(section, "_", id), label = icon("question-circle"), class = id), title = "Click for help", placement = "right", trigger = "hover")
}

regex_help_link <- function(extra_tag) {
  actionLink(
    inputId = paste0("regex_help_", extra_tag), label = div(tags$i("Help/testing"),
                                                   icon("question-circle")), 
    class = "regex_help")
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

undo_button <- function(id) {
  tipify(tertiary_button(id, label = div(icon("undo"), "Undo"), class = "right_align"), 
         title = "Undo the last action.", placement = "bottom", trigger = "hover")
}

reset_button <- function(id) {
  tipify(tertiary_button(id, div(icon("history"), "Reset")), 
         title = "Reset the dataset to its original downloaded state.", placement = "bottom", trigger = "hover")
}

navigation_set <- function(prev, from, to, section_prev = NULL, section_to = NULL) {
  return(div(
    tertiary_button(id = paste0("nav_", from, "_to_", prev, "_", section_prev), label = div(icon('arrow-left'), 'Back')),
    secondary_button(id = paste0("nav_", from, "_to_", to, "_", section_to), label = div('Next', icon('arrow-right')), class = "right_align")
  ))
}