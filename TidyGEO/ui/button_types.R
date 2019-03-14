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