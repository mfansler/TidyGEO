# help icon to add as tag to buttons, etc ---------------------------------


help_button <- function(message = "content", placement = "right") {
  tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover")
}

help_link <- function(id) {
  actionLink(inputId = id, label = icon("question-circle"))
}

help_modal <- function(help_file, images_id = NULL) {
  showModal(
    modalDialog(
      includeMarkdown(help_file),
      conditionalPanel(
        condition = !is.null(images_id),
        uiOutput(images_id)
      ),
      #tags$script(HTML(
      #  "$(document).on('click', '.clickimg', function() {",
      #  "  Shiny.onInputChange('clickimg', $(this).data('value'));",
      #  "});"
      #)),
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

# colored buttons of different types --------------------------------------


primary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(id, div(label, icon), 
                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
}

secondary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(id, div(label, icon), 
                      style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")
}

tertiary_button <- function(id, label, icon = NULL, class = NULL) {
  shiny::actionButton(id, div(label, icon), 
                      style = "color: #fff; background-color: #6baed6; border-color: #6baed6", class = class)
}

# detects variable type & formats string to be written to R script --------


format_string <- function(element) {
  suppressWarnings(if (is.null(element)) {
    return("NULL")
  }
  else if (is.na(element)) {
    return("NA")
  }
  else if (mode(element) == "numeric" ||
           mode(element) == "logical") {
    element <- as.character(element)
  }
  else if (mode(element) == "character") {
    element <-
      sapply(element, function(x) {
        paste0("'", x, "'")
      }, USE.NAMES = FALSE)
  })
  if (length(element) > 1) {
    element <- paste0("c(", paste(element, collapse = ", "), ")")
  }
  return(element)
}

# creates section headings for R script -----------------------------------


commentify <- function(message) {
  
  num_chars <- 75
  comment <- paste0("# ", message, " ")
  comment <- paste0(comment, paste(rep("-", num_chars - nchar(comment)), collapse = ""))
  c("", "", comment, "")
}