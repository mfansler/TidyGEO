

# side panel --------------------------------------------------------------

output$col_to_join1_selector <- renderUI({
  col_selector_ui("data_to_join1", "col_to_join1")
})

output$col_to_join2_selector <- renderUI({
  col_selector_ui("data_to_join2", "col_to_join2")
})

output$col_to_join3_selector <- renderUI({
  col_selector_ui("data_to_join3", "col_to_join3")
})

observeEvent(input$add_dataset, {
  all_vals$join_datatypes_visible <- all_vals$join_datatypes_visible + 1
  this_id <- paste0("data_to_join", all_vals$join_datatypes_visible)
  this_selector_id <- paste0("col_to_join", all_vals$join_datatypes_visible)
  insertUI(
    selector = "#add_dataset",
    where = "beforeBegin",
    ui = div(id = paste0("selector_div", all_vals$join_datatypes_visible),
      selectInput(this_id, "Choose another dataset to join:", choices = c("clinical", "assay", "feature")),
      uiOutput(paste0(this_selector_id, "_selector")),
      radioButtons(paste0("join_behavior_", all_vals$join_datatypes_visible), 
                   paste0("Please choose the action you would like to take when items in the first dataset",
                                           " are not present in the second dataset (and visa versa):"), 
                   choices = c("drop", "keep values from first dataset", "keep values from second dataset", "keep all"))
    )
  )
  if (all_vals$join_datatypes_visible > 2) disable("add_dataset")
  enable("remove_dataset")
  
  insertUI(
    selector = "#data_to_join_previews",
    where = "beforeEnd",
    ui = div(id = paste0("preview_div", all_vals$join_datatypes_visible),
      column(1,
             br(),
             br(),
             br(),
             br(),
             icon("expand-arrows-alt", class = "center_align")
      ),
      column(4,
             DTOutput(paste0("data_to_join", all_vals$join_datatypes_visible, "_preview")),
             uiOutput(paste0("data_to_join", all_vals$join_datatypes_visible, "_rows"))
      )
    )
  )
})

observeEvent(input$remove_dataset, {
  removeUI(
    selector = paste0("#selector_div", all_vals$join_datatypes_visible)
  )
  removeUI(
    selector = paste0("#preview_div", all_vals$join_datatypes_visible)
  )
  all_vals$join_datatypes_visible <- all_vals$join_datatypes_visible - 1
  enable("add_dataset")
  if (all_vals$join_datatypes_visible < 2) disable("remove_dataset")
})


# main panel --------------------------------------------------------------

join_dfs_ui <- tagList(
  h4("Joining datasets"),
  p("Here is a preview of the first ten rows of each column you have selected to join."),
  br(),
  fluidRow(id = "data_to_join_previews",
    column(4,
           DTOutput("data_to_join1_preview"),
           uiOutput("data_to_join1_rows")
    )
  ),
  uiOutput("join_results_preview")
)

get_data_to_join_preview <- function(datatype, selected_col) {
  this_data <- eval(parse(
    text = paste0("colnames(", datatype, "_vals$", datatype, '_data)')
  ))
  font_weights <- sapply(this_data, function(x) if (x == selected_col) paste0("<b>", x, "</b>") else x,
                         USE.NAMES = FALSE)
  matrix(font_weights)
}

get_data_to_join_rows <- function(datatype) {
  eval(parse(
    text = paste0("nrow(", datatype, "_vals$", datatype, '_data)')
  ))
}

data_to_join1_data <- reactive({
  if (!is.null(input$col_to_join1)) {
    get_data_to_join_preview(input$data_to_join1, input$col_to_join1)
  }
})

output$data_to_join1_preview <- renderDT({
  datatable(data_to_join1_data(), rownames = FALSE, colnames = input$data_to_join1, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join1_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join1), "</p>"))
})

data_to_join2_data <- reactive({
  if (!is.null(input$col_to_join2)) {
    get_data_to_join_preview(input$data_to_join2, input$col_to_join2)
  }
})

output$data_to_join2_preview <- renderDT({
  datatable(data_to_join2_data(), rownames = FALSE, colnames = input$data_to_join2, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join2_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join2), "</p>"))
})

data_to_join3_data <- reactive({
  if (!is.null(input$col_to_join3)) {
    get_data_to_join_preview(input$data_to_join3, input$col_to_join3)
  }
})

output$data_to_join3_preview <- renderDT({
  datatable(data_to_join3_data(), rownames = FALSE, colnames = input$data_to_join3, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join3_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join3), "</p>"))
})

output$join_results_preview <- renderUI({
  #Keep all: add the rows up
  #keep values from data1: the number of rows in data1
  #keep values from data2: the number of rows in data2
  #resulting_rows <- if (input$join_behavior == "drop") {
  #  # the minimum of 1 %in% 2 and 2 %in% 1
  #} else if (input$join_behavior == "keep values from data1") {
  #  get_data_to_join_rows(input$data_to_join1)
  #} else if (input$join_behavior == "keep values from data2") {
  #  get_data_to_join_rows(eval(parse(text = paste0("input$data_to_join", all_vals$join_datatypes_visible))))
  #} else {
  #  get_data_to_join_rows(input$data_to_join1) + get_data_to_join_rows(input$data_to_join2) + get_data_to_join_rows(input$data_to_join3)
  #}
  HTML(
    paste0(
      "<p><b>Resulting number of columns: </b>", length(data_to_join1_data()) + length(data_to_join2_data()) + length(data_to_join3_data()), "</p>"
    )#,
    #paste0(
    #  "<p><b>Estimated number of rows: </b>", length(data_to_join1_data()) + length(data_to_join2_data()) + length(data_to_join3_data()), "</p>"
    #)
  )
})