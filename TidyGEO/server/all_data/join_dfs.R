

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
      radioButtons(paste0("join_behavior", all_vals$join_datatypes_visible), 
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
  fluidRow(
    column(4,
      box(
        div(textOutput("selected_to_join1"), style = "text-align: center"),
        background = "light-blue"
      )
    ),
    column(4,
      box(
        div(textOutput("selected_to_join2"), style = "text-align: center"),
        background = "light-blue"
      )
    ),
    column(4,
      box(
        div(textOutput("selected_to_join3"), style = "text-align: center"),
        background = "light-blue"
      )
    )
  ),
  fluidRow(
    column(1,
      br()
    ),
    column(4,
           div(textOutput("selected_join_var1"), style = "text-align: center"),
      style = "border-left: 6px solid; border-right: 6px solid; border-bottom: 6px solid"
    ),
    column(4,
           br(),
           style = "border-right: 6px solid;")
  ),
  fluidRow(
    column(3,
           br()),
    column(6,
           div(textOutput("selected_join_var2"), style = "text-align: center"),
      style = "border-left: 6px solid; border-right: 6px solid; border-bottom: 6px solid;"
    )
  ),
  br(),
  #p("Here is a preview of the first ten rows of each column you have selected to join."),
  #br(),
  #fluidRow(id = "data_to_join_previews",
  #  column(4,
  #         DTOutput("data_to_join1_preview"),
  #         uiOutput("data_to_join1_rows")
  #  )
  #),
  uiOutput("join_results_preview")
)

output$selected_to_join1 <- renderText({
  input$data_to_join1
})

output$selected_to_join2 <- renderText({
  input$data_to_join2
})

output$selected_to_join3 <- renderText({
  input$data_to_join3
})

output$selected_join_var1 <- renderText({
  paste0(input$col_to_join1, " = ", input$col_to_join2)
})

output$selected_join_var2 <- renderText({
  paste0(input$col_to_join2, " = ", input$col_to_join3)
})

if (FALSE) {

get_data_to_join_rows <- function(datatype) {
  eval(parse(
    text = paste0("nrow(", datatype, "_vals$", datatype, '_data)')
  ))
}

output$data_to_join1_preview <- renderDT({
  datatable(data_to_join1_data(), rownames = FALSE, colnames = input$data_to_join1, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join1_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join1), "</p>"))
})



output$data_to_join2_preview <- renderDT({
  datatable(data_to_join2_data(), rownames = FALSE, colnames = input$data_to_join2, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join2_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join2), "</p>"))
})

output$data_to_join3_preview <- renderDT({
  datatable(data_to_join3_data(), rownames = FALSE, colnames = input$data_to_join3, escape = FALSE, 
            options = list(dom = "t", scrollY = 200))
})

output$data_to_join3_rows <- renderUI({
  HTML(paste0("<p><b>Rows: </b>", get_data_to_join_rows(input$data_to_join3), "</p>"))
})

}


get_data_to_join_preview <- function(datatype, selected_col) {
  if (FALSE) {
  this_data <- if (selected_col %in% "colnames") {
    selected_col <- "ID"
    eval(parse(text = paste0("withProgress(colnames(quickTranspose(", datatype, "_vals$", datatype, "_data)))")))
  } else {
    eval(parse(
      text = paste0("colnames(", datatype, "_vals$", datatype, '_data)')
    ))
  }
  font_weights <- sapply(this_data, function(x) if (x == selected_col) paste0("<b>", x, "</b>") else x,
                         USE.NAMES = FALSE)
  matrix(font_weights)
  }
  this_func <- if (selected_col == "colnames") "row" else "col"
  eval(parse(text = paste0("n", this_func, "(", datatype, "_vals$", datatype, "_data)")))
}

data_to_join1_data <- reactive({
  if (!is.null(input$col_to_join1)) {
    get_data_to_join_preview(input$data_to_join1, input$col_to_join1)
  } else 0
})
data_to_join2_data <- reactive({
  if (!is.null(input$col_to_join2)) {
    get_data_to_join_preview(input$data_to_join2, input$col_to_join2)
  } else 0
})
data_to_join3_data <- reactive({
  if (!is.null(input$col_to_join3)) {
    get_data_to_join_preview(input$data_to_join3, input$col_to_join3)
  } else 0
})

output$join_results_preview <- renderUI({
  HTML(
    paste0(
      "<p><b>Resulting number of columns: </b>", data_to_join1_data() + data_to_join2_data() + data_to_join3_data(), "</p>"
    )
  )
})

observeEvent(input$join_columns, {
  all_vals$all_data <- input$data_to_join1
  withProgress({
    incProgress(message = "Performing first join")
    if (all_vals$join_datatypes_visible > 1) {
      all_vals$all_data <- join_data(
        eval(parse(text = paste0(input$data_to_join1, "_vals$", input$data_to_join1, "_data"))), 
        eval(parse(text = paste0(input$data_to_join2, "_vals$", input$data_to_join2, "_data"))), 
        input$col_to_join1, 
        input$col_to_join2, 
        input$join_behavior2
      )
    }
    incProgress(message = "Performing second join")
    if (all_vals$join_datatypes_visible > 2) {
      all_vals$all_data <- join_data(
        all_vals$all_data, 
        eval(parse(text = paste0(input$data_to_join3, "_vals$", input$data_to_join3, "_data"))), 
        input$col_to_join2, 
        input$col_to_join3, 
        input$join_behavior3
      )
    }
  })
})