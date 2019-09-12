
# Side panel --------------------------------------------------------------

suggested_filename <- reactive({
  if (is.null(input$geoID)) {
    NULL
  } else if (input$which_data_to_save == "zip") {
    paste0(input$geoID, ".", input$zipped_filetype)
  } else {
    get_filename(input$which_data_to_save, input$geoID, input$all_file_type)
  }
})

observe({
  updateTextInput(session, "all_user_filename",
                  value = suggested_filename())
})

filetype_choices <- reactive({
  if (input$which_data_to_save == "all" || # the joined data (will likely contain transposed assay data)
      ("assay" %in% datatypes_selected && get_data_member("assay", id_col) == "colnames")) { # transposed assay data
    SUPPORTED_FILE_TYPES[which(SUPPORTED_FILE_TYPES != "txt")] # BRB Array Tools does not support transposed assay data
    } else {
      SUPPORTED_FILE_TYPES # All file types available when assay data not transposed
    }
})

output$all_file_type_select <- renderUI({
  radioButtons("all_file_type", div("File type:", help_link("all", "files_help")), filetype_choices())
})

output$all_evaluate_save <- downloadHandler(
  contentType = if (input$which_data_to_save == "zip") "application/gzip" else paste0("text/", input$all_file_type),
  filename = function() {
    input$all_user_filename
  },
  content = function(file) {
    
    data_to_save <- lapply(datatypes_selected(), function(dt) {
      if (dt == "clinical") {
        myData <- get_data_member("clinical", dataname("clinical"))
        if (!is.null(myData)) {
          myData <- cbind(rownames(myData), myData)
          colnames(myData)[1] <- if (input$all_file_type == "txt") "ExpId" else ""
        }
        myData
      } else {
        get_data_member(dt, dataname(dt))
      }
    })
    filenames <- if (length(datatypes_selected()) > 1) {
      unlist(lapply(datatypes_selected(), get_filename, input$geoID, input$all_file_type))
    } else {
      NULL
    }
    save_data(data_to_save, file, input$all_file_type, filenames)
  }
)

output$all_save_rscript <- downloadHandler(
  filename = function() {
    paste0(input$all_user_filename, ".R")
  },
  content = function(file) {
    filenames <- if (length(datatypes_selected()) > 1) {
      unlist(lapply(datatypes_selected(), get_filename, input$geoID, input$all_file_type))
    } else {
      input$all_user_filename
    }
    save_rscript(datatypes_selected(), file, filenames, input$all_file_type)
  }
)

observeEvent(get_input(nav("3", "2", "all")), {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})

# Main panel --------------------------------------------------------------

save_tag <- "view_save"

save_data_ui <- tagList(
  h4("Saving datasets"),
  p("Here are the datasets you have selected to save."),
  uiOutput("view_data_save")
)

datatypes_selected <- reactive({
  if (input$which_data_to_save == "zip") c("clinical", "assay", "feature") else input$which_data_to_save
})

data_to_save_is_null <- reactive({
  
  null_dts <- unlist(lapply(datatypes_selected(), function(dt) {
    !data_loaded(dt)
  }))
  if (any(null_dts)) {
    disable("all_evaluate_save")
    disable("all_save_rscript")
  } else {
    enable("all_evaluate_save")
    enable("all_save_rscript")
  }
  null_dts
})

output$view_data_save <- renderUI({
  if (!is.null(input$which_data_to_save)) {
    elements <- 
      lapply(1:length(datatypes_selected()), function(i) {
        if (data_to_save_is_null()[i]) {
          get_null_error_message(datatypes_selected()[i])
        } else {
          div(
            hr(),
            col_navigation_set(datatypes_selected()[i], save_tag),
            br(),
            table_for_col_navigation(datatypes_selected()[i], save_tag)  
          )
        }
      })
    titles <- lapply(datatypes_selected(), function(dt) {
      h4(cap_first(dt))
    })
    tagList(
      c(rbind(titles, elements))
    )
  }
})

table_for_col_navigation_server("clinical", save_tag, show_rownames = TRUE)

table_for_col_navigation_server("assay", save_tag)

table_for_col_navigation_server("feature", save_tag)

table_for_col_navigation_server("all", save_tag, show_rownames = TRUE)
