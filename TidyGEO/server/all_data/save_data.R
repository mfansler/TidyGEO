
# Side panel --------------------------------------------------------------


output$all_display_filename <- renderUI({
  file_name <- if (input$which_data_to_save == "zip") {
    paste0(input$geoID, ".", input$zipped_filetype)
    } else {
      get_filename(input$which_data_to_save, input$geoID, input$all_file_type)
    }
  textInput("all_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = file_name)
})

observe({
})

output$all_evaluate_save <- downloadHandler(
  contentType = if (input$which_data_to_save == "zip") "application/gzip" else paste0("text/", input$all_file_type),
  filename = function() {
    input$all_user_filename
  },
  content = function(file) {
    
    data_to_save <- lapply(datatypes_selected(), function(dt) {
      if (dt == "clinical") {
        myData <- clinical_vals$clinical_data
        if (!is.null(myData)) {
          myData <- cbind(rownames(myData), myData)
          colnames(myData)[1] <- if (input$all_file_type == "txt") "ExpId" else ""
        }
        myData
      } else {
        eval(parse(text = paste0(dt, "_vals$", dt, "_data")))
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
    save_rscript(input$which_data_to_save, file, input$all_user_filename, input_all_file_type)
  }
)

# Main panel --------------------------------------------------------------

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
    eval(parse(text = paste0("is.null(", dt, "_vals$", dt, "_data)")))
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
          DTOutput(paste0(datatypes_selected()[i], "_view_save"))
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

output$clinical_view_save <- renderDT({
  clinical_vals$clinical_data
}, options = dt_opts)

output$assay_view_save <- renderDT({
  assay_vals$assay_data
}, options = dt_opts)

output$feature_view_save <- renderDT({
  feature_vals$feature_data
}, options = dt_opts)

output$all_view_save <- renderDT({
  all_vals$all_data
}, options = dt_opts)
