#output$metaSummary <- renderText({printVarsSummary(clinical_in_view())})

output$choose_variable_to_view <- renderUI({
  if (!is.null(clinical_in_view())) {
    choices <- 1:length(colnames(clinical_in_view()))
    choice_names <- colnames(clinical_in_view())
    #if (nrow(clinical_in_view()) < 50) {
      choices <- c(0, choices)
      choice_names <- c("(view all)", choice_names)
    #}
    names(choices) <- choice_names
    
    selectInput("variable_to_view", label = "Choose a variable to view:", choices = choices)
  }
})

if (FALSE) {
  # Create the list of plot names
  plotInput <- reactive({
    if (!is.null(clinical_in_view())) {
      n_plot <- ncol(clinical_in_view())
      total_data <- lapply(1:n_plot, function(i){as.character(clinical_in_view()[,i])})
      return(list("n_plot" = n_plot, "total_data" = total_data))
    }
  })
}

# Create divs
output$plots <- renderUI({
  
  if (!is.null(clinical_in_view())) {
    plot_output_list <- lapply(1:ncol(clinical_in_view()), function(i) {
      #if (!grepl("evalSame", colnames(clinical_in_view())[i])) {
      if (is_all_identical(clinical_in_view()[,i])) {
        div(hr(), HTML(
          paste0("<b>", colnames(clinical_in_view())[i], "</b> consists of all the same value.")
        ), hr())
      } else if (is_all_unique(clinical_in_view()[,i]) && !isAllNum(clinical_in_view()[i])) {
        div(hr(), HTML(
          paste0("<b>", colnames(clinical_in_view())[i], "</b> consists of all unique values.")
        ), hr())
      } else {
        plotname <- make.names(colnames(clinical_in_view())[i])
        div(withSpinner(plotlyOutput(plotname, height = 700, width = "auto"), type = 5), tertiary_button(paste0("savePlot", i), div(icon("download"), "Download plot"), class = "clinical_plot"))
      }
      #}
    })
    if (!is.null(input$variable_to_view)) {
      if (as.numeric(input$variable_to_view) == 0) {
        do.call(tagList, plot_output_list)
      } else {
        #print(plot_output_list[5][[1]])
        plot_output_list[as.numeric(input$variable_to_view)][[1]]
      }
    }
  }
})
# Create the actual plots associated with the plot names
observe({
  if (!is.null(clinical_in_view())) {
    lapply(1:ncol(clinical_in_view()), function(i){
      if (!is_all_identical(clinical_in_view()[,i])) {
        is_all_numeric <- isAllNum(clinical_in_view()[i])
        if (!is_all_unique(clinical_in_view()[,i]) || is_all_numeric) {
          output[[ make.names(colnames(clinical_in_view())[i]) ]] <- renderPlotly({
            suppressWarnings(create_plot(as.character(clinical_in_view()[,i]), input$clinical_plot_color, input$clinical_binwidths, colnames(clinical_in_view())[i], is_all_numeric))
          })
        }
      }
    })
  }
})

observeEvent(input$last_btn_clinical, {
  if (!is.null(input$last_btn_clinical)) {
    clinical_vals$plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_clinical, "savePlot")))
    showModal(
      modalDialog(
        sliderInput("clinical_plot_width", label = "Image width (cm):", min = 20, max = 100, value = 60),
        sliderInput("clinical_plot_height", label = "Image height (cm):", min = 20, max = 100, value = 60),
        radioButtons("clinical_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
        downloadButton("clinical_plot_download"),
        footer = modalButton("Close")
      ))
    session$sendCustomMessage(type = "resetValue", "last_btn_clinical")
  }
})

output$clinical_plot_download <- downloadHandler(
  filename = function() {
    paste(make.names(colnames(clinical_in_view())[clinical_vals$plot_to_save]), input$clinical_plot_filetype, sep = ".")
  },
  content = function(file) {
    plot_to_save <- create_plot_to_save(#plotInput()$total_data[[clinical_vals$plot_to_save]],
      clinical_in_view()[,clinical_vals$plot_to_save], 
      input$clinical_plot_color, 
      input$clinical_binwidths, 
      colnames(clinical_in_view())[clinical_vals$plot_to_save], 
      isAllNum(clinical_in_view()[clinical_vals$plot_to_save]))
    
    ggsave(file, plot_to_save, width = input$clinical_plot_width, height = input$clinical_plot_height, units = "cm", device = input$clinical_plot_filetype)
    
  }
)