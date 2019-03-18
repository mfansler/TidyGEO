output$expr_select_binwidths <- renderUI({
  if (is.null(values$expr_to_display) || !values$expression_warning_state) {
    upper_lim <- 10
  } else {
    upper_lim <- ceiling(max(values$expr_to_display[,which(colnames(values$expr_to_display) != "ID")], na.rm = TRUE))
  }
  sliderInput("expression_binwidths", "Width of bars (for numeric):", min = 0, max = upper_lim, value = ceiling(upper_lim / 2))
})

# Create divs
output$histograms_expression <- renderUI({
  
  if (!is.null(values$expr_to_display)) {
    plot_output_list <- lapply(2:ncol(values$expr_to_display), function(i) {
      plotname <- make.names(colnames(values$expr_to_display)[i])
      div(withSpinner(plotlyOutput(plotname, height = 500, width = "auto"), type = 5), tertiary_button(paste0("savePlot", i), "Download plot", class = "expression_plot"))
    })   
    do.call(tagList, plot_output_list)
  }
})
# Create the actual plots associated with the plot names
observe({
  if (!is.null(values$expr_to_display)) {
    lapply(2:ncol(values$expr_to_display), function(i){
      output[[ make.names(colnames(values$expr_to_display)[i]) ]] <- renderPlotly({
        suppressWarnings(create_plot(as.character(values$expr_to_display[,i]), input$expr_plot_color, input$expression_binwidths, colnames(values$expr_to_display)[i], is_numeric = isAllNum(values$expr_to_display[i])))
      })
    })
  }
})

observeEvent(input$last_btn_expression, {
  values$expr_plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_expression, "savePlot")))
  showModal(
    modalDialog(
      sliderInput("expr_plot_width", label = "Image width (inches):", min = 1, max = 36, value = 6),
      sliderInput("expr_plot_height", label = "Image height (inches):", min = 1, max = 36, value = 6),
      radioButtons("expr_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
      downloadButton("expr_plot_download"),
      footer = modalButton("Close")
    ))
})

output$expr_plot_download <- downloadHandler(
  filename = function() {
    paste(make.names(colnames(values$expr_to_display)[values$expr_plot_to_save]), input$expr_plot_filetype, sep = ".")
  },
  content = function(file) {
    
    plot_to_save <- create_plot_to_save(as.character(values$expr_to_display[,values$expr_plot_to_save]), 
                                        input$expr_plot_color, 
                                        input$expression_binwidths, 
                                        colnames(values$expr_to_display)[values$expr_plot_to_save], 
                                        is_numeric = isAllNum(values$expr_to_display[values$expr_plot_to_save]))
    
    ggsave(file, plot_to_save, width = input$expr_plot_width, height = input$expr_plot_height, device = input$expr_plot_filetype)
    
  }
)