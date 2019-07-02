# Create divs
output$histograms_expression <- renderUI({
  
  if (!is.null(assay_in_view())) {
    plot_output_list <- lapply(1:ncol(assay_in_view()), function(i) {
      plotname <- make.names(colnames(assay_in_view())[i])
      div(withSpinner(plotlyOutput(plotname, height = 500, width = "auto"), type = 5), tertiary_button(paste0("savePlot", i), div(icon("download"), "Download plot"), class = "expression_plot"))
    })   
    do.call(tagList, plot_output_list)
  }
})
# Create the actual plots associated with the plot names
observe({
  if (!is.null(assay_in_view())) {
    lapply(1:ncol(assay_in_view()), function(i){
      output[[ make.names(colnames(assay_in_view())[i]) ]] <- renderPlotly({
        suppressWarnings(create_plot(as.character(assay_in_view()[,i]), input$expr_plot_color, input$expression_binwidths, colnames(assay_in_view())[i], is_numeric = isAllNum(assay_in_view()[i])))
      })
    })
  }
})

observeEvent(input$last_btn_expression, {
  assay_vals$plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_expression, "savePlot")))
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
    paste(make.names(colnames(assay_in_view())[assay_vals$plot_to_save]), input$expr_plot_filetype, sep = ".")
  },
  content = function(file) {
    
    plot_to_save <- create_plot_to_save(as.character(assay_in_view()[,assay_vals$plot_to_save]), 
                                        input$expr_plot_color, 
                                        input$expression_binwidths, 
                                        colnames(assay_in_view())[assay_vals$plot_to_save], 
                                        is_numeric = isAllNum(assay_in_view()[assay_vals$plot_to_save]))
    
    ggsave(file, plot_to_save, width = input$expr_plot_width, height = input$expr_plot_height, device = input$expr_plot_filetype)
    
  }
)