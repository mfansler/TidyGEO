# Create divs
output$histograms_feature <- renderUI({
  
  if (!is.null(feature_in_view())) {
    plot_output_list <- lapply(1:ncol(feature_in_view()), function(i) {
      plotname <- paste0("feature_", make.names(colnames(feature_in_view())[i]))
      div(withSpinner(plotlyOutput(plotname, height = 500, width = "auto"), type = 5), tertiary_button(paste0("feature_savePlot", i), div(icon("download"), "Download plot"), class = "feature_plot"))
    })   
    do.call(tagList, plot_output_list)
  }
})
# Create the actual plots associated with the plot names
observe({
  if (!is.null(feature_in_view())) {
    lapply(1:ncol(feature_in_view()), function(i){
      output[[ paste0("feature_", make.names(colnames(feature_in_view())[i])) ]] <- renderPlotly({
        suppressWarnings(create_plot(as.character(feature_in_view()[,i]), input$feature_plot_color, input$feature_binwidths, colnames(feature_in_view())[i], is_numeric = isAllNum(feature_in_view()[i])))
      })
    })
  }
})

observeEvent(input$last_btn_feature, {
  feature_vals$plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_feature, "feature_savePlot")))
  showModal(
    modalDialog(
      sliderInput("feature_plot_width", label = "Image width (inches):", min = 1, max = 36, value = 6),
      sliderInput("feature_plot_height", label = "Image height (inches):", min = 1, max = 36, value = 6),
      radioButtons("feature_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
      downloadButton("feature_plot_download"),
      footer = modalButton("Close")
    ))
})

output$feature_plot_download <- downloadHandler(
  filename = function() {
    paste(make.names(colnames(feature_in_view())[feature_vals$plot_to_save]), input$feature_plot_filetype, sep = ".")
  },
  content = function(file) {
    
    plot_to_save <- create_plot_to_save(as.character(feature_in_view()[,feature_vals$plot_to_save]), 
                                        input$feature_plot_color, 
                                        input$feature_binwidths, 
                                        colnames(feature_in_view())[feature_vals$plot_to_save], 
                                        is_numeric = isAllNum(feature_in_view()[feature_vals$plot_to_save]))
    
    ggsave(file, plot_to_save, width = input$feature_plot_width, height = input$feature_plot_height, device = input$feature_plot_filetype)
    
  }
)