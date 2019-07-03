
# ui ----------------------------------------------------------------------


observeEvent(input$expression_replace_id, {
  if (!is.null(values$allData) && is.null(feature_vals$feature_data)) {
    get_feature_data()
  }
  showModal(
    modalDialog(
      #HTML('<p>In the <b>assay data</b> table, you will see a column labeled <b>"ID"</b>. This is a unique identifier that usually
      #  corresponds to the <b>probe set</b> used to take some measurement for the given sample ("GSM"). 
      #  Probe sets may not be a useful way to look at patterns in the data.
      #  Often, scientists want to look at <b>genes, transcripts, or exons,</b> for example. The <b>feature data</b> maps
      #  the probe set identifiers (the "ID" column) to their corresponding genes, transcripts, exons, etc.
      #  Frequently, <b>multiple probe sets refer to the same gene.</b>
      #  </p>'), 
      #p(div(em('"Expression profiling analysis usually generates quantitative data for features of interest. 
      #      Features of interest may be genes, transcripts, exons, miRNA, or some other genetic entity."'), 
      #      a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/seq.html", "(GEO)"))),
      HTML('<p>In this window, you can find a column in the feature data'), actionLink("link_to_feature", "(what is this?)"), HTML('that would be more useful than the
        probe set identifiers.  In the case where multiple probe sets refer to the same gene, you might consider 
        combining some of the rows in the assay data (i.e. <b>"summarizing"</b> the data) to keep the "ID" column 
        in the assay data table unique.</p>'),
      wellPanel(
        h4("Feature data", inline = TRUE),
        fluidRow(
          column(1, secondary_button(id = "feature_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
          column(1, offset = 8, secondary_button(id = "feature_next_cols", label = div("Next columns", icon("arrow-right"))))
        ),
        withSpinner(dataTableOutput("featureData"), type = 5),
        actionLink("link_to_feature_tab2", "How do I clean up this data?")
      ),
      uiOutput("exprLabels"),
      uiOutput("summarizeOptions"),
      checkboxInput(inputId = "feature_dropNA", label = div("Drop NA values", 
                                                            help_button(paste("This drops the NA values from the",
                                                                              "column you choose before replacing",
                                                                              "the assay data ID column. This ensures",
                                                                              "that there are no blank entries in",
                                                                              "the ID column.")))),
      
      footer = primary_button(id = "expression_evaluate_id", label = "Replace ID column"), 
      title = div("Choose a different column to use as the ID column", tertiary_button("close_feature_modal", "Cancel", class = "right_align")),
      size = "l",
      easyClose = TRUE
    )
  )
})

observeEvent(input$link_to_feature, {
  removeModal()
  updateTabItems(session, "top_level", "feature_data")
})

observeEvent(input$link_to_feature2, {
  removeModal()
  updateTabItems(session, "top_level", "feature_data")
})

# server ------------------------------------------------------------------


observeEvent(input$close_feature_modal, {
  removeModal()
})

feature_move_by <- reactive({
  ncol(feature_vals$feature_data) / 5
})

observeEvent(input$feature_next_cols, {
  if (!is.null(feature_vals$feature_data) && ncol(feature_vals$feature_data) > 6) {
    start <- min(ncol(feature_vals$feature_data), feature_vals$viewing_subset[1] + feature_move_by())
    end <- min(ncol(feature_vals$feature_data), start + feature_move_by())
    feature_vals$viewing_subset <- c(start, end)
  }
})

observeEvent(input$feature_prev_cols, {
  if (!is.null(feature_vals$feature_data) && ncol(feature_vals$feature_data) > 6) {
    end <- max(2, feature_vals$viewing_subset[2] - feature_move_by())
    start <- max(2, end - feature_move_by())
    feature_vals$viewing_subset <- c(start, end)
  }
})

output$featureData <- DT::renderDT({
  if (!is.null(feature_vals$feature_data)) {
    datatable(feature_vals$feature_data[c(1, feature_vals$viewing_subset[1]:feature_vals$viewing_subset[2])], 
              filter = "top", rownames = FALSE, options = list(dom = "tp",
                                                               pageLength = 5,
                                                               columnDefs = list(list(
                                                                 targets = "_all",
                                                                 ##Makes it so that the table will only display the first 30 chars.
                                                                 ##See https://rstudio.github.io/DT/options.html
                                                                 render = JS(
                                                                            "function(data, type, row, meta) {",
                                                                            "return type === 'display' && typeof data === 'string' && data.length > 15 ?",
                                                                            "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                                            "}")
                                                                            ))))
  }
  else {
    datatable(feature_vals$ft_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})

output$exprLabels <- renderUI({
  selectInput("colForExprLabels", label = div("Please select a column that will identify each gene/transcript/exon/etc.", 
                                              help_button("To keep the same ID column, please choose ID.")), 
              choices = colnames(feature_vals$feature_data)[which(!colnames(feature_vals$feature_data) == "ID")]
  )
})

output$summarizeOptions <- renderUI({
  if (!is.null(input$colForExprLabels) && input$colForExprLabels != "") {
    new_expression_labels <- if (input$feature_dropNA) 
      feature_vals$feature_data[!is.na(input$colForExprLabels), input$colForExprLabels] else feature_vals$feature_data[, input$colForExprLabels]
    #browser()
    can_summarize <- !is_all_unique(new_expression_labels)
    if (can_summarize) {
      choices <- if (assay_vals$warning_state) c("keep all", "mean", "median", "max", "min") else c("keep all")
      selectInput("howToSummarize", label = div("It looks like this column maps to multiple ID values in the assay data.
                                                How would you like to summarize the data?", 
                                                help_button("Groups the data by ID and takes the specified measurement for the group.
                                                            Please note that if you choose 'keep all' you will not be able to transpose
                                                            the data.")), 
                  choices = choices)
    }
    
  }
  })

observeEvent(input$expression_evaluate_id, {
  
  removeModal()
  
  if (!is.null(assay_vals$assay_data) && feature_vals$id_col != input$colForExprLabels) {
    assay_vals$last_data <- assay_vals$assay_data
    
    feature_data <- feature_vals$feature_data
    
    assay_vals$oFile <- saveLines(c(commentify("replace ID column"),
                                    "featureData2 <- featureData"), assay_vals$oFile)
    
    if (feature_vals$id_col != "ID") {
      
      assay_vals$oFile <- saveLines(
        c(paste0("colnames(featureData2)[which(colnames(featureData2) == 'ID')] <- ", 
                 format_string(colnames(assay_vals$orig_feature[which(colnames(feature_data) == "ID")]))),
          paste0("colnames(featureData2)[which(colnames(featureData2) == ", 
                 format_string(feature_vals$id_col), ")] <- 'ID'")), 
        assay_vals$oFile)
      
      colnames(feature_data)[which(colnames(feature_data) == "ID")] <- 
        colnames(assay_vals$orig_feature[which(colnames(feature_data) == "ID")])
      colnames(feature_data)[which(colnames(feature_data) == feature_vals$id_col)] <- "ID"
      
      if (length(which(colnames(feature_data) == "ID")) > 1) {
        assay_vals$oFile <- saveLines("featureData2 <- featureData2[,-1]", assay_vals$oFile)
        
        #You probably shouldn't be doing this every time
        feature_data <- feature_data[,-1]
      }
    }
    
    assay_vals$assay_data <- withProgress(message = "Replacing the ID column", 
                                          replaceID(assay_vals$assay_data, feature_data, input$colForExprLabels, input$howToSummarize, input$feature_dropNA))
    feature_vals$prev_id <- feature_vals$id_col
    feature_vals$id_col <- input$colForExprLabels
    
    before <- length(assay_vals$oFile)
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    assay_vals$oFile <- saveLines(c(paste0("expressionData <- replaceID(expressionData, featureData2, ", 
                                           format_string(input$colForExprLabels), ", ",
                                           format_string(input$howToSummarize), ", ", 
                                           format_string(input$feature_dropNA), ")")), 
                                  assay_vals$oFile)
    
    after <- length(assay_vals$oFile)
    
    assay_vals$current_chunk_len <- after - before
  }
  
  #updateSelectInput(session, "howToSummarize", selected = "keep all")
  
  #shinyjs::disable("expression_replace_id")
  
})