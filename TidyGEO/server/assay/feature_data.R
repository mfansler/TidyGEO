
# ui ----------------------------------------------------------------------


observeEvent(input$expression_replace_id, {
  if (!is.null(values$allData) && !data_loaded("feature")) {
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
        col_navigation_set("feature", "modal"),
        #fluidRow(
        #  column(1, secondary_button(id = "feature_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
        #  column(1, offset = 8, secondary_button(id = "feature_next_cols", label = div("Next columns", icon("arrow-right"))))
        #),
        table_for_col_navigation("feature", "modal"),
        #withSpinner(dataTableOutput("featureData"), type = 5),
        actionLink("link_to_feature2", "How do I clean up this data?")
      ),
      uiOutput("exprLabels"),
      uiOutput("summarizeOptions"),
      uiOutput("dropNA_if_exists"),
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

#feature_move_by <- reactive({
#  ncol(feature_vals$feature_data) / 5
#})

#observeEvent(input$feature_next_cols, {
#  if (!is.null(feature_vals$feature_data) && ncol(feature_vals$feature_data) > 6) {
#    start <- min(ncol(feature_vals$feature_data), feature_vals$viewing_subset[1] + feature_move_by())
#    end <- min(ncol(feature_vals$feature_data), start + feature_move_by())
#    feature_vals$viewing_subset <- c(start, end)
#  }
#})

#observeEvent(input$feature_prev_cols, {
#  if (!is.null(feature_vals$feature_data) && ncol(feature_vals$feature_data) > 6) {
#    end <- max(2, feature_vals$viewing_subset[2] - feature_move_by())
#    start <- max(2, end - feature_move_by())
#    feature_vals$viewing_subset <- c(start, end)
#  }
#})

table_for_col_navigation_server("feature", "modal")

output$exprLabels <- renderUI({
  selectInput("colForExprLabels", label = div("Please select a column that will identify each gene/transcript/exon/etc.", 
                                              help_button("To keep the same ID column, please choose ID.")), 
              choices = colnames(get_data_member("feature", dataname("feature")))[which(!colnames(get_data_member("feature", dataname("feature"))) == "ID")]
  )
})

new_assay_labels <- reactive({
  if (!is.null(input$colForExprLabels) && input$colForExprLabels != "") {
    get_data_member("feature", dataname("feature"))[, input$colForExprLabels]
  }
})

output$summarizeOptions <- renderUI({
  if (!is.null(input$colForExprLabels) && input$colForExprLabels != "") {
    can_summarize <-
      if (!is.null(input$feature_dropNA) && input$feature_dropNA) {
        !is_all_unique(new_assay_labels()[!is.na(new_assay_labels()),])
      } else {
        !is_all_unique(new_assay_labels())
      }
    if (can_summarize) {
      choices <- if (get_data_member("assay", "warning_state")) c("keep all", "mean", "median", "max", "min") else c("keep all")
      selectInput("howToSummarize", label = div("It looks like this column maps to multiple ID values in the assay data.
                                                How would you like to summarize the data?", 
                                                help_button(paste("Groups the data by ID and takes the specified measurement for the group.",
                                                            "Please note that if you choose 'keep all', there will be duplicate entries",
                                                            "in the ID column. These entries will be made unique by appending a '.[number]'",
                                                            "on the end of them."))), 
                  choices = choices)
    }
    
  }
  })

output$dropNA_if_exists <- renderUI({
  if (!is.null(new_assay_labels()) && any(is.na(new_assay_labels()))) {
    checkboxInput(inputId = "feature_dropNA", label = div("Drop NA values", 
                                                          help_button(paste("This drops the NA values from the",
                                                                            "column you choose before replacing",
                                                                            "the assay data ID column. This ensures",
                                                                            "that there are no blank entries in",
                                                                            "the ID column."))))
  }
})

observeEvent(input$expression_evaluate_id, {
  
  removeModal()
  
  if (data_loaded("assay") && get_data_member("feature", "id_col") != input$colForExprLabels) {
    set_x_equalto_y("last_data", get_data_member("assay", dataname("assay")), "assay")
    
    set_undo_point_script("assay")
    knit_scripts("assay", "feature", "assay")
    save_lines(c(commentify("replace ID column"),
                 paste0(dataname("feature"), "2 <- ", dataname("feature"))), "assay", "body")
    
    feature_data <- get_data_member("feature", dataname("feature"))
    
    if (feature_vals$id_col != "ID") {
      save_lines(
        c(paste0("colnames(", dataname("feature"), "2)[which(colnames(", dataname("feature"), "2) == 'ID')] <- ", 
                 format_string(colnames(get_data_member("feature", "orig_data")[which(colnames(feature_data) == "ID")]))),
          paste0("colnames(", dataname("feature"), "2)[which(colnames(", dataname("feature"), "2) == ", 
                 format_string(get_data_member("feature", "id_col")), ")] <- 'ID'")), 
        "assay", "body")
      
      colnames(feature_data)[which(colnames(feature_data) == "ID")] <- 
        colnames(get_data_member("feature", "orig_data")[which(colnames(feature_data) == "ID")])
      colnames(feature_data)[which(colnames(feature_data) == get_data_member("feature", "id_col"))] <- "ID"
      
      if (length(which(colnames(feature_data) == "ID")) > 1) {
        save_lines(paste0(dataname("feature"), "2 <- ", dataname("feature"), "2[,-1]"), "assay", "body")
        
        #You probably shouldn't be doing this every time
        feature_data <- feature_data[,-1]
      }
    }
    
    result <- withProgress(message = "Replacing the ID column", 
                                          replaceID(get_data_member("assay", dataname("assay")), feature_data, input$colForExprLabels, input$howToSummarize, input$feature_dropNA))
    
    set_x_equalto_y(dataname("assay"), result, "assay")
    set_x_equalto_y("prev_id", get_data_member("feature", "id_col"), "feature")
    set_x_equalto_y("id_col", input$colForExprLabels, "feature")
    
    shinyjs::enable("undoEvalExpr")
    
    add_function("replaceID", "assay")
    save_lines(c(paste0(dataname("assay"), " <- replaceID(", dataname("assay"), ", ", dataname("feature"), "2, ", 
                        format_string(input$colForExprLabels), ", ",
                        format_string(input$howToSummarize), ", ", 
                        format_string(input$feature_dropNA), ")")
                 ), 
               "assay", "body")
  }
  
})