
# side panel --------------------------------------------------------------

observe({
  start_time <- Sys.time()
  updateSelectizeInput(
    session = session, 'geoID', server = TRUE,
    choices = series_list,
    options = list(render = I(
      '{
      option: function(item, escape) {
      return "<div><strong>" + escape(item.label) + "</strong> " + escape(item.name) + " </div>"
      }
}'),
      create = TRUE,
      multiple = FALSE,
      maxItems = 5,
      maxOptions = 100
    )
  )
  end_time <- Sys.time()
  print(paste("Populating dropdown", end_time - start_time))
})

platforms <- reactive({
  if (!is.null(input$geoID) && input$geoID != "") {
    session$sendCustomMessage('resetValue', 'platformIndex')
    get_platforms(input$geoID, session)
  }
})

output$platform_options <- renderUI({
  
  closeAlert(session, "fileError")
  
  if (length(platforms()) > 1) {
    platform_links <- list()
    for (i in 1:length(platforms())) {
      id <- str_remove(platforms()[i], "GSE\\d+-")
      id <- str_remove(id, "_series_matrix.txt.gz")
      platform_description <- if (any(str_detect(platform_list$Accession, id)))
        platform_list$description[which(platform_list$Accession == id)] else ""
      platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", id), 
                                   id), icon("external-link"), 
                                 em(platform_description))
    }
    radioButtons(inputId = "platformIndex", label = "Which platform file would you like to use?", 
                 choiceNames = platform_links, 
                 choiceValues = platforms())
  }
})

observeEvent(input$download_data_evaluate, {
  
  platform_index <- if (length(platforms()) == 1) platforms()[1] else input$platformIndex
  
  values$allData <- withProgress(
    load_series(input$geoID, platform_index, session = session), 
    message = "Downloading series data from GEO")
  clinical_vals$clinical_data <- NULL
  assay_vals$assay_data <- NULL
  
  #WRITING COMMANDS TO R SCRIPT
  clinical_vals$oFile <- saveLines(commentify("load series"), clinical_vals$oFile)
  clinical_vals$oFile <- saveLines(paste0("dataSetIndex <- ", format_string(input$platformIndex)), clinical_vals$oFile)
  clinical_vals$oFile <- saveLines(c(paste0("geoID <- ", format_string(input$geoID)), "series_data <- load_series(geoID, dataSetIndex)"), clinical_vals$oFile)
  clinical_vals$download_chunk_len <- length(clinical_vals$oFile)
  
  assay_vals$oFile <- clinical_vals$oFile
  assay_vals$download_chunk_len <- clinical_vals$download_chunk_len
})

observe({
  shinyjs::toggleState("download_data_evaluate", condition = !is.null(input$geoID) && input$geoID != "")
})

observe({
  shinyjs::toggleState(id = "nav_choose_to_assay_button", condition = !is.null(values$allData))
})

observe({
  values$allData
  shinyjs::toggleState(id = "nav_choose_to_clinical_button", condition = !is.null(values$allData))
})

observeEvent(input$nav_choose_to_clinical_button, {
  values$series_needs_download = is.null(values$allData)
  updateTabsetPanel(session, 'top_level', selected = 'Clinical data')
})

observeEvent(input$nav_choose_to_assay_button, {
  values$series_needs_download = is.null(values$allData)
  updateTabsetPanel(session, 'top_level', selected = 'Assay data')
})

# main panel --------------------------------------------------------------


parse_series_summary <- function(geoID) {
  #uses read_file and parses from resulting vector
  tempFile <- file.path(tempdir(), paste0(geoID, "__ncbi.txt"))
  url <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geoID, "&targ=self&form=text&view=quick", sep = "")
  if (!file.exists(tempFile)) {
    download.file(url, tempFile, method = "auto", quiet = TRUE)
  }
  summ2 <- suppressMessages(suppressWarnings(read_file(tempFile)))
  div(h3(paste(geoID, str_split(str_extract(summ2, "!Series_title = [^!]+"), " = ")[[1]][2], sep = ": ")),
      HTML(paste0("<p><b>Organism:</b> ", str_split(str_extract(summ2, "!Series_platform_organism = [^!]+"), " = ")[[1]][2], "</p>")),
      HTML(paste0("<p><b>Experiment type:</b> ", str_split(str_extract(summ2, "!Series_type = [^!]+"), " = ")[[1]][2], "</p>")),
      HTML(paste0("<p><b>Summary:</b> ", str_split(str_extract(summ2, "!Series_summary = [^!]+"), " = ")[[1]][2], "</p>")),
      a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geoID), 
        paste("View", geoID, "on GEO")), icon("external-link"))
}

output$series_information <- renderUI({
  if (is.null(input$geoID) || input$geoID == "") {
    div(
      h4("Series information"),
      p("Please choose a GSE ID from the select box to view a summary of the series here."),
      imageOutput("color_logo")
    )
  } else {
    withProgress(parse_series_summary(input$geoID), message = "Loading series summary")
  }
})

output$color_logo <- renderImage({
  list(src="www/logo_final.png",
       contentType="image/png",
       width = "80%")
}, deleteFile = FALSE)

