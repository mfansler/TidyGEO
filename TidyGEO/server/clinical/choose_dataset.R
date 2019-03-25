saveFileDescription <- function(geoID) {
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
      HTML(paste0("<p><b>Summary:</b> ", str_split(str_extract(summ2, "!Series_summary = [^!]+"), " = ")[[1]][2], "</p>")))
}

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

observe({
  values$series_needs_download
  if (values$series_needs_download && !is.null(input$geoID) && input$geoID != "") {
    values$allData <- withProgress(
      load_series(input$geoID, input$platformIndex, session = session), 
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
    
    #extracted_data <- NULL
    clinical_vals$orig_data <- clinical_vals$clinical_data
  }
})

output$series_information_description <- renderUI({
  if (!is.null(input$geoID)) {
    div(
      p(
        paste0("Here is the webpage for ", input$geoID, ", where you can preview some information about the
               dataset.")), "To view this webpage in a separate browser, click ",
      a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID), 
        " here."), icon("external-link"))
  } else {
    p("Please enter a GEO ID to preview some information about the dataset here.")
  }
})

output$series_information <- renderUI({
  if (!is.null(input$geoID) && input$geoID != "") {
    saveFileDescription(input$geoID)
  }
})

output$platform_options <- renderUI({
  if (!is.null(input$geoID) && input$geoID != "") {
    platforms <- get_platforms(input$geoID, session)
    if (!is.null(platforms)) {
      platform_links <- list()
      for (i in 1:length(platforms)) {
        id <- str_remove(platforms[i], "GSE\\d+-")
        id <- str_remove(id, "_series_matrix.txt.gz")
        platform_description <- if (any(str_detect(platform_list$Accession, id)))
          platform_list$description[which(platform_list$Accession == id)] else ""
        platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", id), 
                                     id), icon("external-link"), 
                                   em(platform_description))
      }
      
      if (length(platforms) > 0) {
        radioButtons(inputId = "platformIndex", label = "Which platform file would you like to use?", 
                     choiceNames = platform_links, 
                     choiceValues = platforms)
      }
    }
  }
})

observeEvent(input$download_data_evaluate, {
  if (is.null(input$geoID) || input$geoID == "") {
    values$errorState <- TRUE
    createAlert(session, "alert", "inputError", title = "Error",
                content = "Please specify a GSE ID.", append = FALSE)
  }
  else {
    closeAlert(session, "inputError")
    closeAlert(session, "fileError")
    values$errorState <- FALSE
    
    #platforms <- get_platforms(input$geoID, session)
    #if (!is.null(platforms)) {
    #  platform_links <- list()
    #  for (i in 1:length(platforms)) {
    #    id <- str_remove(platforms[i], "GSE\\d+-")
    #    id <- str_remove(id, "_series_matrix.txt.gz")
    #    platform_description <- if (any(str_detect(platform_list$Accession, id)))
    #      platform_list$description[which(platform_list$Accession == id)] else ""
    #    platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", id), 
    #                                 id), icon("external-link"), 
    #                               em(platform_description))
    #  }
    #  
    #  if (length(platforms) > 0) {
    #    showModal(modalDialog(radioButtons(inputId = "platformIndex", label = "Which platform file would you like to use?", 
    #                                       choiceNames = platform_links, 
    #                                       choiceValues = platforms), 
    #                          footer = primary_button(id = "usePlatform", label = "Use platform"), size = "s",
    #                          easyClose = TRUE))
    #    if (length(platforms) == 1) {
    #      click("usePlatform")
    #    }
    #  }
    #}
  }
})

observeEvent(input$usePlatform, {
  print(input$platformIndex)
  
  removeModal()
  
  values$allData <- withProgress(
    load_series(input$geoID, input$platformIndex, session = session), 
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
  
  #extracted_data <- NULL
  clinical_vals$orig_data <- clinical_vals$clinical_data
})

#observeEvent(input$load_clinical, {
#  clinical_vals$clinical_data <- withProgress(process_clinical(values$allData, input$platformIndex, input$download_data_filter, session))
#})
observe({
  input$top_level
  if (input$top_level == "Clinical data" && is.null(clinical_vals$clinical_data) && !is.null(values$allData)) {
    clinical_vals$clinical_data <- withProgress(process_clinical(values$allData, session))
    
    #WRITING COMMANDS TO R SCRIPT
    clinical_vals$oFile <- saveLines(commentify("extract clinical data"), clinical_vals$oFile)
    clinical_vals$oFile <- saveLines("clinical_data <- process_clinical(series_data)", clinical_vals$oFile)
    clinical_vals$download_chunk_len <- length(clinical_vals$oFile)
  }
})