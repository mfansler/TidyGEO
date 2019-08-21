
# side panel --------------------------------------------------------------

observe({
  #start_time <- Sys.time()
  updateSelectizeInput(
    session = session, 'geoID', server = TRUE,
    choices = SERIES_LIST,
    options = list(render = I(
      '{
      option: function(item, escape) {
      return "<div><strong>" + escape(item.label) + "</strong> " + escape(item.name) + " </div>"
      }
}'),
      create = TRUE,
      multiple = FALSE,
      maxItems = 5,
      maxOptions = 100,
      placeholder = "Select a dataset or start typing...",
      closeAfterSelect = TRUE
    )
  )
  #end_time <- Sys.time()
  #print(paste("Populating dropdown", end_time - start_time))
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
      platform_description <- if (any(str_detect(PLATFORM_LIST$Accession, id)))
        PLATFORM_LIST$description[which(PLATFORM_LIST$Accession == id)] else ""
      platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", id), 
                                   id), LINK_OUT_ICON, 
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
  clinical_vals[[dataname("clinical")]] <- NULL
  assay_vals[[dataname("assay")]] <- NULL
  feature_vals[[dataname("feature")]] <- NULL
  # WRITING COMMANDS TO R SCRIPT
  for (datatype in ALLOWED_DATATYPES) {
    message <- if (datatype == "all") {
      "No datasets have been joined. Please join some datasets." 
    } else {
      paste("Please load some", datatype, "data.")
    }
    set_x_equalto_y("display_default", data.frame(message), datatype)
    
    add_function("load_series", datatype)
    set_reset_point_script(datatype)
  }
  write_to_header(commentify("load series"), overwrite = TRUE)
  write_to_header(paste0("dataSetIndex <- ", format_string(input$platformIndex)))
  write_to_header(c(paste0("geoID <- ", format_string(input$geoID)), 
               "series_data <- load_series(geoID, dataSetIndex)"))
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
  
  updateTabItems(session, 'top_level', selected = 'clinical_data')
})

observeEvent(input$nav_choose_to_assay_button, {
  values$series_needs_download = is.null(values$allData)
  updateTabItems(session, 'top_level', selected = 'assay_data')
})

# main panel --------------------------------------------------------------

output$series_information <- renderUI({
  if (is.null(values$series_information)) {
    div(
      h4("Series information"),
      p("Please choose a GSE ID from the select box to view a summary of the series here."),
      imageOutput("color_logo")
    )
  } else {
    values$series_information
  }
})

observeEvent(input$geoID, {
  if (input$geoID != "") {
    values$series_information <- withProgress({
      tempFile <- file.path(tempdir(), paste0(input$geoID, "__ncbi.txt"))
      url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID, "&targ=self&form=text&view=quick")
      if (!file.exists(tempFile)) {
        download.file(url, tempFile, method = "auto", quiet = TRUE)
      }
      summ2 <- suppressMessages(suppressWarnings(read_file(tempFile)))
      
      values$pm_id <- str_match(summ2, "!Series_pubmed_id = ([^\\r]+)")[2]
      if (values$paper_info_expanded) {
        values$paper_information <- NULL
        updateButton(session, inputId = "expand_paper_information", label = "View publication information", icon = icon("caret-right"))
        values$paper_info_expanded <- FALSE
      }
      
      div(h3(paste(input$geoID, str_match(summ2, "!Series_title = ([^\\r]+)")[2], sep = ": ")),
          HTML(paste0("<p><b>Organism:</b> ", str_match(summ2, "!Series_platform_organism = ([^\\r]+)")[2], "</p>")),
          HTML(paste0("<p><b>Experiment type:</b> ", str_match(summ2, "!Series_type = ([^\\r]+)")[2], "</p>")),
          HTML(paste0("<p><b>Summary:</b> ", str_match(summ2, "!Series_summary = ([^\\r]+)")[2], "</p>")),
          p(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID), 
              paste("View", input$geoID, "on GEO")), LINK_OUT_ICON),
          hr(),
          tertiary_button("expand_paper_information", div(icon("caret-right"), "View publication information")))
    }, message = "Loading series summary")
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

output$paper_information <- renderUI({
  values$paper_information
})

observeEvent(input$expand_paper_information, {
  if (values$paper_info_expanded) { # close
    values$paper_information <- NULL
    updateButton(session, inputId = "expand_paper_information", icon = icon("caret-right"))
    values$paper_info_expanded <- FALSE
  } else { # open
    values$paper_information <- withProgress({
      if (is.na(values$pm_id)) {
        div(
          br(),
          p(paste0("No publication information found for ", input$geoID, "."))
        )
      } else {
        tempFile <- file.path(tempdir(), paste0(input$geoID, "__abstract.txt"))
        if (!file.exists(tempFile)) {
          url <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/", values$pm_id, "?report=medline&format=text")
          download.file(url, tempFile, method = "auto", quiet = TRUE)
        }
        summ3 <- suppressMessages(suppressWarnings(read_file(tempFile)))
        div(
          br(),
          HTML(paste0("<p><b>Paper:</b> ",
                      str_match(summ3, "TI  - ([^\\r]+)")[2], 
                      " (PMID: <a target= \"_blank\" href=\"https://www.ncbi.nlm.nih.gov/pubmed/", values$pm_id, "\">", values$pm_id, ")</a></p>")),
          HTML(paste0("<p><b>Abstract: </b>", str_replace_all(str_match(str_remove_all(summ3, "\\r\\n"), "AB  - (.+)\\.FAU")[2], " {2,}", " "), ".</p>"))
        )
      }
    }, message = "Loading paper information")
    updateButton(session, inputId = "expand_paper_information", label = icon = icon("caret-down"))
    values$paper_info_expanded <- TRUE
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

output$color_logo <- renderImage({
  list(src="www/logo_final.png",
       contentType="image/png",
       width = "80%")
}, deleteFile = FALSE)

