# creates observers for a navigation set
navigation_set_server <- function(prev, from, to, section_prev = NULL, section_to = NULL) {
  observeEvent(eval(parse(text = paste0("input$nav_", from, "_to_", prev, "_", section_prev))), {
    updateTabsetPanel(session, section_prev, selected = prev)
  })
  observeEvent(eval(parse(text = paste0("input$nav_", from, "_to_", to, "_", section_to))), {
    updateTabsetPanel(session, section_to, selected = to)
  })
}

col_navigation_set_server <- function(datatype, extra_tag = "") {
  observe({
    n_cols <- ncol(get_data_member(datatype, paste0(datatype, "_data")))
    this_viewing_min <- get_data_member(datatype, "viewing_min")
    set_x_equalto_y("use_viewing_subset", !is.null(n_cols) && n_cols > 19, datatype)
    set_x_equalto_y("viewing_subset", c(this_viewing_min, min(this_viewing_min + 5, n_cols)), datatype)
  }, priority = 1)
  
  assign(paste0(datatype, "_in_view"), reactive({
    if (get_data_member(datatype, "use_viewing_subset")) {
      this_viewing_subset <- get_data_member(datatype, "viewing_subset")
      cols_to_view <- this_viewing_subset[1]:this_viewing_subset[2]
      if (get_data_member(datatype, "viewing_min") == 2) {
        cols_to_view <- c(1, cols_to_view)
      }
      get_data_member(datatype, paste0(datatype, "_data"))[cols_to_view]
    } else {
      get_data_member(datatype, paste0(datatype, "_data"))
    }
  }), pos = parent.frame())
  
  output[[paste0("cols_visible_", datatype, "_", extra_tag)]] <- renderText({
    paste("Showing", 
          get_data_member(datatype, "viewing_subset")[1], "to", 
          get_data_member(datatype, "viewing_subset")[2], "of", 
          ncol(get_data_member(datatype, paste0(datatype, "_data"))), 
          "columns")
  })
}


# Clinical side panel -----------------------------------------------------

observeEvent(input$clinical_to_choose_button, {
  updateTabItems(session, "top_level", "choose_dataset")
})
observeEvent(input$clinical_1_to_2_button, {
  updateTabsetPanel(session, "clinical_side_panel", selected = "2")
})
navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("3", "4", "5", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("4", "5", "6", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
navigation_set_server("6", "7", "8", "clinical_side_panel", "clinical_side_panel")
observeEvent(input$clinical_8_to_7_button, {
  updateTabsetPanel(session, "clinical_side_panel", selected = "7")
})
observeEvent(input$clinical_to_assay_data, {
  updateTabItems(session, "top_level", "assay_data")
})

# Expression side panel ---------------------------------------------------

observeEvent(input$assay_to_clinical, {
  updateTabItems(session, "top_level", "clinical_data")
})
observeEvent(input$expression_nav_1_to_2_button, {
  updateTabsetPanel(session, "expression_side_panel", selected = "2")
})
observeEvent(input$expression_nav_2_to_1_button, {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})
observeEvent(input$expression_to_all, {
  updateTabItems(session, "top_level", "all_data")
})

# Feature side panel ------------------------------------------------------

navigation_set_server("1", "2", "3", "feature_side_panel", "feature_side_panel")
navigation_set_server("2", "3", "4", "feature_side_panel", "feature_side_panel")
navigation_set_server("3", "4", "5", "feature_side_panel", "feature_side_panel")
observeEvent(input$start_feature_format, {
  updateTabsetPanel(session, "feature_side_panel", selected = "2")
})
observeEvent(input$back_to_assay, {
  updateTabItems(session, "top_level", "assay_data")
})
observeEvent(input$feature_nav_5_to_4_button, {
  updateTabsetPanel(session, "feature_side_panel", selected = "4")
})
observeEvent(input$feature_5_to_expression_button, {
  updateTabItems(session, "top_level", "assay_data")
})

# All side panel ----------------------------------------------------------

navigation_set_server("1", "2", "3", "all_data_options", "all_data_options")
observeEvent(input$all_to_assay_button, {
  updateTabsetPanel(session, "top_level", "assay_data")
})
observeEvent(input$all_1_to_2_button, {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})
observeEvent(input$all_3_to_2_button, {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})
