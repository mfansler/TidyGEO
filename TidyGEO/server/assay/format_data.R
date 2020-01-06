source(file.path("server", "assay", "feature_data.R"), local = TRUE)$value

observe({
  shinyjs::toggleState("expression_replace_id", condition = !get_data_member("assay", "disable_btns"))
})

observe({
  disable_transpose <- !get_data_member("assay", "disable_btns") #& 
    #get_data_member("assay", "id_col") != "colnames" &
    #any(duplicated(get_data_member("assay", dataname("assay"))[,get_data_member("assay", "id_col")]))
  shinyjs::toggleState("expression_transpose", disable_transpose)
})

# other expression options ------------------------------------------------

observeEvent(input$expression_transpose, {
  if (data_loaded("assay")) {
    status <- withProgress(
      eval_function("assay", "quickTranspose", list(force = TRUE), "transpose data"), 
      message = "Transposing the data"
      )
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in transpose", "Data not transposed.", status)
      )
    } else {
      set_x_equalto_y("prev_id", get_data_member("assay", "id_col"), "assay")
      set_x_equalto_y("id_col", "colnames", "assay")
      set_x_equalto_y("disable_btns", TRUE, "assay")
      shinyjs::enable("undoEvalExpr")
    }
  }
})

observeEvent(input$undoEvalExpr, {
  if (data_loaded("assay")) {
    undo_last_action("assay")
    
    set_x_equalto_y("disable_btns", FALSE, "assay")
    set_x_equalto_y("id_col", get_data_member("assay", "prev_id"), "assay")
    set_x_equalto_y("id_col", get_data_member("feature", "prev_id"), "feature")
    
    shinyjs::disable("undoEvalExpr")
  }
})

observeEvent(get_input(nav("assay", "clinical")), {
  updateTabItems(session, "top_level", "clinical_data")
})
observeEvent(get_input(nav("1", "2", "assay")), {
  updateTabsetPanel(session, "expression_side_panel", selected = "2")
})
