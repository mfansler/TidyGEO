tabsetPanel(id = "expression_side_panel", source(file.path("ui", "assay", "format_data.R"), local = TRUE)$value,
        source(file.path("ui", "assay", "save_data.R"), local = TRUE)$value)