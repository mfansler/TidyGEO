tabsetPanel(id = "expression_side_panel", 
        source(file.path("ui", "feature", "feature_info.R"), local = TRUE)$value,
        source(file.path("ui", "feature", "shift_cells.R"), local = TRUE)$value,
        source(file.path("ui", "feature", "split_pairs.R"), local = TRUE)$value,
        source(file.path("ui", "feature", "split_cols.R"), local = TRUE)$value,
        source(file.path("ui", "feature", "save_data.R"), local = TRUE)$value)