sidebarPanel(
  HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp<b>View assay data</b>&nbsp&nbsp&nbsp"),
  prettySwitch("display_assay_or_feature", NULL, inline = TRUE),
  HTML("<b>View feature data</b>"),
  uiOutput("expression_side_panel_contents")
)