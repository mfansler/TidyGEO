tabPanel(title = icon("question-circle"), value = "1",
         h4("Formatting the feature data"),
         HTML('<p>In the <b>assay data</b> table, you will see a column labeled <b>"ID"</b>. This is a unique identifier that usually
              corresponds to the <b>probe set</b> used to take some measurement for the given sample ("GSM"). 
              Probe sets may not be a useful way to look at patterns in the data.
              Often, scientists want to look at <b>genes, transcripts, or exons,</b> for example. The <b>feature data</b> maps
              the probe set identifiers (the "ID" column) to their corresponding genes, transcripts, exons, etc.
              Frequently, <b>multiple probe sets refer to the same gene.</b>
              </p>'),
         p('In the following tabs, you will be given the opportunity to reformat the feature data. This will be helpful in mapping
           genes, transcripts, exons, etc. to the corresponding probe sets in the assay data.'),
         hr(),
         div(
           tertiary_button("back_to_assay", div(icon("arrow-left"), "Back to assay data")),
           secondary_button("start_feature_format", div("Get started", icon("arrow-right")), class = "right_align"))
         )