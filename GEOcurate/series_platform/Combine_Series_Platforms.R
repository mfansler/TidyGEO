# list of series on GEO as of 11/13/2018 ----------------------------------


series_files <- list.files(pattern = "series.*tsv", full.names = TRUE)
current_file <- read_tsv(series_files[1])
series_list <- current_file  %>%
  mutate(description = paste0(Title, "; Type: ", `Series Type`, "; Taxonomy: ", Taxonomy, "; Samples: ", `Sample Count`)) %>%
  select(Accession, description)
for (i in 2:length(series_files)) {
  current_file <- read_tsv(series_files[i])
  current_file <- current_file  %>%
    mutate(description = paste0(Title, "; Type: ", `Series Type`, "; Taxonomy: ", Taxonomy, "; Samples: ", `Sample Count`)) %>%
    select(Accession, description)
  series_list <- rbind(series_list, current_file)
}
series_list <- arrange(series_list, Accession)
saveRDS(series_list, "../www/series_list.rds")

# list of platforms on GEO as of 11/13/2018 -------------------------------


platform_files <- list.files(pattern = "platform.*tsv", full.names = TRUE)
current_file <- read_tsv(platform_files[1])
platform_list <- current_file  %>%
  mutate(description = paste0(Title, "; Technology: ", Technology, "; Taxonomy: ", Taxonomy, "; Samples: ", `Samples Count`, "; Rows: ", `Data Rows`)) %>%
  select(Accession, description)
for (i in 2:length(platform_files)) {
  current_file <- read_tsv(platform_files[i])
  current_file <- current_file  %>%
    mutate(description = paste0(Title, "; Technology: ", Technology, "; Taxonomy: ", Taxonomy, "; Samples: ", `Samples Count`, "; Rows: ", `Data Rows`)) %>%
    select(Accession, description)
  platform_list <- rbind(platform_list, current_file)
}
saveRDS(platform_list, "../www/platform_list.rds")