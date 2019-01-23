# list of series on GEO as of 11/13/2018 ----------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(feather))

temp_file <- tempfile()
download.file("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series", destfile = temp_file)
geo_page <- read_lines(temp_file)
total_count_line <- geo_page[grep("total_count", geo_page)]
num_index <- gregexpr("[0-9]", total_count_line)
num_series <- substr(total_count_line, start = unlist(num_index)[1], stop = unlist(num_index)[length(unlist(num_index))])

num_groups <- ceiling(as.numeric(num_series) / 5000)

current_file <- suppressMessages(read_tsv("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=accession&mode=tsv&page=1&display=5000"))
series_list <- current_file  %>%
  mutate(description = paste0(Title, "; Type: ", `Series Type`, "; Taxonomy: ", Taxonomy, "; Samples: ", `Sample Count`)) %>%
  select(Accession, description)

pb_series <- txtProgressBar(min = 0, max = num_groups, style = 3, title = "Downloading series")

for (i in 2:num_groups) {
  current_file <- suppressMessages(read_tsv(paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=accession&mode=tsv&page=", i, "&display=5000")))
  current_file <- current_file  %>%
    mutate(description = paste0(Title, "; Type: ", `Series Type`, "; Taxonomy: ", Taxonomy, "; Samples: ", `Sample Count`)) %>%
    select(Accession, description)
  series_list <- rbind(series_list, current_file)
  setTxtProgressBar(pb_series, i)
}

#start_time <- Sys.time()
#saveRDS(series_list, "../www/series_list.rds")
#end_time <- Sys.time()
#print(c("RDS time:", end_time - start_time))
#start_time <- Sys.time()
#file_name <- "../www/series_list.feather"
#if (file.exists(file_name)) {
#  file.remove(file_name)
#}
write_feather(data.frame(label = series_list$Accession, value = series_list$Accession, name = series_list$description), "../www/series_list.feather")
#end_time <- Sys.time()
#print(c("Feather time:", end_time - start_time))

# list of platforms on GEO as of 11/13/2018 -------------------------------

temp_file <- tempfile()
download.file("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms", destfile = temp_file)
geo_page <- read_lines(temp_file)
total_count_line <- geo_page[grep("total_count", geo_page)]
num_index <- gregexpr("[0-9]", total_count_line)
num_platforms <- substr(total_count_line, start = unlist(num_index)[1], stop = unlist(num_index)[length(unlist(num_index))])

num_groups <- ceiling(as.numeric(num_platforms) / 5000)

current_file <- suppressMessages(read_tsv("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=accession&mode=tsv&page=1&display=5000"))
platform_list <- current_file  %>%
  mutate(description = paste0(Title, "; Technology: ", Technology, "; Taxonomy: ", Taxonomy, "; Samples: ", `Samples Count`, "; Rows: ", `Data Rows`)) %>%
  select(Accession, description)

pb_platform <- txtProgressBar(min = 0, max = num_groups, style = 3, title = "Downloading platform")

for (i in 2:num_groups) {
  current_file <- suppressMessages(read_tsv(paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=accession&mode=tsv&page=", i, "&display=5000")))
  current_file <- current_file  %>%
    mutate(description = paste0(Title, "; Technology: ", Technology, "; Taxonomy: ", Taxonomy, "; Samples: ", `Samples Count`, "; Rows: ", `Data Rows`)) %>%
    select(Accession, description)
  platform_list <- rbind(platform_list, current_file)
  setTxtProgressBar(pb_platform, i)
}
#saveRDS(platform_list, "../www/platform_list.rds")
#file_name <- "../www/platform_list.feather"
#if (file.exists(file_name)) {
#  file.remove(file_name)
#}
write_feather(platform_list, "../www/platform_list.feather")
