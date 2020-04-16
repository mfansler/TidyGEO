suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(feather))
suppressPackageStartupMessages(library(httr))

temp_file <- tempfile()
download.file("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series", destfile = temp_file)
geo_page <- read_lines(temp_file)
total_count_line <- geo_page[grep("total_count", geo_page)]
num_index <- gregexpr("[0-9]", total_count_line)
num_series <- substr(total_count_line, start = unlist(num_index)[1], stop = unlist(num_index)[length(unlist(num_index))])

num_groups <- ceiling(as.numeric(num_series) / 5000)

series_list <- NULL
for (i in 1:num_groups) {
  print(paste0("Processing series download for page ", i))

  response <- RETRY("GET", paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=accession&mode=tsv&page=", i, "&display=5000"), times=5)
  current_file <- suppressMessages(read_tsv(response$content)) %>%
    mutate(description = paste0(Title, "; Type: ", `Series Type`, "; Taxonomy: ", Taxonomy, "; Samples: ", `Sample Count`)) %>%
    select(Accession, description)
  series_list <- rbind(series_list, current_file)
}

write_feather(data.frame(label = series_list$Accession, value = series_list$Accession, name = series_list$description), "TidyGEO/www/series_list.feather")

download.file("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms", destfile = temp_file)
geo_page <- read_lines(temp_file)
total_count_line <- geo_page[grep("total_count", geo_page)]
num_index <- gregexpr("[0-9]", total_count_line)
num_platforms <- substr(total_count_line, start = unlist(num_index)[1], stop = unlist(num_index)[length(unlist(num_index))])

num_groups <- ceiling(as.numeric(num_platforms) / 5000)

platform_list <- NULL
for (i in 1:num_groups) {
  print(paste0("Processing platform download for page ", i))

  response <- RETRY("GET", paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=accession&mode=tsv&page=", i, "&display=5000"), times=5)

  current_file <- suppressMessages(read_tsv(response$content)) %>%
    mutate(description = paste0(Title, "; Technology: ", Technology, "; Taxonomy: ", Taxonomy, "; Samples: ", `Samples Count`, "; Rows: ", `Data Rows`)) %>%
    select(Accession, description)
  platform_list <- rbind(platform_list, current_file)
}

write_feather(platform_list, "TidyGEO/www/platform_list.feather")
