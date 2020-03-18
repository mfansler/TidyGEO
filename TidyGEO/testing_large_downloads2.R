library(GEOquery)
library(readr)
library(dplyr)
options(timeout = 300)

in_app <- FALSE

# depends on purrr, dplyr, readr
parseGSEMatrix <- function (fname, AnnotGPL = FALSE, destdir = tempdir(), getGPL = TRUE, 
                            parseCharacteristics = TRUE) 
{
  dat <- read_lines(fname)
  i <- 1
  series_header_row_count <- 0
  while(grepl("^!Series_", dat[i])) {
    series_header_row_count <- series_header_row_count + 1
    i <- i + 1
  }
  #series_header_row_count <- sum(grepl("^!Series_", dat))
  sample_header_start <- purrr::detect_index(dat, function(x) grepl("^!Sample_", x))
  #sample_header_start <- grep("^!Sample_", dat)[1]
  #samples_header_row_count <- sum(grepl("^!Sample_", dat))
  i <- sample_header_start
  samples_header_row_count <- 0
  while(grepl("^!Sample_", dat[i])) {
    samples_header_row_count <- samples_header_row_count + 1
    i <- i + 1
  }
  series_table_begin_line = grep("^!series_matrix_table_begin",
                                dat)
  if (length(series_table_begin_line) != 1) {
    stop("parsing failed--expected only one '!series_data_table_begin'")
  }
  header <- read.table(fname, sep = "\t", header = FALSE, 
                       nrows = series_header_row_count)
  tmpdat <- read.table(fname, sep = "\t", header = FALSE, 
                       nrows = samples_header_row_count, skip = sample_header_start - 
                         1)
  headertmp <- t(header)
  headerdata <- rbind(data.frame(), headertmp[-1, ])
  colnames(headerdata) <- sub("!Series_", "", as.character(header[, 
                                                                  1]))
  headerlist <- lapply(split.default(headerdata, names(headerdata)), 
                       function(x) {
                         as.character(Reduce(function(a, b) {
                           paste(a, b, sep = "\n")
                         }, x))
                       })
  link = "https://www.ncbi.nlm.nih.gov/geo/"
  if (!is.null(headerlist$web_link)) {
    link <- headerlist$web_link
  }
  else if (!is.null(headerlist$geo_accession)) {
    link <- paste(link, "query/acc.cgi?acc=", headerlist$geo_accession, 
                  sep = "")
  }
  ed <- new("MIAME", name = ifelse(is.null(headerlist$contact_name), 
                                   "", headerlist$contact_name), title = headerlist$title, 
            contact = ifelse(is.null(headerlist$contact_email), 
                             "", headerlist$contact_email), pubMedIds = ifelse(is.null(headerlist$pubmed_id), 
                                                                               "", headerlist$pubmed_id), abstract = ifelse(is.null(headerlist$summary), 
                                                                                                                            "", headerlist$summary), url = link, other = headerlist)
  tmptmp <- t(tmpdat)
  sampledat <- rbind(data.frame(), tmptmp[-1, ])
  colnames(sampledat) <- make.unique(sub("!Sample_", "", as.character(tmpdat[, 
                                                                             1])))
  sampledat[["geo_accession"]] = as.character(sampledat[["geo_accession"]])
  rownames(sampledat) = sampledat[["geo_accession"]]
  if (length(grep("characteristics_ch", colnames(sampledat))) > 
      0 && parseCharacteristics) {
    pd = sampledat %>% dplyr::select(dplyr::contains("characteristics_ch")) %>% 
      dplyr::mutate(accession = rownames(.)) %>% mutate_all(as.character) %>% 
      tidyr::gather(characteristics, kvpair, -accession) %>% 
      dplyr::filter(grepl(":", kvpair) && !is.na(kvpair))
    if (nrow(pd)) {
      pd = dplyr::mutate(pd, characteristics = ifelse(grepl("_ch2", 
                                                            characteristics), "ch2", "ch1")) %>% tidyr::separate(kvpair, 
                                                                                                                 into = c("k", "v"), sep = ":", fill = "right", 
                                                                                                                 extra = "merge") %>% dplyr::mutate(k = paste(k, 
                                                                                                                                                              characteristics, sep = ":")) %>% dplyr::select(-characteristics) %>% 
        dplyr::filter(!is.na(v)) %>% dplyr::group_by(accession, 
                                                     k) %>% dplyr::mutate(v = paste0(trimws(v), collapse = ";")) %>% 
        unique() %>% tidyr::spread(k, v)
    }
    else {
      pd = pd %>% dplyr::select(accession)
    }
    sampledat = sampledat %>% dplyr::left_join(pd, by = c(geo_accession = "accession"))
  }
  datamat <- read_tsv(fname, quote = "\"", na = c("NA", "null", 
                                                  "NULL", "Null"), skip = series_table_begin_line, comment = "!series_matrix_table_end", 
                      skip_empty_rows = FALSE)
  tmprownames = datamat[[1]]
  datamat <- as.matrix(datamat[!is.na(tmprownames), -1])
  rownames(datamat) <- tmprownames[!is.na(tmprownames)]
  datamat <- as.matrix(datamat)
  rownames(sampledat) <- colnames(datamat)
  GPL = as.character(sampledat[1, grep("platform_id", colnames(sampledat), 
                                       ignore.case = TRUE)])
  fd = new("AnnotatedDataFrame", data = data.frame(row.names = rownames(datamat)))
  if (getGPL) {
    gpl <- getGEO(GPL, AnnotGPL = AnnotGPL, destdir = destdir)
    vmd <- Columns(gpl)
    dat <- Table(gpl)
    tmpnames = character(0)
    if (ncol(dat) > 0) {
      tmpnames = as.character(dat[, 1])
    }
    tmpnames[is.na(tmpnames)] = "NA"
    rownames(dat) <- make.unique(tmpnames)
    dat <- dat[match(tolower(rownames(datamat)), tolower(rownames(dat))), 
               ]
    rownames(vmd) <- make.unique(colnames(Table(gpl)))
    colnames(dat) <- rownames(vmd)
    fd <- new("AnnotatedDataFrame", data = dat, varMetadata = vmd)
  }
  if (is.null(nrow(datamat))) {
    tmpnames <- names(datamat)
    rownames(sampledat) <- tmpnames
    datamat = matrix(nrow = 0, ncol = nrow(sampledat))
    colnames(datamat) <- tmpnames
  }
  else {
    rownames(datamat) <- rownames(dat)
  }
  eset <- new("ExpressionSet", phenoData = as(sampledat, "AnnotatedDataFrame"), 
              annotation = GPL, featureData = fd, experimentData = ed, 
              exprs = as.matrix(datamat))
  return(list(GPL = as.character(sampledat[1, grep("platform_id", 
                                                   colnames(sampledat), ignore.case = TRUE)]), eset = eset))
}



# A helper function for getGEO --------------------------------------------
# Library dependencies: GEOquery
# Function dependencies:
getAndParseGSEMatrices <- function(GEO, destdir, AnnotGPL, getGPL = TRUE, 
                                   parseCharacteristics = TRUE, platform = NULL) 
{ # Please note: this function was copied from the GEOquery package. It was altered
  # to only download one platform (rather than all the platforms) from GEO and to
  # delete the temp file after downloading.
  #print(platform)
  GEO <- toupper(GEO)
  stub = gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
  if (is.null(platform)) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
    b = GEOquery:::getDirListing(sprintf(gdsurl, stub, GEO))
    platform <- b[1]
  }
  if (in_app) incProgress(message = "Loading GSE file")
  destfile = file.path(destdir, platform)
  if (file.exists(destfile)) {
    message(sprintf("Using locally cached version: %s", 
                    destfile))
  }
  else {
    download.file(sprintf("https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s", 
                          stub, GEO, platform), destfile = destfile, mode = "wb", 
                  method = getOption("download.file.method.GEOquery"))
  }
  if (in_app) incProgress(message = "Parsing GSE matrix")
  
  result <- parseGSEMatrix(destfile, destdir = destdir, 
                                      AnnotGPL = AnnotGPL, getGPL = getGPL)$eset
  file.remove(destfile)
  return(result)
}

# Get expressionSet object from GEO ---------------------------------------
# Library dependencies: GEOquery
# Function dependencies: getAndParseGSEMatrices
getGEO <- function(GEO = NULL, filename = NULL, destdir = tempdir(), 
                   GSElimits = NULL, GSEMatrix = TRUE, AnnotGPL = FALSE, getGPL = TRUE, 
                   parseCharacteristics = TRUE, platform = NULL) 
{ # Please note: this function was copied from the GEOquery package. It was altered
  # to only download one platform (rather than all the platforms) from GEO and to
  # delete the temp file after downloading.
  con <- NULL
  if (!is.null(GSElimits)) {
    if (length(GSElimits) != 2) {
      stop("GSElimits should be an integer vector of length 2, like (1,10) to include GSMs 1 through 10")
    }
  }
  if (is.null(GEO) & is.null(filename)) {
    stop("You must supply either a filename of a GEO file or a GEO accession")
  }
  if (in_app) incProgress(message = "Fetching GSE file")
  if (is.null(filename)) {
    GEO <- toupper(GEO)
    geotype <- toupper(substr(GEO, 1, 3))
    if (GSEMatrix & geotype == "GSE") {
      return(getAndParseGSEMatrices(GEO, destdir, AnnotGPL = AnnotGPL, 
                                    getGPL = getGPL, parseCharacteristics = parseCharacteristics, platform = platform))
    }
    filename <- getGEOfile(GEO, destdir = destdir, AnnotGPL = AnnotGPL)
  }
  ret <- parseGEO(filename, GSElimits, destdir, AnnotGPL = AnnotGPL, 
                  getGPL = getGPL)
  return(ret)
}


to_test <- c("GSE116436", "GSE56047", "GSE80205", "GSE39655", "GSE73103", "GSE49149")

# GSE116436 ---------------------------------------------------------------



start_time <- Sys.time()
t1 <- getGEO(to_test[1], NULL)
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 43.7487 mins

e <- t1$GSE116436_series_matrix.txt.gz
m <- pData(e)

# GSE56047 ----------------------------------------------------------------


start_time <- Sys.time()
t2 <- getGEO(to_test[2])
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 26.99181 mins

e2 <- t2$`GSE56047-GPL10558_series_matrix.txt.gz`
m2 <- pData(e2)

# GSE80205 ----------------------------------------------------------------


start_time <- Sys.time()
t3 <- getGEO(to_test[3])
end_time <- Sys.time()

# Did not load
# Error in .subset2(x, i) : subscript out of bounds

#e3 <- t3
#m3 <- pData(e3)

# GSE39655 ----------------------------------------------------------------


start_time <- Sys.time()
t4 <- getGEO(to_test[4])
end_time <- Sys.time()
print(end_time - start_time)

# Did not load
# Error: cannot allocate vector of size 2.6 Mb
# Time difference of 48.80416 mins

e4 <- t4
m4 <- pData(e4)

# GSE73103 ----------------------------------------------------------------


start_time <- Sys.time()
t5 <- getGEO(to_test[5])
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 1.117518 hours

e5 <- t5$GSE73103_series_matrix.txt.gz
m5 <- pData(e5)

# GSE49149 ----------------------------------------------------------------


start_time <- Sys.time()
t6 <- getGEO(to_test[6])
end_time <- Sys.time()
print(end_time - start_time)

e6 <- t6
m6 <- pData(t6)