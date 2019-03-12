source("geocurateFunctions_User.R")
geoID <- 'GSE1456'
toFilter[1] <- 'sameVals'
toFilter[2] <- 'allDiff'
toFilter[3] <- 'dates'
toFilter[4] <- 'reanalyzed'
toFilter[5] <- 'url'
metaData <- downloadClinical(geoID, toFilter, session = NULL, testFile = NA)
