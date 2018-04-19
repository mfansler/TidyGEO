source('geocurateFunctions_User.R')
toFilter <- NULL
toFilter[1] <- 'sameVals'
toFilter[2] <- 'allDiff'
toFilter[3] <- 'dates'
toFilter[4] <- 'reanalyzed'
toFilter[5] <- 'url'
geoID <- 'GSE1456'
metaData <- downloadClinical(geoID, toFilter)
colsToSplit <- NULL
colsToSplit[1] <- NULL
colsToSplit[0] <- NULL
colsToDivide <- NULL
colsToDivide[1] <- NULL
colsToDivide[0] <- NULL
toSplit <- TRUE
toDivide <- FALSE
delimiter <- ': '
delimiter2 <- ''
allButSplit <- TRUE
allButDivide <- FALSE
metaData <- extractCols(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide)
varsToKeep <- NULL
varsToKeep[1] <- NULL
varsToKeep[0] <- NULL
allButKeep <- TRUE
metaData <- filterCols(metaData, varsToKeep, allButKeep)
newNames <- NULL
newNames[['DEATH']] <- 'Outcome Death'
newNames[['DEATH_BC']] <- 'Breast Cancer Treatment Outcomes Scale-Cosmetic Status'
newNames[['RELAPSE']] <- 'Relapse'
newNames[['SUBTYPE']] <- 'Tumor Subtype'
newNames[['SURV_DEATH']] <- 'Event-Free Survival'
newNames[['SURV_RELAPSE']] <- 'Disease-Free Survival'
metaData <- renameCols(metaData, newNames)
tablesList <- NULL
tablesList[['Tumor.Subtype']] <- data.frame(To_Replace=c('No Subtype'), New_Val=c('NA'))
tablesList[['Event.Free.Survival']] <- data.frame(To_Replace=c('RANGE: 0.18-4.1', 'RANGE: 4.2-8.49'), New_Val=c('ShortSurvival', 'LongSurvival'))
tablesList[['Disease.Free.Survival']] <- data.frame(To_Replace=c('RANGE: 0.23-4.2', 'RANGE: 4.2-8.49'), New_Val=c('ShortSurvival', 'LongSurvival'))
metaData <- substituteVals(metaData, tablesList)
excludesList <- NULL
excludesList[['ELSTON']] <- 'NA'
metaData <- excludeVars(metaData, excludesList)
excludesList <- NULL
excludesList[['ELSTON']] <- 'NA'
excludesList[['Tumor.Subtype']] <- 'NA'
metaData <- excludeVars(metaData, excludesList)
excludesList <- NULL
excludesList[['Tumor.Subtype']] <- 'NA'
excludesList[['Event.Free.Survival']] <- 'LongSurvival'
metaData <- excludeVars(metaData, excludesList)
excludesList <- NULL
excludesList[['Event.Free.Survival']] <- 'LongSurvival'
excludesList[['Disease.Free.Survival']] <- 'ShortSurvival'
metaData <- excludeVars(metaData, excludesList)
metaData <- cbind(rownames(metaData), metaData)
colnames(metaData)[1] <- ''
file <- '/tmp/Rtmp36MgJk/file111184e6cc.csv'
write.csv(metaData, file, row.names = FALSE, col.names = TRUE)
metaData <- cbind(rownames(metaData), metaData)
colnames(metaData)[1] <- ''
file <- '/tmp/Rtmp36MgJk/file1176922576.tsv'
write.table(metaData, file, sep = '	', row.names = FALSE, col.names = TRUE, quote = FALSE)
