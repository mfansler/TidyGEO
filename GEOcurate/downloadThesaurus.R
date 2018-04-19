library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(feather)
library(data.table)
library(splitstackshape)

# Download file -----------------------------------------------------------


#download.file("https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/Thesaurus_18.02d.FLAT.zip", "Thesaurus_18.02d.FLAT.zip", mode = "wb")
thesaurus <- read_tsv("Thesaurus_18.02d.FLAT.zip", col_names = F)
#file.remove("Thesaurus_18.02d.FLAT.zip")

colnames(thesaurus) <- c("Code", "OWL_Location", "Parent_Concept_Code", "Synonyms", "Definition", "Display_Name", "Concept_Type", "Semantic_Type")
thesaurus <- thesaurus[-which(thesaurus$Concept_Type == "Obsolete_Concept"), -which(colnames(thesaurus) == "OWL_Location")]

# Synonyms file -----------------------------------------------------------


thes2 <- cSplit(select(thesaurus, Code, Synonyms), "Synonyms", "|", direction = "long")
thes3 <- thes2
thes3$Synonyms <- lapply(as.character(thes2$Synonyms), function(v) { toupper(v) })
thes3$Synonyms <- as.factor(as.character(thes3$Synonyms))
thes2 <- thes2[!duplicated(thes3),]

write_feather(thes2, "thesaurus.synonyms.feather")
#thesaurus.feather <- read_feather("thesaurus.synonyms.feather")

# Preferred names file ----------------------------------------------------


thes2 <- separate(select(thesaurus, Code, Synonyms), Synonyms, into = "Synonyms", sep = "\\|")
colnames(thes2)[which(colnames(thes2) == "Synonyms")] <- "Preferred_Name"

write_feather(thes2, "thesaurus.preferred.feather")
#thesaurus.feather <- read_feather("thesaurus.preferred.feather")

# Testing download --------------------------------------------------------


testing <- F

if(testing) {
  data_dict <- read_tsv("Clinical_Data_Dictionary.txt")[c(1:5)]
  not_in_thes <- data_dict[which(!data_dict$NCI_Thesaurus_Code %in% thesaurus$Code && !data_dict$NCI_Thesaurus_Code %in% thesaurus$Parent_Concept_Code),]
  View(not_in_thes)
  codes <- NULL
  for (code in unique(not_in_thes$NCI_Thesaurus_Code[which(!is.na(not_in_thes$NCI_Thesaurus_Code))])) {
    if (grepl(",", code)) {
      code <- strsplit(code, ", ")[[1]]
      codes <- c(codes, str_trim(code[1]), str_trim(code[2]))
    }
    else {
      codes <- c(codes, code)
    }
    
  }
  write.table(unique(codes), file = "NotIn_Thesaurus.txt", quote = F, sep = "\t", row.names = F, col.names = F)
}