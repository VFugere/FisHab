# ce script prend les données de l'Alberta, les colle ensemble, puis extrait les lacs pertinents (Lake Pulse)

rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)

setwd('~/Google Drive/Recherche/Lake Pulse Postdoc/data/Alberta/')

# load metadata

metadata <- read_xlsx('AB FWMIS_LakePulse_Info_NOV2019_extraction_listHUC8.xlsx')

# paste all fish datasets together

fish.pres <- data.frame()

root.path <- '~/Google Drive/Recherche/Lake Pulse Postdoc/data/Alberta/Fish_Inventories/'
setwd(root.path)
files <- as.character(list.files())

for (i in 1:length(files)){
  
  sample.name <- files[i]
  bassin_versant <- str_extract(sample.name, "[^_]+")
  sub.path <- paste0(root.path,sample.name,'/')
  setwd(sub.path)
  sub.files <- as.character(list.files())
  
  file1 <- sub.files[str_detect(sub.files, "FishSurvey.csv")]
  if(length(file1) == 0){print('no fish obs file')}
  sample1 <- read.csv(file1, stringsAsFactors = F) %>% mutate_all(as.character)
  if(nrow(sample1) > 0){
    sample1$bassin_versant <- bassin_versant
    fish.pres <- bind_rows(fish.pres, sample1)
  }
    
  setwd(root.path)
  
}

#### filter to only keep lines that are within the metadata ###

fish.pres <- filter(fish.pres, WTB_ID != 'NA', WTB_ID %in% metadata$WB_ID)

fish.sub <- fish.pres %>% group_by(WTB_ID) %>% distinct(SPEC_NAME, .keep_all = T)

setwd('~/Google Drive/Recherche/Lake Pulse Postdoc/data/Alberta/')
write_xlsx(fish.pres, 'all_FishSurveys.xlsx')
write_xlsx(fish.sub, 'all_FishSurveys_summary.xlsx')
