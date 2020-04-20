# ce script prend les données du BC et les colle ensemble

rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)

setwd('~/Google Drive/Recherche/Lake Pulse Postdoc/data/British Columbia/')

# load metadata

metadata <- read_xlsx('LakePulse_sites_BC_FIDQ.xlsx')

# paste all FIDQ data together after removing irrelevant lines when multiple lakes had the same name

fidq <- data.frame()
lakes <- metadata %>% select(id_lakepulse, `waterbody  identifier`) %>% as.data.frame

setwd("./FisHAb_BC_data/fidq/")
files <- as.character(list.files())
#col.types <- sapply(sample, class)

for (i in 1:length(files)){
  sample.name <- files[i]
  sample <- read.csv(sample.name, stringsAsFactors = F)
  lake <- str_extract(sample.name, "[^_]+")
  lake_code <- lakes[match(lake,lakes$id_lakepulse),2]
  sample <- filter(sample, WATERBODY_IDENTIFIER == lake_code)
  sample$WATERSHED_CODE <- as.character(sample$WATERSHED_CODE)
  sample$MAP_NUMBER <- as.character(sample$MAP_NUMBER)
  if(nrow(sample)==0){print(paste0(i,' = no lake'))}
  if(nrow(sample)>1){print(paste0(i,' = more than one lake'))}
  fidq <- bind_rows(fidq, sample)
}

setwd('~/Google Drive/Recherche/Lake Pulse Postdoc/data/British Columbia/')
write_xlsx(fidq, 'all_fidq.xlsx')

# paste all fish datasets together

fish.obs <- data.frame()
fish.pres <- data.frame()

root.path <- '~/Google Drive/Recherche/Lake Pulse Postdoc/data/British Columbia/FisHAb_BC_data/unzipped/'
setwd(root.path)
files <- as.character(list.files())

for (i in 1:length(files)){
  
  sample.name <- files[i]
  lake <- str_extract(sample.name, "[^_]+")
  sub.path <- paste0(root.path,sample.name,'/')
  setwd(sub.path)
  sub.files <- as.character(list.files())
  
  file1 <- sub.files[str_detect(sub.files, "FishObservations")]
  if(length(file1) == 0){print('no fish obs file')}
  sample1 <- read.csv(file1, stringsAsFactors = F) %>% mutate_all(as.character)
  if(nrow(sample1) > 0){
    sample1$ID_LakePulse <- lake
    fish.obs <- bind_rows(fish.obs, sample1)
  }
    
  file2 <- sub.files[str_detect(sub.files, "FishPresence")]
  if(length(file2) == 0){print('no fish pres file')}
  sample2 <- read.csv(file2, stringsAsFactors = F) %>% mutate_all(as.character)
  if(nrow(sample1) > 0){
    sample2$ID_LakePulse <- lake
    fish.pres <- bind_rows(fish.pres, sample2)
  }
  
  setwd(root.path)
  
}

setwd('~/Google Drive/Recherche/Lake Pulse Postdoc/data/British Columbia/')
write_xlsx(fish.obs, 'all_FishObservations.xlsx')
write_xlsx(fish.pres, 'all_FishPresence.xlsx')
write_xlsx(as.data.frame(list.observations), 'species_list.xlsx')
