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
  if(nrow(sample)==0){print(paste0(i,' = no lake'))}
  if(nrow(sample)>1){print(paste0(i,' = more than one lake'))}
  fidq <- bind_rows(fidq, sample)
}

