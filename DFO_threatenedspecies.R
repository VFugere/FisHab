
rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(scales)
library(sp)
library(rworldmap)
library(RColorBrewer)
'%!in%' <- function(x,y)!('%in%'(x,y))

LCE <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

melc <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx',sheet='Biomasse')
melc.sites <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx',sheet='Stations')
#ajouter date
melc$date <- as.Date(str_remove(melc$DATE.PECHE, '\\ .*'),'%m/%d/%y')
melc$year <- as.numeric(format(melc$date, '%Y'))
melc <- melc %>% left_join(melc.sites, by = c('STATION' = 'STATION.BQMA'))
melc <- melc %>% select(ESPE,BIOMASSE,year,LATITUDE.NAD83,LONGITUDE.NAD83)
colnames(melc) <- c('sp.code', 'biomass', 'year', 'lat', 'long')
melc$data <- 'melcc'
melc <- melc %>% select(data, sp.code, lat, long)

mffp <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx',sheet=2) 
good.surveys <- c('PENDJ','PENOF','PENT','PENOC')
bad.sp.codes <- c('RIEN','-','POIS','NI','AU')
colnames(mffp)[3] <- 'LCE'
colnames(mffp)[c(6,8:10)] <- c('yr','Latitude','Longitude','sp.code')
mffp$Latitude <- as.numeric(mffp$Latitude)
mffp$Longitude <- as.numeric(mffp$Longitude)
mffp <- filter(mffp, sp.code %!in% bad.sp.codes)
mffp <- filter(mffp, `Type de pêche (code)` %in% good.surveys) #none removed

mffp.sites <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/QC_all_fish_sites.xlsx')
mffp <- select(mffp, LCE, yr, sp.code)
for(i in 1:nrow(mffp)){
  lce.temp <- mffp[i,'LCE'] %>% as.character
  if(lce.temp %in% mffp.sites$erroneous_LCE_MFFP){
    true.lce <- mffp.sites$LCE[which(mffp.sites$erroneous_LCE_MFFP == lce.temp)]
    mffp[i,'LCE'] <- true.lce
  }
}
mffp.sites <- mffp.sites %>% select(LCE, lce_lat, lce_long)
mffp <- left_join(mffp, mffp.sites, by = 'LCE')
mffp$data <- 'mffp'
mffp <- mffp %>% select(data, sp.code, lce_lat, lce_long)
colnames(mffp)[c(3,4)] <- c('lat','long')

#endangered species
data <- bind_rows(mffp,melc)

endangered.sp <- c('MOHU','EXMA','ANRO','MOSA','ESVE','MOCA','AMPE','ACFU','PECO','ICFO','ICUN','NOBI')

data <- filter(data, sp.code %in% endangered.sp)

write_xlsx(data, '~/Desktop/coords_especesendanger_melcc_mffp.xlsx')
