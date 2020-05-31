## This code loads the MFFP IFA data and cleans it to only keep PEN data
## from sites in the clean site list

rm(list=ls())

library(tidyverse)
library(readxl)
library(sp)

'%!in%' <- function(x,y)!('%in%'(x,y))

good.surveys <- c('PENDJ','PENOF','PENT','PENOC')

# load LCE db, LP db with LCE info, and MFFP fish data

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

INV <- read_xlsx(skip=5,"/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/premier_envoi/IFD- 1 - Rapport Inventaire sur plan d'eau (IPE) 2000-2018.xlsx")
colnames(INV)[1] <- 'LCE'
INV$yr <- lubridate::year(INV$`Date de levée`)
INV$abund <- as.numeric(INV$`Nbre capturé`)

bad.sp.codes <- c('RIEN','-','POIS','NI','AU')
INV <- filter(INV, `Espèce` %!in% bad.sp.codes)
INV <- filter(INV, !is.na(abund)) 

PEN <- filter(INV, `Type de pêche` %in% good.surveys)
PEN <- filter(PEN, `Engin` == 'Filet expérimental') #quelques trucs qui devraient pas être là 

#making a clean site list that includes LCE & RHS identifiers
PEN %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN_sites
colnames(PEN_sites) <- c('LCE','mffp_PEN_nom','survey','gear','station','valid','hasard','mffp_lat','mffp_long','date','species','captures','nb.weighed','total.mass.g','min.length.mm','max.length.mm','yr','abund')
PEN_sites <- select(PEN_sites, LCE:survey, mffp_lat:date, yr)
PEN_sites <- left_join(PEN_sites, LCE)
PEN_sites$mffp_coords <- paste(PEN_sites$mffp_lat,',',PEN_sites$mffp_long)
PEN_sites$LCE_coords <- paste(PEN_sites$lce_lat,',',PEN_sites$lce_long)
PEN_sites <- PEN_sites %>% rename(distance_lce_rhs_m = distance)
#calculating distance between LCE centroid & MFFP sampling coordinate to help identify problematic sites
dists <- numeric(0)
for(i in 1:nrow(PEN_sites)){
  tmp <- PEN_sites[i,]
  if(is.na(tmp$mffp_lat)){dist <- NA}else if(is.na(tmp$lce_lat)){dist <- NA}else{
    dist <- spDists(x=as.matrix(tmp[,c('mffp_long','mffp_lat')]), y =as.matrix(tmp[,c('lce_long','lce_lat')]), longlat = T)}
  dists <- c(dists,as.numeric(dist))
}
PEN_sites$distance_lce_mffp_km <- dists
PEN_sites <- PEN_sites %>% select(LCE,mffp_PEN_nom,lce_nom,rhs_nom,distance_lce_rhs_m,distance_lce_mffp_km,mffp_lat,lce_lat,mffp_long,lce_long,mffp_coords,LCE_coords,survey,date,yr,ecosysteme,RHS,lce_masl,lce_perim_km,lce_area_sq.km,rhs_area_ha,rhs_perim_m)
writexl::write_xlsx(PEN_sites,'~/Desktop/MFFP_PEN_sites_premierenvoi.xlsx')

#a lot of quality control done in QGIS & Excel
#1) confirm that all lakes have the same name in 3 DB (MFFP, LCE, RHS)
#2) when they don't, confirm that mffp % lce fall in the same polygon, and that polygon from the RHS is the right one
#3) Some LCEs were badly entered by the MFFP. Found the right ones using coords and GIS.
#   -TP040600 -> F3280
#   -040600 -> F3280
#   -TPJulesL -> F4944
#   -TPpluto -> F4945
#   -F0627 -> F3276
#4) Some LCEs are missing from the GRHQ, so spatial join returned an erroneous polygon from the RHS.
#   RHS info for these was deleted. These were: 02226 - 
#5) some LCEs are just bad. No coords, or coords outside of Qc, and the LCE is erreoneous.
#   Nothing we can do with that so delete the data before analyses. These are: J2288 - J2290 - TP00001 - F2494

##### REFAISONS LA MÊME CHOSE AVEC LE DEUXIÈME ENVOI DE DONNÉES ####

INV <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx') 
colnames(INV)[3] <- 'LCE'
INV <- dplyr::select(INV, LCE:CPUE)
colnames(INV)[c(4,6:7)] <- c('yr','Latitude','Longitude')
INV$Latitude <- as.numeric(INV$Latitude)
INV$Longitude <- as.numeric(INV$Longitude)

INV <- filter(INV, `Espèce (code)` %!in% bad.sp.codes)
INV <- filter(INV, !is.na(CPUE)) 

PEN2 <- filter(INV, `Type de pêche (code)` %in% good.surveys)

#making a clean site list that includes LCE & RHS identifiers
PEN2 %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN2_sites
colnames(PEN2_sites) <- c('LCE','mffp_PEN2_nom','surveynb','yr','survey','mffp_lat','mffp_long','species','species.name','cpue')
PEN2_sites <- dplyr::select(PEN2_sites, -species.name)
PEN2_sites <- left_join(PEN2_sites, LCE)
PEN2_sites$mffp_coords <- paste(PEN2_sites$mffp_lat,',',PEN2_sites$mffp_long)
PEN2_sites$LCE_coords <- paste(PEN2_sites$lce_lat,',',PEN2_sites$lce_long)
PEN2_sites <- PEN2_sites %>% rename(distance_lce_rhs_m = distance)
#calculating distance between LCE centroid & MFFP sampling coordinate to help identify problematic sites
dists <- numeric(0)
for(i in 1:nrow(PEN2_sites)){
  tmp <- PEN2_sites[i,]
  if(is.na(tmp$mffp_lat)){dist <- NA}else if(is.na(tmp$lce_lat)){dist <- NA}else{
    dist <- spDists(x=as.matrix(tmp[,c('mffp_long','mffp_lat')]), y =as.matrix(tmp[,c('lce_long','lce_lat')]), longlat = T)}
  dists <- c(dists,as.numeric(dist))
}
PEN2_sites$distance_lce_mffp_km <- dists
PEN2_sites <- PEN2_sites %>% dplyr::select(LCE,mffp_PEN2_nom,lce_nom,rhs_nom,distance_lce_rhs_m,distance_lce_mffp_km,mffp_lat,lce_lat,mffp_long,lce_long,mffp_coords,LCE_coords,survey,yr,ecosysteme,RHS,lce_masl,lce_perim_km,lce_area_sq.km,rhs_area_ha,rhs_perim_m)

##extract sites that are NOT in site list from previous dataset
new.sites <- filter(PEN2_sites, LCE %!in% PEN_sites$LCE)
colnames(new.sites)
colnames(PEN_sites)

writexl::write_xlsx(new.sites,'~/Desktop/nouveaux_sites_MFFP_deuxieme_envoi.xlsx')

##extract bad sites from premier envoi that might have new coords in deuxieme envoi to help identify right LCE
mauvais.sites.premier.envoi <- c('J2288','J2290','TP00001','TP040600','TPJulesL','TPpluto','F0627','F2494','A1587','68828','28926','21241','19640','19781','17284','17010','16774','08123','05253','05204','0406000','03366','03169','00663')
weird.sites <- filter(PEN2_sites, LCE %in% mauvais.sites.premier.envoi) %>% filter(!is.na(mffp_lat))

writexl::write_xlsx(weird.sites,'~/Desktop/vieux_sites_problematiques.xlsx')
writexl::write_xlsx(PEN2_sites,'~/Desktop/PEN2.xlsx')

##### generating a list of sites ####

#deleting unsalvageable LCEs
PEN2 <- filter(PEN2, LCE %!in% c('TP00001','F5465','F5463'))

#correcting badly entered LCEs
PEN2$LCE[PEN2$LCE == 'TP040600'] <- 'F3280'
PEN2$LCE[PEN2$LCE == '0406000'] <- 'F3280'
PEN2$LCE[PEN2$LCE == 'TPJulesL'] <- 'F4944'
PEN2$LCE[PEN2$LCE == 'TPpluto'] <- 'F4945'
PEN2$LCE[PEN2$LCE == 'F0627'] <- 'F3276'
PEN2$LCE[PEN2$LCE == '16774'] <- '72652'
PEN2$LCE[PEN2$LCE == '03169'] <- '03170'
PEN2$LCE[PEN2$LCE == '21241'] <- '21421'
PEN2$LCE[PEN2$LCE == '28926'] <- '28925'
PEN2$LCE[PEN2$LCE == 'A1587'] <- 'A1584'
PEN2$LCE[PEN2$LCE == 'E3493'] <- 'C1926'

#removing bad RHS info from LCE db because centroid does not fall into a polygon. In all cases: lakes in western Qc at the border with Ontario
LCE[LCE$LCE == '02226',c(3,11:14)] <- NA
LCE[LCE$LCE == '19509',c(3,11:14)] <- NA
LCE[LCE$LCE == '19532',c(3,11:14)] <- NA
LCE[LCE$LCE == '21421',c(3,11:14)] <- NA

#remove rare river sites
PEN2 <- filter(PEN2, str_length(LCE) == 5)

# #making a clean site list after filtration
PEN2 %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN2_sites

PEN2_sites <- dplyr::select(PEN2_sites, LCE:Longitude)

##adding sites from CC sheet which are not present in CPUE sheet
PEN3 <- read_xlsx(skip=0,"~/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx", sheet = 2)
colnames(PEN3)[3] <- 'LCE'
PEN3_sites <- PEN3[PEN3$LCE %!in% PEN2_sites$LCE,] %>% distinct(LCE,.keep_all = T)
PEN3_sites <- filter(PEN3_sites, LCE %!in% c('TP00001','F5465','F5463'))
PEN3_sites$LCE[PEN3_sites$LCE == 'TP040600'] <- 'F3280'
PEN3_sites$LCE[PEN3_sites$LCE == '0406000'] <- 'F3280'
PEN3_sites$LCE[PEN3_sites$LCE == 'TPJulesL'] <- 'F4944'
PEN3_sites$LCE[PEN3_sites$LCE == 'TPpluto'] <- 'F4945'
PEN3_sites$LCE[PEN3_sites$LCE == 'F0627'] <- 'F3276'
PEN3_sites$LCE[PEN3_sites$LCE == '16774'] <- '72652'
PEN3_sites$LCE[PEN3_sites$LCE == '03169'] <- '03170'
PEN3_sites$LCE[PEN3_sites$LCE == '21241'] <- '21421'
PEN3_sites$LCE[PEN3_sites$LCE == '28926'] <- '28925'
PEN3_sites$LCE[PEN3_sites$LCE == 'A1587'] <- 'A1584'
PEN3_sites$LCE[PEN3_sites$LCE == 'E3493'] <- 'C1926'
colnames(PEN3_sites)[c(6,8,9)] <- c('yr','Latitude','Longitude')
PEN3_sites <- dplyr::select(PEN3_sites, LCE:Longitude)
PEN3_sites <- PEN3_sites[PEN3_sites$LCE %!in% PEN2_sites$LCE,]
#remove rare river sites
PEN3_sites <- filter(PEN3_sites, str_length(LCE) == 5)
PEN3_sites$Latitude <- as.numeric(PEN3_sites$Latitude)
PEN3_sites$Longitude <- as.numeric(PEN3_sites$Longitude)

PEN2_sites <- bind_rows(PEN2_sites,PEN3_sites)

PEN2_sites <- left_join(PEN2_sites, LCE) %>% arrange(LCE)
PEN2_sites <- dplyr::select(PEN2_sites, -distance, -`NB d'inventaire`, -ecosysteme)

colnames(PEN2_sites)[c(2,4,5,6)] <- c('mffp_nom','survey','mffp_lat','mffp_long')

#checking that distances are small
dists <- numeric(0)
for(i in 1:nrow(PEN2_sites)){
  tmp <- PEN2_sites[i,]
  if(is.na(tmp$mffp_lat)){dist <- NA}else if(is.na(tmp$lce_lat)){dist <- NA}else{
    dist <- spDists(x=as.matrix(tmp[,c('mffp_long','mffp_lat')]), y =as.matrix(tmp[,c('lce_long','lce_lat')]), longlat = T)}
  dists <- c(dists,as.numeric(dist))
}

PEN2_sites$dist_mffp_lce <- dists

writexl::write_xlsx(PEN2_sites,'~/Desktop/MFFP_sites_clean_deuxieme_envoi.xlsx')


# #### CLEANING UP ####
# 
# #deleting unsalvageable LCEs 
# PEN <- filter(PEN, LCE %!in% c('J2288','J2290','TP00001','F2494')) #in new file, only TP000001 is problematic
# 
# #correcting badly entered LCEs
# PEN$LCE[PEN$LCE == 'TP040600'] <- 'F3280'
# PEN$LCE[PEN$LCE == '0406000'] <- 'F3280'
# PEN$LCE[PEN$LCE == 'TPJulesL'] <- 'F4944'
# PEN$LCE[PEN$LCE == 'TPpluto'] <- 'F4945'
# PEN$LCE[PEN$LCE == 'F0627'] <- 'F3276'
# PEN$LCE[PEN$LCE == '03169'] <- '03170'
# PEN$LCE[PEN$LCE == '03169'] <- '16774'
# PEN$LCE[PEN$LCE == '21241'] <- '21421'
# PEN$LCE[PEN$LCE == '28926'] <- '28925'
# PEN$LCE[PEN$LCE == 'A1587'] <- 'A1584'
# PEN$LCE[PEN$LCE == '16774'] <- '72652'
# 
# #removing bad RHS info from LCE db because centroid does not fall into a polygon. In all cases: lakes in western Qc at the border with Ontario
# LCE[LCE$LCE == '02226',c(3,11:14)] <- NA
# LCE[LCE$LCE == '19509',c(3,11:14)] <- NA
# LCE[LCE$LCE == '19532',c(3,11:14)] <- NA
# LCE[LCE$LCE == '21421',c(3,11:14)] <- NA
# 
# #some MFFP coordinates are erroneous and fall very far from polygons that correspond to LCE number entered by the MFFP.
# #if no way to fix them, then better remove altogether since fish data will not match 
# bad.lce <- c('00663','17010','17284','68828') #THIS HAS BEEN CORRECTED IN NEW SITE LIST
# 
# # #making a clean site list after filtration
# PEN %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN_sites
# colnames(PEN_sites) <- c('LCE','mffp_PEN_nom','survey','gear','station','valid','hasard','mffp_lat','mffp_long','date','species','captures','nb.weighed','total.mass.g','min.length.mm','max.length.mm','last_yr_sampled','abund')
# PEN_sites <- select(PEN_sites, LCE:survey, last_yr_sampled)
# 
# #remove rare river sites
# PEN_sites <- filter(PEN_sites, str_length(LCE) == 5)
# 
# PEN_sites <- left_join(PEN_sites, LCE)
# PEN_sites <- PEN_sites %>% rename(distance_lce_rhs_m = distance)
# writexl::write_xlsx(PEN_sites,'~/Desktop/MFFP_PEN_sites_clean_premierenvoi.xlsx')
# 
# 
# 
