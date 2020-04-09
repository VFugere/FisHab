## This code loads the MFFP IFA data and cleans it to only keep PEN data
## from sites in the clean species list

rm(list=ls())

library(tidyverse)
library(readxl)

'%!in%' <- function(x,y)!('%in%'(x,y))

good.surveys <- c('PENDJ','PENOF','PENT','PENOC')

# load LCE db, LP db with LCE info, and MFFP fish data

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

INV <- read_xlsx(skip=5,"/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFD- 1 - Rapport Inventaire sur plan d'eau (IPE) 2000-2018.xlsx")
colnames(INV)[1] <- 'LCE'
INV$yr <- lubridate::year(INV$`Date de levée`)
INV$abund <- as.numeric(INV$`Nbre capturé`)

bad.sp.codes <- c('RIEN','-','POIS','NI','AU')
INV <- filter(INV, `Espèce` %!in% bad.sp.codes)
INV <- filter(INV, !is.na(abund)) 

PEN <- filter(INV, `Type de pêche` %in% good.surveys)
PEN <- filter(PEN, `Engin` == 'Filet expérimental') #quelques trucs qui devraient pas être là 
PEN <- filter(PEN, LCE != '04060000') #removing the one river site

# #making a clean site list that includes LCE & RHS identifiers
# PEN %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN_sites
# colnames(PEN_sites) <- c('LCE','mffp_PEN_nom','survey','gear','station','valid','hasard','mffp_lat','mffp_long','date','species','captures','nb.weighed','total.mass.g','min.length.mm','max.length.mm','yr','abund')
# PEN_sites <- select(PEN_sites, LCE:survey, mffp_lat:date, yr)
# PEN_sites <- left_join(PEN_sites, LCE)
# PEN_sites$mffp_coords <- paste(PEN_sites$mffp_lat,',',PEN_sites$mffp_long)
# PEN_sites$LCE_coords <- paste(PEN_sites$lce_lat,',',PEN_sites$lce_long)
# PEN_sites <- PEN_sites %>% rename(distance_lce_rhs_m = distance)
# #calculating distance between LCE centroid & MFFP sampling coordinate to help identify problematic sites
# dists <- numeric(0)
# for(i in 1:nrow(PEN_sites)){
#   tmp <- PEN_sites[i,]
#   if(is.na(tmp$mffp_lat)){dist <- NA}else if(is.na(tmp$lce_lat)){dist <- NA}else{
#     dist <- spDists(x=as.matrix(tmp[,c('mffp_long','mffp_lat')]), y =as.matrix(tmp[,c('lce_long','lce_lat')]), longlat = T)}
#   dists <- c(dists,as.numeric(dist))
# }
# PEN_sites$distance_lce_mffp_km <- dists
# PEN_sites <- PEN_sites %>% select(LCE,mffp_PEN_nom,lce_nom,rhs_nom,distance_lce_rhs_m,distance_lce_mffp_km,mffp_lat,lce_lat,mffp_long,lce_long,mffp_coords,LCE_coords,survey,date,yr,ecosysteme,RHS,lce_masl,lce_perim_km,lce_area_sq.km,rhs_area_ha,rhs_perim_m)
# #writexl::write_xlsx(PEN_sites,'~/Desktop/MFFP_PEN_sites.xlsx')

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
#   Nothing we can do with that so delete the data before analyses. These are: J2288 - J2290 - TP00001

#deleting unsalvageable LCEs 
PEN <- filter(PEN, LCE %!in% c('J2288','J2290','TP00001','F2494'))

#correcting badly entered LCEs
PEN$LCE[PEN$LCE == 'TP040600'] <- 'F3280'
PEN$LCE[PEN$LCE == '0406000'] <- 'F3280'
PEN$LCE[PEN$LCE == 'TPJulesL'] <- 'F4944'
PEN$LCE[PEN$LCE == 'TPpluto'] <- 'F4945'
PEN$LCE[PEN$LCE == 'F0627'] <- 'F3276'
PEN$LCE[PEN$LCE == '03169'] <- '03170'
PEN$LCE[PEN$LCE == '03169'] <- '16774'
PEN$LCE[PEN$LCE == '21241'] <- '21421'
PEN$LCE[PEN$LCE == '28926'] <- '28925'
PEN$LCE[PEN$LCE == 'A1587'] <- 'A1584'
PEN$LCE[PEN$LCE == '16774'] <- '72652'

#removing bad RHS info from LCE db because centroid does not fall into a polygon. In all cases: lakes in western Qc at the border with Ontario
LCE[LCE$LCE == '02226',c(3,11:14)] <- NA
LCE[LCE$LCE == '19509',c(3,11:14)] <- NA
LCE[LCE$LCE == '19532',c(3,11:14)] <- NA
LCE[LCE$LCE == '21421',c(3,11:14)] <- NA

#some MFFP coordinates are erroneous and fall very far from polygons that correspond to LCE number entered by the MFFP.
#if no way to fix them, then better remove altogether since fish data will not match 
bad.lce <- c('00663','17010','17284','68828')

# #making a clean site list after filtration
PEN %>% arrange(LCE, desc(yr)) %>% distinct(LCE, .keep_all = T) -> PEN_sites
colnames(PEN_sites) <- c('LCE','mffp_PEN_nom','survey','gear','station','valid','hasard','mffp_lat','mffp_long','date','species','captures','nb.weighed','total.mass.g','min.length.mm','max.length.mm','last_yr_sampled','abund')
PEN_sites <- select(PEN_sites, LCE:survey, last_yr_sampled)
PEN_sites <- left_join(PEN_sites, LCE)
PEN_sites <- PEN_sites %>% rename(distance_lce_rhs_m = distance)
writexl::write_xlsx(PEN_sites,'~/Desktop/MFFP_PEN_sites_clean.xlsx')
