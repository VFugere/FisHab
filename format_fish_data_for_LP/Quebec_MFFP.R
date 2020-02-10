# ce script prend l'IFD du MFFP et extrait les données pour les lacs LP
# si un lac LP a été échantilloné plusieurs fois, des filtres séquentiels sont appliqués:
# 1) données provenant de PENDJ, PENOF, PENT, ou PENOC priorisé par rapport à PNN
# 2) les échantillonages plus récents sont priorisés
# 3) la distance entre les points d'échantillonage est aussi calculés pour identifier
# les sites problématiques. Les coordonnées de chaque site a été vérifiée dans google maps

rm(list=ls())

library(tidyverse)
library(readxl)
library(scales)
library(sp)

good.surveys <- c('PENDJ','PENOF','PENT','PENOC')

# load LCE db, LP db with LCE info, and MFFP fish data

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

LP_LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/LP_Qc_NoLacLCE.xlsx') %>%
  rename(RHS = ID_RHS)

INV <- read_xlsx(skip=5,"/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFD- 1 - Rapport Inventaire sur plan d'eau (IPE) 2000-2018.xlsx")
colnames(INV)[1] <- 'LCE'

# match MFFP and LP db

INV %>% select(LCE:Longitude) %>% distinct(LCE, .keep_all = T) -> inv_sites
inv_sites <- left_join(inv_sites, LCE)

# #sites LP sans LCE
# LP_LCE[is.na(LP_LCE$LCE),] -> LP_noLCE
# #peut-on retrouver ces sites dans la bdd inv?
# #adding the closest lake to data
# out <- data.frame()
# inv_sites_with_coords <- filter(inv_sites, !is.na(Latitude), !is.na(Longitude))
# for(i in 1:4){
#   tmp <- LP_noLCE[i,]
#   dist.df <- as.matrix(tmp[,c(3,2)])
#   coords.mffp <- inv_sites_with_coords %>% select(Longitude,Latitude) %>% as.matrix
#   dist.df <- rbind(dist.df, coords.mffp)
#   dist.mat <- sp::spDists(dist.df, longlat=TRUE)
#   min.dist <- which.min(dist.mat[2:nrow(dist.mat),1])
#   tmp <- cbind(tmp,inv_sites_with_coords[min.dist,])
#   out <- bind_rows(out,tmp)
# }
# out$coordsLP <- paste(out$latitude,out$longitude,sep=', ')
# out$coordsMFFP <- paste(out$Latitude,out$Longitude,sep=', ')
# INV %>% filter(LCE == '68828') 

# # des données PENOF sont dispos pour le Lac Lake Pulse LCE = '68828'
# # Par contre il y a un problème avec ce numéro LCE dans bdd IFD... le lce est faux. Alors je mets le LCE custom 
# # inventé pour LP, dans la bdd du MFFP. Il y sûrement d'autres LCE éronnés

INV[INV$LCE == '68828','LCE'] <- 'LP066'

rm(inv_sites)

LP_LCE <- LP_LCE %>% select(-LCE, -Notes) %>% rename(LCE = LCE_adapt)

#sum(LP_LCE$LCE %in% INV$LCE) #26 lacs

inv_lp <- filter(INV, LCE %in% LP_LCE$LCE)

inv_lp <- inv_lp[,c(1:4,8:16)]
# table(inv_lp$`Type de pêche`)
# table(inv_lp$`Engin`)
# #beaucoup de pêche non-normalisée

names(inv_lp)[2:13] <- c('MFFP_name','MFFP_program','MFFP_gear','MFFP_lat','MFFP_long',
                         'sampling_date','species','nb_captured','nb_weighed',
                         'total_mass_g','min_length_mm','max_length_mm')

LP_LCE <- LP_LCE %>% select(Lake_ID:lake_name,LCE)
names(LP_LCE)[2:4] <- c('LP_lat','LP_long','LP_name')

inv_lp <- left_join(inv_lp,LP_LCE,by='LCE')



