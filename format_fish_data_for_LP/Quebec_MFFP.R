# ce script prend l'IFA du MFFP et extrait les données pour les lacs LP
# la correspondence spatiale des sites se fait grâce aux numéros LCEs qui ont été vérifiés pour
# la liste de sites MFFP et la liste de sites LP

rm(list=ls())

library(tidyverse)
library(readxl)

'%!in%' <- function(x,y)!('%in%'(x,y))

# load LCE db, LP db with LCE info, and MFFP fish data

LP_LCE <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/LP_Qc_NoLacLCE.xlsx') %>%
  rename(RHS = ID_RHS)

PEN_sites <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/MFFP_PENsites_BasicInfo.xlsx')

CPUE <- read_xlsx(skip=0,"~/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx")
colnames(CPUE)[3] <- 'LCE'

PA <- read_xlsx(skip=0,"~/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx", sheet = 2)
colnames(PA)[3] <- 'LCE'

qc.codes <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/LP/fish_output/taxonomic_codes.xlsx', sheet = 'QC-MFFP')
fh.codes <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/LP/fish_output/taxonomic_codes.xlsx', sheet = 'clean')
qc.codes$FS <- fh.codes$fish_species_ID[match(qc.codes$binomial,fh.codes$binomial)]
qc.codes$FS[139] <- 'FS322'
rm(fh.codes)
qc.codes <- filter(qc.codes, !is.na(binomial))

#### LCE CLEANUP in MFFP DB ####

#deleting unsalvageable LCEs
PA <- filter(PA, LCE %!in% c('TP00001','F5465','F5463'))
#correcting badly entered LCEs
PA$LCE[PA$LCE == 'TP040600'] <- 'F3280'
PA$LCE[PA$LCE == '0406000'] <- 'F3280'
PA$LCE[PA$LCE == 'TPJulesL'] <- 'F4944'
PA$LCE[PA$LCE == 'TPpluto'] <- 'F4945'
PA$LCE[PA$LCE == 'F0627'] <- 'F3276'
PA$LCE[PA$LCE == '16774'] <- '72652'
PA$LCE[PA$LCE == '03169'] <- '03170'
PA$LCE[PA$LCE == '21241'] <- '21421'
PA$LCE[PA$LCE == '28926'] <- '28925'
PA$LCE[PA$LCE == 'A1587'] <- 'A1584'
PA$LCE[PA$LCE == 'E3493'] <- 'C1926'
#remove rare river sites
PA <- filter(PA, str_length(LCE) == 5)
#join with clean site list

#### format PA database ####

PA <- select(PA, LCE:`Espèce (code)`, -`Nb d'inventaire`)
colnames(PA) <- c('LCE','mffp_nom','yr','survey','lat','long','species')
PA <- PA %>% select(-lat, -long, -survey) #since will use the correct centroid from LCE db
#remove duplicate entries (species caught in multiple years)
PA <- PA %>% group_by(LCE) %>% distinct(species, .keep_all = T) %>% ungroup
PA <- PA %>% select(LCE,species) %>% filter(LCE %in% LP_LCE$LCE)
n_distinct(PA$LCE) #17 lakes with PEN data
PA <- filter(PA, species %!in% c('POIS','RIEN'))
PA$FS <- qc.codes$FS[match(PA$species,qc.codes$code)]
PA$pres <- 1
PA <- select(PA, -species)
PA <- PA %>% group_by(LCE) %>% distinct(FS, .keep_all = T) %>% spread(FS, pres) %>% ungroup

#add LP column name
LP_LCE <- select(LP_LCE, Lake_ID, LCE)
PA <- left_join(PA, LP_LCE)
PA$data_source <- 'QC_PE'
PA <- select(PA, data_source, Lake_ID, FS014:FS363) %>% as.data.frame
PA[is.na(PA)] <- 0

writexl::write_xlsx(PA, '~/Google Drive/Recherche/Lake Pulse Postdoc/data/LP/fish_output/QC_MFFP_deuxiemeevoi.xlsx')

#############
#### OLD ####
#############

# #### match MFFP and LP db ####
# 
# # INV %>% select(LCE:Longitude) %>% distinct(LCE, .keep_all = T) -> inv_sites
# # inv_sites <- left_join(inv_sites, LCE)
# # 
# # #sites LP sans LCE
# # LP_LCE[is.na(LP_LCE$LCE),] -> LP_noLCE
# # #peut-on retrouver ces sites dans la bdd inv?
# # #adding the closest lake to data
# # out <- data.frame()
# # inv_sites_with_coords <- filter(inv_sites, !is.na(Latitude), !is.na(Longitude))
# # for(i in 1:4){
# #   tmp <- LP_noLCE[i,]
# #   dist.df <- as.matrix(tmp[,c(3,2)])
# #   coords.mffp <- inv_sites_with_coords %>% select(Longitude,Latitude) %>% as.matrix
# #   dist.df <- rbind(dist.df, coords.mffp)
# #   dist.mat <- sp::spDists(dist.df, longlat=TRUE)
# #   min.dist <- which.min(dist.mat[2:nrow(dist.mat),1])
# #   tmp <- cbind(tmp,inv_sites_with_coords[min.dist,])
# #   out <- bind_rows(out,tmp)
# # }
# # out$coordsLP <- paste(out$latitude,out$longitude,sep=', ')
# # out$coordsMFFP <- paste(out$Latitude,out$Longitude,sep=', ')
# # INV %>% filter(LCE == '68828') 
# 
# # # des données PENOF sont dispos pour le Lac Lake Pulse LCE = '68828'
# # # Par contre il y a un problème avec ce numéro LCE dans bdd IFD... le lce est faux. Alors je mets le LCE custom 
# # # inventé pour LP, dans la bdd du MFFP. Il y sûrement d'autres LCE éronnés
# 
# INV[INV$LCE == '68828','LCE'] <- 'LP066'
# 
# LP_LCE <- LP_LCE %>% select(-LCE, -Notes) %>% rename(LCE = LCE_adapt)
# 
# #sum(LP_LCE$LCE %in% INV$LCE) #26 lacs
# 
# inv_lp <- filter(INV, LCE %in% LP_LCE$LCE)
# 
# inv_lp <- inv_lp[,c(1:4,8:16)]
# # table(inv_lp$`Type de pêche`)
# # table(inv_lp$`Engin`)
# # #beaucoup de pêche non-normalisée
# 
# names(inv_lp)[2:13] <- c('MFFP_name','MFFP_program','MFFP_gear','MFFP_lat','MFFP_long',
#                          'sampling_date','species','nb_captured','nb_weighed',
#                          'total_mass_g','min_length_mm','max_length_mm')
# 
# ### need to go from long to wide
# 
# inv_wide <- inv_lp %>% select(LCE, MFFP_program, sampling_date, species, nb_captured) %>%
#   filter(species != '-') %>%
#   mutate(nb_captured = as.numeric(nb_captured)) %>%
#   group_by(LCE, MFFP_program, sampling_date, species) %>%
#   summarize(nb_captured = sum(nb_captured, na.rm=T)) %>% 
#   spread(species, nb_captured, fill = NA)
# 
# inv_lp_meta <- inv_lp %>% filter(species != '-') %>% select(LCE:sampling_date)
# inv_lp_meta$LCE_prog_date <- with(inv_lp_meta, paste(LCE,MFFP_program,sampling_date,sep='_'))
# inv_lp_meta <- distinct(inv_lp_meta, LCE_prog_date, .keep_all = T)
# inv_lp_meta <- inv_lp_meta %>% select(-LCE_prog_date)
# 
# inv_lp <- left_join(inv_lp_meta, inv_wide, by = c('LCE', 'MFFP_program', 'sampling_date'))
# rm(inv_lp_meta,inv_wide)
# 
# LP_LCE <- LP_LCE %>% select(Lake_ID:lake_name,LCE)
# names(LP_LCE)[2:4] <- c('LP_lat','LP_long','LP_name')
# 
# inv_lp <- right_join(LP_LCE,inv_lp,by='LCE')
# 
# ### add spatial distance among points & total catch ##
# 
# distance_MFFP_LP_km <- numeric(0)
# 
# for(i in 1:nrow(inv_lp)){
#   sub <- inv_lp[i,]
#   point1 <- c(sub$MFFP_long,sub$MFFP_lat)
#   point2 <- c(sub$LP_long,sub$LP_lat)
#   distance <- pointDistance(point1, point2, lonlat=T)/1000
#   distance_MFFP_LP_km <- c(distance_MFFP_LP_km,as.numeric(distance))
# }
#   
# inv_lp$distance_MFFP_LP_km <- distance_MFFP_LP_km
# 
# inv_lp <- arrange(inv_lp, LCE, MFFP_program, desc(sampling_date), distance_MFFP_LP_km)
# 
# inv_lp$total_catch <- apply(inv_lp[,12:85], 1, sum, na.rm=T)
# 
# inv_lp <- inv_lp %>% select(Lake_ID,LCE,MFFP_program, MFFP_gear, sampling_date, distance_MFFP_LP_km, LP_name, MFFP_name, LP_lat, MFFP_lat, LP_long, MFFP_long, total_catch, everything())
# 
# ### choose the best line of data based on criteria listed above
# 
# clean <- inv_lp[0,]
# 
# good.surveys <- c('PENDJ','PENOF','PENT','PENOC')
# 
# lake_list <- distinct(inv_lp, Lake_ID) %>% pull(Lake_ID)
# for(i in 1:n_distinct(inv_lp$Lake_ID)){
#   sub <- filter(inv_lp, Lake_ID == lake_list[i])
#   if(nrow(sub) == 1){clean <- bind_rows(clean,sub)}else{
#   if(sum(sub$MFFP_program %in% good.surveys) > 0){sub <- filter(sub, MFFP_program %in% good.surveys)}
#   sub$year <- substr(sub$sampling_date, 1, 4)
#   sub <- arrange(sub, desc(year), distance_MFFP_LP_km)
#   sub <- select(sub, -year)
#   clean <- bind_rows(clean,sub[1,])}
# }
# 
# # writexl::write_xlsx(clean, '~/Desktop/MFFP_LP.xlsx')
# 
# ### all lake names match
# ### all distances are small enough
# 
# ## checked everything in gmaps and it matches
# 
# # LP	mffp	verified on gmaps
# # 48.775038,-78.990471	48.823151,-78.947556	yes
# # 47.686078,-70.354174	47.6808,-70.35613	yes
# # 46.227152,-76.065994	46.21611,-76.04401	yes
# # 46.458514,-75.53259	46.4652,-75.5211	yes
# # 46.938328,-71.387602	46.9342,-71.3898	yes
# # 45.803346,-71.89833	45.804444,-71.902778	yes
# # 45.948123,-74.143544	45.95046,-74.14225	yes
# # 45.134266,-72.264132	45.135028,-72.281222	yes
# # 46.389302,-70.48155	46.391266,-70.48434	yes
# # 45.535899,-70.892581	45.52933,-70.88844	yes
# # 45.689327,-70.919835	45.69089,-70.92398	yes
# # 48.789738,-64.822024	,	NA
# # 48.797257,-64.58328	,	NA
# # 47.184158,-69.563804	47.18328,-69.56432	yes
# # 46.084681,-71.507629	46.083822,-71.510011	yes
# # 45.451114,-72.144109	45.44548,-72.15079	yes
# # 45.536429,-72.035147	45.54207,-72.04121	yes
# # 45.257565,-71.991253	45.24766,-72.00045	yes
# # 47.426446,-68.780087	47.426992,-68.776832	yes
# # 47.702298,-68.588895	47.681017,-68.584082	yes
# # 47.671886,-68.834148	47.65621,-68.82032	yes
# # 47.416559,-69.890442	47.423019,-69.880005	yes
# # 45.992206,-74.005122	45.9909,-73.984	no, but mffp point is not in a lake. Just off to the east of croche
# # 48.232562,-71.249643	48.231,-71.251	yes
# # 48.214755,-79.052164	48.213796,-79.070927	yes
# # 49.858757,-68.740622	49.85873,-68.73976	yes
# 
# ### check if i can improve data for lakes with PNN. Are there more than one surveys?
# 
# # lakes.to.check <- c(2,5,7,11,12,13,17,23,24,25)
# # 
# # i<-lakes.to.check[10]
# # sub <- filter(inv_lp, Lake_ID == lake_list[i])
# # sub
# 
# #parfois une seule entrée, parfois plusieurs, toujours de la PNN, parfois plusieurs engins
# #les écarts de date et de distance ne sont pas extrêmes
# #regroupons toutes ces données, pour arriver à quelque chose de similaire aux données pour d'autres provinces
# #où la pêche n'est pas normalisée
# 
# # #y a-t-il des lacs qui ont plus qu'un type de PEN?
# # i<-26
# # sub <- filter(inv_lp, Lake_ID == lake_list[i])
# # sub
# # #non! mais certains lacs ont un PEN + PECPM. 
# # #mais en date du 11 Fev 2020, le PECPM est incomplet, donc exclue
# 
# # recompute, summing across surveys of the same type (either PNN or PEN when avail)
# 
# clean <- inv_lp[0,]
# 
# lake_list <- distinct(inv_lp, Lake_ID) %>% pull(Lake_ID)
# for(i in 1:length(lake_list)){
#   sub <- filter(inv_lp, Lake_ID == lake_list[i])
#   #pêche non normalisée: pool all obs, report all gear, 
#   #most recent sampling, mean distance of all points, and total catch across samples
#   if(sum(sub$MFFP_program %in% good.surveys) == 0){
#     sub_meta <- sub[1,1:2]
#     sub_meta$MFFP_program <- 'PNN' #pour overwrite la possibilité d'un PS
#     sub_meta$MFFP_gear <- paste(unique(sub$MFFP_gear),collapse = '_')
#     sub_meta$sampling_date <- max(sub$sampling_date)
#     sub_meta$distance_MFFP_LP_km <- mean(sub$distance_MFFP_LP_km, na.rm=T)
#     sub_meta <- bind_cols(sub_meta,sub[1,7:12])
#     sub_com <- sub[,13:ncol(sub)]
#     sub_com_clean <- sub_com[1,] # loop reports NA if only NAs for a given species, and 0 if true zeros were recorded
#     for(j in 1:ncol(sub_com)){
#       obs <- sub_com[,j] %>% drop_na %>% nrow
#       if(obs == 0){sub_com_clean[,j] <- NA}
#       else if(obs > 0){sub_com_clean[,j] <- sum(sub_com[,j],na.rm=T)}
#     }
#     sub <- bind_cols(sub_meta,sub_com_clean)
#     clean <- bind_rows(clean,sub)}
#   # if lake has at least one PEN sample, then discard PNN, and pool all PEN samples
#   # since all from the same program, for LP lakes at least
#   else if(sum(sub$MFFP_program %in% good.surveys) > 0){
#     sub <- filter(sub, MFFP_program %in% good.surveys)
#     sub_meta <- sub[1,1:3]
#     sub_meta$MFFP_gear <- paste(unique(sub$MFFP_gear),sep='_')
#     sub_meta$sampling_date <- max(sub$sampling_date)
#     sub_meta$distance_MFFP_LP_km <- mean(sub$distance_MFFP_LP_km, na.rm=T)
#     sub_meta <- bind_cols(sub_meta,sub[1,7:12])
#     sub_com <- sub[,13:ncol(sub)]
#     sub_com_clean <- sub_com[1,] # loop reports NA if only NAs for a given species, and 0 if true zeros were recorded
#     for(j in 1:ncol(sub_com)){
#       obs <- sub_com[,j] %>% drop_na %>% nrow
#       if(obs == 0){sub_com_clean[,j] <- NA}
#       else if(obs > 0){sub_com_clean[,j] <- sum(sub_com[,j],na.rm=T)}
#       }
#     sub <- bind_cols(sub_meta,sub_com_clean)
#     clean <- bind_rows(clean,sub)
#   }
# }
# 
# clean <- rename(clean, 'most_recent_sample' = sampling_date)
# 
# writexl::write_xlsx(clean, '~/Desktop/MFFP_LP.xlsx')