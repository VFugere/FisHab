# code exploratoire analyse de données IFD PEN-OC/DJ/T/OF
# pour projet gradients de taille & modélisation distribution espèces charismatiques

rm(list=ls())

library(tidyverse)
library(readxl)
library(scales)
library(sp)
library(rworldmap)
library(RColorBrewer)

'%!in%' <- function(x,y)!('%in%'(x,y))

good.surveys <- c('PENDJ','PENOF','PENT','PENOC')

# load LCE db, LP db with LCE info, and MFFP fish data

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

# #en date du 7 feb 2020, les données de PS sont inutilisables (on nous a fourni les quotas, pas les captures...)
# PS <- read_xlsx(skip=2,'/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFA - Rapport Pêche Sportive 2000-2018.xlsx')
# colnames(PS)[2] <- 'LCE'
# #n_distinct(PS$LCE) #9135 sites!
# #sum(is.na(PS$LCE))
# PS %>% distinct(LCE) %>% select(LCE) -> ps_sites
# ps_sites <- left_join(ps_sites, LCE)
# #table(ps_sites$ecosysteme) #8918 lacs, 53 rivières

INV <- read_xlsx(skip=5,"/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFD- 1 - Rapport Inventaire sur plan d'eau (IPE) 2000-2018.xlsx")
colnames(INV)[1] <- 'LCE'
INV$yr <- lubridate::year(INV$`Date de levée`)
INV$abund <- as.numeric(INV$`Nbre capturé`)

bad.sp.codes <- c('RIEN','-','POIS','NI','AU')
INV <- filter(INV, `Espèce` %!in% bad.sp.codes)
INV <- filter(INV, !is.na(abund)) 

INV %>% distinct(LCE, .keep_all = T) %>% select(LCE,`Nom plan d'eau`,`Type de pêche`) -> inv_sites
inv_sites <- left_join(inv_sites, LCE)

#extracting PEN and PNN from DB
PNN <- filter(INV, `Type de pêche` == 'PNN')
PEN <- filter(INV, `Type de pêche` %in% good.surveys)
PEN <- filter(PEN, `Engin` == 'Filet expérimental') #quelques trucs qui devraient pas être là 
PEN <- filter(PEN, LCE != '04060000') #removing the one river site, which might be in Reservoir des Sables (LCE suspicious)

# #can i use LCE to identify sites?
# sum(is.na(PEN$LCE))
# sum(str_length(PEN$LCE) > 1)
# #yes, all sites have an LCE

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
#writexl::write_xlsx(PEN_sites,'~/Desktop/MFFP_PEN_sites.xlsx')

#a lot of quality control done in QGIS & Excel
#1) confirm that all lakes have the same name in 3 DB (MFFP, LCE, RHS)
#2) when they don't, confirm that mffp % lce fall in the same polygon, and that polygon from the RHS is the right one
#3) Some LCEs were badly entered by the MFFP. Found the right ones using coords and GIS.
#   -TPO040600 -> F3280
#   -O040600 -> F3280
#   -TPJulesL -> F4944
#   -TPpluto -> F4945
#   -F0627 -> F3276
#4) Some LCEs are missing from the GRHQ, so spatial join returned an erroneous polygon from the RHS.
#   RHS for these was deleted. These were: 02226 - 
#5) some LCEs are just bad. No coords, or coords outside of Qc, and the LCE is erreoneous.
#   Nothing we can do with that so delete the data before analyses. These are: J2288 - J2290 - TP00001


PNN %>% distinct(LCE, .keep_all = T) %>% select(LCE,`Nom plan d'eau`,`Type de pêche`) -> PNN_sites
PNN_sites <- left_join(PNN_sites, LCE)
sum(PNN_sites$LCE %in% PEN_sites$LCE)

# #exporter fichiers
# PEN %>% group_by(`Espèce`) %>% summarize(abund = sum(`abund`)) %>% arrange(desc(abund)) %>% writexl::write_xlsx(., '~/Desktop/poissonsMFFP.xlsx')

# ## recoupement avec autres BDD du Qc
# load("/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/QGIS_output_fishab.qgz.RData")
# melcc <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx', col_types = 'text') %>%
#   select(STATION) %>% rename(LCE = STATION) %>% distinct(LCE)
# LCE_poissons <- bind_rows(select(inv_sites,LCE), select(ps_sites,LCE), select(melcc, LCE)) %>% distinct(LCE) %>% drop_na
# RSVL <- RSVL %>% filter(NO_LAC %in% LCE_poissons$LCE) %>% select(NOM_LAC:FICHE_SUIV)
# benthos <- benthos %>% filter(NO_LCE %in% LCE_poissons$LCE) %>% select(NO_STATION:DOCUM)
# profiles <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/profils lacs/Demande de Annick St-Pierre FisHab - RS.XLSX')
# colnames(profiles)[10] <- 'LCE'
# profiles <- profiles %>% filter(LCE %in% LCE_poissons$LCE)
# writexl::write_xlsx(path= '~/Desktop/MFFP_MELCC_overlap.xlsx',
#                     x = list(rsvl = RSVL, benthos = benthos, profiles = profiles))
# poiss_benthos <- INV %>% filter(LCE %in% benthos$NO_LCE)
# writexl::write_xlsx(poiss_benthos,path= '~/Desktop/poissons_sites_benthos.xlsx')

# #test ensemencement: peut-on retrouver un lac par son nom?
# PEN_sites$nom_court <- sub(',\\s.*','',PEN_sites$nom.minuscule)
# test<-readxl::read_xlsx('~/Desktop/ensem_Lau_2017.xlsx')
# test$Nom <- sub(',\\s.*','',test$Nom)
# lau_sites <- filter(PEN_sites, nom_court %in% test$Nom)
# writexl::write_xlsx(lau_sites,'~/Desktop/testsites.xlsx')
# filter(PEN,LCE=='00442')

#combien de time series
ts <- PEN %>% mutate(LCE_yr = paste0(LCE,'_',yr)) %>% distinct(LCE_yr, .keep_all = T) %>% 
  select(LCE:Engin,Latitude,Longitude,yr,LCE_yr) %>% add_count(LCE) %>% distinct(LCE, .keep_all = T) %>% 
  select(LCE:Longitude,n) %>% rename(yrs.of.data = n)
hist(ts$yrs.of.data, breaks=30)
max(ts$yrs.of.data) #pas de time series utilisables

#combien de zero pour les espèces ciblées?
savi <- PEN %>% filter(`Type de pêche` == 'PENDJ', `Espèce` == 'SAVI') %>%
  group_by(LCE,yr) %>% summarize(abund = sum(abund))
sum(savi$abund == 0)
sana <- PEN %>% filter(`Type de pêche` == 'PENT', `Espèce` == 'SANA') %>%
  group_by(LCE,yr) %>% summarize(abund = sum(abund))
sum(sana$abund == 0)
safo <- PEN %>% filter(`Type de pêche` == 'PENOF', `Espèce` == 'SAFO') %>%
  group_by(LCE,yr) %>% summarize(abund = sum(abund))
sum(safo$abund == 0) #1 zero
saal <- PEN %>% filter(`Type de pêche` == 'PENOC', `Espèce` == 'SAAL') %>%
  group_by(LCE,yr) %>% summarize(abund = sum(abund))
sum(saal$abund == 0)


#### carte ####

PEN_sites <- INV %>% filter(`Type de pêche` %in% good.surveys) %>% select(LCE:Longitude) %>% distinct(LCE, .keep_all = T) %>% rename(lat=Latitude,long=Longitude)
PNN_sites <- INV %>% filter(`Type de pêche` %!in% good.surveys) %>% select(LCE:Longitude) %>% distinct(LCE, .keep_all = T)%>% rename(lat=Latitude,long=Longitude)

#map
map <- getMap(resolution = "low")
cols <- brewer.pal(7, 'Dark2')[1:3]
#cols <- nationalparkcolors::park_palette("Saguaro",3)
#cols <- brewer.pal(11, 'RdYlBu')[c(3,6,9)]

ps_sites$col <- 3
ps_sites$alph <- 0.4
ps_sites$ptcex <- 0.15

PEN_sites$col <- 1
PEN_sites$alph <- 0.8
PEN_sites$ptcex <- 0.4

PNN_sites$col <- 2
PNN_sites$alph <- 0.5
PNN_sites$ptcex <- 0.3

all_sites <- bind_rows(PEN_sites,PNN_sites,ps_sites) %>% select(lat,long,col,alph,ptcex) %>% filter(!is.na(lat), !is.na(long))

x <- all_sites$long
y <- all_sites$lat

xrange <- range(x)+c(-1,1)
yrange <- range(y)+c(-1,1)

pdf('~/Desktop/MFFPmap.pdf',width=16,height = 14, pointsize = 30)
plot(map, xlim = xrange, ylim = yrange,col='gray95',border=0,asp=1.2,axes=F,cex.lab=0.5)
points(x=x,y=y,pch=16,col=alpha(cols[all_sites$col],all_sites$alph),cex=all_sites$ptcex)
legend(x=xrange[2]-8,y=yrange[2],legend=c('standardized surveys','non-standardized surveys','sport fishing'),pch=16,col=cols,bty='n')
dev.off()

### GRADIENTS taille-latitude ####

inv <- INV
rm(INV)

inv$max.l <- as.numeric(inv$`Long max (mm)`)

inv <- inv %>% group_by(LCE, Espèce) %>%
  summarize(lat = mean(Latitude, na.rm=T), long = mean(Longitude, na.rm=T), size = max(max.l, na.rm=T))
colnames(inv)[2] <- 'species'

inv <- inv %>% filter(!is.na(lat))
inv <- inv %>% filter(!is.na(size)) %>% filter(is.finite(size))
inv <- as.data.frame(inv)

data <- inv

data <- data %>% add_count(species)

taxlist <- data %>% distinct(species) %>% pull(species)
data$scl.size <- 0
data$scl.lat <- 0
for(i in 1:length(taxlist)){
  tmp <- filter(data, species == taxlist[i])
  scl.size <- scale(tmp$size, scale = T)[,1]
  scl.lat <- scale(tmp$lat, scale = F)[,1]
  data[data$species == taxlist[i], 'scl.size'] <- scl.size
  data[data$species == taxlist[i], 'scl.lat'] <- scl.lat
}

data$species <- as.factor(data$species)
data$scl.size[!is.finite(data$scl.size)] <- NA
data <- data %>% filter(size > 0)

reg.results <- data.frame('focalsp' = character(0),
                          'min.lat' = numeric(0),
                          'max.lat' = numeric(0),
                          'n' = numeric(0),
                          'slope' = numeric(0),
                          'SE' = numeric(0),
                          'p' = numeric(0),
                          'r2' = numeric(0), 
                          'mean.size' =numeric(0),
                          stringsAsFactors = F)

for(i in 1:nlevels(data$species)){
  focalsp <- levels(data$species)[i]
  spdat <- data %>% filter(species == focalsp)
  spdat <- spdat %>% drop_na(scl.size)
  if(nrow(spdat) >= 10){
    #lmmod <- lm(ind.size~x,spdat)
    lmmod <- lm(scl.size~lat,spdat)
    r2 <- summary(lmmod)$r.squared
    slope <- coef(lmmod)[2]
    p <- summary(lmmod)$coefficients[2,4]
    SE <- summary(lmmod)$coefficients[2,2]
    n <- nrow(spdat)
    min.lat <- min(spdat$lat)
    max.lat <- max(spdat$lat)
    mean.size <- mean(spdat$size)
    results1 <- c(focalsp)
    results2 <- c(min.lat,max.lat,n,slope,SE,p,r2,mean.size)
    reg.results[nrow(reg.results)+1,1] <- results1
    reg.results[nrow(reg.results),2:9] <- results2
  }
}

reg.results$xrange <- reg.results$max.lat-reg.results$min.lat

reg.results$bubblesize <- rescale(reg.results$r2, c(1,4))
reg.results$bubblecol <- 'dark gray'
reg.results$bubblecol[reg.results$p < 0.05 & reg.results$slope < 0] <- 'red'
reg.results$bubblecol[reg.results$p < 0.05 & reg.results$slope > 0] <- 'blue'
reg.results$bubble.pch <- 16
reg.results$bubble.pch[reg.results$bubblecol == 'dark gray'] <- 1

hist(reg.results$slope, breaks=50)
# 
#plot à la Dornelas
plot(r2~slope,reg.results,bty='n',pch=16,col=alpha(bubblecol,0.5),cex=bubblesize)
abline(v=0,lty=2)
#not very exciting...

#####

pdf('~/Desktop/regressions.pdf',width=8,height=7,pointsize=14)

plotfunctions::emptyPlot(xlim = range(data$lat),yaxt='n',xaxt='n',ann=F, ylim=c(0,quantile(data$size,0.99)),bty='l')
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab='latitude')
title(ylab='max length (mm)',line=2.8)
for(i in 1:nlevels(data$species)){
  focalsp <- levels(data$species)[i]
  spdat <- data %>% filter(species == focalsp) %>% arrange(lat)
  spdat <- filter(spdat, !is.na(scl.size))
  if(nrow(spdat) >= 10){
    lmmod <- lm(scl.size~lat,spdat)
    lncol <- 'dark gray'
    lnwd <- 1
    if(summary(lmmod)$coefficients[2,4] < 0.05){
      lnwd <- 1
      if(summary(lmmod)$coefficients[2,1] > 0){lncol <- 'blue'}else{
        lncol <- 'red'}
    }
    lmmod <- lm(size~lat,spdat)
    points(fitted(lmmod)~spdat$lat,col=alpha(lncol,0.5),type='l',lwd=lnwd)
  }
}

dev.off()

##### slopes  #####

pdf('~/Desktop/slopes.pdf',width=14,height=6,pointsize=14)
par(mfrow=c(1,3),cex=1,mar=c(4,2.5,0,0),oma=c(0,2,0,0))

plot(slope~n,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',yaxt='n',xaxt='n',ann=F, log = 'x')
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab=number~of~lakes,line=2.8)
abline(h=0,lty=3)

mtext(expression(Delta~body~mass~per~degree~latitude~(sd)),outer = T,side=2,line=0.5)

# which.max(reg.results$n)
# reg.results[244,]
# reg.results %>% arrange(n) %>% pull(focalsp)

plot(slope~xrange,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',yaxt='n',xaxt='n',ann=F,xlim=(range(reg.results$xrange) + c(-0.5,0.5)))
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab='latitudinal range',line=2.8)
#title(ylab=slope~(beta),line=2.8)
abline(h=0,lty=3)

plot(slope~mean.size,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',log='x',yaxt='n',xaxt='n',ann=F)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
#title(ylab=slope~(beta),line=2.8)
title(xlab='max length (mm)',line=2.8)
abline(h=0,lty=3)

dev.off()
