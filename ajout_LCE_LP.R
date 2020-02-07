# ce script ajoute des LCEs au site lake pulse n'ayant pas de LCE car l'union dans GIS avec la GRHQ a retourné un polygone
# sans numéro LCE. Trouve le point LCE le plus proche et export un fichier xlsx.
# verdict: ces lacs n'ont vraiment pas de LCE

rm(list=ls())

library(tidyverse)
library(readxl)
library(scales)
library(sp)

# 0) load GIS info with LCE and RHS databases

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

LP_LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/LP_Qc_NoLacLCE.xlsx') %>%
  rename(RHS = ID_RHS)

LP_LCE[is.na(LP_LCE$LCE),] %>% select(-LCE) -> LP_noLCE
#peut-on retrouver ces sites dans la bdd LCE en utilisant la proximité géographique?
#adding the closest lake to data
out <- data.frame()
LCE <- filter(LCE, ecosysteme == 'lac') #getting rid of rivers
#getting rid of sites outside of quadrat of interest, to reduce computation time
LCE <- filter(LCE, lat >= min(LP_noLCE$latitude), lat <= max(LP_noLCE$latitude))
LCE <- filter(LCE, long >= min(LP_noLCE$longitude), long <= max(LP_noLCE$longitude))
coords.LCE <- LCE[,c('long','lat')] %>% as.matrix

for(i in 1:4){
  tmp <- LP_noLCE[i,]
  coords.LP <- as.matrix(tmp[,c(3,2)])
  dists <- spDistsN1(pts = coords.LCE, pt = coords.LP, longlat = T)
  min.dist <- which.min(dists)
  tmp <- cbind(tmp,LCE[min.dist,])
  out <- bind_rows(out,tmp)
}

out$coordsLP <- paste(out$latitude,out$longitude,sep=', ')
out$coordsLCE <- paste(out$lat,out$long,sep=', ')
writexl::write_xlsx(out, '~/Desktop/LP_LCE_db.xlsx')

###

# coordsLP	coordsLCE	Vérifié goole?
#   49.858757, -68.740622	49.86252006, -68.72065817	oui, le même lac
# 50.718456, -71.765768	50.6922898154, -71.6847352099	oui, pas le même lac
# 45.966328, -74.301609	45.9664332832, -74.2957206426	oui, le même lac
# 50.204665, -66.179286	50.2942100483, -66.2864789032	oui, pas le même lac

# Bref, 06-066 == 