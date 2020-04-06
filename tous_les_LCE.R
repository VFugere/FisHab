#ce script load la couche GIS LCE contenant les centroids de lac et les centroides de rivières
#une union a été fait dans qGIS pour ajouter un numéro RHS à partir de la GRHQ
#on joint ensuite les deux jeux de données (lac + rivières) pour obtenir une
#base de données contenant tous les numéros LCE existants dans le fichier téléchargé
#sur données Québec à l'automne 2019.

library(tidyverse)
library(readxl)
library(sp)
library(rgdal)

load("/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/QGIS_output_fishab.qgz.RData")

LCE_lac <- select(LCE_lac, NO_LAC, ID_RHS, NOM_LAC_MINUSCULE, NOM_LAC, LATITUDE_DEGRE_DEC_NAD83, LONGITUDE_DEGRE_DEC_NAD83, ALTITUDE_LAC, PERIMETRE_LAC, SUPERFICIE_NETTE_LAC, SUP_HA, PERIM_M, TOPONYME, Distance)
colnames(LCE_lac) <- c('LCE', 'RHS','lce_nom.minuscule','lce_nom','lce_lat','lce_long','lce_masl','lce_perim_km','lce_area_sq.km','rhs_area_ha','rhs_perim_m','rhs_nom','distance')
LCE_lac$ecosysteme <- 'lac'

LCE_riv <- select(LCE_riv, NO_COURS_DEAU, ID_RHS, NOM_COURS_DEAU_MINUSCULE, NOM_COURS_DEAU, LATITUDE_DEG_DEC_NAD83, LONGITUDE_DEG_DEC_NAD83, SUP_HA, PERIM_M, TOPONYME, Distance)
colnames(LCE_riv) <- c('LCE','RHS','lce_nom.minuscule','lce_nom','lce_lat','lce_long','rhs_area_ha','rhs_perim_m','rhs_nom','distance')
LCE_riv$ecosysteme <- 'riviere'

touslesLCE <- bind_rows(LCE_lac,LCE_riv) %>% select(ecosysteme, LCE, RHS, everything())
writexl::write_xlsx(touslesLCE, '/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

