#ce script load la couche GIS LCE contenant les centroids de lac et les centroides de rivières
#une union a été fait dans qGIS pour ajouter un numéro RHS à partir de la GRHQ
#on joint ensuite les deux jeux de données (lac + rivières) pour obtenir une
#base de données contenant tous les numéros LCE existants dans le fichier téléchargé
#sur données Québec à l'automne 2019.

library(tidyverse)
library(readxl)

load("/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS/QGIS_output_fishab.qgz.RData")

LCE_lac <- select(LCE_lac, NO_LAC, ID_RHS, LATITUDE_DEGRE_DEC_NAD83, LONGITUDE_DEGRE_DEC_NAD83, NOM_LAC, NOM_LAC_MINUSCULE)
colnames(LCE_lac)[1] <- 'LCE'
colnames(LCE_lac)[2:6] <- c('RHS','lat','long','nom','nom.minuscule')
LCE_lac$ecosysteme <- 'lac'

LCE_riv <- select(LCE_riv, NO_COURS_DEAU, ID_RHS, LATITUDE_DEG_DEC_NAD83, LONGITUDE_DEG_DEC_NAD83, NOM_COURS_DEAU, NOM_COURS_DEAU_MINUSCULE)
colnames(LCE_riv)[1] <- 'LCE'
colnames(LCE_riv)[2:6] <- c('RHS','lat','long','nom','nom.minuscule')
LCE_riv$ecosysteme <- 'riviere'

touslesLCE <- bind_rows(LCE_lac,LCE_riv)
writexl::write_xlsx(touslesLCE, '~/Desktop/tous_les_LCE.xlsx')

