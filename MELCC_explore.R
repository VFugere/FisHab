
library(tidyverse)
library(readxl)
library(writexl)

poisson <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx',sheet='Biomasse')

poisson %>% group_by(ESPE) %>% summarize(bm = sum(BIOMASSE)) %>% arrange(desc(bm))
poisson %>% group_by(ESPE) %>% summarize(bm = sum(QUANTITE)) %>% arrange(desc(bm))

n_distinct(poisson$ESPE)

#ajouter date
poisson$date <- as.Date(str_remove(poisson$DATE.PECHE, '\\ .*'),'%m/%d/%y')
poisson$year <- as.numeric(format(poisson$date, '%Y'))

#vieux poiss
old <- filter(poisson, year < 1991)
table(old$RIVIERE)
table(old$STATION)
table(old$METH.P)
old.g <- filter(old, METH.P == 'À GUÉ')
table(old.g$RIVIERE)
table(old.g$STATION)


sites <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx',sheet='Stations')

write_csv(sites,'~/Desktop/MELCC_sites.csv')

benthos.a <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/Benthos sur des substrats articifiels/benthos.accdb.xlsx',sheet='Stations_benthos')
benthos.a <- benthos.a %>% distinct(Stations) %>% filter(Stations %in% sites$STATION.BQMA)

benthos.n <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/benthos/20200409_donneesBrutes_AnnickStPierre_Fishab.xlsx',sheet='Stations')
stations <- as.numeric(benthos.n$`Station BQMA`)
stations[stations %in% sites$STATION.BQMA] %in% benthos.a$Stations
#only adds 3 sites...

218/334

sites <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/poissons/poissons.accdb.xlsx',sheet='Stations',col_types='text')

chimie <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/MELCC/suivi_eau_surface/suivi_physicochimique.xlsx')
chimie <- chimie %>% distinct(`Numéro de station BQMA *`) %>% pull(`Numéro de station BQMA *`)
chimie <- sub('.', '', chimie)
length(chimie[chimie %in% sites$STATION.BQMA])
235/334

idec <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/IDEC/BD-IDEC3/STATION.xlsx')
idec %>% distinct(BQMA_ID) %>% filter(BQMA_ID %in% sites$STATION.BQMA)

10/334
