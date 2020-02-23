#this script takes the benthos data on artififical substrate (MELCC) and match it with fishing stations (MELCC).

library(tidyverse)
library(readxl)
library(writexl)
library(sp)

benthos <- read_xlsx('~/Desktop/20191126_metadonnees_benthos_substratArtificiel_GRIL.xlsx',sheet='Liste des stations benthos',skip=2)
colnames(benthos) <- c('station','nom.riv.benthos','lat.benthos','long.benthos')

poisson <- read_xlsx('~/Desktop/metadonnees_donnees_peche.xlsx',sheet='Liste des stations de pêche',skip=1)
colnames(poisson) <- c('station','LCE','nom.riv.poissons','lat.poissons','long.poissons')

merge <- inner_join(benthos,poisson, by = 'station')

glimpse(merge)

distances <- numeric(0)

for(i in 1:nrow(merge)){
  pt1 <- merge[i,c('long.benthos','lat.benthos')] %>% as.matrix
  pt2 <- merge[i,c('long.poissons','lat.poissons')] %>% as.matrix
  distances[i] <- spDistsN1(pt1,pt2, longlat = T)
}
#all benthic + fish sites from the same station are either at the same spot or really close. Maximum distance is 150m.

merge$distance.coords.m <- distances*1000
write_xlsx(merge, '~/Desktop/merge.xlsx')
