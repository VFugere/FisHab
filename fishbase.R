#Ce script télécharge toutes les informations contenus dans FishBase pour toutes les espèces de poissons
#d'eau douce canadiens, ou du moins pour toutes les espèces retrouvées au moins une fois dans une des bases
#de données de poissons du projet FisHab.
#initié par Vincent Fugère, avril 2020

rm(list=ls())

library(tidyverse)
library(skimr)
library(magrittr)
library(readxl)
library(writexl)
library(rfishbase)

# species_codes <- read_xlsx('~/Desktop/taxonomic_codes(v2020-06-17).xlsx')
# save(species_codes, file = './formatted_open_data/species_codes.RData')

load('./formatted_open_data/species_codes.RData')
#check species list

fish <- species_codes$clean.species.name 
fish <- str_replace(fish, '_', ' ')
fish <- fish[!is.na(fish)]
fish <- unique(fish)
fish <- sort(fish)
  
fish.val <- validate_names(fish)
fish[!fish %in% fish.val]
rm(fish.val)

#species basic info

species.df <- data.frame()
for(i in 1:length(fish)){
  sp.tmp <- as.data.frame(species(fish[i]))
  species.df <- bind_rows(species.df, sp.tmp)
}
rm(sp.tmp)

#ecology tab

ecology.df <- data.frame()
for(i in 1:length(fish)){
  ecol.tmp <- as.data.frame(ecology(fish[i]))
  ecology.df <- bind_rows(ecology.df, ecol.tmp)
}
rm(ecol.tmp)
duplicated(ecology.df$Species)
ecology.df[ecology.df$Species == 'Acipenser oxyrinchus',]
ecology.df[ecology.df$Species == 'Oncorhynchus clarkii',]
ecology.df[ecology.df$Species == 'Salmo trutta',]
ecology.df[ecology.df$Species == 'Salvelinus malma',]
ecology.df <- ecology.df %>% distinct(Species, .keep_all = T)

#diet

diet.df <- data.frame()

for(i in 1:length(fish)){
  tmp <- as.data.frame(diet(fish[i]))
  diet.df <- bind_rows(diet.df, tmp)
}

#brain size

brains.df <- data.frame()

for(i in 1:length(fish)){
  tmp <- as.data.frame(brains(fish[i]))
  brains.df <- bind_rows(brains.df, tmp)
}
brains.df <- brains.df %>% select(Species, BodyWeight:EncCoeff) %>% drop_na

#estimated trophic level

estimate.df <- data.frame()

for(i in 1:length(fish)){
  tmp <- as.data.frame(estimate(fish[i]))
  estimate.df <- bind_rows(estimate.df, tmp)
}

#estimated trophic level#

genetics.df <- data.frame()

for(i in 1:length(fish)){
  tmp <- as.data.frame(genetics(fish[i]))
  genetics.df <- bind_rows(genetics.df, tmp)
}

#export

species_list_FB <- fish
rm(i,fish,tmp, species_codes)
save.image(file='./formatted_open_data/fishbase.RData')
