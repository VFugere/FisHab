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

species_codes <- read_xlsx('~/Desktop/taxonomic_codes(v2020-05-05).xlsx')

fish <- species_codes$clean.species.name 
fish <- str_replace(fish, '_', ' ')
fish <- fish[!is.na(fish)]
fish <- unique(fish)
fish <- sort(fish)
  
fish.val <- validate_names(fish)
fish[!fish %in% fish.val]

fish[fish == 'Carassius cariassius'] <- 'Carassius carassius'
fish[fish == 'Cichlasoma managuense'] <- 'Parachromis managuensis'
fish[fish == 'Icthyomyzon castaneus'] <- 'Ichthyomyzon castaneus'
fish[fish == 'Lampetra japonica'] <- 'Lethenteron camtschaticum'
fish[102] <- 'Lampetra richardsoni'
fish[fish == 'Lepisosteus platyrhinchus'] <- 'Lepisosteus platyrhincus'
fish[fish == 'Moxostoma duquesnei'] <- 'Moxostoma duquesnii'
fish[fish == 'Mylcheilus caurinus'] <- 'Mylocheilus caurinus'
fish[fish == 'Oncorhynchus aquabonita'] <- 'Oncorhynchus mykiss'
fish[fish == 'Phoxinus neogaeus'] <- 'Chrosomus neogaeus'
fish[fish == 'Triglopsis quadricornis'] <- 'Myoxocephalus quadricornis'

fish <- unique(fish)

fish.val <- validate_names(fish)
fish[!fish %in% fish.val]
rm(fish.val)

species.df <- data.frame()

for(i in 1:length(fish)){
  sp.tmp <- as.data.frame(species(fish[i]))
  ecol.tmp <- as.data.frame(ecology(fish[i]))
  brains.tmp <- as.data.frame(brains(fish[i]))
  species.df <- bind_rows(species.df, sp.tmp)
  ecology.df <- bind_rows(ecology.df, ecol.tmp)
  
}

ecology.df <- data.frame()

for(i in 1:length(fish)){
  sp.tmp <- as.data.frame(species(fish[i]))
  ecol.tmp <- as.data.frame(ecology(fish[i]))
  brains.tmp <- as.data.frame(brains(fish[i]))
  species.df <- bind_rows(species.df, sp.tmp)
  ecology.df <- bind_rows(ecology.df, ecol.tmp)
  
}

brains.df <- data.frame()

for(i in 1:length(fish)){
  sp.tmp <- as.data.frame(species(fish[i]))
  ecol.tmp <- as.data.frame(ecology(fish[i]))
  brains.tmp <- as.data.frame(brains(fish[i]))
  species.df <- bind_rows(species.df, sp.tmp)
  ecology.df <- bind_rows(ecology.df, ecol.tmp)
  
}

duplicated(species.df$Species)
duplicated(ecology.df$Species)

ecology.df[ecology.df$Species == 'Acipenser oxyrinchus',]
ecology.df[ecology.df$Species == 'Oncorhynchus clarkii',]
ecology.df[ecology.df$Species == 'Salmo trutta',]
ecology.df[ecology.df$Species == 'Salvelinus malma',]

ecology.df <- ecology.df %>% distinct(Species, .keep_all = T)

list_fields("Trophic Level")
