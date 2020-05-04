#Ce script télécharge toutes les informations contenus dans FishBase pour toutes les espèces de poissons
#d'eau douce canadiens, ou du moins pour toutes les espèces retrouvées au moins une fois dans une des bases
#de données de poissons du projet FisHab.
#initié par Vincent Fugère, avril 2020

library(tidyverse)
library(skimr)
library(magrittr)
library(readxl)
library(writexl)
library(rfishbase)

fish <- c("Oreochromis niloticus", "Salmo trutta")
fish <- validate_names(c("Oreochromis niloticus", "Salmo trutta"))

species(fish[1])
ecology(fish[1])
