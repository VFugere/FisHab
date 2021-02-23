
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

#### traits for Marco

load('./formatted_open_data/species_codes.RData')
load('./formatted_open_data/fishbase.RData')

#make one giant matrix

se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
make.italic <- function(x) as.expression(lapply(x, function(y) bquote(italic(.(y)))))
'%!in%' <- function(x,y)!('%in%'(x,y))
mean.nona <- function(x){mean(x,na.rm=TRUE)}

brains.df <- brains.df %>% group_by(Species) %>% summarise_all(mean.nona)
diet.df <- diet.df %>% group_by(Species) %>% summarise_if(is.numeric, mean.nona)
sum(table(ecology.df$Species) > 1)
sum(table(estimate.df$Species) > 1)
sum(table(genetics.df$Species) > 1)
genetics.df <- genetics.df %>% group_by(Species) %>% summarise_if(is.numeric, mean.nona)

fish.traits <- left_join(species.df, ecology.df, by = 'Species') %>% 
  left_join(estimate.df, by='Species') %>%
  left_join(brains.df, by='Species') %>% 
  left_join(diet.df, by='Species') %>%
  left_join(genetics.df, by='Species')

marco.list <- read.csv('./formatted_open_data/Marco_spNames_clean_MELCC.csv', stringsAsFactors = F) %>% pull(species) %>% as.character
marco.list %in% fish.traits$Species
marco.list[marco.list %!in% fish.traits$Species]

marco.traits <- filter(fish.traits, Species %in% marco.list)
writexl::write_xlsx(marco.traits, '~/Desktop/Marco_traits.xlsx')

### morphological traits stolen from Su et al (2021) Science

load('./formatted_open_data/Su_2001/trait with missing value filled')
su <- PH_10705 %>% mutate_if(is.factor, as.character)

marco <- data.frame('Genus.species' = marco.list, stringsAsFactors = F)
marco$Genus <- sub(' .*', '', marco$Genus.species)
marco$species <- sub('.* ', '', marco$Genus.species)

marco$Genus %in% su$Genus
su.sub <- su %>% filter(Genus %in% marco$Genus)
su.sub$Genus.species <- str_replace(su.sub$Genus.species, '\\.', ' ')
  
marco$Genus.species %in% su.sub$Genus.species
marco <- marco %>% left_join(su.sub)

missing <- marco$Genus[is.na(marco$EdHd)]
missing <- su.sub %>% group_by(Genus) %>% summarise_if(is.numeric, mean) %>% filter(Genus %in% missing)

write_xlsx(marco, '~/Desktop/marco.xlsx')
write_xlsx(missing, '~/Desktop/missing.xlsx')
