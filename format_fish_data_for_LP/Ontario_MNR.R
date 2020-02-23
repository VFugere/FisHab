rm(list=ls())

library(tidyverse)
library(readxl)
library(sp)
library(writexl)

#how many LP lakes in Ontario dataset?

load("/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/LP/basic_data.RData")

#this is the presence-absence dataset, could also use the abundance dataset
BsMdata <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/Ontario BsM/UQAM_BsM_DataRequest2019_BA.xlsx', sheet=2)

#calculate, for each lake pulse lake, which lake is closest in BsM database

data <- basic.data %>% select(Lake_ID,lake_name,latitude,longitude,province,area,depth_m) %>%
  rename(name = lake_name, lat = latitude, long = longitude, depth = depth_m) %>%
  filter(province == 'ONTARIO')

fdata <- BsMdata %>% rename(f.name = Lake_Name, f.lat = Lat, f.long = Long, f.area = Area_ha, f.depth = Depth_Max, BsM_ID = Waterbody_LID) %>%
  mutate(f.area = f.area * 0.01) %>% #converts to km2
  select(f.name,f.lat,f.long,f.area,f.depth,BsM_ID) %>%
  filter(!is.na(f.lat))

fdata[fdata$f.long > 50, 'f.long'] <- -(fdata[fdata$f.long > 50, 'f.long'])

#adding the closest lake to data
out <- data.frame()

for(i in 1:87){
  tmp <- data[i,]
  dist.df <- as.matrix(tmp[,c(4,3)])
  dist.df <- rbind(dist.df, as.matrix(fdata[,c(3,2)]))
  dist.mat <- spDists(dist.df, longlat=TRUE)
  min.dist <- which.min(dist.mat[2:nrow(dist.mat),1])
  tmp <- cbind(tmp,fdata[min.dist,])
  out <- bind_rows(out,tmp)
}

out$coordsLP <- paste(out$lat,out$long,sep=', ')
out$coordsBsM <- paste(out$f.lat,out$f.long,sep=', ')

write_xlsx(out, '~/Desktop/LP_BsM_matches.xlsx')

# a simpler way based on names?

fdata$f.name <- str_replace(fdata$f.name, 'L.', 'Lake')
sum(data$name %in% fdata$f.name)

sub<-data[data$name %in% fdata$f.name,]
sub <- left_join(sub,fdata,by=c('name' = 'f.name'))
sub <- sub %>% select(name,lat,f.lat,long,f.long,area,f.area,depth,f.depth,everything())
sub <- sub %>% mutate(area.diff = area-f.area, depth.diff = depth-f.depth)
sub <- sub %>% select(name, area.diff, depth.diff, everything())

sub$coordsLP <- paste(sub$lat,sub$long,sep=', ')
sub$coordsBsM <- paste(sub$f.lat,sub$f.long,sep=', ')

write_xlsx(sub, '~/Desktop/LP_BsM_Name_matches.xlsx')

### all the matches have been checked on google maps

# getting the fish data from the BsM and adding LP_ID

matches <- read_xlsx('~/Google Drive/Recherche/Lake Pulse Postdoc/data/Ontario BsM/LP_BsM_matches.xlsx', sheet = 'matches')

matches <- select(matches, LP.ID, BsM.ID)

out.fish <- BsMdata %>% filter(Waterbody_LID %in% matches$BsM.ID)

out.fish <- left_join(out.fish, matches, by = c('Waterbody_LID' = 'BsM.ID'))

out.fish <- select(out.fish, LP.ID, Waterbody_LID, everything())
out.fish <- as.data.frame(out.fish)

out.fish <- as.data.frame(sapply(out.fish,sub,pattern='\\*',replacement='1'))

out.fish[out.fish == '\\*']

write_xlsx(out.fish, '~/Desktop/LP_Ontario_Fish.xlsx')
