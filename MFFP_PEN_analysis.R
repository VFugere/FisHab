# analyse de données PEN du MFFP
# Vincent Fugère 2021+

#cleanup
rm(list=ls())

#libs
library(readxl)
library(itsadug)
library(scales)
library(sp)
library(raster)
library(rworldmap)
library(RColorBrewer)
library(devtools)
library(party)
library(skimr)
library(tidyverse)
library(mgcv)

#functions
make.italic <- function(x) as.expression(lapply(x, function(y) bquote(italic(.(y)))))
'%!in%' <- function(x,y)!('%in%'(x,y))
mean.nona <- function(x){mean(x, na.rm=T)}
sum.nona <- function(x){sum(x, na.rm=T)}

#utils
#colors <- wesanderson::wes_palette('FantasticFox1',3)
#map <- getMap(resolution='low')

#load data
sites <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS output/MFFP_PENsites_BasicInfo.xlsx')
hlakes <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS output/MFFP_PENsites_HydroLakes.xlsx') %>% 
  filter(`in_QGIS_LCE_falls_within_HL_polygon` == 'yes')
yves <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/Yves Prairie/QC_all_fish_sites_lakes_GSW_w_attributes.xlsx')
land.use <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS/MFFP_lulc/clean_MELCC_UT_LacsPEN.xlsx', sheet='merge')
lu.code <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS/MFFP_lulc/clean_MELCC_UT_LacsPEN.xlsx', sheet='codes')
connect <- read.csv('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/Alex connectivity/MFFP_LakeCentrality.csv') %>% select(-X) %>% filter(cent.length != 0)
cpue <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx', sheet=1)
com <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/MFFP/deuxieme_envoi/PÊCHES EXPÉRIMENTALES 1988-2019 v20mars_SG (3).xlsx', sheet=2)
fish.codes <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/LP/fish_output/taxonomic_codes.xlsx', sheet='QC-MFFP')
geo <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/SIGEOM/phys-chem_SIGEOM(2021-03-17).xlsx')
habitat <- read_xlsx('~/Desktop/FisHab_Habitat PEN.xlsx', sheet = 'Valeurs ponctuelles')

#### format fish data ####

#cpue
cpue <- select(cpue, 3, 5:7, 10, 12)
colnames(cpue) <- c('LCE','n.stations','year','survey','species','CPUE')
cpue <- arrange(cpue, LCE, desc(year))
#filter(cpue, survey == 'PENOC') %>% with(.,table(species,LCE))
#parfois saal, parfois saoq, jamais les deux dans le même lac
#plutôt que de remplacer le code d'espèce, je peux simplement filtrer par type de survey

#when a lake has two consecutive years, we average (only applies to PENDJ)
#otherwise we take the last year
cpue.clean <- cpue[0,]

for(survey in c('PENOF','PENT','PENDJ')){
  surv.dat <- cpue[cpue$survey == survey,]
  for(lac in unique(surv.dat$LCE)){
    sub <- surv.dat[surv.dat$LCE == lac,]
    if(nrow(sub) == 1){
      cpue.clean <- rbind(cpue.clean,sub)
    }else{
      if(-1 %in% diff(sub$year)){
        idx <- which.min(diff(sub$year)==-1)
        sub <- sub[c(idx,idx+1),]
        #sum.stations <- sum(sub$n.stations)
        #w.mean <- weighted.mean(x = sub$CPUE, w = sub$n.stations) # DO NOT USE weighted mean
        uw.mean <- mean(x = sub$CPUE, na.rm=T) # number of stations DOES NOT reflect sampling effort. An internal code for survey number
        yr <- sub$year[which.max(sub$n.stations)]
        sub$n.stations <- sub$n.stations[which.max(sub$n.stations)]
        sub <- sub[1,]
        sub$CPUE <- uw.mean
        sub$year <- yr
        cpue.clean <- rbind(cpue.clean,sub)
      }else{
        sub <- sub[1,]
        cpue.clean <- rbind(cpue.clean,sub)
      }
    }
  }
}

#com
com <- select(com, 3, 5:7, 10)
colnames(com) <- c('LCE','n.stations','year','survey','species')
bad.sp.codes <- c('RIEN','-','POIS','NI','AU')
com <- filter(com, species %!in% bad.sp.codes)
genera <- fish.codes %>% filter(!is.na(genus))
speciesid <- fish.codes %>% filter(!is.na(species))
taxon.list <- com %>% distinct(species) %>% pull(species)
id.at.other.levels <- c('COSP','CYSP','CRSP','STSP','SFXN','PRSP')
com <- filter(com, species %!in% id.at.other.levels)
#replacing older codes with newer ones
com$species[com$species == 'STVI'] <- 'SAVI'
com$species[com$species == 'NOCO'] <- 'LUCO'
com$species[com$species == 'STCA'] <- 'SACA'
com$species[com$species == 'SEMA'] <- 'MAMA'
com$species[com$species == 'ICNE'] <- 'AMNE'
com$species[com$species == 'SASV'] <- 'SAAL'
#saoq to saal
com$species[com$species == 'SAOQ'] <- 'SAAL'
#on enleve les duplicats
com <- com %>% distinct(LCE,year,survey,species,.keep_all = T)
#cleanup
rm(genera,speciesid,bad.sp.codes,id.at.other.levels)
taxon.list <- com %>% distinct(species) %>% pull(species)

# #combien de time series
# ts <- cpue %>% add_count(LCE) %>% group_by(LCE) %>% mutate('yr.range' = (max(year) - min(year))) %>% rename(yrs.of.data = n) %>% distinct(LCE, .keep_all = T) %>% ungroup
# ts <- filter(ts, yrs.of.data > 1)
# hist(ts$yrs.of.data, breaks=30)
# hist(ts$yr.range, breaks=30)
# plot(yr.range~yrs.of.data,ts)

#### format env data ####

sites$coords <- paste(round(sites$lce_lat,2),round(sites$lce_long,2),sep=',')
env <- select(sites, LCE, lce_lat, lce_long, coords)
yves$coords <- paste(round(yves$lce_lat,2),round(yves$lce_long,2),sep=',')
env <- yves %>% select(coords, Zmax:q) %>% right_join(env)
env <- hlakes %>% filter(in_QGIS_LCE_falls_within_HL_polygon == 'yes') %>% 
  select(LCE,HL_Lake_area,HL_Shore_dev,HL_Depth_avg,HL_Wshd_area,HL_Elevation,HL_Res_time) %>%
  right_join(env, by='LCE')
rm(yves,hlakes)

watershed.area.km2 <- data.frame('LCE' = land.use$LCE, 'watershed.area.km2' = apply(land.use[,2:ncol(land.use)], 1, sum)/1000000, stringsAsFactors = F)
env <- left_join(env, watershed.area.km2, by='LCE')

#### add habitat data ####

hab.clean <- habitat %>% select(No_lac,Transparence:conductivité) %>% group_by(No_lac) %>% summarize_all(~mean(.,na.rm=T))
env <- left_join(env,hab.clean, by = c('LCE' = 'No_lac'))
rm(hab.clean)

#### add world clim data ####

tmax <- raster('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS/WorldClim/tmaxBrick.tif') %>% 
  mean
tmin <- raster('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/GIS/WorldClim/tminBrick.tif') %>% 
  mean
#library(rgdal)
#writeRaster(tmax, '~/Desktop/worldclim_max_mean.tif', format='GTiff')
pensites <- SpatialPoints(coords = env[,c('lce_long','lce_lat')])
env$tmin <- raster::extract(tmax, pensites) #names inverted in Marco's worldclim files
env$tmax <- raster::extract(tmin, pensites)
# library(viridis)
# cols <- viridis(50)
# plot(lce_lat ~ lce_long, data=env,pch=16,col=cols[cut(env$tmax,50)])
rm(tmax,tmin,pensites)

#### format land use data ####

lu.long <- land.use %>% pivot_longer(cols=UT_2050:UT_6141, names_to = 'CODE_UT', values_to = 'm.sq')
codes <- lu.code %>% select(CODE_UT, DESC_CAT)
codes$CODE_UT <- paste0('UT_',codes$CODE_UT)
#unique(lu.long$CODE_UT) %in% codes$CODE_UT
lu.long <- lu.long %>% left_join(codes, by = 'CODE_UT')
lu.long <- lu.long %>% select(-CODE_UT) %>% group_by(LCE, DESC_CAT) %>% summarize(m.sq = sum(m.sq))
land.use <- pivot_wider(lu.long, id_cols = LCE, names_from = DESC_CAT, values_from = m.sq)
land.use <- select(land.use, -Aquatique)
land.use[,2:8] <- vegan::decostand(land.use[,2:8], method = 'total')
rm(codes,lu.code,lu.long)
env <- left_join(env, land.use)

# #plot of histograms
# plots <- lapply(c(2:6,8),
#                 function(col) ggplot2::qplot(land.use[[col]],
#                                              geom='histogram',
#                                              binwidth=0.05,
#                                              xlab = names(land.use)[[col]]))
# cowplot::plot_grid(plotlist=plots,ncol=2)

# #### formatting SIGEOM data ####

geo <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/SIGEOM/phys-chem_SIGEOM(2021-03-17).xlsx')

geo$coords <- paste(round(geo$lce_lat,2),round(geo$lce_long,2),sep=',')
geo$year <- as.numeric(substr(geo$DATE_ECHN,1,4))
#filter very old obs and only keep data from the same period as fish data
geo <- filter(geo, year > 1987)
geo$PH <- str_replace(geo$PH, ',', '.')
geo$PH[geo$PH == '0'] <- NA
geo$PH <- as.numeric(geo$PH)
geo <- geo %>% group_by(coords) %>% summarize_at(vars(PH,SIO2:TR2O3), mean.nona)
geo <- geo[-331,] #abberant outlier

ordi <- geo[,3:93]
ordi <- as.matrix(ordi)
ordi[ordi < 0] <- 0
ordi <- log1p(ordi)
p <- vegan::rda(ordi ~ 1)
#s <- summary(p)
g <-as.data.frame(vegan::scores(p)$sites[,1:2])
# xlims <- range(g[,1])
# ylims <- range(g[,2])
# par(mfrow=c(1,1),mar=c(2,2,2,1),oma=c(3,3,1,1))
# plot(g$PC2 ~ g$PC1, type = 'p',pch=16,col=alpha('black',0.5),ann=F,xlim=xlims,ylim=ylims,cex=1.2,lwd=1)
# mtext(paste("PC1 (", round(s$cont$importance[2,1]*100, 1), "%)", sep = ""),side=1,outer=T,line=1.2)
# mtext(paste("PC2 (", round(s$cont$importance[2,2]*100, 1), "%)", sep = ""),side=2,outer=T,line=1.2)

geo <- bind_cols(geo, g[,1:2])
env <- geo %>% select(coords, PC1, PC2) %>% right_join(env)
rm(p,g,ordi)

#### a few filters ####

#remove lat > 55
env <- env %>% filter(lce_lat < 55)

#HL is better for altitude than lce, yves is better for depth than HL, and simon better for watershed area 
env <- env %>% select(-HL_Depth_avg, - HL_Wshd_area)

#removing bad columns: some rare LU
env <- env %>% select(-(`Non classifié`:`Sol nu et lande`))

#removing useless column
env <- env %>% select(-q, -coords)
env$HL_Elevation <- as.numeric(env$HL_Elevation)

#renaming columns
names(env) <- c('Sigeom PC1','Sigeom PC2','LCE','lake.area','ShoreDev','masl','residence_time','Zmax','Zmean','lat','long',
                'watershed.area','transparency','pH','conductivity','Tmin','Tmax','LU.%ag','LU.%Anthro','LU.%Cut','LU.%Forest','LU.%Wetlands')

# #remove vars with a lot of NAs?
# skimr::skim(env)
# env <- drop_na(env)

#rearrange
env <- env %>% select(LCE, lat, long, `Sigeom PC1`, `Sigeom PC2`, everything()) %>% 
  arrange(LCE)

#connect
connect <- connect %>% select(1:3)
names(connect) <- c('LCE','species','connectivity')

#cols
cols <- RColorBrewer::brewer.pal(4,'Dark2')
cols2 <- viridis::viridis(50)

# #checking if genetic diversity correlates with connectivity
# gd <- read_csv('/Users/vincentfugere/Google Drive/Recherche/FisHab_LakePulse/data/Ferchaud gen div/ferchaud_joined.csv')
# gd <- gd %>% filter(ID_RHS %in% sites$RHS, distance < 1000)
# gd$LCE <- sites$LCE[match(gd$ID_RHS,sites$RHS)]
# gd <- connect %>% filter(LCE %in% gd$LCE) %>% 
#   pivot_wider(id_cols = LCE, names_from = species, values_from = connectivity) %>%
#   left_join(gd)
# gd.sub <- gd %>% select(`Brook trout (high)`,MeanFst,`Average Pi`,POLYMORPHIC_PROP)
# plot(gd.sub)

#### random forest for walleye

dat <- filter(cpue.clean, survey == 'PENDJ') %>%
  select(LCE, year, CPUE) %>%
  inner_join(select(env,-`Sigeom PC1`,-`Sigeom PC2`))
dat <- connect %>% filter(str_detect(species, 'Walleye'), LCE %in% dat$LCE, str_detect(species, '(high)')) %>%
  select(-species) %>% inner_join(dat) %>% select(-LCE) %>% select(CPUE, year, everything()) %>% 
  mutate_at(vars(CPUE), log10) %>% mutate_at(vars(year), as.factor) %>%
  mutate_at(vars(connectivity:`LU.%Wetlands`), scale)
dat <- filter(dat, is.finite(CPUE), !is.na(CPUE))

r2.vec <- numeric(0)
imp.vec <- data.frame()
for(i in 1:100){
  smpl <- sample(nrow(dat), round(nrow(dat)*.75))
  training.set <- dat[smpl,]
  test.set <- dat[-smpl,]
  mod <-cforest(CPUE ~ ., training.set, controls = cforest_unbiased(mtry=6))
  test.results <- predict(mod, newdata = test.set)
  r2.vec <- c(r2.vec,round(caret::postResample(pred = test.results, obs = test.set$CPUE)[2],4))
  vars <- varimp(mod, conditional = F)
  vars[vars < 0] <- 0
  vars <- (vars/sum(vars))*100
  imp.vec <- bind_rows(imp.vec, vars)
}
r2 <- round(median(r2.vec),4)
r2 <- bquote(italic(r)^2~'(pred vs. obs)'==.(r2))
vars <- apply(imp.vec, 2, median)
vars <- sort(vars)
pdf('~/Desktop/walleye1.pdf',width = 8, height = 5, pointsize = 12)
par(mar=c(4,12,1,1))
barplot(vars,col=cols[1],border=0,horiz=T,las=1)
title(xlab='relative influence (%)')
legend('bottomright',bty='n',legend=r2)
dev.off()

# dat2 <- filter(cpue.clean, survey == 'PENDJ') %>%
#   select(LCE, CPUE, year) %>% mutate_at(vars(year), as.factor)
# env2 <- select(env, LCE, lat, long, Tmax, masl, Zmean)
# dat2 <- inner_join(dat2, env2) %>% select(-LCE) %>%
#   mutate_at(vars(CPUE), log10) %>% mutate_at(vars(lat:masl), scale)
# dat2 <- filter(dat2, masl > -1.5)
# 
# mod <- gam(CPUE ~ te(lat,long,bs='gp') + ti(Tmax, k=5) + ti(masl, k=5) + ti(Tmax,masl,k=5) + s(year,bs='re',k=2), data=dat2, correlation = corSpher(form = ~ lat + long))
# gam.check(mod)
# summary(mod)
# pdf('~/Desktop/walleye2.pdf',width = 6, height = 5, pointsize = 12)
# par(mar=c(4,4,2,2))
# fvisgam(mod, view=c('Tmax','masl'),color=cols2,hide.label=T,plot.type='persp',theta=45,main=NULL,zlab = 'log10 CPUE',xlab='Tmax',ylab='masl')
# #fvisgam(mod, view=c('Tmax','masl'),dec=1,color=cols2,hide.label=T,xlab='log10 Tmax',ylab='log10 masl',main = 'log10 CPUE')
# dev.off()
# 
# pdf('~/Desktop/walleye3.pdf',width = 6, height = 5, pointsize = 12)
# plot_smooth(mod, view = 'Tmax',col=cols[1],rug=F,print.summary=F,rm.ranef = T,xlab = 'altitude',ylab = 'log10 CPUE', hide.label = T,bty='o',h0 = NULL)
# pval <- round(summary(mod)$s.table[3,4],4)
# r2 <- round(summary(mod)$r.sq,4)
# legend('bottomright',legend=c(paste0('p = ',pval),paste0('r2 = ',r2)),bty='n')
# dev.off()

#### random forest for brook char

dat <- filter(cpue.clean, survey == 'PENOF') %>%
  select(LCE, CPUE, year) %>%
  inner_join(env)
dat <- connect %>% filter(str_detect(species, 'Brook trout'), LCE %in% dat$LCE, str_detect(species, '(high)')) %>%
  select(-species) %>% inner_join(dat) %>% select(-LCE) %>% select(CPUE, year, everything()) %>% 
  mutate_at(vars(CPUE), log10) %>% mutate_at(vars(year), as.factor) %>%
  mutate_at(vars(connectivity:`LU.%Wetlands`), scale)
dat <- filter(dat, is.finite(CPUE), !is.na(CPUE))

r2.vec <- numeric(0)
imp.vec <- data.frame()
for(i in 1:100){
  smpl <- sample(nrow(dat), round(nrow(dat)*.75))
  training.set <- dat[smpl,]
  test.set <- dat[-smpl,]
  mod <-cforest(CPUE ~ ., training.set, controls = cforest_unbiased(mtry=6))
  test.results <- predict(mod, newdata = test.set)
  r2.vec <- c(r2.vec,round(caret::postResample(pred = test.results, obs = test.set$CPUE)[2],4))
  vars <- varimp(mod, conditional = F)
  vars[vars < 0] <- 0
  vars <- (vars/sum(vars))*100
  imp.vec <- bind_rows(imp.vec, vars)
}
r2 <- round(median(r2.vec),4)
r2 <- bquote(italic(r)^2~'(pred vs. obs)'==.(r2))
vars <- apply(imp.vec, 2, median)
vars <- sort(vars)
pdf('~/Desktop/brookTrout1.pdf',width = 8, height = 5, pointsize = 12)
par(mar=c(4,12,1,1))
barplot(vars,col=cols[3],border=0,horiz=T,las=1)
title(xlab='relative influence (%)')
legend('bottomright',bty='n',legend=r2)
dev.off()

# dat2 <- filter(cpue.clean, survey == 'PENOF') %>%
#   select(LCE, CPUE, year)
# env2 <- select(env, LCE, lat, long, Tmax, Zmean)
# dat2 <- inner_join(dat2, env2) %>% select(-LCE) %>%
#   mutate_at(vars(CPUE), log10) %>% mutate_at(vars(year), as.factor)
# 
# mod <- gam(CPUE ~ te(lat,long,bs='gp') + s(Tmax, k=8) + s(year, bs='re',k=2), data=dat2, correlation = corSpher(form = ~ lat + long))
# gam.check(mod)
# summary(mod)
# pdf('~/Desktop/brookTrout2.pdf',width = 6, height = 5, pointsize = 12)
# par(mar=c(4,4,2,2))
# plot_smooth(mod, view = 'Tmax',col=cols2[3],rug=F,rm.ranef = T,print.summary=F,xlab = 'Tmax',ylab = 'log10 CPUE', hide.label = T,bty='o',h0 = NULL,xlim=c(21.5,25))
# pval <- round(summary(mod)$s.table[2,4],4)
# r2 <- round(summary(mod)$r.sq,4)
# legend('bottomright',legend=c(paste0('p = ',pval),paste0('r2 = ',r2)),bty='n')
# dev.off()

#### random forest for lake trout ####

dat <- filter(cpue.clean, survey == 'PENT') %>%
  select(LCE, CPUE, year) %>%
  inner_join(env)
dat <- connect %>% filter(str_detect(species, 'Lake trout'), LCE %in% dat$LCE, str_detect(species, '(low)')) %>%
  select(-species) %>% inner_join(dat) %>% select(-LCE) %>% select(CPUE, year, everything()) %>% 
  mutate_at(vars(CPUE), log10) %>% mutate_at(vars(year), as.factor) %>%
  mutate_at(vars(connectivity:`LU.%Wetlands`), scale)
dat <- filter(dat, is.finite(CPUE), !is.na(CPUE))

r2.vec <- numeric(0)
imp.vec <- data.frame()
for(i in 1:100){
  smpl <- sample(nrow(dat), round(nrow(dat)*.75))
  training.set <- dat[smpl,]
  test.set <- dat[-smpl,]
  mod <-cforest(CPUE ~ ., training.set, controls = cforest_unbiased(mtry=6))
  test.results <- predict(mod, newdata = test.set)
  r2.vec <- c(r2.vec,round(caret::postResample(pred = test.results, obs = test.set$CPUE)[2],4))
  vars <- varimp(mod, conditional = F)
  vars[vars < 0] <- 0
  vars <- (vars/sum(vars))*100
  imp.vec <- bind_rows(imp.vec, vars)
}
r2 <- round(median(r2.vec),4)
r2 <- bquote(italic(r)^2~'(pred vs. obs)'==.(r2))
vars <- apply(imp.vec, 2, median)
vars <- sort(vars)
pdf('~/Desktop/touladi1.pdf',width = 8, height = 5, pointsize = 12)
par(mar=c(4,12,1,1))
barplot(vars,col=cols[4],border=0,horiz=T,las=1)
title(xlab='relative influence (%)')
legend('bottomright',bty='n',legend=r2)
dev.off()

#### random forest for diversity

dat <- com %>% group_by(LCE,survey) %>%
  summarise('richness' = n_distinct(species), 'nb_inv' = sum(n.stations)) %>%
  left_join(env, by = 'LCE') %>% arrange(LCE) %>% drop_na
dat <- connect %>% filter(LCE %in% dat$LCE, str_detect(species, '(high)')) %>%
  group_by(LCE) %>% summarize(connectivity = mean(connectivity)) %>%
  inner_join(dat) %>% select(-LCE) %>% 
  mutate_at(vars(richness,nb_inv), log10) %>% mutate_at(vars(survey), as.factor)

par(mfrow=c(1,2))
plot(richness~nb_inv,dat,ylab='log10 richness',xlab="log10 Nb. d'inventaire",pch=16)
plot(y=(10^dat$richness),x=(10^dat$nb_inv),ylab='richesse spécifique',xlab="Nb. d'inventaire",pch=16)

mod <- gam(richness ~ s(nb_inv,k=10),data=dat)
plot_smooth(mod, view = 'nb_inv',col=1,add=T,rug=F)
dat$richness <- resid(mod)
dat <- select(dat, -nb_inv)

r2.vec <- numeric(0)
imp.vec <- data.frame()
for(i in 1:100){
  smpl <- sample(nrow(dat), round(nrow(dat)*.75))
  training.set <- dat[smpl,]
  test.set <- dat[-smpl,]
  mod <-cforest(richness ~ ., training.set, controls = cforest_unbiased(mtry=6))
  test.results <- predict(mod, newdata = test.set)
  r2.vec <- c(r2.vec,round(caret::postResample(pred = test.results, obs = test.set$richness)[2],4))
  vars <- varimp(mod, conditional = F)
  vars[vars < 0] <- 0
  vars <- (vars/sum(vars))*100
  imp.vec <- bind_rows(imp.vec, vars)
}
r2 <- round(median(r2.vec),4)
r2 <- bquote(italic(r)^2~'(pred vs. obs)'==.(r2))
vars <- apply(imp.vec, 2, median)
vars <- sort(vars)
pdf('~/Desktop/richness.pdf',width = 8, height = 5, pointsize = 12)
par(mar=c(4,12,1,1))
barplot(vars,col=cols[2],border=0,horiz=T,las=1)
title(xlab='relative influence (%)')
legend('bottomright',bty='n',legend=r2)
dev.off()

## map

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(viridis)

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
xlims <- range(dat$long)+c(-1,1)
ylims <- range(dat$lat)+c(-2,0.5)
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = xlims, ylim = ylims) +
  #annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data=dat,aes(x=long,y=lat,col=richness)) + scale_color_viridis()

bp <- ggplot(dat, aes(x=survey, y=richness, fill=survey)) + 
  geom_boxplot()+
  labs(x="Inventaire", y = "Richesse normalisée")
bp + theme_classic()

dat2 <- dat %>%
  select(richness, survey, lat, long, Tmax)

mod <- gam(richness ~ te(lat,long,bs='gp') + s(Tmax, k=15) + s(survey, bs='re',k=3), data=dat2, correlation = corSpher(form = ~ lat + long))
gam.check(mod)
summary(mod)
pdf('~/Desktop/richness2.pdf',width = 6, height = 5, pointsize = 12)
par(mar=c(4,4,2,2))
plot_smooth(mod, view = 'Tmax',xlim=c(22,26),col=cols[2],rug=F,rm.ranef = T,print.summary=F,xlab = 'Tmax',ylab = 'richness (resid.)', hide.label = T,bty='o',h0 = NULL)
pval <- round(summary(mod)$s.table[2,4],4)
r2 <- round(summary(mod)$r.sq,4)
legend('bottomright',legend=c(paste0('p = ',pval),paste0('r2 = ',r2)),bty='n')
dev.off()
