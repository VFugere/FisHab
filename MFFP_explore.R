# code exploratoire analyse de données IFD PEN-OC/DJ/T/OF

rm(list=ls())

library(tidyverse)
library(readxl)
library(scales)
library(sp)

good.surveys <- c('PENDJ','PENOF','PENT','PENOC')

# load LCE db, LP db with LCE info, and MFFP fish data

LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/tous_les_LCE.xlsx')

LP_LCE <- read_xlsx('/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/GIS output/LP_Qc_NoLacLCE.xlsx') %>%
  rename(RHS = ID_RHS)

# #en date du 7 feb 2020, les données de PS sont inutilisables (on nous a fourni les quotas, pas les captures...)
# PS <- read_xlsx(skip=2,'/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFA - Rapport Pêche Sportive 2000-2018.xlsx')
# colnames(PS)[2] <- 'LCE'
# n_distinct(PS$LCE) #9135 sites!
# PS %>% distinct(LCE) %>% select(LCE) -> ps_sites
# ps_sites <- left_join(ps_sites, LCE)
# table(ps_sites$ecosysteme) #8918 lacs, 53 rivières

INV <- read_xlsx(skip=5,"/Users/vincentfugere/Google Drive/Recherche/Lake Pulse Postdoc/data/MFFP/IFD- 1 - Rapport Inventaire sur plan d'eau (IPE) 2000-2018.xlsx")
colnames(INV)[1] <- 'LCE'
n_distinct(INV$LCE) #3260 sites!

# #only keeping the good stuff (see metadata)
# INV <- filter(INV, `Type de pêche` %in% good.surveys)
# n_distinct(INV$LCE) #648 sites

INV %>% select(LCE:Longitude) %>% distinct(LCE, .keep_all = T) -> inv_sites
sum(inv_sites$LCE %in% LCE$LCE)/nrow(inv_sites) # 98.8 % of sites have LCE
inv_sites <- left_join(inv_sites, LCE)
table(inv_sites$ecosysteme) #2357 lacs, 864 rivières
table(INV$`Type de pêche`)

# #sites LP sans LCE
# LP_LCE[is.na(LP_LCE$LCE),] -> LP_noLCE
# #peut-on retrouver ces sites dans la bdd inv?
# #adding the closest lake to data
# out <- data.frame()
# inv_sites_with_coords <- filter(inv_sites, !is.na(Latitude), !is.na(Longitude))
# for(i in 1:4){
#   tmp <- LP_noLCE[i,]
#   dist.df <- as.matrix(tmp[,c(3,2)])
#   coords.mffp <- inv_sites_with_coords %>% select(Longitude,Latitude) %>% as.matrix
#   dist.df <- rbind(dist.df, coords.mffp)
#   dist.mat <- sp::spDists(dist.df, longlat=TRUE)
#   min.dist <- which.min(dist.mat[2:nrow(dist.mat),1])
#   tmp <- cbind(tmp,inv_sites_with_coords[min.dist,])
#   out <- bind_rows(out,tmp)
# }
# out$coordsLP <- paste(out$latitude,out$longitude,sep=', ')
# out$coordsMFFP <- paste(out$Latitude,out$Longitude,sep=', ')
# INV %>% filter(LCE == '68828') 

# # des données PENOF sont dispos pour ce Lac Lake Pulse '68828'
# # Par contre il y a un problème avec ce numéro LCE dans bdd IFD... le lce est faux. Alors je mets le LCE custom 
# # inventé pour LP, dans la bdd du MFFP. Il y sûrement d'autres LCE éronnés

INV[INV$LCE == '68828','LCE'] <- 'LP066'

rm(inv_sites)

LP_LCE <- LP_LCE %>% select(-LCE, -Notes) %>% rename(LCE = LCE_adapt)

# 3) combien se recoupent?

sum(LP_LCE$LCE %in% INV$LCE) #26 lacs
#sum(LP_LCE$LCE %in% PS$LCE)
#sum(LP_LCE$LCE %in% c(PS$LCE,INV$LCE))

inv_lp <- filter(INV, LCE %in% LP_LCE$LCE)
inv_lp <- inv_lp[,c(1:4,10:16)]
table(inv_lp$`Type de pêche`)
table(inv_lp$`Engin`)
#beaucoup de pêche non-normalisée

inv_lp <- left_join(inv_lp,LP_LCE,by='LCE')
inv_lp <- arrange(inv_lp, desc(`Date de levée`), LCE)
                  

inv_lp <- inv_lp %>% filter(`Type de pêche` %in% good.surveys)



### GRADIENTS taille-latitude ####


inv <- INV
rm(INV)

inv$max.l <- as.numeric(inv$`Long max (mm)`)

inv <- inv %>% group_by(LCE, Espèce) %>%
  summarize(lat = mean(Latitude, na.rm=T), long = mean(Longitude, na.rm=T), size = max(max.l, na.rm=T))
colnames(inv)[2] <- 'species'

inv <- inv %>% filter(!is.na(lat))
inv <- inv %>% filter(!is.na(size)) %>% filter(is.finite(size))
inv <- as.data.frame(inv)

data <- inv

data <- data %>% add_count(species)

taxlist <- data %>% distinct(species) %>% pull(species)
data$scl.size <- 0
data$scl.lat <- 0
for(i in 1:length(taxlist)){
  tmp <- filter(data, species == taxlist[i])
  scl.size <- scale(tmp$size, scale = T)[,1]
  scl.lat <- scale(tmp$lat, scale = F)[,1]
  data[data$species == taxlist[i], 'scl.size'] <- scl.size
  data[data$species == taxlist[i], 'scl.lat'] <- scl.lat
}

data$species <- as.factor(data$species)
data$scl.size[!is.finite(data$scl.size)] <- NA
data <- data %>% filter(size > 0)

reg.results <- data.frame('focalsp' = character(0),
                          'min.lat' = numeric(0),
                          'max.lat' = numeric(0),
                          'n' = numeric(0),
                          'slope' = numeric(0),
                          'SE' = numeric(0),
                          'p' = numeric(0),
                          'r2' = numeric(0), 
                          'mean.size' =numeric(0),
                          stringsAsFactors = F)

for(i in 1:nlevels(data$species)){
  focalsp <- levels(data$species)[i]
  spdat <- data %>% filter(species == focalsp)
  spdat <- spdat %>% drop_na(scl.size)
  if(nrow(spdat) >= 10){
    #lmmod <- lm(ind.size~x,spdat)
    lmmod <- lm(scl.size~lat,spdat)
    r2 <- summary(lmmod)$r.squared
    slope <- coef(lmmod)[2]
    p <- summary(lmmod)$coefficients[2,4]
    SE <- summary(lmmod)$coefficients[2,2]
    n <- nrow(spdat)
    min.lat <- min(spdat$lat)
    max.lat <- max(spdat$lat)
    mean.size <- mean(spdat$size)
    results1 <- c(focalsp)
    results2 <- c(min.lat,max.lat,n,slope,SE,p,r2,mean.size)
    reg.results[nrow(reg.results)+1,1] <- results1
    reg.results[nrow(reg.results),2:9] <- results2
  }
}

reg.results$xrange <- reg.results$max.lat-reg.results$min.lat

reg.results$bubblesize <- rescale(reg.results$r2, c(1,4))
reg.results$bubblecol <- 'dark gray'
reg.results$bubblecol[reg.results$p < 0.05 & reg.results$slope < 0] <- 'red'
reg.results$bubblecol[reg.results$p < 0.05 & reg.results$slope > 0] <- 'blue'
reg.results$bubble.pch <- 16
reg.results$bubble.pch[reg.results$bubblecol == 'dark gray'] <- 1

hist(reg.results$slope, breaks=50)
# 
#plot à la Dornelas
plot(r2~slope,reg.results,bty='n',pch=16,col=alpha(bubblecol,0.5),cex=bubblesize)
abline(v=0,lty=2)
#not very exciting...

#####

pdf('~/Desktop/regressions.pdf',width=8,height=7,pointsize=14)

plotfunctions::emptyPlot(xlim = range(data$lat),yaxt='n',xaxt='n',ann=F, ylim=c(0,quantile(data$size,0.99)),bty='l')
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab='latitude')
title(ylab='max length (mm)',line=2.8)
for(i in 1:nlevels(data$species)){
  focalsp <- levels(data$species)[i]
  spdat <- data %>% filter(species == focalsp) %>% arrange(lat)
  spdat <- filter(spdat, !is.na(scl.size))
  if(nrow(spdat) >= 10){
    lmmod <- lm(scl.size~lat,spdat)
    lncol <- 'dark gray'
    lnwd <- 1
    if(summary(lmmod)$coefficients[2,4] < 0.05){
      lnwd <- 1
      if(summary(lmmod)$coefficients[2,1] > 0){lncol <- 'blue'}else{
        lncol <- 'red'}
    }
    lmmod <- lm(size~lat,spdat)
    points(fitted(lmmod)~spdat$lat,col=alpha(lncol,0.5),type='l',lwd=lnwd)
  }
}

dev.off()

##### slopes  #####

pdf('~/Desktop/slopes.pdf',width=14,height=6,pointsize=14)
par(mfrow=c(1,3),cex=1,mar=c(4,2.5,0,0),oma=c(0,2,0,0))

plot(slope~n,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',yaxt='n',xaxt='n',ann=F, log = 'x')
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab=number~of~lakes,line=2.8)
abline(h=0,lty=3)

mtext(expression(Delta~body~mass~per~degree~latitude~(sd)),outer = T,side=2,line=0.5)

# which.max(reg.results$n)
# reg.results[244,]
# reg.results %>% arrange(n) %>% pull(focalsp)

plot(slope~xrange,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',yaxt='n',xaxt='n',ann=F,xlim=(range(reg.results$xrange) + c(-0.5,0.5)))
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
title(xlab='latitudinal range',line=2.8)
#title(ylab=slope~(beta),line=2.8)
abline(h=0,lty=3)

plot(slope~mean.size,reg.results,bty='n',pch=bubble.pch,col=alpha(bubblecol,0.5),cex=bubblesize,bty='l',log='x',yaxt='n',xaxt='n',ann=F)
axis(1,cex.axis=1,lwd=0,lwd.ticks=1)
axis(2,cex.axis=1,lwd=0,lwd.ticks=1)
#title(ylab=slope~(beta),line=2.8)
title(xlab='max length (mm)',line=2.8)
abline(h=0,lty=3)

dev.off()