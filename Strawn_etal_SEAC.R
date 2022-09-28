rm(list = ls()) #clears project data
options(scipen=999) #turns off scientific notation

library(sf)
library(tidyverse)
library(spatialEco)
library(sp)
library(raster)
library(rgeos)
library(lsr)



#Import Project Area and Sites
project_area <- st_read(file.choose())
arch_sites <- st_read(file.choose())
site_count<-nrow(arch_sites)

rivers<-st_read(file.choose())
raw_material<-st_read(file.choose())

#Import Project Rasters
elevation_dem<- raster(file.choose())



##Plot Archaeological Sites
ggplot() + geom_sf(data = project_area) + geom_sf(data = arch_sites)

##Filter By Category
Paleo_Sites<- arch_sites %>%
  dplyr::filter(., Paleoindia > 0)

Paleo_count<-nrow(Paleo_Sites)

EarlyArchaic_Sites<- arch_sites %>%
  dplyr::filter(., Early_Arch > 0)

EarlyArchaic_count<-nrow(EarlyArchaic_Sites)

Multi_Sites<- arch_sites %>%
  dplyr::filter(., Multicompo > 0)

Multi_count<-nrow(Multi_Sites)


#########Paleoindian Nearest Neighbor################################################

times=1000
Paleo_Sim=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(Paleo_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  nni_results<-nni(samples.sp)
  nni_value<-nni_results$NNI
  Paleo_Sim[i]=nni_value
}


hist(Paleo_Sim, main="Paleo Sites - NNI Simulation", xlab="NNI Index",xlim=c(0,2))



times=1000
Paleo_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  nni_results<-nni(resamples.sp)
  nni_value<-nni_results$NNI
  Paleo_Boot[i]=nni_value
}


hist(Paleo_Boot, main="Paleo Sites - NNI Resample", xlab="NNI Index",xlim=c(0,2))


Paleo_expected <- cbind(Paleo_Sim, x1='expected')
Paleo_observed <- cbind(Paleo_Boot, x1='observed')

Paleo.test<-rbind(Paleo_expected,Paleo_observed)
Paleo.test.df<-as.data.frame(Paleo.test)

Paleo.test.df$Paleo_Sim <- as.numeric(as.character(Paleo.test.df$Paleo_Sim))

Paleo.test.df<- rename(Paleo.test.df, NNI = Paleo_Sim)
Paleo.test.df <- cbind(Paleo.test.df, x2='Paleo')

ggplot(Paleo.test.df, aes(x1, NNI)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "NNI")

t.test(NNI ~ x1, data = Paleo.test.df)
wilcox.test(NNI ~ x1, data = Paleo.test.df, alternative = "two.sided")


#########Early Archaic Nearest Neighbor#################################################


times=1000
EarlyArchaic_Sim=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(EarlyArchaic_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  nni_results<-nni(samples.sp)
  nni_value<-nni_results$NNI
  EarlyArchaic_Sim[i]=nni_value
}


hist(EarlyArchaic_Sim, main="EarlyArchaic Sites - NNI Simulation", xlab="NNI Index",xlim=c(0,2))



times=1000
EarlyArchaic_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-EarlyArchaic_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  nni_results<-nni(resamples.sp)
  nni_value<-nni_results$NNI
  EarlyArchaic_Boot[i]=nni_value
}


hist(EarlyArchaic_Boot, main="EarlyArchaic Sites - NNI Resample", xlab="NNI Index",xlim=c(0,2))


EarlyArchaic_expected <- cbind(EarlyArchaic_Sim, x1='expected')
EarlyArchaic_observed <- cbind(EarlyArchaic_Boot, x1='observed')

EarlyArchaic.test<-rbind(EarlyArchaic_expected,EarlyArchaic_observed)
EarlyArchaic.test.df<-as.data.frame(EarlyArchaic.test)

EarlyArchaic.test.df$EarlyArchaic_Sim <- as.numeric(as.character(EarlyArchaic.test.df$EarlyArchaic_Sim))

EarlyArchaic.test.df<- rename(EarlyArchaic.test.df, NNI = EarlyArchaic_Sim)
EarlyArchaic.test.df <- cbind(EarlyArchaic.test.df, x2='EarlyArchaic')

ggplot(EarlyArchaic.test.df, aes(x1, NNI)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "NNI")

t.test(NNI ~ x1, data = EarlyArchaic.test.df)
wilcox.test(NNI ~ x1, data = EarlyArchaic.test.df, alternative = "two.sided")




#########Multi-Component Nearest Neighbor###############################


times=1000
Multi_Sim=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(Multi_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  nni_results<-nni(samples.sp)
  nni_value<-nni_results$NNI
  Multi_Sim[i]=nni_value
}


hist(Multi_Sim, main="Multi-Component Sites - NNI Simulation", xlab="NNI Index",xlim=c(0,2))



times=1000
Multi_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Multi_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  nni_results<-nni(resamples.sp)
  nni_value<-nni_results$NNI
  Multi_Boot[i]=nni_value
}


hist(Multi_Boot, main="Multi-Component Sites - NNI Resample", xlab="NNI Index",xlim=c(0,2))


Multi_expected <- cbind(Multi_Sim, x1='expected')
Multi_observed <- cbind(Multi_Boot, x1='observed')

Multi.test<-rbind(Multi_expected,Multi_observed)
Multi.test.df<-as.data.frame(Multi.test)

Multi.test.df$Multi_Sim <- as.numeric(as.character(Multi.test.df$Multi_Sim))

Multi.test.df<- rename(Multi.test.df, NNI = Multi_Sim)
Multi.test.df <- cbind(Multi.test.df, x2='Multi')

ggplot(Multi.test.df, aes(x1, NNI)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "NNI")

t.test(NNI ~ x1, data = Multi.test.df)
wilcox.test(NNI ~ x1, data = Multi.test.df, alternative = "two.sided")


########### Nearest Neighbor - Multiple Comparison #################################################

comp.df<-rbind(Paleo.test.df, EarlyArchaic.test.df, Multi.test.df) %>%
  dplyr::filter(., x1 == 'observed')

comp.df$x2 <- factor(comp.df$x2  , levels=c("Paleo", "EarlyArchaic", "Multi"))

summary(comp.df$x2)

ggplot(comp.df, aes(x2, NNI)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "NNI")+ 
  ggtitle("Nearest Neighbor Index")


res.aov <- aov(NNI ~ x2, data = comp.df) #ANOVA
summary(res.aov) #Anova

TukeyHSD(res.aov) #Tukey's HSD groupings
oneway.test(NNI ~ x2, data = comp.df) #Welch's T-Test
kruskal.test(NNI ~ x2, data = comp.df) #Kruskall Wallis




################Elevation  ##########################################################################

elevation.p <- rasterToPoints(elevation_dem)
elevation.df <- data.frame(elevation.p)


times=100
elev_rand=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(site_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  rasValue1<-raster::extract(elevation_dem, samples.sp)

  rand_mean<-mean(rasValue1)

  elev_rand[i]=rand_mean
}


hist(elev_rand, main="Mean Elevation for Random Points", xlab="meters")



times=100
Paleo_Boot_Ele=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  rasValue1<-raster::extract(elevation_dem, resamples.sp)
  rasValue1.df<-as.data.frame(rasValue1)
  
  observed_mean<-mean(rasValue1.df$rasValue1)
  
  Paleo_Boot_Ele[i]=observed_mean
}

hist(Paleo_Boot_Ele, main="Mean Elevation for Bootstrapped Paleoindian Sites", xlab="meters")


times=100
EarlyArchaic_Boot_Ele=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-EarlyArchaic_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  rasValue1<-raster::extract(elevation_dem, resamples.sp)
  rasValue1.df<-as.data.frame(rasValue1)
  
  observed_mean<-mean(rasValue1.df$rasValue1)
  
  EarlyArchaic_Boot_Ele[i]=observed_mean
}

hist(EarlyArchaic_Boot_Ele, main="Mean Elevation for Bootstrapped Early Archaic Sites", xlab="meters")



times=100
Multi_Boot_Ele=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Multi_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  rasValue1<-raster::extract(elevation_dem, resamples.sp)
  rasValue1.df<-as.data.frame(rasValue1)
  
  observed_mean<-mean(rasValue1.df$rasValue1)
  
  Multi_Boot_Ele[i]=observed_mean
}

hist(Multi_Boot_Ele, main="Mean Elevation for Bootstrapped Multi-Component Sites", xlab="meters")


elev_rand <- cbind(elev_rand, x1='Random')
Paleo_Boot_Ele <- cbind(Paleo_Boot_Ele, x1='Paleoindian')
EarlyArchaic_Ele <- cbind(EarlyArchaic_Boot_Ele, x1='Early Archaic')
Multi_Boot_Ele <- cbind(Multi_Boot_Ele, x1='Multi-Component')

elevation_test<-rbind(elev_rand, Paleo_Boot_Ele, EarlyArchaic_Ele, Multi_Boot_Ele)
elevation_test.df<-as.data.frame(elevation_test)

elevation_test.df$elev_rand <- as.numeric(as.character(elevation_test.df$elev_rand))

elevation_test.df<- rename(elevation_test.df, meters = elev_rand)

elevation_test.df$x1 <- factor(elevation_test.df$x1  , levels=c("Paleoindian", "Early Archaic", "Multi-Component", "Random"))
summary(elevation_test.df)

ggplot(elevation_test.df, aes(x1, meters)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "meters")+ 
  ggtitle("Elevation")



ele.aov <- aov(meters ~ x1, data = elevation_test.df) #ANOVA
summary(ele.aov) #ANOVA
etaSquared(ele.aov) #Effect size for ANOVA

TukeyHSD(ele.aov) #Tukey's HSD groupings
oneway.test(meters ~ x1, data = elevation_test.df) #Welch's T-Test
kruskal.test(meters ~ x1, data = elevation_test.df) #Kruskall Wallis





###################Distance to Rivers###############################################################

ggplot() + geom_sf(data = project_area) + geom_sf(data = arch_sites)+ 
  geom_sf(data=raw_material)+ geom_sf(data=rivers)

Paleo_Sites.sp<-as(Paleo_Sites, "Spatial")
EarlyArchaic_Sites.sp<-as(EarlyArchaic_Sites, "Spatial")
Multi_Sites.sp<-as(Multi_Sites, "Spatial")

rivers.sp<-as(rivers, "Spatial")

paleo.dist.rivers<-apply(gDistance(Paleo_Sites.sp, rivers.sp,byid=TRUE),2,min)
earlyarchaic.dist.rivers<-apply(gDistance(EarlyArchaic_Sites.sp, rivers.sp,byid=TRUE),2,min)
multi.dist.rivers<-apply(gDistance(Multi_Sites.sp, rivers.sp,byid=TRUE),2,min)

hist(paleo.dist.rivers)
hist(earlyarchaic.dist.rivers)
hist(multi.dist.rivers)

#######Paleo Bootstrap

times=100
Paleo_River_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rivers.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  Paleo_River_Boot[i]=mean_dist
}

hist(Paleo_River_Boot)

#######Early Archaic Bootstrap

times=100
EarlyArchaic_River_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rivers.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  EarlyArchaic_River_Boot[i]=mean_dist
}

hist(EarlyArchaic_River_Boot)

#######Multi Bootstrap

times=100
Multi_River_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rivers.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  Multi_River_Boot[i]=mean_dist
}

hist(Multi_River_Boot)


#########random sim


times=100
Dist_River_Sim=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(site_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  
  dist_results<-apply(gDistance(samples.sp, rivers.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  Dist_River_Sim[i]=mean_dist
}

hist(Dist_River_Sim)



RiverDist_rand <- cbind(Dist_River_Sim, x1='Random')
Paleo_Boot_RiverDist <- cbind(Paleo_River_Boot, x1='Paleoindian')
EarlyArchaic_RiverDist <- cbind(EarlyArchaic_River_Boot, x1='Early Archaic')
Multi_Boot_RiverDist <- cbind(Multi_River_Boot, x1='Multi-Component')

RiverDist_test<-rbind(RiverDist_rand, Paleo_Boot_RiverDist, EarlyArchaic_RiverDist, 
                      Multi_Boot_RiverDist)
RiverDist_test.df<-as.data.frame(RiverDist_test)

RiverDist_test.df$Dist_River_Sim <- as.numeric(as.character(RiverDist_test.df$Dist_River_Sim))

RiverDist_test.df<- rename(RiverDist_test.df, meters = Dist_River_Sim)

RiverDist_test.df$x1 <- factor(RiverDist_test.df$x1  , levels=c("Paleoindian", "Early Archaic", "Multi-Component", "Random"))
summary(elevation_test.df)

ggplot(RiverDist_test.df, aes(x1, meters)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "meters")+ 
  ggtitle("Distance to Major River")



RiverDist.aov <- aov(meters ~ x1, data = RiverDist_test.df) #ANOVA
summary(RiverDist.aov) #ANOVA
etaSquared(RiverDist.aov)#Effect Size for ANOVA

TukeyHSD(RiverDist.aov) #Tukey's HSD groupings
oneway.test(meters ~ x1, data = elevation_test.df) #Welch's T-Test
kruskal.test(meters ~ x1, data = elevation_test.df) #Kruskall Wallis




###################Distance to Raw Material########################################################

ggplot() + geom_sf(data = project_area) + geom_sf(data = arch_sites)+ 
  geom_sf(data=raw_material)

rawmat.sp<-as(raw_material, "Spatial")

paleo.dist.rawmat<-apply(gDistance(Paleo_Sites.sp, rawmat.sp,byid=TRUE),2,min)
earlyarchaic.dist.rawmat<-apply(gDistance(EarlyArchaic_Sites.sp, rawmat.sp,byid=TRUE),2,min)
multi.dist.rawmat<-apply(gDistance(Multi_Sites.sp, rawmat.sp,byid=TRUE),2,min)

hist(paleo.dist.rawmat)
hist(earlyarchaic.dist.rawmat)
hist(multi.dist.rawmat)


#######Paleo Bootstrap

times=100
Paleo_RawMat_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rawmat.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  Paleo_RawMat_Boot[i]=mean_dist
}

hist(Paleo_RawMat_Boot)

#######Early Archaic Bootstrap

times=100
EarlyArchaic_RawMat_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rawmat.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  EarlyArchaic_RawMat_Boot[i]=mean_dist
}

hist(EarlyArchaic_RawMat_Boot)

#######Multi Bootstrap

times=100
Multi_RawMat_Boot=vector(length=times)
for(i in 1:times){
  
  arch_sites_coords<-Paleo_Sites$geometry
  resample <- sample(arch_sites_coords, replace = TRUE)
  resamples.sp<-as (resample, 'Spatial')
  
  dist_results<-apply(gDistance(resamples.sp, rawmat.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  Multi_RawMat_Boot[i]=mean_dist
}

hist(Multi_RawMat_Boot)


#########random sim


times=100
RawMat_Sim=vector(length=times)
for(i in 1:times){
  
  samples_per_polygon <- rep(site_count, nrow(project_area))
  samples <- st_sample(project_area, samples_per_polygon)
  samples.sp<-as (samples, 'Spatial')
  
  dist_results<-apply(gDistance(samples.sp, rawmat.sp,byid=TRUE),2,min)
  mean_dist<-mean(dist_results)
  RawMat_Sim[i]=mean_dist
}

hist(RawMat_Sim)



RawMat_rand <- cbind(RawMat_Sim, x1='Random')
Paleo_Boot_RawMatDist <- cbind(Paleo_RawMat_Boot, x1='Paleoindian')
EarlyArchaic_RawMatDist <- cbind(EarlyArchaic_RawMat_Boot, x1='Early Archaic')
Multi_Boot_RawMatDist <- cbind(Multi_RawMat_Boot, x1='Multi-Component')

RawMat_test<-rbind(RawMat_rand, Paleo_Boot_RawMatDist, EarlyArchaic_RawMatDist, 
                      Multi_Boot_RawMatDist)
RawMat_test.df<-as.data.frame(RawMat_test)

RawMat_test.df$RawMat_Sim <- as.numeric(as.character(RawMat_test.df$RawMat_Sim))

RawMat_test.df<- rename(RawMat_test.df, meters = RawMat_Sim)

RawMat_test.df$x1 <- factor(RawMat_test.df$x1  , levels=c("Paleoindian", "Early Archaic", "Multi-Component", "Random"))
summary(elevation_test.df)

ggplot(RawMat_test.df, aes(x1, meters)) +
  geom_boxplot() + labs(x = "Group") + labs(y = "meters") + 
  ggtitle("Distance to Raw Material")



RawMat.aov <- aov(meters ~ x1, data = RawMat_test.df) #ANOVA
summary(RawMat.aov) #ANOVA
etaSquared(RawMat.aov) #Effect Size for ANOVA

TukeyHSD(RawMat.aov) #Tukey's HSD groupings
oneway.test(meters ~ x1, data = RawMat_test.df) #Welch's T-Test
kruskal.test(meters ~ x1, data = RawMat_test.df) #Kruskall Wallis


