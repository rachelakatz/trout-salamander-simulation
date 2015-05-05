
deerfield_featureID <- read.delim("~/Desktop/simulation/deefield_featureID.txt")
upstream_AreaSqKM <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_AreaSqKM.csv")
upstream_forest_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_forest_MEAN.csv")
upstream_elevation_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_elevation_MEAN.csv")
upstream_fwsopenwater_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_fwsopenwater_MEAN.csv")
upstream_fwswetlands_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_fwswetlands_MEAN.csv")
upstream_impervious_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_impervious_MEAN.csv")
upstream_developed_MEAN <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_developed_MEAN.csv")
upstream_deg_barr_all <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/covariates/upstream_deg_barr_all.csv")
upstream_highresflowkm <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromkyle/shapefiles/deerfieldhighresflowlines_detailed.txt")
upstream_culvertcount <- read.csv("~/Dropbox/Headwater SDM/gis/data files/fromana_streamcontinuity_04292015/culvert_count.txt")

covars<- cbind(upstream_AreaSqKM[,1:2],upstream_forest_MEAN[,2],upstream_elevation_MEAN[,2],
               upstream_fwsopenwater_MEAN[,2],upstream_fwswetlands_MEAN[,2],upstream_impervious_MEAN[,2],
               upstream_developed_MEAN[,2],upstream_deg_barr_all[,2])
upstream_highresflowkm<-upstream_highresflowkm[,5:6]
upstream_culvertcount<-cbind(upstream_culvertcount$FEATUREID,upstream_culvertcount$gWshed);
colnames(upstream_culvertcount)<-c("FEATUREID","upstream_culvertcount")

total <-NULL
total <- merge(deerfield_featureID,covars,by="FEATUREID")
total <- merge(total,upstream_highresflowkm,by="FEATUREID")
total <- merge(total,upstream_culvertcount,by="FEATUREID")

str(total)
plot(total)
write.csv(total,file="deerfield_upstreamcovariates.csv",row.names = TRUE)

catchments<-subset(total,AreaSqKM<350)
summary(catchments)
hist(catchments$AreaSqKM,breaks=50)


master_deerfield_catchment_covariates <- read.delim("~/Desktop/simulation/master_deerfield_catchment_covariates.txt")
summary(master_deerfield_catchment_covariates)

watershed_meankmoccupied<-rep(NA,2)
for(s in 1:no.species){watershed_meankmoccupied[s]<-mean(occupancykm[,s,])}
plot(watershed_meankmoccupied[1],watershed_meanoccupied[2])
deerfield_catchment_covariates_subset1<-data.frame(cbind(
  deerfield_catchment_covariates$meanMaxTemp,
  deerfield_catchment_covariates$forest,
  deerfield_catchment_covariates$fwswetland,
  deerfield_catchment_covariates$elevation,
  deerfield_catchment_covariates$deg_barr_all,
  deerfield_catchment_covariates$impervious,
  deerfield_catchment_covariates$highreskm,
  deerfield_catchment_covariates$upstream_culvertcount))
colnames(deerfield_catchment_covariates_subset1)<-c("maxtemp","forest","wetland","elev","degbar","imperv","streamkm","culvertcount")
plot(deerfield_catchment_covariates_subset1)
round(cor(deerfield_catchment_covariates_subset1),2) # look at correlations among variables
summary(deerfield_catchment_covariates_subset1)
