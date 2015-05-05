# Simple Deerfield Landscape Decision Model Under Uncertainty – Prototype 1
# Updated 05052015 RAK

########################### Decision Context ########################### 

# Two fundamental objectives:
# 1.	maximize watershed-level spring salamander occupancy (km occupied via highres catchments)
# 2.	maximize watershed-level brook trout occupancy (km occupied via highres catchments))

# Two management action themes:
# warning: these may not be exactly right yet - just brainstorming simply options to start
# 1.	land protection (maintain any type of forest cover in a catchment)
# a.	limit % in any catchment from falling below X % forested 	
# i.e., limit development within catchments
# b.	limit % of catchments from falling below 100% forested 
# i.e., limit development among catchments
# 2.	aquatic barriers
# a.	remove dams in catchment (i.e., upstream impoundment area?)	# ask dan about this metric; used TNC_bar_deg for now
# b.	remove culverts in catchment (i.e., affects local habitat, not connectivity) 

########################### Description of Variables ########################### 

# import covariates
master_data <- read.delim("~/Desktop/simulation/master_deerfield_catchment_covariates.txt")

# catchments: based on high resolution flowline
# 1039 delineated from highres flow lines (kyle)
# removed 79 catchments > 300 km2 to remove drainages directly into the Deerfield mainstem
# removed 1 catchment with stream temps = 0
# resulted in n = 959 catchments
no.catchments<-959

# subset master_data 
deerfield_data<-subset(master_data,EXCLUDE==0)
str(deerfield_data)
summary(deerfield_data)

# species: salamander = 1, trout = 2
no.species<-2

# number of simulations
no.simulations<-10

# store mean and SD of each effect
no.betas<-7
beta<-rep(NA,no.catchments*no.betas*no.species*no.simulations)
dim(beta)<-c(no.simulations,no.species,no.betas,no.catchments)

# stream km: total stream km based on high resolution flowlines within each catchment (min km estimate?)
# highreskm[c]	(min=0.0208km, max=9.3124km, mean=2.0365km)	# from kyle (highres_detailed)

# random intercept (average occupancy) by catchment (or subwatershed?)
# hypothesis: occupancy[trout] randomly varied among catchments due to unexplained factors
# hypothesis: occupancy[salamander] randomly varied among catchments due to unexplained factors
for (sim in 1:no.simulations){ for (catchment in 1:no.catchments){
  beta[sim,1,1,catchment] <- rnorm(1, mean= 2.95, sd=2.95*0.1)		# logit-mean of 2.95 = mean of 0.95 # need expert elicitation
  beta[sim,2,1,catchment] <- rnorm(1, mean= 2.95, sd=2.95*0.1)		# logit-mean of 1.10 = mean of 0.75 # re-estimate this for the Deerfield
}}
par(mfrow=c(2,1));hist(1/(1+exp(-beta[,1,1,])),breaks=50,main="salamander random intercept",xlim=c(min(1/(1+exp(-beta[,,1,]))),1));abline(v=mean(1/(1+exp(-beta[,1,1,]))),col="red");hist(1/(1+exp(-beta[,2,1,])),breaks=50,main="brooktrout random intercept",xlim=c(min(1/(1+exp(-beta[,,1,]))),1));abline(v=mean(1/(1+exp(-beta[,2,1,]))),col="red")

# effect of percforest in the upstream catchment
# effect varies by catchment (random slope)
# hypothesis: occupancy[trout] increases with increasing forest cover (linear)
# hypothesis: occupancy[salamander] increases with increasing forest cover (linear) more than trout?
# forest[c] 	(min=0.25, max=1.00, mean=0.89)	# from kyle
# beta effect is on 1 unit (1SD = 10%) increase in percent forest cover # see normal_matrix
for (sim in 1:no.simulations){ for (catchment in 1:no.catchments){
  beta[sim,1,2,catchment] <- rnorm(1, mean= 0.1, sd=0)		# exp(0.5) = 11x MORE likely to occur per unit #need expert elicitation
  beta[sim,2,2,catchment] <- rnorm(1, mean= 0.05, sd=0)		# exp(0.05) = 5x MORE likely to occur per unit #re-estimate this for the Deerfield
}}
#par(mfrow=c(2,1));plot(density(beta[,1,1,]),main="salamander random percforest");plot(density(beta[,2,2,]),main="brooktrout random percforest")

# effect of percwetland in the upstream catchment
# effect varies by catchment (random slope)
# hypothesis: occupancy[trout]…
# hypothesis: occupancy[salamander] increases with increasing wetland (linear)
# wetland[c]  (min=0.00, max=0.28, mean=0.03)		# from kyle
# beta effect is on 1 unit (1SD = 4%) increase in percent wetland # see normal_matrix
for (sim in 1:no.simulations){ for (catchment in 1:no.catchments){
  beta[sim,1,3,catchment] <- rnorm(1, mean=0.01, sd=0)		# exp(0.01) = 1x MORE likely to occur per unit #need expert elicitation
  beta[sim,2,3,catchment] <- rnorm(1, mean=-0.01, sd=0)		# exp(-0.01) = 1x LESS likely to occur per unit #re-estimate this for the Deerfield
}}
#par(mfrow=c(2,1));plot(density(beta[,1,3,]),main="salamander random percwetland");plot(density(beta[,2,3,]),main="brooktrout random percwetland")

# effect of meanmaxtemp
# hypothesis: occupancy[trout] decreases with increasing temperatures (linear)
# hypothesis: occupancy[salamander] is not affected by increasing temperatures, until reaching X degC, then declines (threshold)
# meanMaxTemp[c]	(min=18.59C, max=33.00C, mean=21.41C)	# from deerfield dan's temp model
# beta effect is on 1 unit (1SD = 1.5C) increase in meanmaxtemp # see normal_matrix
for (sim in 1:no.simulations){ #for (catchment in 1:no.catchments){
  beta[sim,1,4,1:no.catchments] <- rnorm(1, mean=0, sd=0)		# need expert elicitation (evan may have some of this data - 3xnp’s from midatl - jofherp)
  beta[sim,2,4,1:no.catchments] <- rnorm(1, mean=-0.10, sd=0)		# exp(-0.10) = 10x LESS likely to occur # re-estimate this for the Deerfield
} #}
#par(mfrow=c(2,1));plot(density(beta[,1,4,]),main="salamander random temp");plot(density(beta[,2,4,]),main="brooktrout random temp")

# effect of nodamsupstream in the upstream catchment (local habitat) # ask dan about the dam/impoundment metric
# hypothesis: occupancy[trout]…decreases with the number of dams in the catchment (linear)
# hypothesis: occupancy[salamander]…decreases with the number of dams in the catchment (linear)
# nodamsupstream[c]  (min=0, max=810, mean=12)		# from kyle (TNC deg barr - don't know what this means!)
for (sim in 1:no.simulations){ #for (catchment in 1:no.catchments){
  beta[sim,1,5,1:no.catchments] <- rnorm(1, mean=0, sd=0)		# need expert elicitation
  beta[sim,2,5,1:no.catchments] <- rnorm(1, mean=0, sd=0)		# re-estimate this for the Deerfield
} #}
#par(mfrow=c(2,1));plot(density(beta[,1,5,]),main="salamander random dam");plot(density(beta[,2,5,]),main="brooktrout random dam")

# effect of noculvertsupstream in the upstream catchment (local habitat)
# hypothesis: occupancy[trout] decreases with the number of culverts in the catchment (linear)
# hypothesis: occupancy[salamander] decreases with the number of culverts in the catchment (linear) 
# noculvertssupstream[c]  		(min=0, max=23, mean=1.6)		# rachel calculated from streamcontinuity + ana highres
for (sim in 1:no.simulations){ #for (catchment in 1:no.catchments){
  beta[sim,1,6,1:no.catchments] <- rnorm(1, mean=0.1, sd=0)		# need expert elicitation
  beta[sim,2,6,1:no.catchments] <- rnorm(1, mean=-0.1, sd=0)		# re-estimate this for the Deerfield
} #}
#par(mfrow=c(2,1));plot(density(beta[,1,6,]),main="salamander random culvert");plot(density(beta[,2,6,]),main="brooktrout random culvert")

# effect of troutpres on salamander occupancy
# hypothesis: occupancy[salamander] is lower in catchments with brook trout (binary)
# values based on model predictions
for (sim in 1:no.simulations){ #for (catchment in 1:no.catchments){
  beta[sim,1,7,1:no.catchments] <- rnorm(1, mean=0, sd=0)		# need expert elicitation
  beta[sim,2,7,1:no.catchments] <- 0	
} #}	
#par(mfrow=c(2,1));plot(density(beta[,1,6,]),main="salamander random culvert");plot(density(beta[,2,6,]),main="brooktrout random culvert")

# effect of meandailyflow?
# hypothesis: occupancy[trout] increases with increasing mean daily flow, but may decline during flood years (quadratic)
# hypothesis: occupancy[salamander] increases with increasing mean daily flow, but may decline during flood years (quadratic)
# meandailyflow[c]	# don’t have this variable…ask dan. 
# for (sim in 1:no.simulations){ #for (catchment in 1:no.catchments){
# beta[sim,1,X,c] ~ dnorm(mean=0, sd=100)			# need expert elicitation and/or meta-analysis and/or direct estimation
# beta[sim,2,X,c] ~ dnorm(mean=0, sd=100) 		# re-estimate this for the Deerfield
# }}
# other fragmentation, landscape, habitat covariates…

str(beta)

########################### Future Scenarios ############################################

####### climate uncertainty 
# influences temperature covariate (meanmaxtemp)
no.climatemodels<-3
climatemodel1 <- 0 	# +0C/catchment
climatemodel2 <- 1	# +1C/catchment
climatemodel3 <- 2	# +2C/catchment

climatemodelpred<-rep(NA,no.catchments*no.climatemodels);dim(climatemodelpred)<-c(no.climatemodels,no.catchments)
climatemodelpred[1,1:no.catchments]<-deerfield_data$meanMaxTemp[1:no.catchments]+climatemodel1	
climatemodelpred[2,1:no.catchments]<-deerfield_data$meanMaxTemp[1:no.catchments]+climatemodel2	
climatemodelpred[3,1:no.catchments]<-deerfield_data$meanMaxTemp[1:no.catchments]+climatemodel3	

climatemodelbeliefs<-list()
climatemodelbeliefs$equal<-c(1/3,1/3,1/3)
climatemodelbeliefs$cm1<-c(1,0,0)
climatemodelbeliefs$cm2<-c(0,1,0)
climatemodelbeliefs$cm3<-c(0,0,1)

####### land development uncertainty
# influences percent forest cover covariate (percforest)
no.landdevmodels<-3
landdevmodel1 <- 0    # no development = change in forest cover
landdevmodel2 <- 0.01	# 1% loss in forest cover in EACH catchment (this should vary with different probs per catchement)
landdevmodel3 <- 0.10	# 10% loss in forest cover in EACH catchment (this should vary with different probs per catchement)

landdevmodelpred<-rep(NA,no.catchments*no.landdevmodels);dim(landdevmodelpred)<-c(no.landdevmodels,no.catchments)
landdevmodelpred[1,1:no.catchments]<-deerfield_data$forest[1:no.catchments]-deerfield_data$forest[1:no.catchments]*landdevmodel1
landdevmodelpred[2,1:no.catchments]<-deerfield_data$forest[1:no.catchments]-deerfield_data$forest[1:no.catchments]*landdevmodel2
landdevmodelpred[3,1:no.catchments]<-deerfield_data$forest[1:no.catchments]-deerfield_data$forest[1:no.catchments]*landdevmodel3	

landdevmodelbeliefs<-list()
landdevmodelbeliefs$equal<-c(1/3,1/3,1/3)
landdevmodelbeliefs$cm1<-c(1,0,0)
landdevmodelbeliefs$cm2<-c(0,1,0)
landdevmodelbeliefs$cm3<-c(0,0,1)


########################### Management Actions and Portfolios ###########################

# see list in master_deerfield_catchment_covariates.txt 

# plot current percforest values
# hist(deerfield_data$forest, breaks=100);abline(v=mean(deerfield_data$forest),col="blue")
# forestaction1 = no action - current forest cover in all catchments 
# forestaction2 = increase all catchments to 90% forest cover = X acres protected
# forestaction3 = increase all catchments to 80% forest cover = X acres protected
# forestaction4 = increase all catchments to 70% forest cover = X acres protected

# plot current dam deg_barr_all values
# hist(deerfield_data$deg_barr_all, breaks=100);abline(v=mean(deerfield_data$deg_barr_all),col="blue")
# damaction1 = no action - current number of dams/impounded area in all catchments
# damaction2 = remove X largest dams/impounded areas
# damaction3 = remove X smallest dams/impounded areas

# plot current culvertcount values
# hist(deerfield_data$upstream_culvertcount, breaks=100);abline(v=mean(deerfield_data$upstream_culvertcount),col="blue")
# culvertaction1 = no action - current number of culverts in all catchments
# culvertaction2 = reduce to 1 culvert per catchment
# culvertaction3 = reduce to 0 culverts per catchment 

# other actions?

# Create action portfolios for each combination of land protection and barriers (in R)
# assume effects are additive (not synergistic or account for cost efficiencies) 
no.portfolios<-36
portfolio<-rep(NA,no.catchments*3*no.portfolios);dim(portfolio)<-c(no.catchments,3,no.portfolios)
portfolio[1:no.catchments,1:3,1]  <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction1,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,2]  <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction2,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,3]  <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction3,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,4]  <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction1,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,5]  <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction2,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,6]  <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction3,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,7]  <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction1,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,8]  <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction2,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,9]  <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction3,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,10] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction1,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,11] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction2,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,12] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction3,deerfield_data$damaction1)
portfolio[1:no.catchments,1:3,13] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction1,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,14] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction2,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,15] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction3,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,16] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction1,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,17] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction2,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,18] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction3,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,19] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction1,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,20] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction2,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,21] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction3,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,22] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction1,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,23] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction2,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,24] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction3,deerfield_data$damaction2)
portfolio[1:no.catchments,1:3,25] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction1,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,26] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction2,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,27] <- cbind(deerfield_data$forestaction1,deerfield_data$culvertaction3,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,28] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction1,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,29] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction2,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,30] <- cbind(deerfield_data$forestaction2,deerfield_data$culvertaction3,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,31] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction1,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,32] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction2,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,33] <- cbind(deerfield_data$forestaction3,deerfield_data$culvertaction3,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,34] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction1,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,35] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction2,deerfield_data$damaction3)
portfolio[1:no.catchments,1:3,36] <- cbind(deerfield_data$forestaction4,deerfield_data$culvertaction3,deerfield_data$damaction3)

########################### Simple Occupancy Model ########################### 

###### 1. species-specific occupancy model

# normalize variables
normal_matrix<-rep(NA,no.betas*2);dim(normal_matrix)<-c(no.betas,2)
colnames(normal_matrix)<-c("mean","sd");rownames(normal_matrix)<-c("int","forest","wetland","temp","dam","culvert","trout")
normal_matrix["forest",]<-c(mean(deerfield_data$forest),sd(deerfield_data$forest))
normal_matrix["wetland",]<-c(mean(deerfield_data$fwswetland),sd(deerfield_data$fwswetland))
normal_matrix["temp",]<-c(mean(deerfield_data$meanMaxTemp),sd(deerfield_data$meanMaxTemp))
normal_matrix["dam",]<-c(mean(deerfield_data$deg_barr_all),sd(deerfield_data$deg_barr_all))
normal_matrix["culvert",]<-c(mean(deerfield_data$upstream_culvertcount),sd(deerfield_data$upstream_culvertcount))

normalized_covars<-list()
normalized_covars$forest<-(deerfield_data$forest-rep(normal_matrix["forest","mean"],no.catchments))/(rep(normal_matrix["forest","sd"],no.catchments))
normalized_covars$wetland<-(deerfield_data$fwswetland-rep(normal_matrix["wetland","mean"],no.catchments))/(rep(normal_matrix["wetland","sd"],no.catchments))
normalized_covars$temp<-(deerfield_data$meanMaxTemp-rep(normal_matrix["temp","mean"],no.catchments))/(rep(normal_matrix["temp","sd"],no.catchments))
normalized_covars$dam<-(deerfield_data$deg_barr_all-rep(normal_matrix["dam","mean"],no.catchments))/(rep(normal_matrix["dam","sd"],no.catchments))
normalized_covars$culvert<-(deerfield_data$upstream_culvertcount-rep(normal_matrix["culvert","mean"],no.catchments))/(rep(normal_matrix["culvert","sd"],no.catchments))

normalized_climatemodelpred<-rep(NA,no.catchments*no.climatemodels);dim(normalized_climatemodelpred)<-c(no.climatemodels,no.catchments)
normalized_climatemodelpred[1,]<-(climatemodelpred[1,]-rep(normal_matrix["temp","mean"],no.catchments))/(rep(normal_matrix["temp","sd"],no.catchments))
normalized_climatemodelpred[2,]<-(climatemodelpred[2,]-rep(normal_matrix["temp","mean"],no.catchments))/(rep(normal_matrix["temp","sd"],no.catchments))
normalized_climatemodelpred[3,]<-(climatemodelpred[3,]-rep(normal_matrix["temp","mean"],no.catchments))/(rep(normal_matrix["temp","sd"],no.catchments))
#plot(normalized_climatemodelpred[1,]);lines(normalized_climatemodelpred[2,]);lines(normalized_climatemodelpred[3,])

normalized_portfolio_covars<-rep(NA,no.catchments*3*no.portfolios);dim(normalized_portfolio_covars)<-c(no.catchments,3,no.portfolios)
for (i in 1:no.portfolios){
  normalized_portfolio_covars[,1,i]<-(portfolio[,1,i]-rep(normal_matrix["forest","mean"],no.catchments))/(rep(normal_matrix["forest","sd"],no.catchments))
  normalized_portfolio_covars[,2,i]<-(portfolio[,2,i]-rep(normal_matrix["culvert","mean"],no.catchments))/(rep(normal_matrix["culvert","sd"],no.catchments))
  normalized_portfolio_covars[,3,i]<-(portfolio[,3,i]-rep(normal_matrix["dam","mean"],no.catchments))/(rep(normal_matrix["dam","sd"],no.catchments))
}

no.portfolios<-36
alpha<-rep(NA,no.species*no.catchments*no.simulations*no.portfolios*no.climatemodels);dim(alpha)<-c(no.simulations,no.species,no.catchments,no.portfolios,no.climatemodels)
occupancy<-rep(NA,no.species*no.catchments*no.simulations*no.portfolios*no.climatemodels);dim(occupancy)<-c(no.simulations,no.species,no.catchments,no.portfolios,no.climatemodels)
occupancykm<-rep(NA,no.species*no.catchments*no.simulations*no.portfolios*no.climatemodels);dim(occupancykm)<-c(no.simulations,no.species,no.catchments,no.portfolios,no.climatemodels)
watershed_occupancykm<-rep(NA,no.species*no.simulations*no.portfolios*no.climatemodels);dim(watershed_occupancykm)<-c(no.simulations,no.species,no.portfolios,no.climatemodels)
mean_watershed_occupancykm<-rep(NA,no.species*no.portfolios*no.climatemodels);dim(mean_watershed_occupancykm)<-c(no.species,no.portfolios,no.climatemodels)
sd_watershed_occupancykm<-rep(NA,no.species*no.portfolios*no.climatemodels);dim(sd_watershed_occupancykm)<-c(no.species,no.portfolios,no.climatemodels)

# start loop
# 50 sims + 36 portfolios runtime: <20min
# 100 sims + 36 portfolios runtime: <30min
# 10 sims + 36 porfolios + 3 climatemodels runtime: 1:02-
for(sim in 1:no.simulations){
  for(s in 1:no.species){
    for(c in 1:no.catchments){
      for(p in 1:no.portfolios){
        for(cm in 1:no.climatemodels){
          #for(devel in 1:3){
        
        alpha[sim,s,c,p,cm] <-
          beta[sim,s,1,c]+
          beta[sim,s,2,c]*normalized_portfolio_covars[c,1,p]  + 
          beta[sim,s,3,c]*normalized_covars$wetland[c]  + 
          beta[sim,s,4,c]*normalized_climatemodelpred[cm,c] + 
          beta[sim,s,5,c]*normalized_portfolio_covars[c,3,p] + 
          beta[sim,s,6,c]*normalized_portfolio_covars[c,2,p]
        
        #beta[s,7,c]*troutpres[c,p] 
        #troutpres[1:catchment]~binom(occupancy[trout,catchment]) # draw 0,1 for trout presence
        
        # occupancy = 0.5 when alpha = 0
        occupancy[sim,s,c,p,cm] <- 1/(1+exp(-alpha[sim,s,c,p,cm]))
        
        # convert to km occupied
        occupancykm[sim,s,c,p,cm] <- rbinom(1,1,occupancy[sim,s,c,p,cm])*deerfield_data$highreskm[c]
        
        watershed_occupancykm[sim,s,p,cm]<-sum(occupancykm[sim,s,,p,cm])
        mean_watershed_occupancykm[s,p,cm]<-mean(watershed_occupancykm[,s,p,cm])
        sd_watershed_occupancykm[s,p,cm]  <-sd(watershed_occupancykm[,s,p,cm])
        
        
          #} # devel scenario
        } # cm
      } # p
    } # c
  } # s
} # sim

# end loop

par(mfrow=c(2,2))
# plot prob(occpunacy) across catchments from each simulation 
plot(occupancy[,1,,1,1],occupancy[,2,,1,1],xlab="salamander occupancy",ylab="brooktrout occupancy",main="p1: no action")
points(occupancy[,1,,1,2],occupancy[,2,,1,2],pch=19,type="p",col="blue")

plot(watershed_occupancykm[,1,1,1],watershed_occupancykm[,2,1,1],xlab="salamander occupancy km",ylab="brooktrout occupancy km",main="p1: no action + cm1")

# plot watershed km occupied from each simulation for forest actions (1-4)
plot(watershed_occupancykm[,1,1,1],watershed_occupancykm[,2,1,1],
     xlim=c(min(watershed_occupancykm[,,,],1800),max(watershed_occupancykm[,,,])), 
     ylim=c(min(watershed_occupancykm[,,,],1800),max(watershed_occupancykm[,,,])),
     xlab="sal km",ylab="trout km",pch=19,main="4 forest actions") # current forest
points(watershed_occupancykm[,1,4,1],watershed_occupancykm[,2,4,1],col="green",type="p",pch=19)  # 90% forest
points(watershed_occupancykm[,1,7,1],watershed_occupancykm[,2,7,1],col="yellow",type="p",pch=19) # 80% forest
points(watershed_occupancykm[,1,10,1],watershed_occupancykm[,2,10,1],col="blue",type="p",pch=19) # 70% forest
legend("topleft",legend=c("current forest","min 90% forest","min 80% forest","min 70% forest"),col=c("black","green","yellow","blue"),pch=19,cex=0.5,ncol=1)

# plot the Pareto frontier for each portfolio (mean simulation km)
plot(mean_watershed_occupancykm[1,,1],mean_watershed_occupancykm[2,,1],
     xlim=c(min(mean_watershed_occupancykm[,,1]),max(mean_watershed_occupancykm[,,1])), 
     ylim=c(min(mean_watershed_occupancykm[,,1]),max(mean_watershed_occupancykm[,,1])),
     xlab="mean sal km",ylab="mean trout km",pch=c(1:no.portfolios),col=c(1:no.portfolios))
text(mean_watershed_occupancykm[1,,1],mean_watershed_occupancykm[2,,1],labels=c(1:no.portfolios),pos=4,cex=0.5,main="36 portfolios")




########################### Simple Utility Function Model ########################### 

#utilityscore[p] = occ.streamkmsum[1,p]*weight[1,wmethod] +  occ.streamkmsum[2,p]*weight[2,wmethod]

# extract best and worst case across portfolios for values tradeoffs
#minoccupancykmsum[s]<- min(occupancykm[s,])
#maxoccupancykmsum[s]<- max(occupancykm[s,])

# 4 weighting methods (wmethod): equal weight, rank-sum, swing weight
# requires worst (min) and best (max) across occupancykmsum[s,] (see derived output)
# equal weight; weight[s] = 1/no.objectives (i.e, 0.50,0.50) 					  # assumes equal rank
# rank-sum; weight[s] = 2(no.objectives+1-rank[s])/no.objectives(no.objectives+1) # only depends on rank
# swing weight; weight[s] = score[s]/(sum(score[]) # only depends on score

# assume risk neutral (occupancy is linearly related to happiness - we could also add an adverse and seeking behavior simulation)

########################### Sensitivity Analysis ###########################

###### uncertainty affecting occupancy (science uncertainty) 
# see example in Wenger et al.Glob. Chang. Biol. 3343–3354 (2013). doi:10.1111/gcb.12294)
# include residual error (keep random intercepts) in all sensitivity analyses, else predictions would be 100% certain 
1. exclude parameter uncertainty (use mean estimates)
2. exclude model uncertainty (use highest AIC trout and salamander model - only if we have multiple competing)
3. exclude climate uncertainty (use X climate model?)
4. exclude land development uncertainty (use X development projection?)
# create tornato digram with the 95% confidence intervals of km occupied 

plot(occupancykmsum[salamander,p=1]) #p=1 no action
plot(occupancykmsum[trout,p=1]) #p=1 no action





###### uncertainty affecting the optimal decision (decision uncertainty)
1. what is the optimal portfolio (or set) under all uncertainty?
a. climate/temp projections (none +0C, low +1C, medium +2C, high +3C) 
uniform distribution of belief weights = 0.25,0.25,0.25,0.25
b. landuse development projections (low, medium, high)
uniform distribution of belief weights = 0.33, 0.33, 0.33
c. each parameter effect (for each of the 6 covariates)
normal distribution of belief weights (normal dist)
2. what is the optimal portfolio (or set) if climate change is certain (100% = none, low, medium, or high)?
3. what is the optimal portfolio (or set) if land development is certainty (100% = low, medium, high)?
4. what is the optimal portfolio (or set) if each of the effects/parameters are certain (100% = lower 95%, mean, upper 95%)?
5. what is the optimal portfolio (or set) if we knew the system perfectly? (complete certainty - all combinations?)




########################### Next Steps ###########################
1. Utility function assumes linear happiness - explore how portfolio performance changes with risk adverse vs. risk seeking behaviors?
2. Include spatial variation in actions? 
3. Include alternative models? biological?
4. Include headwater flow predictions (i.e., headwaters may contract due to decreasing baseflows via climate change or landuse development)
5. Include variation in values-trade offs (weights) - what combination of values (individual variation) will switch the optimal portfolio?