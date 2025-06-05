###The following code is a simulated example for the methods in 'Estimating intracluster correlation for ordinal data with an
###application to audiometry data.' For more information please contact Benjamin Langworthy at langworthy.ben@gmail.com

###Ordinal fits mixed effects cumulative probit and logistic models
library(ordinal)
###lme4 for mixed effects linear models
library(lme4)
library(VCA)

###Data set characteristics
nsub <- 35
clustsize <- 5

resd <- 1
errsd <- 1
beta <- 1

###########################################
########Single Level Clustering############
###########################################

###Simulate random effects for each 'subject'
re <- rnorm(nsub,mean=0,sd = resd)

###Simulate error terms
err <- rnorm(nsub*clustsize,mean=0,sd =errsd)


###Simulate covariate, unique for each observation
x <- rnorm(nsub*clustsize,mean=0,sd = 1)

###IDs indicating each cluster
ids <- rep(1:nsub,each=clustsize)
###Repeat random effects for each cluster
refull <- rep(re,each=clustsize)

###Calculate output following mixed effects linear model form
y <- x*beta + refull + err

###Make data ordinal by rounding to nearest even integer
yord <- round(y/2,digits=0)

###Create data for model fitting
dat <- data.frame(cbind(ids,y,yord,x,refull,err))
###yord in correct format for mixed effects cumulative logit/probit models
dat$yord <- factor(dat$yord,ordered = TRUE)
###Numeric for when we fit ordinal output in mixed effects linear model
dat$yordnum <- as.numeric(dat$yord)
dat$ids <- as.factor(dat$ids)

###Fit mixed effects cumulative probit and logistic models, clmm2 allows for estimation of profile confidence interval
mixedord2 <- clmm2(yord~x,random=ids,data = dat,link="probit",Hess=TRUE)
mixedordlog2 <- clmm2(yord~x,random=ids,data = dat,link="logistic",Hess=TRUE)

###Calculate adjusted ICC estimate using mixed effects cumulative probit and logistic models
iccadjord2 <- mixedord2$stDev^2/(mixedord2$stDev^2 + 1)
iccadjordlog2 <- mixedordlog2$stDev^2/(mixedordlog2$stDev^2 + pi^2/3)

###Estimate profile confidence interval for random effect in mixed effects cumulative probit and logistic models
mixedordprof <- profile(mixedord2,nSteps = 30)
mixedordci <- confint(mixedordprof)
mixedordlogprof <- profile(mixedordlog2,nSteps = 30)
mixedordlogci <- confint(mixedordlogprof)

###Transform confidence intervals to be for adjusted ICC
iccadjordprofci <- mixedordci^2/(mixedordci^2 + 1)
iccadjordlogprofci <- mixedordlogci^2/(mixedordlogci^2 + pi^2/3)

##########################################
########Multi Level Clustering############
##########################################

###In addition to individual level random effects we have 2 ears and ear level random effect
nears <- 2
reearsd <- 1

reear <- rnorm(nsub*nears,mean=0,sd=reearsd)
errmulti <- rnorm(nsub*clustsize*nears,mean=0,sd =errsd)
###IDs indicating each cluster and ear
ids <- rep(1:nsub,each=clustsize*nears)
ears <- rep(rep(1:nears,each=clustsize),nsub)
###Repeat random errors for each cluster
refullmulti <- rep(re,each=clustsize*nears)

###Repeat ear random effect
reearfull <- rep(reear,each=clustsize)

###Covariate values 
xmulti <- rnorm(nsub*clustsize*nears,mean=0,sd = 1)

###Calculate output following mixed effects linear model form
ymulti <- xmulti*beta + refullmulti + reearfull + errmulti

###Make data ordinal by rounding to nearest even integer
yordmulti <- round(ymulti/2,digits=0)

###Create data for model fitting
datmulti <- data.frame(cbind(ids,ears,y,yord,x,refull,err))
###yord in correct format for mixed effects cumulative logit/probit models
datmulti$yord <- factor(datmulti$yord,ordered = TRUE)
###Numeric for when we fit ordinal output in mixed effects linear model
datmulti$yordnum <- as.numeric(datmulti$yord)
###Id variables need to be factors for clmm to work
datmulti$ids <- as.factor(datmulti$ids)
datmulti$ears <- as.factor(datmulti$ears)

###Fit mixed effects cumulative probit and logistic models, only clmm allows for multi-level random effects
mixedord <- clmm(yord~x+ (1|ids) + (1|ids:ears),data = dat,link="probit",control = list(innerCtrl = "warnOnly",checkRanef="warn"))
mixedordlog <- clmm(yord~x+ (1|ids) + (1|ids:ears),data = dat,link="logit",control = list(innerCtrl = "warnOnly",checkRanef="warn"))


###Calculate adjusted ICC estimate using mixed effects cumulative probit and logistic models
iccadjord <- (mixedord$ST$ids^2 + mixedord$ST$`ids:ears`^2)/(mixedord$ST$ids^2 + mixedord$ST$`ids:ears`^2 + 1)
iccadjordlog <- (mixedordlog$ST$ids^2 + mixedordlog$ST$`ids:ears`^2)/(mixedordlog$ST$ids^2 + mixedordlog$ST$`ids:ears`^2 + pi^2/3)


###Variance covariance matrix for random effects
revars <- vcov(mixedord)[c("ST1","ST2"),c("ST1","ST2")]
revarslog <- vcov(mixedordlog)[c("ST1","ST2"),c("ST1","ST2")]

###Hessian for adjusted ICC
hsd <- matrix(c((2*mixedord$ST[1][[1]])/(mixedord$ST[1][[1]]^2+mixedord$ST[2][[1]]^2+1)^2,
                (2*mixedord$ST[2][[1]])/(mixedord$ST[1][[1]]^2+mixedord$ST[2][[1]]^2+1)^2),ncol = 1)
hsdlog <- matrix(c((2*mixedordlog$ST[1][[1]]*pi^2/3)/(mixedordlog$ST$ids^2 + mixedordlog$ST$`ids:ears`^2 + pi^2/3)^2,
                   (2*mixedordlog$ST[2][[1]]*pi^2/3)/(mixedordlog$ST$ids^2 + mixedordlog$ST$`ids:ears`^2 + pi^2/3)^2),ncol = 1)

###Adjusted ICC variance based on delta method
varsd <- t(hsd)%*%revars%*%hsd
varsdlog <- t(hsdlog)%*%revarslog%*%hsdlog

###Confidence interval for adjusted ICC based on cumulative probit and logistic models
iccadjordci <- iccadjord +c(-1,1)*1.96*sqrt(as.numeric(varsd))
iccadjordlogci <- iccadjordlog +c(-1,1)*1.96*sqrt(as.numeric(varsdlog))

