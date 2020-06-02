library("ecoreg", lib.loc="C:/Program Files/R/R-3.1.0/library")


## independent (marginal) aggregate level covariates
## -------------------------------------------------

# read.csv "drinkingLA"
# read.csv "smokingLA"
# agg <- merge(drinkingLA.dat[,c("Name","Indicator.value")], smokingLA.dat[,c("Name","Indicator.value")], by="Name")


## LA stats from: https://docs.google.com/spreadsheet/ccc?key=0AonYZs4MzlZbdFY2LThicTlNSThHNHA5Q2hjUFRyanc#gid=6
## Census 2011 data.xls

pop.dat <- read.csv("C:/Users/ngreen1/Dropbox/data/census_pop_males_agegrp_la.csv", check.names=FALSE)
rf.dat <- read.csv("C:/Users/ngreen1/Dropbox/data/census_ukborn_qualif_white_la.csv", check.names=FALSE)
dat <- merge(rf.dat, pop.dat, by.x="LSOA_CODE", by.y="ONS code")

# hist(dat$"White (%)", breaks=30)

agegrp.dat <- read.csv("C:/Users/ngreen1/Dropbox/data/5year_agegroups_pop.csv", check.names=FALSE)
dat <- merge(dat, agegrp.dat[,c("Area code","15-24%","15-34%")], by.x="LSOA_CODE", by.y="Area code")

## join Regions
##TODO##
dat <- merge(dat, geo_lookup)


## using SAR (SAM) for covariance dependence between binary variables
## ------------------------------------------------------------------

# SAM <- read.delim("C:/Users/ngreen1/Dropbox/data/chlamydia/SAR (SAM 2001)/UKDA-7207-tab/tab/01uklicsam-20070301.tab", check.names=FALSE)
SAM <- read.csv("C:/Users/ngreen1/Dropbox/data/chlamydia/SAR (SAM 2001)/UKDA-7207-tab/tab/uklicsam.csv", check.names=FALSE)#, nrows=100000)

## order lookup table by New ONS code and add add sequence
##TODO##

## join Regions
##TODO##
SAM <- merge(SAM, geo_lookup)


SAM$age1624 <- SAM$agea%in%c(16,20)
SAM$age1634 <- SAM$agea%in%c(16,20,25,30)
SAM$nonUK <- SAM$cobirta==5
SAM$WhiteBrit <- SAM$ethewa==1
SAM$male <- SAM$sex==1


## cross-correlations
## ------------------

## LA
i <- 1
# colnames <- c("nonUK", "WhiteBrit", "male", "age1624")
colnames <- c("male", "age1624")
crossLA <- matrix(NA,
              nrow=length(unique(SAM$lacode)),
              ncol= 2^length(colnames))

for (la in unique(SAM$lacode)){
  crossLA[i,] <- c(prop.table(table(SAM[SAM$lacode==la, colnames])))
  i <- i+1
}

pairs(crossLA)
prop.table(table(SAM[,c("age1624","nonUK","WhiteBrit","male")]))

## Region
i <- 1
crossRegion <- matrix(NA,
                  nrow=length(unique(SAM$region)),
                  ncol= 2^length(colnames))

for (region in unique(SAM$region)){
  crossRegion[i,] <- c(prop.table(table(SAM[SAM$region==region, colnames])))
  i <- i+1
}

pairs(crossRegion)


#  ------------------------------------------------------------------------



## Simulate some aggregate data and some combined aggregate and
## individual data. 
ng <- 50
N <- rep(100, ng)
set.seed(1)
ctx <- cbind(deprivation = rnorm(ng), mean.income = rnorm(ng))
phi <- cbind(nonwhite = runif(ng), smoke = runif(ng))
sim.df <- as.data.frame(cbind(ctx, phi))
mu <- qlogis(0.05)  ## Disease with approximate 5% prevalence
alpha.c <- log(c(1.01, 1.02))  ## Odds ratios for group-level deprivation and mean imcome
alpha <- log(c(1.5, 2)) ## Odds ratios for individual-level ethnicity and smoking
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha)
sim2 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, isam=7)

## Fit the model to recover the simulated odds ratios.
aggdata <- as.data.frame(cbind(y=sim1$y, sim.df))
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco

## Combining with individual-level data
## doesn't improve the precision of the estimates.
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
                     binary = ~ nonwhite + smoke,
                     iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
                     data = aggdata, idata=sim2$idata)
agg.indiv.eco

## However, suppose we have much lower between-area variance in the
## mean covariate value.
phi <- cbind(nonwhite = runif(ng, 0, 0.3), smoke = runif(ng, 0.1, 0.4))
sim.df <- as.data.frame(cbind(ctx, phi))
sim1 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha)
sim2 <- sim.eco(N, ctx=~deprivation+mean.income, binary=~nonwhite+smoke, data = sim.df,  mu=mu, alpha.c=alpha.c, alpha=alpha, isam=10)
aggdata <- as.data.frame(cbind(y=sim1$y, sim.df))

## The aggregate data now contain little information about the
## individual-level effects, and we get biased estimates of the true individual model. 
agg.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
               binary = ~ nonwhite + smoke,  data = aggdata)
agg.eco

## We need individual-level data to be able to estimate the
## individual-level effects accurately. 
agg.indiv.eco <- eco(cbind(y, N) ~ deprivation + mean.income,
                     binary = ~ nonwhite + smoke,
                     iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
                     data = aggdata, idata=sim2$idata)
agg.indiv.eco

## But then why not just study the individual data?  Combining with
## aggregate data improves precision.  
indiv.eco <- eco(iformula = y ~ deprivation + mean.income + nonwhite + smoke, 
                 idata=sim2$idata)
indiv.eco
