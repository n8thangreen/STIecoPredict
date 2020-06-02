# http://data.princeton.edu/pop510/hospBUGS.html
# run R as administrator

## with over-dispersion parameter, ONS urban-rural classification
l

library(MASS)
library(R2WinBUGS)
library(plyr)
library(lattice)
library(mcmcplots)

# load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")
load("../../scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

Natsal0 <- Natsal

## LAs in Natsal with over 100 individuals sampled
subnames <- names(table(Natsal$laname)[(table(Natsal$laname))>100])
Natsal <- Natsal[Natsal$laname%in%subnames,]

## bootstrap sample Natsal for study design
if(!exists("ss")){
    ss <- sample(1:nrow(Natsal), prob=Natsal$total_wt, replace=TRUE)
    Natsal <- Natsal[ss,]}

# Natsal <- subset(Natsal, age>=15 & age<25)   #NCSP range
Natsal <- subset(Natsal, age>=15 & age<45)   #boost sampled range

LA_factor_lookup <- data.frame(`LA.Name`=levels(LA_factor.name), numeric=sort(unique(as.numeric(LA_factor.name))))
UrbRur <- merge(LA_factor_lookup, LAclassification.dat[,c("LA Name", "Numerical classification")],
                by.x="LA.Name", by.y="LA Name", all.x=TRUE)[,"Numerical classification"]

LA_factor.name <- droplevels(as.factor(Natsal$laname))
ethnic2.name <- droplevels(as.factor(Natsal$ethnic2))

WinBUGSdata <- data.frame(cttestly = Natsal$cttestly,
                          la_factor = as.numeric(LA_factor.name),
                          ONSclass = Natsal$`Numerical classification`,
                          male = as.numeric(Natsal$rsex)-1,
                          student = as.numeric(Natsal$student),
                          smokenow = as.numeric(Natsal$smokenow),
                          dage = Natsal$dage,
                          age = Natsal$dage-min(Natsal$dage)+1,
                          ethngrp = as.numeric(ethnic2.name))

Nage <- length(table(WinBUGSdata$age))
Nla <- length(table(WinBUGSdata$la_factor))
Nethgrp <- length(table(WinBUGSdata$ethngrp))

# setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")
setwd("../../scripts/WinBUGS/temp_WinBUGS_output")

sink("model.txt")
cat("
    model
    {

    # Priors
    alpha ~ dunif(-2,1)
    b.male ~ dunif(-2,1)
    b.student ~ dunif(-2,1)

    # Hyperpriors
    #tau.age ~ dunif(0,0.5)
    tau.age ~ dgamma(0.001, 0.001)
    #tau.ethngrp ~ dunif(2,3)
    tau.ethngrp ~ dgamma(0.001, 0.001)
    #tau.la ~ dunif(20,200)
    tau.la ~ dgamma(0.001, 0.001)

    # Likelihood
    for(i in 1:n) {

        cttestly[i] ~ dbin(p[i], N)
        logit(p[i]) <- alpha + b.male*male[i] + b.student*student[i] + v[age[i]] + w[ethgrp[i]] + r[la_factor[i]]
    }
    # age
    for(k in 1:K) {
        v[k] ~ dnorm(0, tau.age)
    }
    # ethnic group
    for(l in 1:L) {
        w[l] ~ dnorm(0, tau.ethngrp)
    }
    # LA
    for(s in 1:S) {
        r[s] ~ dnorm(0, tau.la)
    }

    }
    ", fill=TRUE)
sink()

# Bundle data
win.data <- list(male = WinBUGSdata$male,
                 age = WinBUGSdata$age,
                 student = WinBUGSdata$student,
                 ethgrp = WinBUGSdata$ethngrp,
                 la_factor = WinBUGSdata$la_factor,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata), K=Nage, L=Nethgrp, S=Nla, N = 1)

# Initial values function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.01, tau.ethngrp=3, tau.la=1),
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.01, tau.ethngrp=3, tau.la=1),
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.01, tau.ethngrp=3, tau.la=1)
    )


# Parameters to estimate
params <- c("alpha", "b.male", "b.student", "tau.age", "tau.ethngrp", "tau.la", "v", "w", "r")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 10000 #Number of draws from posterior
nb <- 200 #Number of draws to discard as burn-in
nt <- 100 #Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(),
            # bugs.directory = 'C:/Program Files/WinBUGS14')
            bugs.directory = 'C:/Users/nathan.green.PHE/Documents/WinBUGS14')

# save(out, file="MCMCoutput.RData")
# outmcmc <- as.mcmc.list(out)


ethngrp.names <- tolower(levels(ethnic2.name))  #w  #levels(Natsal$ethnic2)[sort(unique(as.numeric(Natsal$ethnic2)))]
age.names <- as.character(sort(unique(WinBUGSdata$dage))) #v
la.names <- as.character(levels(LA_factor.name))  #r

## read results back in
coda1.file <- "coda1.txt"
coda2.file <- "coda2.txt"
coda3.file <- "coda3.txt"
coda1 <- read.delim(coda1.file, header=FALSE)
codaIndex <- read.delim("codaIndex.txt", header=FALSE)
outmat <- data.frame(matrix(coda1$V2, nrow = min(which(coda1$V1==max(coda1$V1)))))
names(outmat) <- codaIndex$V1

## replace parameter names with proper labels
names(outmat)[grepl("w\\[.*\\]", names(outmat))] <- ethngrp.names
names(outmat)[grepl("v\\[.*\\]", names(outmat))] <- age.names
names(outmat)[grepl("r\\[.*\\]", names(outmat))] <- la.names

save(outmat, file="outmat.RData")
outmcmc <- read.bugs(c(coda1.file, coda2.file, coda3.file))

# summary(outmcmc)
# traceplot(outmcmc)
# densplot(outmcmc)
# xyplot(outmcmc)
# densityplot(outmcmc)
# mcmcplot(outmcmc)


###########
## plots ##
###########

## using bugs object
## -----------------

# par(mar=c(5,15,4,2))
# caterplot(out, lwd=c(1,4), style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=1.2,
#           # parms = c("alpha", "b.male", "b.student"), denstrip = TRUE)
#           parms = c("w", "v", "r", "b.male", "b.student"),
#           labels = c(ethngrp.names, age.names, la.names, "male", "student"))
# axis(1); axis(3)
# abline(v=0, lty=2)
#
#
# ## LAs only
# par(mar=c(5,15,4,2))
# caterplot(out, lwd=c(1,2), style = "plain", col="black", pch=19, reorder=TRUE, axes=F, labels = NA,
#           # parms = c("alpha", "b.male", "b.student"), denstrip = TRUE)
#           parms = c("r"))
# axis(1); axis(3)
# abline(v=0, lty=2, col="red")
#
# ## age only
# par(mar=c(5,15,4,2))
# caterplot(out, style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=0.8,
#           # parms = c("alpha", "b.male", "b.student"), denstrip = TRUE)
#           parms = c("v"),
#           labels = age.names)
# axis(1); axis(3)
# abline(v=0, lty=2, col="red")


## using mcmc object
## -----------------

par(mar=c(5,15,4,2))
caterplot(outmcmc, lwd=c(1,4), style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=1.2,
          parms = c("w", "b.male", "b.student"),
          labels = c(ethngrp.names, "male", "student"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")


## LAs only
par(mar=c(5,5,4,2))
caterplot(outmcmc, lwd=c(1,2), style = "plain", col="black", pch=19, reorder=TRUE, axes=F, labels = NA,
          # parms = c("alpha", "b.male", "b.student"), denstrip = TRUE)
          parms = c("r"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## age only
par(mar=c(5,10,4,2))
caterplot(outmcmc, style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=0.8,
          # parms = c("alpha", "b.male", "b.student"), denstrip = TRUE)
          parms = c("v"),
          labels = age.names)
axis(1); axis(3)
abline(v=0, lty=2, col="red")

