# http://data.princeton.edu/pop510/hospBUGS.html
# run R as __administrator__

rm(list = ls())

library(MASS)
library(R2WinBUGS)
library(plyr)
library(lattice)
library(mcmcplots)
library(Hmisc)

load("../../mrp/data/cleaned-regn-input-mrpNatsal.RData")


###############
## prep data ##
###############

Natsal0 <- Natsal

Natsal <- subset(Natsal, age>=15 & age<25)   #NCSP range
# Natsal <- subset(Natsal, age>=15 & age<45)   #boost sampled range

## for consistent cuts across arrays
interiorcuts <- cut2(sim_prop_la$Conception.rate.per.1.000.women.in.age.group, g=10, onlycuts = T)
conception.under18$Conception.decile <- cut2(conception.under18$Conception.rate.per.1.000.women.in.age.group, cuts = interiorcuts)

# convert all factors to integer from 1 to max
# and then map this back to original name at the end
LA_factor.name <- droplevels(as.factor(Natsal$laname))
metUA_factor.name <- droplevels(as.factor(Natsal$metcounty_UA))
ethnic2.name <- droplevels(as.factor(Natsal$ethnic2))
ONS.name <- droplevels(as.factor(Natsal$`Numerical classification`))
gor.name <- droplevels(as.factor(Natsal$gor))
conception.name <- droplevels(as.factor(Natsal$Conception.decile))
gorLondon <- Natsal$gor==9
IMD.upperQ <- Natsal$`Average Score`>28

IMD.upperQ[is.na(IMD.upperQ)] <- FALSE  ##TODO## fix original join (herefordshire, hull, peterborough, stoke on trent)
conception.name[is.na(conception.name)] <- "[33.1,35.8)"  ##TODO## fix original join (city of london, cornwall, hackney, southend-on-sea)

## create look-up table for values only in sample
LA_factor_lookup <- data.frame(`LA.Name` = levels(LA_factor.name),
                               `la_factor` = sort(unique(as.numeric(LA_factor.name))))
LA_factor_lookup <- merge(LA_factor_lookup, laregionlookup2013, by.x="LA.Name", by.y="la_name", all.x=TRUE)     #remove LAs not in sample
LA_factor_lookup <- merge(LA_factor_lookup, LAclassification.dat[,c("LA Name", "Classification", "Numerical classification")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)
LA_factor_lookup <- merge(LA_factor_lookup, IMD.dat2010[,c("LA Name", "Average Score")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)
LA_factor_lookup <- merge(LA_factor_lookup, conception.under18[,c("Name", "Conception.decile")],
                          by.x = "LA.Name", by.y = "Name", all.x = TRUE)
LA_factor_lookup <- within(LA_factor_lookup, london <- as.numeric(gor==9))
LA_factor_lookup$IMD.upperQ <- LA_factor_lookup$"Average Score">28

LA_factor_lookup$metcounty_UA <- toupper(LA_factor_lookup$metcounty_UA) #move to prep file...

LA_factor_lookup$"Numerical classification" <- droplevels(as.factor(LA_factor_lookup$"Numerical classification"))
LA_factor_lookup$metcounty_UA <- droplevels(as.factor(LA_factor_lookup$metcounty_UA))
LA_factor_lookup$gor <- droplevels(as.factor(LA_factor_lookup$gor))
LA_factor_lookup$Conception.decile <- droplevels(as.factor(LA_factor_lookup$Conception.decile))
LA_factor_lookup$london <- droplevels(as.factor(LA_factor_lookup$london))

## match factor levels in Natsal and lookup
LA_factor.name <- factor(LA_factor.name, levels = levels(LA_factor_lookup$LA.Name))
metUA_factor.name <- factor(metUA_factor.name, levels = levels(LA_factor_lookup$metcounty_UA))
ONS.name <- factor(ONS.name, levels = levels(LA_factor_lookup$`Numerical classification`))
# conception.name <- factor(conception.name, levels = levels(LA_factor_lookup$Conception.decile))

WinBUGSdata <- data.frame(cttestly = Natsal$cttestly,
                          student = as.numeric(Natsal$student),
                          male = as.numeric(Natsal$rsex)-1,
                          dage = Natsal$dage,
                          age = Natsal$dage-min(Natsal$dage)+1,
                          ethngrp = as.numeric(ethnic2.name),
                          IMD.upperQ = as.numeric(IMD.upperQ),
                          ONSclass = as.numeric(ONS.name),
                          smokenow = as.numeric(Natsal$smokenow),
                          conception = as.numeric(conception.name)
)

Nage <- length(table(WinBUGSdata$age))
Nethgrp <- length(table(WinBUGSdata$ethngrp))
NONSclass <- length(table(WinBUGSdata$ONSclass))
Nconception <- length(table(WinBUGSdata$conception))


###########
## model ##
###########

# setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")
setwd("../../WinBUGS/temp_WinBUGS_output")

sink("model.txt")
cat("
    model
    {

    # Hyperpriors
    tau.age ~ dgamma(0.001, 0.001)
    tau.ethngrp ~ dgamma(0.001, 0.001)
    tau.ons ~ dgamma(0.001, 0.001)
    tau.conception ~ dgamma(0.001, 0.001)

    # Priors
    alpha ~ dunif(-2,1)
    b.male ~ dunif(-2,1)
    b.IMD ~ dunif(-2,1)
    b.student ~ dunif(-2,1)

    # Likelihood
    for(i in 1:n) {

    cttestly[i] ~ dbin(p[i], N)
    logit(p[i]) <- alpha + b.male*male[i] +
                    b.student*student[i] + b.IMD*IMD.upperQ[i] +
                    v[age[i]] + w[ethgrp[i]] +
                    u[ONSclass[i]] + d[conception[i]]

    }
    # age
    for(k in 1:K) {
        v[k] ~ dnorm(0, tau.age)
    }
    # ethnic group
    for(l in 1:L) {
        w[l] ~ dnorm(0, tau.ethngrp)
    }
    # ONS urban-rural classification
    for(j in 1:J) {
        u[j] ~ dnorm(0, tau.ons)
    }
    # <=18 years old conception rate
    for(e in 1:E) {
        d[e] ~ dnorm(0, tau.conception)
    }

    }
    ", fill=TRUE)
sink()


########################
## WinBUGS input data ##
########################

win.data <- list(male = WinBUGSdata$male,
                 age = WinBUGSdata$age,
                 student = WinBUGSdata$student,
                 ethgrp = WinBUGSdata$ethngrp,
                 ONSclass = WinBUGSdata$ONSclass,
                 conception = WinBUGSdata$conception,
                 IMD.upperQ = WinBUGSdata$IMD.upperQ,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata),
                 K = Nage,
                 L = Nethgrp, J = NONSclass, E = Nconception,
                 N = 1)

# Initial values function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
    list(alpha=-1, b.male=-1, b.student=0, b.IMD=0,
         tau.age=0.1, tau.ethngrp=2, tau.ons=0.4, tau.conception=0.1),
    list(alpha=-1, b.male=-1, b.student=0, b.IMD=0,
         tau.age=0.2, tau.ethngrp=3, tau.ons=0.1, tau.conception=0.1),
    list(alpha=-1, b.male=-1, b.student=0, b.IMD=0,
         tau.age=0.5, tau.ethngrp=1, tau.ons=0.2, tau.conception=0.1)
)

# Parameters to estimate
params <- c("alpha", "b.male", "b.student", "b.IMD",
            "tau.age", "tau.ethngrp", "tau.ons", "tau.conception",
            "v", "w", "u", "d")

# MCMC settings
nc <- 3         #Number of chains
ni <- 100000    #Number of draws from posterior
nb <- 200      #Number of draws to discard as burn-in
nt <- 250     #Thinning rate

bugsDir <- 'C:/Program Files/WinBUGS14'
#bugsDir <- 'C:/Program Files (x86)/WinBUGS14'
# bugsDir <- 'C:/Users/nathan.green.PHE/Documents/WinBUGS14'

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(),
            bugs.directory = bugsDir)

save(out, file="MCMCoutput.RData")

outmcmc <- as.mcmc.list(out)


##################
## clean output ##
##################

ethngrp.names <- tolower(levels(ethnic2.name))  #w
age.names <- as.character(sort(unique(WinBUGSdata$dage))) #v
UrbRur.names <- as.character(levels(ONS.name))  #u
conception.names <- as.character(levels(conception.name))  #d

## read results back in
coda1.file <- "../temp_WinBUGS_output/coda1.txt"
coda2.file <- "../temp_WinBUGS_output/coda2.txt"
coda3.file <- "../temp_WinBUGS_output/coda3.txt"
coda1 <- read.delim(coda1.file, header=FALSE)
codaIndex <- read.delim("../temp_WinBUGS_output/codaIndex.txt", header=FALSE)
outmat <- data.frame(matrix(coda1$V2, nrow = min(which(coda1$V1==max(coda1$V1)))))
names(outmat) <- codaIndex$V1

## replace parameter names with proper labels
names(outmat)[grepl("w\\[.*\\]", names(outmat))] <- ethngrp.names
names(outmat)[grepl("v\\[.*\\]", names(outmat))] <- age.names
names(outmat)[grepl("u\\[.*\\]", names(outmat))] <- UrbRur.names
names(outmat)[grepl("u\\[.*\\]", names(outmat))] <- conception.names


save(outmat, file="outmat.RData")

outmcmc <- read.bugs(c(coda1.file, coda2.file, coda3.file))

###########
## plots ##
###########
## mcmc object

par(mar=c(5,15,4,2))
caterplot(outmcmc, lwd=c(1,4), style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=1.2,
          parms = c("w", "u", "b.male", "b.student"),
          labels = c(ethngrp.names, UrbRur.names, "male", "student"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")


## LAs only
par(mar=c(5,5,4,2))
caterplot(outmcmc, lwd=c(1,2), style = "plain", col="black", pch=19, reorder=TRUE, axes=F, labels = NA,
          parms = c("r"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## age only
par(mar=c(5,10,4,2))
caterplot(outmcmc, style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=0.8,
          parms = c("v"),
          labels = age.names)
axis(1); axis(3)
abline(v=0, lty=2, col="red")

mcmcplot(outmcmc)


