
library(MASS)
library(R2WinBUGS)
library(plyr)
library(lattice)
library(mcmcplots)

load("../../scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")


###############
## prep data ##
###############

Natsal0 <- Natsal

if(!exists("ss")){
    ss <- sample(1:nrow(Natsal), prob=Natsal$total_wt, replace=TRUE)
    Natsal <- Natsal[ss,]}

Natsal <- subset(Natsal, age>=15 & age<45)   #boost sampled range

LA_factor.name <- droplevels(as.factor(Natsal$laname))
ethnic2.name <- droplevels(as.factor(Natsal$ethnic2))
ONS.name <- droplevels(as.factor(Natsal$`Numerical classification`))
gorLondon <- Natsal$gor==9

LA_factor_lookup <- data.frame(`LA.Name`=levels(LA_factor.name), numeric=sort(unique(as.numeric(LA_factor.name))))
UrbRur <- merge(LA_factor_lookup, LAclassification.dat[,c("LA Name", "Numerical classification")],
                by.x="LA.Name", by.y="LA Name", all.x=TRUE)[,"Numerical classification"]

UrbRur.name <- droplevels(as.factor(UrbRur))

London_lookup <- data.frame(`LA Name`=LA_factor.name, london=as.numeric(gorLondon))
London_lookup <- London_lookup[!duplicated(London_lookup$`LA.Name`),]
LAlondon <- London_lookup[,2]

WinBUGSdata <- data.frame(cttestly = Natsal$cttestly,
                          la_factor = as.numeric(LA_factor.name),
                          ONSclass = as.numeric(ONS.name),
                          male = as.numeric(Natsal$rsex)-1,
                          student = as.numeric(Natsal$student),
                          smokenow = as.numeric(Natsal$smokenow),
                          dage = Natsal$dage,
                          age = Natsal$dage-min(Natsal$dage)+1,
                          ethngrp = as.numeric(ethnic2.name),
                          gorLondon = as.numeric(gorLondon))

Nage <- length(table(WinBUGSdata$age))
Nla <- length(table(WinBUGSdata$la_factor))
Nethgrp <- length(table(WinBUGSdata$ethngrp))
NUrbRur <- length(table(UrbRur.name))


###########
## model ##
###########

# setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")
setwd("../../scripts/WinBUGS/temp_WinBUGS_output")

sink("model.txt")
cat("
    model
    {

    # Hyperpriors
    tau.age ~ dgamma(0.001, 0.001)
    tau.ethngrp ~ dgamma(0.001, 0.001)
    tau.la ~ dgamma(0.001, 0.001)
    tau.ons ~ dgamma(0.001, 0.001)
    tau.b ~ dgamma(0.001, 0.001)

    # Priors
    alpha ~ dunif(-2,1)
    b.male ~ dunif(-2,1)
    b.student ~ dunif(-2,1)

    # Likelihood
    for(i in 1:n) {

    b[i] ~ dnorm(0, tau.b)
    cttestly[i] ~ dbin(p[i], N)
    logit(p[i]) <- alpha + b.male*male[i] + b.student*student[i] +
    v[age[i]] + w[ethgrp[i]] + r[la_factor[i]] + b[i]
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
    r[s] ~ dnorm(r.hat[s], tau.la)
    r.hat[s] <- u[UrbRur[s]]
    }
    # ONS urban-rural classification
    for(j in 1:J) {
    u[j] ~ dnorm(0, tau.ons)
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
                 UrbRur = as.numeric(UrbRur.name),
                 la_factor = WinBUGSdata$la_factor,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata), K = Nage, L = Nethgrp, S = Nla, J = NUrbRur, N = 1)

# Initial values function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.1, tau.ethngrp=2, tau.la=1, tau.ons=0.4, tau.b=1),
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.1, tau.ethngrp=3, tau.la=1, tau.ons=0.1, tau.b=1),
    list(alpha=-1, b.male=-1, b.student=0, tau.age=0.5, tau.ethngrp=1, tau.la=1, tau.ons=0.2, tau.b=1)
)

# Parameters to estimate
params <- c("alpha", "b.male", "b.student", "tau.age", "tau.ethngrp", "tau.la", "tau.ons", "tau.b", "v", "w", "r", "u")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 1000 #Number of draws from posterior
nb <- 20 #Number of draws to discard as burn-in
nt <- 100 #Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(),
            # bugs.directory = 'C:/Program Files/WinBUGS14')
            bugs.directory = 'C:/Users/nathan.green.PHE/Documents/WinBUGS14')

