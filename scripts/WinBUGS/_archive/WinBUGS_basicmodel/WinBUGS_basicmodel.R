# http://data.princeton.edu/pop510/hospBUGS.html

library(MASS)
library(R2WinBUGS)
library(plyr)

load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

Natsal$gor_factor <- as.factor(Natsal$gor)
Natsal$gor_factor <- as.numeric(Natsal$gor_factor)

WinBUGSdata <- data.frame(cttestly=Natsal$cttestly, gor_factor=Natsal$gor_factor, rsex=as.numeric(Natsal$rsex)-1)
# WinBUGSdata <- rbind(WinBUGSdata, c(NA,NA,NA))

setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")

sink("model.txt")
cat("
    model
    {

    # Priors
    alpha ~ dnorm(0,0.005)
    b.sex ~ dnorm(0,0.005)

    # Hyperprior
    tau ~ dgamma(0.001, 0.001)

    # Likelihood
    for(i in 1:n) {
        cttestly[i] ~ dbin(p[i], N)
        logit(p[i]) <- alpha + b.sex*rsex[i] + u[gor_factor[i]]
    }
    # Regions
    for(j in 1:m) {
        u[j] ~ dnorm(0, tau)
    }

    }
    ", fill=TRUE)
sink()

# Bundle data
win.data <- list(rsex = WinBUGSdata$rsex,
                 gor_factor = WinBUGSdata$gor_factor,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata), m = 9, N = 1)

# Inits function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.sex=rlnorm(1))}
inits <- list(
    list(alpha=0, b.sex=1, tau=0.5),
    list(alpha=0, b.sex=1, tau=0.5),
    list(alpha=0, b.sex=1, tau=0.5))

# Parameters to estimate
params <- c("alpha", "b.sex", "tau")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 1000 #Number of draws from posterior
nb <- 200 #Number of draws to discard as burn-in
nt <- 5 #Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE,
            DIC = TRUE, working.directory = getwd(), bugs.directory = 'C:/Program Files/WinBUGS14')
outmcmc <- read.bugs(out)

summary(outmcmc)
traceplot(outmcmc)
densityplot(outmcmc)

library(mcmcplots)
caterplot(out, lwd=c(1,4), style = "plain", col="black", pch=19, parms = c("b.age","b.sex"))



