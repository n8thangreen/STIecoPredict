# http://data.princeton.edu/pop510/hospBUGS.html
# run R as administrator


library(MASS)
library(R2WinBUGS)
library(plyr)

load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

# Natsal <- subset(Natsal, age>15 & age<25)   #NCSP range

Natsal$gor_factor <- droplevels(as.factor(Natsal$gor))
Natsal$gor_factor <- as.numeric(Natsal$gor_factor)

Natsal$LA_factor <- droplevels(as.factor(Natsal$laname))
Natsal$LA_factor <- as.numeric(Natsal$LA_factor)


WinBUGSdata <- data.frame(cttestly = Natsal$cttestly,
                          la_factor = Natsal$LA_factor,
                          gor_factor = Natsal$gor_factor,
                          male = as.numeric(Natsal$rsex)-1,
                          student = as.numeric(Natsal$student),
                          smokenow = as.numeric(Natsal$smokenow),
                          dage = Natsal$dage,
                          age = Natsal$dage-min(Natsal$dage)+1,
                          ethngrp = as.numeric(Natsal$ethnic2))

## make a vector of lookup gor in for loop
x <- WinBUGSdata[ ,c("la_factor", "gor_factor")]
y <- x[!duplicated(x),]
gor_factor <- (y[order(y$la_factor), "gor_factor"])

Nla  <- length(gor_factor)
Nage <- length(table(WinBUGSdata$age))

setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")

sink("model.txt")
cat("
    model
    {

    # Priors
    alpha ~ dnorm(0,1)
    b.male ~ dnorm(0,0.01)
    b.student ~ dnorm(0,0.01)

    # Hyperprior
    tau.gor ~ dgamma(0.01, 0.01)
    tau.age ~ dgamma(0.001, 0.001)
    tau.ethngrp ~ dgamma(0.001, 0.001)
    tau.la ~ dgamma(0.01, 0.01)

    # Likelihood
    for(i in 1:n) {

    cttestly[i] ~ dbin(p[i], N)
    logit(p[i]) <- alpha + b.male*male[i] + b.student*student[i] +
                    v[age[i]] + w[ethgrp[i]] + r[la_factor[i]]
    }
    # Regions
    for(j in 1:m) {
        u[j] ~ dnorm(0, tau.gor)
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
        r[s] ~ dnorm(u[gor_factor[s]], tau.la)
    }

    }
    ", fill=TRUE)
sink()

# Bundle data
win.data <- list(male = WinBUGSdata$male,
                 age = WinBUGSdata$age,
                 student = WinBUGSdata$student,
                 ethgrp = WinBUGSdata$ethngrp,
                 gor_factor = gor_factor,
                 la_factor = WinBUGSdata$la_factor,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata), m = 9, K = Nage, L = 6, S=Nla, N = 1)

# Inits function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
    list(alpha=1, b.male=1, b.student=1, tau.gor=0.01, tau.age=0.01, tau.ethngrp=0.01, tau.la=0.01),
    list(alpha=1, b.male=2, b.student=2, tau.gor=0.02, tau.age=0.02, tau.ethngrp=0.02, tau.la=0.02),
    list(alpha=1, b.male=3, b.student=3, tau.gor=0.05, tau.age=0.03, tau.ethngrp=0.03, tau.la=0.03)
)

# Parameters to estimate
params <- c("alpha", "b.male", "b.student", "tau.gor", "tau.age", "tau.ethngrp", "tau.la", "u", "v", "w", "r")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 10000 #Number of draws from posterior
nb <- 1000 #Number of draws to discard as burn-in
nt <- 40 #Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(), bugs.directory = 'C:/Program Files/WinBUGS14')

save(out, file="MCMCoutput.RData")

outmcmc <- as.mcmc.list(out)

library(lattice)

# outmcmc <- read.bugs(out)
#
# summary(outmcmc)
# traceplot(outmcmc)
# densplot(outmcmc)
xyplot(outmcmc)
densityplot(outmcmc)
# mcmcplot(out)

ethngrp.names <- tolower(levels(Natsal$ethnic2)[-7])    #levels(Natsal$ethnic2)[sort(unique(as.numeric(Natsal$ethnic2)))]
age.names <- as.character(16:24)
gor.names <- c("North East", #1
               "North West", #2
               "Yorkshire and The Humber", #4
               "East Midlands", #5
               "West Midlands", #6
               "South West", #7
               "East", #8
               "London", #9
               "South East") #10

library(mcmcplots)
par(mar=c(5,15,4,2))
caterplot(out, lwd=c(1,4), style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=1.2,
          # parms = params <- c("alpha", "b.male", "b.student"), denstrip = TRUE)
          parms = params <- c("w", "v", "u", "b.male", "b.student"),
          labels = c(ethngrp.names, age.names, gor.names, "male", "student"))
axis(1); axis(3)

abline(v=0, lty=2)
