# http://data.princeton.edu/pop510/hospBUGS.html
# run R as administrator


library(MASS)
library(R2WinBUGS)
library(plyr)

load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

Natsal$gor_factor <- as.factor(Natsal$gor)
Natsal$gor_factor <- as.numeric(Natsal$gor_factor)

WinBUGSdata <- data.frame(cttestly=Natsal$cttestly,
                          gor_factor=Natsal$gor_factor,
                          male=as.numeric(Natsal$rsex)-1,
                          student=as.numeric(Natsal$student),
                          smokenow=as.numeric(Natsal$smokenow),
                          dage=Natsal$dage,
                          ethnic2=as.numeric(Natsal$ethnic2),
                          white=as.numeric(Natsal$ethnic2=="WHITE"),
                          age16=as.numeric(Natsal$dage==16),
                          age17=as.numeric(Natsal$dage==17),
                          age18=as.numeric(Natsal$dage==18),
                          age19to24=as.numeric(Natsal$dage%in%19:24),
                          age25to29=as.numeric(Natsal$dage%in%25:29),
                          age30to35=as.numeric(Natsal$dage%in%30:35),
                          age36to44=as.numeric(Natsal$dage%in%36:44),
                          age45plus=as.numeric(Natsal$dage%in%45:100))

setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")

sink("model.txt")
cat("
    model
    {

    # Priors
    alpha ~ dnorm(0,1)
    b.male ~ dnorm(0,0.01)
    b.age16 ~ dnorm(0,1)
    b.age17 ~ dnorm(0,1)
    b.age18 ~ dnorm(0,1)
    b.age19to24 ~ dnorm(0,1)
    b.age25to29 ~ dnorm(0,1)
    b.age30to35 ~ dnorm(0,1)
    b.age36to44 ~ dnorm(0,1)
    b.age45plus ~ dnorm(0,1)
    b.white ~ dnorm(0,0.01)
    b.student ~ dnorm(0,0.01)

    # Hyperprior
    tau ~ dgamma(0.001, 0.001)

    # Likelihood
    for(i in 1:n) {

        cttestly[i] ~ dbin(p[i], N)
        logit(p[i]) <- alpha + b.male*male[i] +
                        b.age16*age16[i] + b.age17*age17[i] + b.age18*age18[i] + b.age19to24*age19to24[i] +
                        b.age25to29*age25to29[i] + b.age30to35*age30to35[i] + b.age36to44*age36to44[i] + b.age45plus*age45plus[i] +
                        b.white*white[i] + b.student*student[i] + u[gor_factor[i]]
        }
    # Regions
    for(j in 1:m) {
        u[j] ~ dnorm(0, tau)
        }

    }
    ", fill=TRUE)
sink()

# Bundle data
win.data <- list(male = WinBUGSdata$male,
                 age16 = WinBUGSdata$age16,
                 age17 = WinBUGSdata$age17,
                 age18 = WinBUGSdata$age18,
                 age19to24 = WinBUGSdata$age19to24,
                 age25to29 = WinBUGSdata$age25to29,
                 age30to35 = WinBUGSdata$age30to35,
                 age36to44 = WinBUGSdata$age36to44,
                 age45plus = WinBUGSdata$age45plus,
                 white = WinBUGSdata$white,
                 student = WinBUGSdata$student,
                 gor_factor = WinBUGSdata$gor_factor,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata), m = 9, N = 1)

# Inits function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
    list(alpha=1, b.male=1, b.age16=1, b.age17=1, b.age18=1, b.age19to24=1, b.age25to29=1, b.age30to35=1, b.age36to44=1, b.age45plus=1, b.white=1, b.student=1, tau=0.01),
    list(alpha=1, b.male=2, b.age16=1, b.age17=1, b.age18=1, b.age19to24=1, b.age25to29=1, b.age30to35=1, b.age36to44=1, b.age45plus=1, b.white=2, b.student=2, tau=0.02),
    list(alpha=1, b.male=3, b.age16=1, b.age17=1, b.age18=1, b.age19to24=1, b.age25to29=1, b.age30to35=1, b.age36to44=1, b.age45plus=1, b.white=3, b.student=3, tau=0.05)
)

# Parameters to estimate
params <- c("alpha", "b.male", "b.age16", "b.age17", "b.age18", "b.age19to24", "b.age25to29", "b.age30to35", "b.age36to44", "b.age45plus", "b.white", "b.student", "tau")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 15000 #Number of draws from posterior
nb <- 400 #Number of draws to discard as burn-in
nt <- 20 #Thinning rate

# Start Gibbs sampling
out <- bugs(data=win.data, inits=inits, parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(), bugs.directory = 'C:/Program Files/WinBUGS14')

save(out, file="MCMCoutput.RData")

# outmcmc <- read.bugs(out)
#
# summary(outmcmc)
# traceplot(outmcmc)
# densityplot(outmcmc)
# mcmcplot(out)

library(mcmcplots)
par(mar=c(5,6,4,2))
caterplot(out, lwd=c(1,4), style = "plain", col="black", pch=19,
          parms = params <- c("alpha", "b.male", "b.age16", "b.age17", "b.age18", "b.age19to24", "b.age25to29", "b.age30to35",
                              "b.age36to44", "b.age45plus", "b.white", "b.student"))
abline(v=0, lty=2)


