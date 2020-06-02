# http://data.princeton.edu/pop510/hospBUGS.html
# run R as __administrator__


rm(list = ls())

library(MASS)
library(R2WinBUGS)
library(plyr)
library(lattice)
library(mcmcplots)
library(Hmisc)
library(STIecoPredict)

wd <- getwd()

load("C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")
load("C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/adjacency_matrix_england-list.RData")


###############
## prep data ##
###############

## Natsal-3 ##
Natsal0 <- Natsal
Natsal  <- subset(Natsal, age >= 15 & age < 25)   #NCSP range
Natsal  <- subset(Natsal, sam1yr == 0) # opposite sex partner in last year only
Natsal$sex1yr <- (Natsal$het1yr != 0 | Natsal$sam1yr != 0)


## clean area with no neighbours
island.names <- names(adjacency_matrix.england$adj[adjacency_matrix.england$adj == 0])
adjacency_matrix.england$num[island.names] <- 0
adjacency_matrix.england$adj <- adjacency_matrix.england$adj[adjacency_matrix.england$adj != 0]
adjacency_matrix.england$sumNumNeigh <- sum(adjacency_matrix.england$num)

allLAnames <- names(adjacency_matrix.england$num)
allLAnames <- STIecoPredict:::LAnameClean(allLAnames)

allLAnames.withNeighbours <- allLAnames[!allLAnames %in% island.names]
allLAnames.withNeighbours <- STIecoPredict:::LAnameClean(allLAnames.withNeighbours)

adjacency_matrix.england$num <- unname(adjacency_matrix.england$num)
adjacency_matrix.england$adj <- unname(adjacency_matrix.england$adj)

laregionlookup2013$metcounty_UA <- toupper(laregionlookup2013$metcounty_UA)
laregionlookup2013.england <- merge(data.frame(la_name = allLAnames), laregionlookup2013, sort = FALSE)

# convert all factors to integer from 1 to max
# and then map this back to original name at the end

## for consistent cuts across arrays
## use post-stratification array as baseline
interiorcuts <- cut2(sim_prop_la$Conception.rate.per.1.000.women.in.age.group, g = 10, onlycuts = T)
conception.under18$Conception.decile <- cut2(conception.under18$Conception.rate.per.1.000.women.in.age.group, cuts = interiorcuts)

# convert all factors to integer from 1 to max
# and then map this back to original name at the end
LA_factor.name <- droplevels(as.factor(Natsal$laname))
metUA_factor.name <- droplevels(as.factor(Natsal$metcounty_UA))
ethnic2.name <- droplevels(as.factor(Natsal$ethnic2))
ONS.name <- droplevels(as.factor(Natsal$`Numerical classification`))
gor.name <- droplevels(as.factor(Natsal$gor))
conception.name <- droplevels(as.factor(Natsal$Conception.decile))
gorLondon <- Natsal$gor == 9
IMD.upperQ <- Natsal$`Average Score` > 28


## fillin missing values
IMD.upperQ[is.na(IMD.upperQ)] <- FALSE  ##TODO## fix original join (herefordshire, hull, peterborough, stoke on trent)
conception.name[is.na(conception.name)] <- levels(conception.name)[6] #"[33.1,35.8)"  ##TODO## fix original join (city of london, cornwall, hackney, southend-on-sea)
ONS.name[is.na(ONS.name)] <- 3  ##TODO## fix join

## number (levels) according to full lists
LA_factor.name <- factor(LA_factor.name, levels = allLAnames)
metUA_factor.name <- factor(metUA_factor.name, levels = unique(laregionlookup2013.england$metcounty_UA))


## create look-up table for values only in sample
## this is useful to map back to the original names from which the integers are based

LA_factor_lookup <- data.frame(`LA.Name` = levels(LA_factor.name),
                               `la_factor` = seq_along(levels(LA_factor.name)))
LA_factor_lookup <- merge(LA_factor_lookup, laregionlookup2013, by.x = "LA.Name", by.y = "la_name", all.x = TRUE)     #removes LAs not in sample
LA_factor_lookup <- merge(LA_factor_lookup, LAclassification.dat[ ,c("LA Name", "Classification", "Numerical classification")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)
LA_factor_lookup <- merge(LA_factor_lookup, IMD.dat2010[ ,c("LA Name", "Average Score")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)
LA_factor_lookup <- merge(LA_factor_lookup, conception.under18[ ,c("Name", "Conception.decile")],
                          by.x = "LA.Name", by.y = "Name", all.x = TRUE)
LA_factor_lookup <- within(LA_factor_lookup, london <- as.numeric(gor == 9))
LA_factor_lookup$IMD.upperQ <- LA_factor_lookup$"Average Score" > 28

LA_factor_lookup$"Numerical classification" <- droplevels(as.factor(LA_factor_lookup$"Numerical classification"))
LA_factor_lookup$metcounty_UA <- droplevels(as.factor(LA_factor_lookup$metcounty_UA))
LA_factor_lookup$gor <- droplevels(as.factor(LA_factor_lookup$gor))
LA_factor_lookup$Conception.decile <- droplevels(as.factor(LA_factor_lookup$Conception.decile))
LA_factor_lookup$london <- droplevels(as.factor(LA_factor_lookup$london))
LA_factor_lookup$metcounty_UA.factor <- as.numeric(LA_factor_lookup$metcounty_UA)


########
## match factor levels in Natsal and lookup
########
##TODO## do we need this??

# LA_factor.name <- factor(LA_factor.name, levels = levels(LA_factor_lookup$LA.Name))
# metUA_factor.name <- factor(metUA_factor.name, levels = levels(LA_factor_lookup$metcounty_UA))
# ONS.name <- factor(ONS.name, levels = levels(LA_factor_lookup$`Numerical classification`))
# conception.name <- factor(conception.name, levels = levels(LA_factor_lookup$Conception.decile))


WinBUGSdata <- data.frame(cttestly = Natsal$cttestly,
                          student = as.numeric(Natsal$student),
                          male = as.numeric(Natsal$rsex)-1, #1-male, 0-female
                          dage = Natsal$dage,
                          age = Natsal$dage-min(Natsal$dage)+1,
                          ethngrp = as.numeric(ethnic2.name),
                          IMD.upperQ = as.numeric(IMD.upperQ),
                          gor = as.numeric(gor.name),
                          metcounty_UA = as.numeric(metUA_factor.name),
                          gorLondon = as.numeric(gorLondon),
                          ONSclass = as.numeric(ONS.name),
                          la_factor = as.numeric(LA_factor.name),
                          smokenow = as.numeric(Natsal$smokenow),
                          conception = as.numeric(conception.name)
                          )

Nage <- length(table(WinBUGSdata$age))
Nla <- length(table(WinBUGSdata$la_factor))
Nla.full <- length(adjacency_matrix.england$num)
Nethgrp <- length(table(WinBUGSdata$ethngrp))
NONSclass <- length(table(WinBUGSdata$ONSclass))
Nmetcounty_UA <- length(table(WinBUGSdata$metcounty_UA))
Nmetcounty_UA.full <- length(unique(laregionlookup2013.england$metcounty_UA))
Nconception <- length(table(WinBUGSdata$conception))
Ngor <- length(table(WinBUGSdata$gor))


###########
## model ##
###########

# setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts")
setwd("C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/WinBUGS/temp_WinBUGS_output")

sink("model.txt")
cat("
    model
    {

    # Hyperpriors

    sd.car ~ dunif(0,5)
    tau.car <- pow(sd.car,-2)
    # tau.car ~ dgamma(0.001, 0.001)

    tau.age ~ dgamma(0.001, 0.001)
    tau.ethngrp ~ dgamma(0.001, 0.001)
    tau.la ~ dgamma(0.001, 0.001)
    tau.ons ~ dgamma(0.001, 0.001)
    tau.met ~ dgamma(0.001, 0.001)
    tau.gor ~ dgamma(0.001, 0.001)
    tau.conception ~ dgamma(0.001, 0.001)

    # sd.car <- sd(car[])
    sd.r <- sd(r[])
    lambda <- sd.car/(sd.car+sd.r)


    # Priors

    ##TODO## alpha ~ dflat()		## the 'improper' flat distn as intercept for CAR model?

    alpha ~ dunif(-2,1)
    b.male ~ dunif(-2,1)
    b.IMD ~ dunif(-2,1)
    b.student ~ dunif(-2,1)

    # Likelihood
    for(i in 1:n) {

      cttestly[i] ~ dbin(p[i], N)
      logit(p[i]) <- alpha + b.male*male[i] + b.student*student[i] + b.IMD*IMD.upperQ[i] +
                      v[age[i]] +
                      w[ethgrp[i]] +
                      r[la.factor[i]] +
                      g[metcounty.UA[i]] +
                      h[gor[i]] +
                      u[ONSclass[i]] +
                      d[conception[i]] +
                      car[la.factor[i]]
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
    # met county
    for(m in 1:M) {
      g[m] ~ dnorm(0, tau.met)
    }
    # Region
    for(q in 1:Q) {
      h[q] ~ dnorm(0, tau.gor)
    }
    # ONS urban-rural classification
    for(j in 1:J) {
      u[j] ~ dnorm(0, tau.ons)
    }
    # <=18 years old conception rate
    for(e in 1:E) {
      d[e] ~ dnorm(0, tau.conception)
    }

    # neighbour contribution
    for (nn in 1:sumNumNeigh) {
      weights[nn] <- 1
    }

    # spatially structured errors
    car[1:S] ~ car.normal(adj[], weights[], num[], tau.car)

    }
    ", fill = TRUE)
sink()


########################
## WinBUGS input data ##
########################

win.data <- list(male = WinBUGSdata$male,
                 age = WinBUGSdata$age,
                 student = WinBUGSdata$student,
                 ethgrp = WinBUGSdata$ethngrp,
                 ONSclass = WinBUGSdata$ONSclass,
                 la.factor = WinBUGSdata$la_factor,
                 metcounty.UA = WinBUGSdata$metcounty_UA,
                 gor = WinBUGSdata$gor,
                 conception = WinBUGSdata$conception,
                 IMD.upperQ = WinBUGSdata$IMD.upperQ,
                 cttestly = WinBUGSdata$cttestly,
                 n = nrow(WinBUGSdata),
                 K = Nage, L = Nethgrp, J = NONSclass, E = Nconception,
                 S = Nla.full,
                 M = Nmetcounty_UA.full,
                 Q = Ngor,
                 sumNumNeigh = adjacency_matrix.england$sumNumNeigh,
                 adj = adjacency_matrix.england$adj,
                 num = adjacency_matrix.england$num,
                 N = 1)

# Initial values function
# inits <- function(){ list(alpha=rlnorm(1), b.age=rlnorm(1), b.male=rlnorm(1))}
inits <- list(
  list(alpha = -1, b.male = -1, b.student = 0, b.IMD = 0,
       tau.age = 0.1, tau.ethngrp = 2, tau.ons = 0.4, tau.conception = 0.1, tau.gor = 0.1, tau.la = 1, tau.met = 0.1,
       sd.car = 3),
  list(alpha = -1, b.male = -1, b.student = 0, b.IMD = 0,
       tau.age = 0.1, tau.ethngrp = 3, tau.ons = 0.1, tau.conception = 0.2, tau.gor = 0.1, tau.la = 2, tau.met = 0.2,
       sd.car = 1),
  list(alpha = -1, b.male = -1, b.student = 0, b.IMD = 0,
       tau.age = 0.5, tau.ethngrp = 1, tau.ons = 0.2, tau.conception = 0.3, tau.gor = 0.1, tau.la = 3, tau.met = 0.3,
       sd.car = 2)
)

inits.car <- rep(0, adjacency_matrix.england$sumNumNeigh)

# Parameters to estimate (output)
params <- c("alpha", "b.male", "b.student", "b.IMD",
            "tau.age", "tau.ethngrp", "tau.ons", "tau.conception", "tau.gor", "tau.la", "tau.met", "tau.car",
            "v", "w", "u", "d", "h", "r", "g", "car", "lambda")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 100000 #Number of draws from posterior
nb <- 200 #Number of draws to discard as burn-in
nt <- 250 #Thinning rate

bugsDir <- 'C:/Program Files (x86)/WinBUGS14'
# bugsDir <- 'C:/Users/nathan.green.PHE/Documents/WinBUGS14'

# Start Gibbs sampling
out <- bugs(data = win.data,
            inits = inits,
            parameters.to.save = params,
            model.file = "model.txt",
            n.thin = nt, n.chains = nc, n.burnin = nb,
            n.iter = ni, debug = TRUE, DIC = TRUE,
            working.directory = getwd(),
            bugs.directory = bugsDir)

save(out, file = "MCMCoutput.RData")

outmcmc <- as.mcmc.list(out)


##################
## clean output ##
##################

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

ethngrp.names <- sapply(levels(ethnic2.name), simpleCap)  #w  #levels(Natsal$ethnic2)[sort(unique(as.numeric(Natsal$ethnic2)))]
age.names <- as.character(sort(unique(WinBUGSdata$dage))) #v
la.names <- sapply(as.character(levels(LA_factor.name)), simpleCap)  #r
gor.names <- as.character(levels(gor.name))  #h
metUA.names <- sapply(as.character(levels(metUA_factor.name)), simpleCap)  #g
UrbRur.names <- as.character(levels(ONS.name))  #u
conception.names <- as.character(levels(conception.name))  #d
allLAnames.withNeighbours.names <- sapply(as.character(allLAnames.withNeighbours), simpleCap)
## note that the ONS classifications are numbers as well as ages but they do not overlap

## recover the original factor values from the level numbers
## if not directly available
gor.names <- as.character(unique(merge(data.frame(gor.names), LA_factor_lookup[,c("gor","region_name")],
                                       by.x="gor.names", by.y="gor", all.x=T, all=F, sort = F))$region_name)
gor.names <- sapply(gor.names, simpleCap)
UrbRur.names <- as.character(unique(merge(data.frame(UrbRur.names), LA_factor_lookup[,c("Classification","Numerical classification")],
                                          by.x="UrbRur.names", by.y="Numerical classification", all.x=T, all=F, sort = F))$Classification)

## read results back in
coda1.file <- "../WinBUGS/WinBUGS_MAIN/WinBUGS_MAIN-all_LA-vars/coda1.txt"
coda2.file <- "../WinBUGS/WinBUGS_MAIN/WinBUGS_MAIN-all_LA-vars/coda2.txt"
coda3.file <- "../WinBUGS/WinBUGS_MAIN/WinBUGS_MAIN-all_LA-vars/coda3.txt"
##or
# coda1.file <- "../temp_WinBUGS_output/coda1.txt"
# coda2.file <- "../temp_WinBUGS_output/coda2.txt"
# coda3.file <- "../temp_WinBUGS_output/coda3.txt"

outmcmc <- read.bugs(c(coda1.file, coda2.file, coda3.file))

## alternative approach
# coda1 <- read.delim(coda1.file, header=FALSE)
# codaIndex <- read.delim("../WinBUGS/WinBUGS_MAIN/WinBUGS_MAIN-all_LA-vars/codaIndex.txt", header=FALSE)
# # codaIndex <- read.delim("../temp_WinBUGS_output/codaIndex.txt", header=FALSE)
# outmat <- data.frame(matrix(coda1$V2, nrow = min(which(coda1$V1==max(coda1$V1)))))
# names(outmat) <- codaIndex$V1

outmat <- plyr::ldply(outmcmc, data.frame)

## put parameters names in increasing order
names(outmat) <- gsub("\\.", "", names(outmat))
library(gtools) #http://stackoverflow.com/questions/17531403/how-to-sort-a-character-vector-where-elements-contain-letters-and-numbers-in-r
outmat <- outmat[, mixedsort(names(outmat))]

## replace parameter names with proper labels
## read.bugs
names(outmat)[grepl("^w[1234567890]+", names(outmat))] <- ethngrp.names
names(outmat)[grepl("^v[1234567890]+", names(outmat))] <- age.names
names(outmat)[grepl("^r[1234567890]+", names(outmat))] <- la.names
names(outmat)[grepl("^g[1234567890]+", names(outmat))] <- paste("met.", metUA.names, sep="")
names(outmat)[grepl("^h[1234567890]+", names(outmat))] <- gor.names
names(outmat)[grepl("^u[1234567890]+", names(outmat))] <- UrbRur.names
names(outmat)[grepl("^d[1234567890]+", names(outmat))] <- conception.names
names(outmat)[grepl("^car[1234567890]+", names(outmat))] <- paste("car.", allLAnames.withNeighbours.names, sep="")

names(outmat) <- gsub("^TAU", "tau.", names(outmat), ignore.case = TRUE)
names(outmat) <- gsub("BIMD", "b.IMD", names(outmat), ignore.case = TRUE)
names(outmat) <- gsub("BMALE", "b.male", names(outmat), ignore.case = TRUE)
names(outmat) <- gsub("BSTUDENT", "b.student", names(outmat), ignore.case = TRUE)


## read.delim
# names(outmat)[grepl("w\\[.*\\]", names(outmat))] <- ethngrp.names
# names(outmat)[grepl("v\\[.*\\]", names(outmat))] <- age.names
# names(outmat)[grepl("r\\[.*\\]", names(outmat))] <- la.names
# names(outmat)[grepl("g\\[.*\\]", names(outmat))] <- metUA.names
# names(outmat)[grepl("h\\[.*\\]", names(outmat))] <- gor.names
# names(outmat)[grepl("u\\[.*\\]", names(outmat))] <- UrbRur.names
# names(outmat)[grepl("d\\[.*\\]", names(outmat))] <- conception.names


save(outmat, file="../mrp/data/outmat.RData")


######################
## diagnostic plots ##
######################
## using mcmc object

par(mar=c(5,15,4,2))
caterplot(outmcmc, lwd=c(1,4), style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=1.2,
          parms = c("w", "b.male", "b.student", "d", "h", "u"),
          labels = c(ethngrp.names, "Male", "Student", conception.names, gor.names, UrbRur.names))
axis(1); axis(3)
abline(v=0, lty=2, col="red")


## LAs only
par(mar=c(5,5,4,2))
caterplot(outmcmc, lwd=c(1,2), style = "plain", col="black", pch=19, reorder=TRUE, axes=F, labels = NA,
          parms = c("r"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## metUA only
par(mar=c(5,5,4,2))
caterplot(outmcmc, lwd=c(1,2), style = "plain", col="black", pch=19, reorder=TRUE, axes=F, labels = NA,
          parms = c("g"))
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## age only
par(mar=c(5,10,4,2))
caterplot(outmcmc, style = "plain", col="black", pch=19, reorder=FALSE, axes=F, cex.labels=0.8,
          parms = c("v"),
          labels = age.names)
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## CAR only
par(mar=c(5,10,4,2))
caterplot(outmcmc, style = "plain", col="black", pch=19, reorder=TRUE, axes=F, cex.labels=0.8,
          parms = "car",
          labels = NA)
axis(1); axis(3)
abline(v=0, lty=2, col="red")

## lambda only
hist(outmat$lambda, breaks=30, xlim=c(0,1), freq = FALSE, xlab="Lambda", main="")
lines(density(outmat$lambda, bw = 0.05), col="red")

mcmcplot(outmcmc)

setwd(wd)


