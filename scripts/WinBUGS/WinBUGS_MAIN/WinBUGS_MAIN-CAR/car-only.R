# run R as __administrator__


rm(list = ls())

library(R2WinBUGS)
wd <- getwd()

load("../../../mrp/data/adjacency_matrix_england-list.RData")

island.names <- names(adjacency_matrix.england$adj[adjacency_matrix.england$adj==0])
adjacency_matrix.england$num[island.names] <- 0
adjacency_matrix.england$adj <- adjacency_matrix.england$adj[adjacency_matrix.england$adj!=0]
adjacency_matrix.england$sumNumNeigh <- sum(adjacency_matrix.england$num)

###########
## model ##
###########

setwd("../../../WinBUGS/temp_WinBUGS_output")

sink("model.txt")
cat("
   model
    {
    
    # tau.car ~ dgamma(0.01, 0.001)

    sd ~ dunif(0,5)
    tau.car <- pow(sd,-2)

    # neighbour contribution
    for (nn in 1:sumNumNeigh) {
      weights[nn] <- 1
    }
    # spatially structured errors
    b.car[1:326] ~ car.normal(adj[], weights[], num[], tau.car)
    
    }
    ", fill=TRUE)
sink()


########################
## WinBUGS input data ##
########################

win.data <- list(sumNumNeigh = adjacency_matrix.england$sumNumNeigh,
                 adj = adjacency_matrix.england$adj,
                 num = adjacency_matrix.england$num)

# inits <- list(
#   list(tau.car=0.001),
#   list(tau.car=0.001),
#   list(tau.car=0.001))
# 
# # Parameters to estimate (output)
# params <- c("tau.car")

inits <- list(
  list(sd=1),
  list(sd=2),
  list(sd=3))

inits.car <- rep(0, adjacency_matrix.england$sumNumNeigh)

# Parameters to estimate (output)
params <- c("sd")

# MCMC settings
nc <- 3 #Number of Chains
ni <- 100000 #Number of draws from posterior
nb <- 2#00 #Number of draws to discard as burn-in
nt <- 25#0 #Thinning rate

bugsDir <- 'C:/Program Files (x86)/WinBUGS14'
# bugsDir <- 'C:/Users/nathan.green.PHE/Documents/WinBUGS14'

## Start Gibbs sampling

# out <- bugs(data="england_lad_2011-Neighbour-list-in-BUGS-format.txt", inits=inits, parameters.to.save=params,
out <- bugs(data=win.data,
            inits=inits,
            parameters.to.save=params,
            model.file="model.txt", n.thin=nt, n.chains=nc, n.burnin=nb,
            n.iter=ni, debug = TRUE, DIC = FALSE,
            working.directory = getwd(),
            bugs.directory = bugsDir)

setwd(wd)

