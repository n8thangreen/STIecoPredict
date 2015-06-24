# ##
# ## simulate LA populations
# ## as weighted random samples
# ## for use in NATSAL classifier
# ## locallevel_pop_sim.R
# ##
# ## N Green
# ## April 2014
#
#
# library(plyr, quietly = TRUE)
# library(class, quietly = TRUE)
#
#
# ##TODO##
# ## parallelise simulation across cores
# # library(doMC) #loads multicore and foreach
# # library(snowfall)
# # library('parallel')
#
#
#
#
# #####################
# ## load input data ##
# #####################
#
# # source("./sim_config.R")
# load("./R_analyses/Chlamydia_classifier/sim_logistic_regn/workspaces/pop_sim_input.RData")
#
# NATSAL.dat <- read.csv("C:/Users/nathan.green/Documents/Chlamydia/data/NATSAL/Natsal-3_extract_2April2014.csv")
#
#
#
# ###################
# ## preprocessing ##
# ###################
#
# if(!exists("ageRange")){ageRange <- 16:24}
#
# ## number of individuals per LA
# n <- 10000
#
#
# ## simulate a sample, for each LA
# ## using the reference tables
# ind <- res <- list()
#
# ## match LA names in LA-level and national data sets
# # laNames <- sort(unique(popLAagesex.dat$Name[popLAagesex.dat$country=="E"]))
# laNames <- smokingLA.dat$Name[smokingLA.dat$Name%in%popLAagesex.dat$Name]
# # laNames <- sort(unique(popLAagesex.dat$lad2012_code[popLAagesex.dat$country=="E"]))
#
# # la <- "Tower Hamlets"
#
# ## match Natsal-3 and reference table ethnic group names
# NATSALethnic.names <- toupper(levels(NATSAL.dat$ethnic)[levels(NATSAL.dat$ethnic)!="Not answered"])
#
# ## ONS table ethnic groups
# # unlist(lapply(strsplit(unique(
# #   unlist(lapply(strsplit(names(ethnicityLA.dat[["male"]]), "Ethnic Group: "), function(x) x[2]))), ";"), function(x) x[1]))
#
#
# ## ONS age group lookup table
# ONSagegrps <- c("15", "16 to 17", "18 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39")
# names(ONSagegrps) <- c("(14,15]", "(15,17]", "(17,19]", "(19,24]", "(24,29]", "(29,34]", "(34,39]")
#
#
# ## income group relabelling lookup table
# NATSALincome.names <- c("<2,500","2,500-4,999","5,000-9,999","10,000-19,999","20,000-29,999","30,000-39,999","40,000-49,999","50,000+")
# names(NATSALincome.names) <- c("[-1000,2500)","[2500,5000)","[5000,10000)","[10000,,20000)","[20000,30000)","[30000,40000)","[40000,50000)","[50000,1e+06)")
#
#
# ## set consistent random seed
# set.seed(1968)
#
#
#
# ################
# ## simulation ##
# ################
#
# # sink("simlog.Rout")   #log file
#
#
# for (la in laNames){
#
#   print(la)
#   print(paste(which(la==laNames), "of", length(laNames)))
#
#   ## stratified population
#   ## by la and age
#   subpop <- (popLAagesex.dat$Name==la) & (popLAagesex.dat$Age%in%ageRange)
#
#
#   ## relative LA-level prevalence adjustments
#   ## ----------------------------------------
#   ## NB >=18 proxy for all ages of interest
#
#
#   adj.smoke  <- with(smokingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="England"])
#   adj.drink  <- with(drinkingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="England"])
#   adj.income <- lapply(incomeLA.dat, function(x) x$Mean[x$LA_Name==la]/x$Mean[x$LA_Name=="England"])  #Men, Women
#
#   ## if data not available
#   adj.smoke  <- fillEmpty(adj.smoke)
#   adj.drink  <- fillEmpty(adj.drink)
#   adj.income <- fillEmpty(adj.income)
#
#
#   ## sample AGE & SEX
#   ## ----------------
#   popsample <- sample(1:sum(subpop), size=n, prob=popLAagesex.dat[subpop, "2012"], replace=TRUE)
#   age <- popLAagesex.dat[subpop,"Age"][popsample]
#   sex <- popLAagesex.dat[subpop,"Sex"][popsample]
#
#   ## change from factor
#   age.char <- as.character(age)
#   sex.char <- as.character(sex)
#
#
#   rv.drink <- runif(n)
#
#   for (i in 1:n){
#
#     ## sample SMOKING status
#     ## ---------------------
#     psmoke <- adj.smoke * smokingage.dat[smokingage.dat$Age==age.char[i] &
#                                            smokingage.dat$Sex==sex.char[i], "2010"]/100
#
#     ## assign smoking status for England-wide age groups
#     smoke <- sample(c("YES", "NO"), 1, prob=c(psmoke,1-psmoke))
#
#     ## sample DRINKING status
#     ## ----------------------
#     pdrink <- adj.drink * drinkingage.dat[drinkingage.dat$Age==age.char[i] &
# #                                             drinkingage.dat$Freq=="one or more" &
#                                             drinkingage.dat$Units%in%c("more than 4 per day", "more than 3 per day") &
#                                             drinkingage.dat$Sex==sex.char[i], "2011"]/100
#
#     ## assign drinking status for England-wide age groups
#     drink <- rv.drink[i]<=pdrink
#
#     ## sample ETHNICITY
#     ## ----------------
#     ## convert age to age group
#     agegrp <- cut(age[i], breaks=c(14,15,17,19,24,29,34,39), right=TRUE)
#     agegrp <- as.character(ONSagegrps[agegrp])
#
#     ## sample ethnicity from new table
#     sset <- ethnicityLA.melt[[sex[i] ]][ethnicityLA.melt[[sex[i] ]]$LA_Names==la &
#                                         ethnicityLA.melt[[sex[i] ]]$age==agegrp,]
#     pethnic <- sset$value
# #     nethnic <- sset$NATSAL  #original ethnic groups
#     nethnic <- sset$ethnic2   #amalgamated ethnic groups
#
#     ethnic <- sample(nethnic, 1, prob=pethnic)
#
#
#
#     ## sample INCOME
#     ## -------------
# #     income <- adj.income[[sex[i] ]] * incomeage.dat$median.income[incomeage.dat$Age==age.char[i] &
#     income <- adj.income[[sex[i] ]] * incomeage.dat$mean.income[incomeage.dat$Age==age.char[i] &
#                                                                   incomeage.dat$Sex==sex.char[i]]
#
# ##TODO## could add error to the estimate...
# #     income <- sample(levels(NATSAL.dat$income), 1, prob=)
#
#     ## convert to income interval and consistent format
#     incomegrp <- cut(income, breaks=c(-1000,2500,5000,10000,20000,30000,40000,50000,1e+06), right=FALSE)
#     incomegrp <- as.character(NATSALincome.names[incomegrp])
#
#
#
#     ind[[i]] <- data.frame(dage=max(ageRange)-age[i],
#                            rsex=sex[i],
#                            smokenow=smoke,
#                            increasingdrinker=drink,
#                            ethnic=ethnic,
#                            income=incomegrp)
#   }
#
#   res[[as.character(la)]] <- ind
#
# }
# # unlink("simlog.Rout")
#
# save(res, file="./data/output/LAsim/LAsim_submodel_TEMP.RData")
#
#
#
#
#
