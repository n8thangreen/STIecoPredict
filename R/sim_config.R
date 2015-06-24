# ##
# ## configuration file for input data
# ## for the simulate LA populations
# ## as weighted random samples
# ## for use in NATSAL classifier
# ## sim_config.R
# ##
# ## N Green
# ## July 2014
#
#
# library(plyr, quietly = TRUE)
# library(stringr)
# library(reshape2)
#
#
# setwd(".\\data")
#
# ethnicgroup_lookup <- read.csv("./ONSNatsal_ethgrp_mapping.csv", check.names=FALSE)
#
# ## ethnic group mapping to fewer, amalgamated groups
# Natsalethgrp.mapping <- read.csv("C:/Users/nathan.green/Documents/chlamydia/classifier/data/Natsal_fewerethgrps_mapping.csv")
#
#
# ## different area groupings look-up table
# area.lookup <- read.csv(".\\lookup_data_area.csv")
#
#
# ## LA population, by age & sex
# ## ---------------------------
# ## 2001-2012
# popLAagesex.dat <- read.csv(".\\ONS_popn_age&sex\\pop_age_sex_LA.csv", check.names=FALSE)
#
#
#
#
# ## smoking
# ## -------
# ### LA only prevalence
# ###
# smokingLA.dat <- read.csv(".\\risk_factors\\smoking\\smoking_LA.csv")
# ### age-group only prevalence
# ### 2010
# smokingage.dat <- read.csv(".\\risk_factors\\smoking\\smoking_agegrp.csv", check.names=FALSE)
# smokingLA.dat$Name <- LAnameClean(smokingLA.dat$Name)
#
#
#
#
#
# ## drinking
# ## --------
# ### LA only prevalence
# ### 2008-2009, >16 yr olds
# drinkingLA.dat <- read.csv(".\\risk_factors\\drinking\\increasingandhigherriskdrinking_LA.csv")
# drinkingLA.dat$Name <- LAnameClean(drinkingLA.dat$Name)
# ### age-group only prevalence
# ### 2011
# drinkingage.dat <- read.csv(".\\risk_factors\\drinking\\drinking_agegrp_freq.csv", check.names=FALSE)    #deprecated in favour of below
# drinkingage.dat <- read.csv(".\\risk_factors\\drinking\\drinking_agegrp_units.csv", check.names=FALSE)
#
#
#
#
#
# ## income
# ## ------
# ## 2011-2012
# #   incomeregion.dat <- read.csv(".\\risk_factors\\income\\income_by_regions.csv")  #deprecated
# #   read.csv("./risk_factors/income/income_countymedian_2011.csv")
# incomeage.dat <- read.csv(".\\risk_factors\\income\\income_by_age_sex.csv")
# # ASHE (ONS) 2011
# incomeLA.dat <- list()
# incomeLA.dat[["Men"]]   <- read.csv("./risk_factors/income/incomeLA_male_2011.csv", colClasses=c("Median"="integer", "Mean"="integer"))
# incomeLA.dat[["Women"]] <- read.csv("./risk_factors/income/incomeLA_female_2011.csv", colClasses=c("Median"="numeric", "Mean"="numeric"))
#
# incomeLA.dat[["Men"]]$LA_Name   <- LAnameClean(str_trim(incomeLA.dat[["Men"]]$LA_Name))
# incomeLA.dat[["Women"]]$LA_Name <- LAnameClean(str_trim(incomeLA.dat[["Women"]]$LA_Name))
#
#
#
#
# ## ethnicity
# ## ---------
# ## Census 2011
# # ethnicityLA.dat <- read.csv("./risk_factors/ethnicity/la_ethgrp_pop.csv", check.names=FALSE)   #deprecated
# # ethnicityLA.dat$Area <- LAnameClean(ethnicityLA.dat$Area)
# ethnicityLA.dat <- list()
# ethnicityLA.dat[["Men"]]   <- read.csv("./risk_factors/ethnicity/census2011_LA_ethgrp_agegrp_male.csv", check.names=FALSE)
# ethnicityLA.dat[["Women"]] <- read.csv("./risk_factors/ethnicity/census2011_LA_ethgrp_agegrp_male.csv", check.names=FALSE)
#
#
#
#
# ###############
# ## rearrange ##
# ###############
#
# ##TODO##
# ## tidy this up
# ##lapply? function?
#
# ethnicityLA.melt <- list()
# ethnicityLA.melt[["Men"]]   <- melt(ethnicityLA.dat[["Men"]])
# ethnicityLA.melt[["Women"]] <- melt(ethnicityLA.dat[["Women"]])
#
# ethnicityLA.melt[["Men"]]  <- data.frame(ethnicityLA.melt[["Men"]][,names(ethnicityLA.melt[["Men"]] )!="variable"],
#                                          colsplit(as.character(ethnicityLA.melt[["Men"]]$variable), ";", c("age","ethngrp")))
# ethnicityLA.melt[["Men"]]$age <- gsub("Age: Age ", "", ethnicityLA.melt[["Men"]]$age)
# ethnicityLA.melt[["Men"]]$ethngrp <- gsub("Ethnic Group: ", "", ethnicityLA.melt[["Men"]]$ethngrp)
# ethnicityLA.melt[["Men"]]$ethngrp <- gsub("; measures: Value", "", ethnicityLA.melt[["Men"]]$ethngrp)
# ethnicityLA.melt[["Men"]]$ethngrp <- str_trim(ethnicityLA.melt[["Men"]]$ethngrp)
# ethnicityLA.melt[["Men"]]  <- merge(ethnicityLA.melt[["Men"]] , ethnicgroup_lookup, by.x="ethngrp", by.y="ONS", all=F)
# ethnicityLA.melt[["Men"]]  <- merge(ethnicityLA.melt[["Men"]] , Natsalethgrp.mapping, by.x="NATSAL", by.y="ethnic", all.x=T, all.y=F)
#
#
#
# ethnicityLA.melt[["Women"]]  <- data.frame(ethnicityLA.melt[["Women"]][,names(ethnicityLA.melt[["Women"]] )!="variable"],
#                                            colsplit(as.character(ethnicityLA.melt[["Women"]]$variable), ";", c("age","ethngrp")))
# ethnicityLA.melt[["Women"]]$age <- gsub("Age: Age ", "", ethnicityLA.melt[["Women"]]$age)
# ethnicityLA.melt[["Women"]]$ethngrp <- gsub("Ethnic Group: ", "", ethnicityLA.melt[["Women"]]$ethngrp)
# ethnicityLA.melt[["Women"]]$ethngrp <- gsub("; measures: Value", "", ethnicityLA.melt[["Women"]]$ethngrp)
# ethnicityLA.melt[["Women"]]$ethngrp <- str_trim(ethnicityLA.melt[["Women"]]$ethngrp)
# ethnicityLA.melt[["Women"]]  <- merge(ethnicityLA.melt[["Women"]] , ethnicgroup_lookup, by.x="ethngrp", by.y="ONS", all=F)
# ethnicityLA.melt[["Women"]]  <- merge(ethnicityLA.melt[["Women"]] , Natsalethgrp.mapping, by.x="NATSAL", by.y="ethnic", all.x=T, all.y=F)
#
# # save(ethnicityLA.melt, file="./risk_factors/ethnicity/ethnicityLA_melt.RData")
# # load(file="./data/risk_factors/ethnicity/ethnicityLA_melt.RData")
#
#
# setwd("..\\")
#
# rm(list=lsf.str())    #remove functions only
# save.image("./R_analyses/Chlamydia_classifier/sim_logistic_regn/workspaces/pop_sim_input.RData")
#
#
