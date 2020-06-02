#
# http://data.princeton.edu/pop510/hospBUGS.html
# run R as __administrator__


rm(list = ls())

library(MASS)
library(plyr)
library(lattice)
library(Hmisc)

load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")
# load("../../mrp/data/cleaned-regn-input-mrpNatsal.RData")


###############
## prep data ##
###############
# convert all factors to integer from 1 to max
# and then map this back to original name at the end

MIN_AGE <- 15
MAX_AGE <- 25

## Natsal-3 ##
Natsal0 <- Natsal
Natsal  <- subset(Natsal, age >= MIN_AGE & age < MAX_AGE)   #NCSP range
Natsal  <- subset(Natsal, sam1yr == 0) #opposite sex partner in last year only

Natsal$sex1yr <- (Natsal$het1yr != 0 | Natsal$sam1yr != 0) #why?

IMD_quintile_5 <- quantile(IMD.dat2010$`Average Score`, probs = 0.8)

## for consistent cuts across arrays
## use post-stratification array as baseline

interiorcuts <- cut2(sim_prop_la$Conception.rate.per.1.000.women.in.age.group,
                     g = 10, onlycuts = TRUE)

conception.under18$Conception.decile <- cut2(conception.under18$Conception.rate.per.1.000.women.in.age.group,
                                             cuts = interiorcuts)


# convert all factors to integer ------------------------------------------

# from 1 to max
# and then map this back to original name at the end

LA_factor.name <- droplevels(as.factor(Natsal$laname))
metUA_factor.name <- droplevels(as.factor(Natsal$metcounty_UA))
ethnic2.name <- droplevels(as.factor(Natsal$ethnic2))
ONS.name <- droplevels(as.factor(Natsal$`Numerical classification`))
gor.name <- droplevels(as.factor(Natsal$gor))
conception.name <- droplevels(as.factor(Natsal$Conception.decile))

gorLondon <- Natsal$gor == 9
IMD.upperQ <- Natsal$`Average Score` > IMD_quintile_5

IMD.upperQ[is.na(IMD.upperQ)] <- FALSE  ##TODO## fix original join (herefordshire, hull, peterborough, stoke on trent)

# sub mid-range
conception.name[is.na(conception.name)] <- "[33.0,35.8)"  ##TODO## fix original join (city of london, cornwall, hackney, southend-on-sea)


##########################
## create look-up table ##
##########################

# for values only in sample
# this is useful to map back to the original names from which the integers are based

LA_factor_lookup <- data.frame(`LA.Name` = levels(LA_factor.name),
                               `la_factor` = sort(unique(as.numeric(LA_factor.name))))

LA_factor_lookup <- merge(LA_factor_lookup, laregionlookup2013,
                          by.x = "LA.Name", by.y = "la_name", all.x = TRUE)     #removes LAs not in sample

LA_factor_lookup <- merge(LA_factor_lookup, LAclassification.dat[,c("LA Name", "Classification", "Numerical classification")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)

LA_factor_lookup <- merge(LA_factor_lookup, IMD.dat2010[,c("LA Name", "Average Score")],
                          by.x = "LA.Name", by.y = "LA Name", all.x = TRUE)

LA_factor_lookup <- merge(LA_factor_lookup, conception.under18[,c("Name", "Conception.decile")],
                          by.x = "LA.Name", by.y = "Name", all.x = TRUE)

LA_factor_lookup <- within(LA_factor_lookup,
                           london <- as.numeric(gor == 9))

LA_factor_lookup$IMD.upperQ <- LA_factor_lookup$"Average Score" > IMD_quintile_5

LA_factor_lookup$metcounty_UA.factor <- as.numeric(LA_factor_lookup$metcounty_UA)

LA_factor_lookup$metcounty_UA <- toupper(LA_factor_lookup$metcounty_UA) #move to prep file...

LA_factor_lookup$"Numerical classification" <- droplevels(as.factor(LA_factor_lookup$"Numerical classification"))
LA_factor_lookup$metcounty_UA <- droplevels(as.factor(LA_factor_lookup$metcounty_UA))
LA_factor_lookup$gor <- droplevels(as.factor(LA_factor_lookup$gor))
LA_factor_lookup$Conception.decile <- droplevels(as.factor(LA_factor_lookup$Conception.decile))
LA_factor_lookup$london <- droplevels(as.factor(LA_factor_lookup$london))



# match factor levels in Natsal and lookup --------------------------------

LA_factor.name <- factor(LA_factor.name, levels = levels(LA_factor_lookup$LA.Name))
metUA_factor.name <- factor(metUA_factor.name, levels = levels(LA_factor_lookup$metcounty_UA))
ONS.name <- factor(ONS.name, levels = levels(LA_factor_lookup$`Numerical classification`))
# conception.name <- factor(conception.name, levels = levels(LA_factor_lookup$Conception.decile))



# output data -------------------------------------------------------------

Natsal_multistate_data <- data.frame(cttestly = Natsal$cttestly,
                                     student = as.numeric(Natsal$student),
                                     male = as.numeric(Natsal$rsex) - 1, #1-male, 0-female
                                     dage = Natsal$dage,
                                     age = Natsal$dage - min(Natsal$dage) + 1,
                                     ethngrp = as.numeric(ethnic2.name),
                                     IMDupperQ = as.numeric(IMD.upperQ),
                                     gor = as.numeric(gor.name),
                                     metcounty_UA = as.numeric(metUA_factor.name),
                                     gorLondon = as.numeric(gorLondon),
                                     ONSclass = as.numeric(ONS.name),
                                     la_factor = as.numeric(LA_factor.name),
                                     smokenow = as.numeric(Natsal$smokenow),
                                     conception = as.numeric(conception.name)
)

LA_multistate_data <- 
  Natsal_multistate_data[!duplicated(la_factor), ] %>% 
  dplyr::select(la_factor, IMDupperQ, student, conception, gor) %>% 
  arrange(la_factor)


save(Natsal_multistate_data,
     file = "C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/stan/Natsal-multistate-logistic-data.RData")



# exploratory plots -------------------------------------------------------

plot(table(age, cttestly))
plot(table(male, cttestly))
plot(table(ethngrp, cttestly))


