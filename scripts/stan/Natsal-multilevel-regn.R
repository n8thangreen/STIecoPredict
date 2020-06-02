
library(rstan)
library(shinystan)


setwd("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/stan/")

load("Natsal-multistate-logistic-data.RData")
attach(Natsal_multistate_data)


y <- cttestly
N <- length(y)
n.age <- max(age)
n.la <- max(la_factor)
n.ethngrp <- max(ethngrp)
#n.ONSclass <- max(ONSclass)
n.metcounty_UA <- max(metcounty_UA)
n.conception <- max(conception)
n.gor <- max(gor)

# model
dataList.1 <- list(N = N,
                   n_age = n.age,
                   n_la = n.la,
                   n_ethngrp = n.ethngrp,
                   #n_ONSclass = n.ONSclass, 
                   #n_metcounty_UA = n.metcounty_UA,
                   # n_conception = n.conception,
                   # n_gor = n.gor,
                   # la = la_factor,                             #local authority
                   age = age,                                  #age by year
                   # student = LA_multistate_data$student,       #student in full-time education
                   male = male,                                #male gender
                   # IMDupperQ = LA_multistate_data$IMDupperQ,   #index of multiple deprivation upper quintile
                   ethngrp = ethngrp,                          #ethnic group
                   #ONSclass = ONSclass,
                   #metcounty_UA = metcounty_UA,
                   # conception = LA_multistate_data$conception, #decile of conception rate per 1000 women
                   # gor = LA_multistate_data$gor,               #government office region
                   y = y)

multilevel_logistic <- stan(file = 'Natsal-multilevel-regn.stan',
                            data = dataList.1,
                            iter = 1000,
                            chains = 4,
                            thin = 1,
                            control = list(adapt_delta = 0.99))

print(multilevel_logistic)

pairs(multilevel_logistic, pars = c("b_age[1]", "b_age[2]", "b_age[3]", "b_age[4]", "b_age[5]", "b_age[6]", "b_age[7]", "b_age[8]", "b_age[9]"))
pairs(multilevel_logistic, pars = c("b_ethngrp[1]","b_ethngrp[2]","b_ethngrp[3]","b_ethngrp[4]","b_ethngrp[5]","b_ethngrp[6]"))

my_shinystan <- as.shinystan(multilevel_logistic)  ## check the posteriors and parameters converge
launch_shinystan(my_shinystan)
