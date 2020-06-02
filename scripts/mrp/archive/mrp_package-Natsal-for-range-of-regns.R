##
## Natsal chlamydia regression and post-stratification
## following Gelman and MRP approach using mrp package
##


library(mrp)
# library(mrpdata)
library(stats)
library(plyr)


#############
## fitting ##
#############

## intercept for each sex, age group
## because theres no LA identifier in the Natsal data we can just predict each LA separately

out <- c(NULL,NULL)

for (la in unique(LApop$Name)){
  
  mrp.simple <- mrp(cttestly ~ sex+age,
                    data=Natsal,
                    population=LApop[LApop$Name==la,],
                    pop.weights="pop")
  
  out <- rbind(out, c(la, round(poststratify(object=mrp.simple), digits=6)))
}
# print(100*poststratify(mrp.simple, ~ age+sex), digits=2)
# xtable::xtable(100*poststratify(mrp.simple, ~ age+sex), digits=2)

out[order(out[,2]),]
    
# save(out, file="C:\\Users\\ngreen1\\Dropbox\\small-area & chlamydia\\R_code\\scripts\\mrp\\data\\out_sexage.RData")
load("C:/Users/nathan.green/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/out_sexage.RData")

##TODO##
#why doesnt this work??
#should be faster than above
# mrp.simple <- mrp(cttestly ~ sex+age,
#                   data=Natsal,
#                   pop.weights="pop")
# 
# for (la in unique(LApop$Name)){
# 
#   out <- rbind(out, c(la, round(poststratify(object=mrp.simple, population=LApop[LApop$Name==la,c("sex","age","pop")]), digits=2)))
# }


### without group level predictors

## baseline regression

out <- c(NULL,NULL)

for (la in unique(sim_prop_la$LAname)){
  
  mrp.simple <- mrp(cttestly ~ sex+age+ethnic2+smokenow, #+increasingdrinker,
                    data=Natsal,
                    population=sim_prop_la[sim_prop_la$LAname==la,],
                    pop.weights="totalprob")
  
  out <- rbind(out, c(la, round(poststratify(object=mrp.simple), digits=6)))
}

save(out, file="C:\\Users\\ngreen1\\Dropbox\\small-area & chlamydia\\R_code\\scripts\\mrp\\out_sexageethgrpsmoke.RData")

## paired interactions

## geographic interaction
### just a gors indicators included


### with group level predictors
### la|region with urban/rural and student percentages



## no individual level variables (area only)




