
# checks and test

#  age-sex only population data --------------------------------------------------

sapply(unique(sim_prop_la$LAname)[1:30], function(name) sum(sim_prop_la[sim_prop_la$LAname == name,"p.agesex"]))

sim_prop_la.agesex <-  sim_prop_la[!duplicated(sim_prop_la[,c("sex", "age", "LAname")]),]   #remove repeated (age-sex) probabilities

##test that probabilities add up to 1 within each LA
sapply(unique(sim_prop_la.agesex$LAname)[1:30], function(name) sum(sim_prop_la.agesex[sim_prop_la.agesex$LAname == name,"p.agesex"]))

all(sort(unique(Natsal$age)) - sort(unique(LApop$age)) == FALSE)    #same age ranges

union(unique(Natsal$sex), unique(LApop$sex))    #same gender

sapply(unique(LApop$Name)[1:30], function(name) sum(LApop[LApop$Name == name,"pop"])) #LA population totals