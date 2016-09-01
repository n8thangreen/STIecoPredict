
sim_prop <- locallevel_pop_props()
##TODO## LA~age+sex+smokenow)
head(sim_prop[[1]], 100)

sim1 <- sim_prop[[1]]

## sum across all ethnic groups (should be 1)
tapply(sim1$p.ethnic2, list(sim1$dage, sim1$rsex), sum)


