
fit <- glmer(formula = cttestly ~ (1|sex)+(1|age), data = Natsal, family = binomial(link="logit"))

display(fit)
ranef(fit)$sex
se.ranef(fit)$sex
ranef(fit)$age
se.ranef(fit)$age


#  using joint dataset --------------------------------------------------------

pred <- invlogit(fixef(fit)["(Intercept)"] +
                 ranef(fit)$age[as.character(sim_prop_la.agesex$age),1] +
                 ranef(fit)$sex[as.character(sim_prop_la.agesex$sex),1])

predweighted <- pred * sim_prop_la.agesex$p.agesex
LApred <- tapply(predweighted, sim_prop_la.agesex$LAname, sum)
as.data.frame(LApred[order(LApred)])


#  using raw age-sex datsset --------------------------------------------------

pred <- invlogit(fixef(fit)["(Intercept)"] +
                 ranef(fit)$age[as.character(LApop$age),1] +
                 ranef(fit)$sex[as.character(LApop$sex),1])

predweighted <- pred * LApop$pop.adj
LApred2 <- tapply(predweighted, LApop$Name, sum)
as.data.frame(LApred2[order(LApred2)])


