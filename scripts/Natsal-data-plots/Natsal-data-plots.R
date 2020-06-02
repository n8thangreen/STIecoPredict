

load("../mrp/data/cleaned-regn-input-mrpNatsal.RData")

Natsal0 <- Natsal

rsample <- sample(x=1:nrow(Natsal), prob=Natsal$total_wt, replace=TRUE)
Natsal <- Natsal[rsample,]


attach(Natsal)

# http://stats.stackexchange.com/questions/94026/how-can-i-improve-the-predictive-power-of-this-logistic-regression-model

names(Natsal)

#x11()
# par(mfrow=c(2,1))
par(mar=c(12,4,4,2))
tab <- table(cttestly, ethnicgrp)
barplot(tab, legend=levels((cttestly)), ylim=c(0,2000), beside=TRUE, main="Eethnic group", las=2)
x <- barplot(prop.table(tab,2)*100, xaxt="n")#, legend=levels((cttestly)))
 labs <- paste(colnames(tab), "")
 text(cex=1, x=x-.5, y=-50.5, labs, xpd=TRUE, srt=45)

#x11()
tab <- table(cttestly, smokenow)
barplot(tab, legend=levels((cttestly)), beside=TRUE, main="Smoke now")
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, dage)
barplot(tab, legend=levels((cttestly)), beside=TRUE, main="Age (years)")
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, rsex)
barplot(tab, legend=levels((cttestly)), beside=TRUE, main="Sex")
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, drinkoft)
barplot(tab, legend=levels(cttestly), beside=TRUE, main="Drink often", las=2)
barplot(prop.table(tab,2)*100, las=2)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, drinknum)
barplot(tab, legend=levels(cttestly), beside=TRUE, main="Drink number", las=2)
barplot(prop.table(tab,2)*100, las=2)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, manyalc2, main="How many alcoholic units")
barplot(tab, legend=levels(cttestly), beside=TRUE)
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))

plot(table(drinkoft, manyalc2), main="drink often vs how many alcholic units")

#x11()
tab <- table(cttestly, increasingdrinker)
barplot(tab, legend=levels((cttestly)), beside=TRUE, main="Increasing drinker")
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, rnssecgp_4)
barplot(tab, legend=levels((cttestly)), beside=TRUE, main="Occupation", las=2)
barplot(prop.table(tab,2)*100, las=2)#, legend=levels((cttestly)))

#x11()
tab <- table(cttestly, student)
barplot(tab, legend=levels(cttestly), beside=TRUE, main="Student")
barplot(prop.table(tab,2)*100)#, legend=levels((cttestly)))


#x11()
tab <- table(cttestly, gor)
barplot(tab, legend=levels(cttestly), beside=TRUE, main="Geographic Region")
barplot(prop.table(tab,2)*100, ylim=c(80,100))




#################################################
# conditional on having had sex in the last year
#################################################

#x11()
tab1 <- table(cttestly, rsex)
tab2 <- table(cttestly[het1yr!=0 | sam1yr!=0], rsex[het1yr!=0 | sam1yr!=0])
par(mfrow=c(1,2))
barplot(prop.table(tab1,2)*100)#, legend=levels((cttestly)))
barplot(prop.table(tab2,2)*100)#, legend=levels((cttestly)))


tab1 <- table(cttestly[het1yr!=0 | sam1yr!=0], dage[het1yr!=0 | sam1yr!=0])
tab2 <- table(cttestly, dage)
par(mfrow=c(1,2))
barplot(prop.table(tab1,2)*100)#, legend=levels((cttestly)))
barplot(prop.table(tab2,2)*100)#, legend=levels((cttestly)))


par(mar=c(12,4,4,2))
tab1 <- table(cttestly, ethnicgrp)
tab2 <- table(cttestly[het1yr!=0 | sam1yr!=0], ethnicgrp[het1yr!=0 | sam1yr!=0])
par(mfrow=c(1,2))
x <- barplot(prop.table(tab1,2)*100, xaxt="n")#, legend=levels((cttestly)))
labs <- paste(colnames(tab1), "")
text(cex=1, x=x-.5, y=-50.5, labs, xpd=TRUE, srt=45)
x <- barplot(prop.table(tab2,2)*100, xaxt="n")#, legend=levels((cttestly)))
labs <- paste(colnames(tab2), "")
text(cex=1, x=x-.5, y=-50.5, labs, xpd=TRUE, srt=45)











detach(Natsal)





