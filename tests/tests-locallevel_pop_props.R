
tests.sex <- function(ageRange){

    ##labeling consistent
    stopifnot(c("Men", "Women")%in%smokingage.dat$Sex==c(TRUE,TRUE))
    stopifnot(sum(ageRange%in%smokingage.dat$Age)==length(ageRange))

    stopifnot(c("Men", "Women")%in%drinkingage.dat$Sex==c(TRUE,TRUE))
    stopifnot(sum(ageRange%in%drinkingage.dat$Age)==length(ageRange))

    # stopifnot(c("Men", "Women")%in%incomeage.dat$Sex==c(TRUE,TRUE))
    # stopifnot(sum(ageRange%in%incomeage.dat$Age)==length(ageRange))

    stopifnot(c("Men", "Women")%in%names(ethnicityLA.melt)==c(TRUE,TRUE))
    # stopifnot(sum(agegrp%in%ethnicityLA.melt$Men$age)==length(agegrp))

    stopifnot(c("Men", "Women")%in%studentCensus$Sex==c(TRUE,TRUE))
}

test.calcTotalProbs <- function(SIM){

    totalprob <- calcTotalProbs(formula=as.formula(y~age+sex), data=SIM, extracols = c("LAname","gor"))
    stopifnot(all(tapply(totalprob$totalprob, totalprob$LAname, sum)==1))

    totalprob <- calcTotalProbs(formula=as.formula(y~age+sex+smokenow), data=SIM, extracols = c("LAname","gor"))
    stopifnot(all(tapply(totalprob$totalprob, totalprob$LAname, sum)>0.999 & tapply(totalprob$totalprob, totalprob$LAname, sum)<1.001))

    totalprob <- calcTotalProbs(formula=as.formula(y~age+sex+increasingdrinker), data=SIM, extracols = c("LAname","gor"))
    stopifnot(all(tapply(totalprob$totalprob, totalprob$LAname, sum)>0.999 & tapply(totalprob$totalprob, totalprob$LAname, sum)<1.001))

    totalprob <- calcTotalProbs(formula=as.formula(y~age+sex+ethnic2), data=SIM, extracols = c("LAname","gor"))
    stopifnot(all(tapply(totalprob$totalprob, totalprob$LAname, sum)>0.999 & tapply(totalprob$totalprob, totalprob$LAname, sum)<1.001))

    totalprob <- calcTotalProbs(formula=as.formula(y~age+sex+smokenow+increasingdrinker+ethnic2), data=SIM, extracols = c("LAname","gor"))
    stopifnot(all(tapply(totalprob$totalprob, totalprob$LAname, sum)>0.999 & tapply(totalprob$totalprob, totalprob$LAname, sum)<1.001))
}


aggregate(temp$p.ethnic2, by=list(temp$student, temp$sex, temp$age), sum)
