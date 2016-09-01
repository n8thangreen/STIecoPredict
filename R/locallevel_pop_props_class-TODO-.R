

#' Estimate Local Level Population Proportions
#'
#' Combined LA level data for single risk factors and non-LA but multiple risk factor data sets.
#' Estimates a (partial-) joint distribution for each LA using the reference tables.
#' This is in contrast to \code{\link{locallevel_pop_sim}} which generates a sample of individuals
#' so is much slower to run.
#' Makes relative (to national average) LA-level prevalence adjustments.
#'
#' Input data is in \code{data(pop_sim_input)} in \code{STIecoPredict} package.
#'
#' @return list by each LA
#'
#' @seealso \code{\link{locallevel_pop_sim}}

locallevel_pop_props.class <- function(input){

    require(plyr, quietly = TRUE)
    require(class, quietly = TRUE)
    require(assertive)

    if(!exists("ageRange")){ageRange <- 16:24}

    res <- list()

    stopifnot(class(input)=="pop_props")

    ##TODO##

#     popLAagesex.dat$Name <- toupper(popLAagesex.dat$Name)
#     smokingLA.dat$Name  <- toupper(smokingLA.dat$Name)
#     drinkingLA.dat$Name <- toupper(drinkingLA.dat$Name)
#     incomeLA.dat$Men$LA_Name <- toupper(incomeLA.dat$Men$LA_Name)
#     incomeLA.dat$Women$LA_Name <- toupper(incomeLA.dat$Women$LA_Name)
#     ethnicityLA.melt$Men$LA_Names <- toupper(ethnicityLA.melt$Men$LA_Names)
#     ethnicityLA.melt$Women$LA_Names <- toupper(ethnicityLA.melt$Women$LA_Names)
#
#     Natsal.dat$ethnicgrp <-  as.factor(toupper(Natsal.dat$ethnicgrp))
#     ethnicityLA.melt$Men$ethnic2 <- as.factor(toupper(ethnicityLA.melt$Men$ethnic2))
#     ethnicityLA.melt$Women$ethnic2 <- as.factor(toupper(ethnicityLA.melt$Women$ethnic2))
#
#     laNames <- smokingLA.dat$Name[smokingLA.dat$Name%in%unique(popLAagesex.dat$Name)]
#     warning("Places dropped because names don't match:",
#             paste(smokingLA.dat$Name[!smokingLA.dat$Name%in%unique(popLAagesex.dat$Name)], collapse = ", "), noBreaks. = FALSE)
#
#     ## match Natsal-3 and reference table ethnic group names
#     NATSALethnic.names <- levels(NATSAL.dat$ethnic)[levels(NATSAL.dat$ethnic)!="NOT ANSWERED"]
#
#     smokingage.dat$Sex  <- relevel(relevel(smokingage.dat$Sex, ref = "Women"), ref = "Men")
#     drinkingage.dat$Sex <- relevel(relevel(drinkingage.dat$Sex, ref = "Women"), ref = "Men")
#     incomeage.dat$Sex <- relevel(relevel(incomeage.dat$Sex, ref = "Women"), ref = "Men")
#     popCensus$Sex <- relevel(relevel(popCensus$Sex, ref = "Women"), ref = "Men")
#     studentCensus$Sex <- relevel(relevel(studentCensus$Sex, ref = "Women"), ref = "Men")
#
#     stopifnot(c("Men", "Women")%in%smokingage.dat$Sex==c(TRUE,TRUE))
#     stopifnot(sum(ageRange%in%smokingage.dat$Age)==length(ageRange))
#
#     stopifnot(c("Men", "Women")%in%drinkingage.dat$Sex==c(TRUE,TRUE))
#     stopifnot(sum(ageRange%in%drinkingage.dat$Age)==length(ageRange))
#
#     stopifnot(c("Men", "Women")%in%incomeage.dat$Sex==c(TRUE,TRUE))
#     stopifnot(sum(ageRange%in%incomeage.dat$Age)==length(ageRange))
#
#     stopifnot(c("Men", "Women")%in%names(ethnicityLA.melt)==c(TRUE,TRUE))
#     stopifnot(sum(agegrp%in%ethnicityLA.melt$Men$age)==length(agegrp))
#
#     stopifnot(c("Men", "Women")%in%studentCensus$Sex==c(TRUE,TRUE))
#
#
#     ethnicityLA.melt$Men$ethnic2 <- revalue(ethnicityLA.melt$Men$ethnic2, c("WHITE BRITISH"="WHITE", "WHITE OTHER"="WHITE"))  #collapse White levels
#     ethnicityLA.melt$Men$ethnic2 <- factor(ethnicityLA.melt$Men$ethnic2, levels=c(levels(ethnicityLA.melt$Men$ethnic2), "NOT ANSWERED"))  #add extra level
#     ethnicityLA.melt$Men$ethnic2 <- relevel(ethnicityLA.melt$Men$ethnic2, ref = "WHITE")  #set White as baseline level
#     Natsal.dat$ethnicgrp <- factor(Natsal.dat$ethnicgrp, levels=levels(ethnicityLA.melt$Men$ethnic2)) #match level order
#     stopifnot(levels(Natsal.dat$ethnicgrp)%in%levels(ethnicityLA.melt$Men$ethnic2))
#
#     ethnicityLA.melt$Women$ethnic2 <- revalue(ethnicityLA.melt$Women$ethnic2, c("WHITE BRITISH"="WHITE", "WHITE OTHER"="WHITE"))  #collapse White levels
#     ethnicityLA.melt$Women$ethnic2 <- factor(ethnicityLA.melt$Women$ethnic2, levels=c(levels(ethnicityLA.melt$Women$ethnic2), "NOT ANSWERED"))  #add extra level
#     ethnicityLA.melt$Women$ethnic2 <- relevel(ethnicityLA.melt$Women$ethnic2, ref = "WHITE")  #set White as baseline level
#     Natsal.dat$ethnicgrp <- factor(Natsal.dat$ethnicgrp, levels=levels(ethnicityLA.melt$Women$ethnic2)) #match level order
#     stopifnot(levels(Natsal.dat$ethnicgrp)%in%levels(ethnicityLA.melt$Women$ethnic2))
#
#     for (la in laNames){
#
#         print(la)
#         print(paste(which(la==laNames), "of", length(laNames)))
#
#         subpop <- (popLAagesex.dat$Name==la) & (popLAagesex.dat$Age%in%ageRange)
#         size.subpop <- sum(subpop)
#
#         adj.smoke  <- with(smokingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
#         adj.drink  <- with(drinkingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
#         adj.income <- lapply(incomeLA.dat, function(x) x$Mean[x$LA_Name==la]/x$Mean[x$LA_Name=="ENGLAND"])  #Men, Women
#
#         ## if data not available
#         adj.smoke  <- fillEmpty(adj.smoke)
#         adj.drink  <- fillEmpty(adj.drink)
#         adj.income <- fillEmpty(adj.income)
#
#         age <- popLAagesex.dat[subpop, "Age"]
#         sex <- popLAagesex.dat[subpop, "Sex"]
#
#         age.char <- as.character(age)
#         sex.char <- as.character(sex)
#
#         agegrp <- convertAge2ageGroup(age)
#
#         psmoke <- subset(smokingage.dat, Age%in%age.char & Sex%in%sex.char)
#         psmoke <- psmoke[order(psmoke$Sex,psmoke$Age), ]
#         psmoke <- adj.smoke * psmoke[,"2010"]/100
#
#         pdrink <- subset(drinkingage.dat, Age%in%age.char &
#                              Units%in%c("more than 4 per day", "more than 3 per day") &
#                              Sex%in%sex.char)
#         pdrink <- pdrink[order(pdrink$Sex,pdrink$Age), ]
#         pdrink <- adj.drink * pdrink[, "2011"]/100
#
#         pincome <- subset(incomeage.dat, Age%in%age.char & Sex%in%sex.char)
#         pincome <- pincome[order(pincome$Sex,pincome$Age), ]
#
#         incomeM <- adj.income$Men * pincome[pincome$Sex=="Men", "mean.income"]
#         incomeW <- adj.income$Women * pincome[pincome$Sex=="Women", "mean.income"]
#
#         pincome <- c(incomeM, incomeW)
#         incomegrp <- convertIncome2incomeGroup(pincome)
#
#         p.agesex <- popLAagesex.dat[subpop,]
#         p.agesex <- p.agesex[order(p.agesex$Sex, p.agesex$Age),"2012"]
#         p.agesex <- p.agesex/sum(p.agesex)
#
#         ethnicityLA <- ldply(ethnicityLA.melt, .id="rsex")
#         ethnicityLA <- ethnicityLA[ethnicityLA$LA_Names==la & ethnicityLA$age%in%agegrp,]
#         ethnicity.totals  <- aggregate(ethnicityLA$value, by=list(ethnicityLA$rsex, ethnicityLA$age), sum)
#
#         ## standardise for given age-sex probability of ethngrp
#         for (i in 1:nrow(ethnicityLA)){
#             whichrow <- ethnicity.totals$Group.1==ethnicityLA$rsex[i] & ethnicity.totals$Group.2==ethnicityLA$age[i]
#             ethnicityLA$p.ethngrp[i] <- ethnicityLA$value[i]/ethnicity.totals$x[whichrow]
#         }
#
#         ethnicityLA <- ethnicityLA[order(ethnicityLA$rsex, ethnicityLA$age), ]
#
#         p.ethngrp.agegrp <- aggregate(p.ethngrp ~ rsex + age + ethnic2, data=ethnicityLA, FUN=sum)  #sum duplicate ethnic2 rows
#
#         is_equal_to(sum(p.ethngrp.agegrp[p.ethngrp.agegrp$rsex=="Men"& p.ethngrp.agegrp$age=="20 to 24", "p.ethngrp"]), 1)
#
#
#         student$p.student <- studentCensus[studentCensus$LAname==la,"value"]/popCensus[popCensus$LAname==la,"value"]
#
#         p.student <- NULL
#         for(i in 1:length(sex.char)){ #subset and order
#             p.student <- c(p.student, student$p.student[student$Sex==sex.char[i] & student$agegrp==agegrp[i]])
#         } #could merge instead?
#
#
#         p.agesex.res <- data.frame(agegrp = agegrp,
#                                    dage = age.char,
#                                    rsex = sex.char,
#                                    p.agesex = p.agesex,
#                                    p.smokenow = psmoke,
#                                    p.increasingdrinker = pdrink,
#                                    income = incomegrp,
#                                    p.student = p.student)
#
#         ## we make the assumption that within each age interval
#         ## the probability of each risk factor is uniform i.e. the same
#         p.agesex.res <- merge(p.agesex.res, p.ethngrp.agegrp[ ,c("rsex","age","ethnic2","p.ethngrp")],
#                               by.x=c("agegrp","rsex"), by.y=c("age", "rsex"), all = TRUE)
#
#         res[[as.character(la)]] <- p.agesex.res
#     }

    invisible(res)
}
