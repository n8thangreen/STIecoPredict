
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
#' @param ageRange Integer sequence of subset of age, which the probability of belonging to will sum to 1.
#'
#' @return list by each LA
#'
#' @seealso \code{\link{locallevel_pop_sim}}
#'
#' @examples
#' data("locallevel_pop_input")
#' sim_prop <- locallevel_pop_props()

locallevel_pop_props <- function(ageRange = 16:24){

    require(plyr, quietly = TRUE)
    require(reshape2)
    require(class, quietly = TRUE)
    require(assertive)

    data("locallevel_pop_input")

    res <- list()

    ## add some asserts here...
    # assert_all_are_equal_to((table(popLAagesex.dat$Name))

    popLAagesex.dat$Name <- toupper(popLAagesex.dat$Name)
    smokingLA.dat$Name  <- toupper(smokingLA.dat$Name)
    drinkingLA.dat$Name <- toupper(drinkingLA.dat$Name)
    householdsize_LA$Name <- toupper(householdsize_LA$Name)
    studentCensus$LAname  <- toupper(studentCensus$LAname)
    ethnicityLA.melt$Men$LA_Names <- toupper(ethnicityLA.melt$Men$LA_Names)
    ethnicityLA.melt$Women$LA_Names <- toupper(ethnicityLA.melt$Women$LA_Names)

    popLAagesex.dat$Name <- LAnameClean(popLAagesex.dat$Name)
    smokingLA.dat$Name  <- LAnameClean(smokingLA.dat$Name)
    drinkingLA.dat$Name <- LAnameClean(drinkingLA.dat$Name)
    householdsize_LA$Name <- LAnameClean(householdsize_LA$Name)
    studentCensus$LAname  <- LAnameClean(studentCensus$LAname)
    ethnicityLA.melt$Men$LA_Names <- LAnameClean(ethnicityLA.melt$Men$LA_Names)
    ethnicityLA.melt$Women$LA_Names <- LAnameClean(ethnicityLA.melt$Women$LA_Names)

    Natsalethnicgrp <- as.factor(toupper(Natsalethnicgrp))
    ethnicityLA.melt$Men$ethnic2 <- as.factor(toupper(ethnicityLA.melt$Men$ethnic2))
    ethnicityLA.melt$Women$ethnic2 <- as.factor(toupper(ethnicityLA.melt$Women$ethnic2))

    ##TODO## exhaustive list?
    laNames <- smokingLA.dat$Name[smokingLA.dat$Name%in%unique(popLAagesex.dat$Name)]

    warning("Places dropped because names don't match: ",
            paste(smokingLA.dat$Name[!smokingLA.dat$Name%in%unique(popLAagesex.dat$Name)], collapse = ", "), noBreaks. = FALSE)
    warning("Places dropped because names don't match: ",
            paste(drinkingLA.dat$Name[!drinkingLA.dat$Name%in%unique(popLAagesex.dat$Name)], collapse = ", "), noBreaks. = FALSE)
    warning("Places dropped because names don't match: ",
            paste(unique(studentCensus$LAname[!studentCensus$LAname%in%unique(popLAagesex.dat$Name)]), collapse = ", "), noBreaks. = FALSE)
    warning("Places dropped because names don't match: ",
            paste(unique(ethnicityLA.melt$Men$LA_Name[!ethnicityLA.melt$Men$LA_Name%in%unique(popLAagesex.dat$Name)]), collapse = ", "), noBreaks. = FALSE)


    smokingage.dat$Sex  <- relevel(relevel(smokingage.dat$Sex, ref = "Women"), ref = "Men")
    livealone_agesex$Sex  <- relevel(relevel(livealone_agesex$Sex, ref = "Women"), ref = "Men")
    drinkingage.dat$Sex <- relevel(relevel(drinkingage.dat$Sex, ref = "Women"), ref = "Men")
    popCensus$Sex <- relevel(relevel(popCensus$Sex, ref = "Women"), ref = "Men")
    studentCensus$Sex <- relevel(relevel(studentCensus$Sex, ref = "Women"), ref = "Men")

    # tests.sex(ageRange)

    ethnicityLA.melt$Men$ethnic2 <- revalue(ethnicityLA.melt$Men$ethnic2, c("WHITE BRITISH"="WHITE", "WHITE OTHER"="WHITE"))  #collapse White levels
    ethnicityLA.melt$Men$ethnic2 <- factor(ethnicityLA.melt$Men$ethnic2, levels=c(levels(ethnicityLA.melt$Men$ethnic2), "NOT ANSWERED"))  #add extra level
    ethnicityLA.melt$Men$ethnic2 <- relevel(ethnicityLA.melt$Men$ethnic2, ref = "WHITE")  #set White as baseline level
    Natsalethnicgrp <- factor(Natsalethnicgrp, levels=levels(ethnicityLA.melt$Men$ethnic2)) #match level order
    stopifnot(levels(Natsalethnicgrp)%in%levels(ethnicityLA.melt$Men$ethnic2))

    ethnicityLA.melt$Women$ethnic2 <- revalue(ethnicityLA.melt$Women$ethnic2, c("WHITE BRITISH"="WHITE", "WHITE OTHER"="WHITE"))  #collapse White levels
    ethnicityLA.melt$Women$ethnic2 <- factor(ethnicityLA.melt$Women$ethnic2, levels=c(levels(ethnicityLA.melt$Women$ethnic2), "NOT ANSWERED"))  #add extra level
    ethnicityLA.melt$Women$ethnic2 <- relevel(ethnicityLA.melt$Women$ethnic2, ref = "WHITE")  #set White as baseline level
    Natsalethnicgrp <- factor(Natsalethnicgrp, levels=levels(ethnicityLA.melt$Women$ethnic2)) #match level order
    stopifnot(levels(Natsalethnicgrp)%in%levels(ethnicityLA.melt$Women$ethnic2))

    ethnicityLA <- ldply(ethnicityLA.melt, .id="rsex")  #long format

    popCensus.long <- melt(popCensus, id.vars = c("LAname","geography.code","Sex"))

    studentCensus$agegrp.check <- make.names(studentCensus$agegrp)
    studentCensus <- merge(studentCensus, popCensus.long, by.x=c("LAname","Sex","agegrp.check"), by.y=c("LAname","Sex","variable"), all.x=TRUE)
    studentCensus <- within(studentCensus, p.student <- value.x/value.y)

    for (la in laNames){

        print(la)
        print(paste(which(la==laNames), "of", length(laNames)))

        subpop <- (popLAagesex.dat$Name==la) & (popLAagesex.dat$Age%in%ageRange)
        size.subpop <- sum(subpop)

        adj.smoke  <- with(smokingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
        adj.drink  <- with(drinkingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
        adj.livealone  <- with(householdsize_LA, One.in.household[Name==la]/mean(One.in.household))

        ## if data not available
        adj.smoke  <- fillEmpty(adj.smoke)
        adj.drink  <- fillEmpty(adj.drink)
        adj.livealone  <- fillEmpty(adj.livealone)

        age <- popLAagesex.dat[subpop, "Age"]
        sex <- popLAagesex.dat[subpop, "Sex"]

        age.char <- as.character(age)
        sex.char <- as.character(sex)

        agegrp  <- convertAge2ageGroup(age)
        agegrp2 <- convertAge2ageGroup2(age)

        psmoke <- subset(smokingage.dat, Age%in%age.char & Sex%in%sex.char)
        psmoke <- psmoke[order(psmoke$Sex,psmoke$Age), ]
        psmoke <- adj.smoke * psmoke[,"2010"]/100

        pdrink <- subset(drinkingage.dat, Age%in%age.char &
                             Units%in%c("more than 4 per day", "more than 3 per day") &
                             Sex%in%sex.char)
        pdrink <- pdrink[order(pdrink$Sex,pdrink$Age), ]
        pdrink <- adj.drink * pdrink[, "2011"]/100

        plivealone <- subset(livealone_agesex, Age%in%age.char & Sex%in%sex.char)
        plivealone <- plivealone[order(plivealone$Sex,plivealone$Age), ]
        plivealone <- adj.livealone * plivealone[,"2011"]/100

        p.agesex <- popLAagesex.dat[subpop,]
        p.agesex <- p.agesex[order(p.agesex$Sex, p.agesex$Age), "2012"]
        p.agesex <- p.agesex/sum(p.agesex)

        ethnicity.la <- ethnicityLA[ethnicityLA$LA_Names==la & ethnicityLA$age%in%agegrp,]
        ethnicity.la <- aggregate(ethnicity.la$value, by=list(ethnicity.la$rsex, ethnicity.la$age, ethnicity.la$ethnic2), sum)
        names(ethnicity.la) <- c("rsex","age","ethnic2","value")
        ethnicity.totals <- aggregate(ethnicity.la$value, by=list(ethnicity.la$rsex, ethnicity.la$age), sum)
        names(ethnicity.totals) <- c("rsex","age","value")

        ## standardise for given age-sex probability of ethngrp
        ethnicity.la$p.ethnic2 <- NA
        for (i in 1:nrow(ethnicity.la)){
            whichrow <- ethnicity.totals$rsex == ethnicity.la$rsex[i] &
                        ethnicity.totals$age == ethnicity.la$age[i]
            ethnicity.la$p.ethnic2[i] <- ethnicity.la$value[i]/ethnicity.totals$value[whichrow]
        }

        ethnicity.la <- ethnicity.la[order(ethnicity.la$rsex, ethnicity.la$age), ]

        is_equal_to(sum(ethnicity.la$p.ethnic2[ethnicity.la$rsex=="Men" & ethnicity.la$age=="20 to 24"]), 1)

        # p.ethnic2.agegrp <- aggregate(p.ethnic2 ~ rsex + age + ethnic2, data=ethnicity.la, FUN=sum)  #sum duplicate ethnic2 rows
        # is_equal_to(sum(p.ethnic2.agegrp[p.ethnic2.agegrp$rsex=="Men"& p.ethnic2.agegrp$age=="20 to 24", "p.ethnic2"]), 1)

        p.student <- NULL
        for(i in seq_along(sex.char)){ #subset and order
            p.student <- c(p.student, studentCensus$p.student[studentCensus$LAname==la &
                                                              studentCensus$Sex==sex.char[i] &
                                                              studentCensus$agegrp==agegrp2[i]])
        } #could merge instead?

        p.agesex.res <- data.frame(agegrp = agegrp,
                                   dage = age.char,
                                   rsex = sex.char,
                                   p.agesex = p.agesex,
                                   p.smokenow = psmoke,
                                   p.increasingdrinker = pdrink,
                                   p.student = p.student,
                                   p.livealone = plivealone)

        ## we make the assumption that within each age interval
        ## the probability of each risk factor is uniform i.e. the same
        p.agesex.res <- merge(p.agesex.res, ethnicity.la[ ,c("rsex","age","ethnic2","p.ethnic2")],
                              by.x=c("agegrp","rsex"), by.y=c("age", "rsex"), all = TRUE)

        res[[as.character(la)]] <- p.agesex.res
    }

    invisible(res)
}


#' Area_adjusted_agesex_probs
#'
#' @param la.dat Area (LA) level data
#' @param agesex.dat Age-sex data
#' @param la.vals Column name in la.dat of probability
#' @param agesex.vals Column name in agesex.dat
#' @param lanames Column name of LA names
#' @param la Single LA name to calculate result
#' @param ages Character vector of ages of interest (repeated for males and females)
#' @param sexes Character vector of sexes (repeated for each ageof interest)
#'
#' @return probs
#'
area_adjusted_agesex_probs <- function(la.dat, agesex.dat, la.vals, agesex.vals, lanames, la, ages, sexes){

    names(dat) <- toupper(names(dat))
    adj  <- with(ladat, vals[laname==la]/vals[lanames=="ENGLAND"])
    adj  <- fillEmpty(adj)
    probs <- agesex.dat[agesex.dat$AGE%in%ages & agesex.dat$SEX%in%sexes, ]
    probs <- probs[order(probs$SEX, probs$AGE), ]
    probs <- adj * probs[ ,agesexvals]/100
    probs
}


#' Aggregate Individual Simulation Data into Categories
#'
#' @param df Individual level risk factor/demographic data for each la (list) from \code{\link{locallevel_pop_sim}}.
#'
#' @return area aggregated list
#'
#' @examples
#'
aggregate_indiv_categories <- function(df = res.df1){

    res.agg <- list()
    for (i in 1:length(res.df1)){
        res.agg[[i]] <- ddply(df, .(df$dage, df$rsex, df$smokenow, df$increasingdrinker), nrow)
    }
    names(res.agg) <- names(res.df1)
    res.agg
}

