
#' Join All Outcome Data
#'
#' Combines area level post-stratified predictions with surveillance data.
#' Take predictions from different models.
#'
#' ##TODO##
#' I've commented-out the logisitic knn predictions and used the MRP one instead
#'
#' @param pred Prediction array
#' @param ...

joinAllOutcomeData <- function(pred, ...){

    require(scales)
    require(ggplot2)
    require(lattice)
    require(plyr)
    require(STIecoPredict)

    #system.file(package="STIecoPredict")

    ########################################################################
    ## load data ###########################################################
    ########################################################################

    popLA.dat <- read.csv(".\\raw-data\\ONS_LA_population_2011.csv", check.names=FALSE)

    ## surveillance data
    ### 2012 16-34 yr olds
    CTADGUM.dat2012 <- read.csv(".\\raw-data\\CTADGUM_data2012_age1634.csv", check.names=FALSE)
    ### 2011 16-24 yr olds
    # CTADGUM.dat <- read.csv(".\\data\\surveillance\\1624ageSurveillanceData2011.csv", check.names=FALSE)
    CTADGUM.dat2011 <- read.csv(".\\raw-data\\Surveillance_data2011_age1524.csv", check.names=FALSE)    #newer version from Ellie

    ## index of multiple deprivation
    IMD.dat2010 <- read.csv(".\\raw-data\\LA_IMD.csv", check.names=FALSE)
    IMD.dat2010$`LA NAME` <- gsub(" District", "", IMD.dat2010$`LA NAME`)

    houseprice_earnings.dat <- read.csv(".\\raw-data\\houseprice_earnings.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")

    LAclassification.dat <- read.csv(".\\raw-data\\LA_classification.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")


    #########################################################################
    ## preprocess ###########################################################
    #########################################################################

    CTADGUM.dat2012$"LA Name" <- toupper(CTADGUM.dat2012$"LA Name")
    CTADGUM.dat2012$"LA Name" <- LAnameClean(CTADGUM.dat2012$"LA Name")
    CTADGUM.dat2011$"LA Name" <- toupper(CTADGUM.dat2011$"LA Name")
    CTADGUM.dat2011$"LA Name" <- LAnameClean(CTADGUM.dat2011$"LA Name")
    popLA.dat$LA <- toupper(popLA.dat$LA)
    popLA.dat$LA <- LAnameClean(popLA.dat$LA)
    houseprice_earnings.dat$"Local authority" <- toupper(houseprice_earnings.dat$"Local authority")
    houseprice_earnings.dat$"Local authority" <- LAnameClean(houseprice_earnings.dat$"Local authority")
    IMD.dat2010$`LA NAME` <- toupper(IMD.dat2010$`LA NAME`)
    IMD.dat2010$`LA NAME` <- LAnameClean(IMD.dat2010$`LA NAME`)
    LAclassification.dat$Name <- toupper(LAclassification.dat$Name)
    LAclassification.dat$Name <- LAnameClean(LAclassification.dat$Name)

    ## non-London LAs
    LAnames.nonLondon <- popLA.dat$LA[popLA.dat$Area!="LONDON"]
    LAnames.nonLondon <- LAnames.nonLondon[LAnames.nonLondon != ""]
    ## London LAs
    LAnames.London <- popLA.dat$LA[popLA.dat$Area=="LONDON"]
    LAnames.London <- LAnames.London[LAnames.London != ""]

    # grep("(pred$)|(pred\\.)", names(pred))
    # if(!is.null(rownames(pred))) cbind(pred, la=rownames(pred))
    # pred.knn <- cbind(pred.knn, la=rownames(pred.knn))
    # pred.log <- cbind(pred.log, la=rownames(pred.log))

    ## this is a fudge because there are some missing areas
    ## add them in as NAs
    # pred.knn <- rbind(pred.knn, data.frame(la=setdiff(CTADGUM.dat2012$`LA Name`, pred.knn$la),Coverage=NA))
    # pred.log <- rbind(pred.log, data.frame(la=setdiff(CTADGUM.dat2012$`LA Name`, pred.log$la),Coverage=NA))

    ###########
    ## joins ##
    ###########

    # pred <- merge(pred.log[,c("la","Coverage")],
    #               pred.knn[,c("la","Coverage")], by="la")

    CTADGUM_pred <- merge(CTADGUM.dat2011[,c("LA Name","Combined Coverage")],
                          CTADGUM.dat2012[,c("LA Name", "Combined Coverage")],
                          by="LA Name", all=TRUE)
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Combined Coverage.x"] <- "surv2011.1524"
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Combined Coverage.y"] <- "surv2012.1634"

    CTADGUM_pred <- merge(CTADGUM_pred, pred, by.x="LA Name", by.y="LAname")
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.x"] <- "pred.log"
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.y"] <- "pred.knn"

    adj <- 0.87 # double counting correction, from Sarah Woodall's paper

    CTADGUM_pred <- within(CTADGUM_pred, {

        ## DEDUPLICATION for repeat tests
        `surv2011.1524` <- `surv2011.1524`*adj
        `surv2012.1634` <- `surv2012.1634`*adj

        ##TODO##
        ##hack to remove outliers. could Winsorize instead...
        # elseif(CTADGUM_pred$surv2012.1634>upperlim, , CTADGUM_pred$surv2012.1634>upperlim)
        # `surv2012.1634`[`LA Name`=="Kettering"] <- NA#0.5221963
        # `surv2012.1634`[`LA Name`=="Dacorum"] <- NA#0.4177031
        # `surv2011.1524`[`LA Name`=="Chesterfield"] <- NA#0.6716591
    })

    ## LA-level statistics for points

    ## density
    CTADGUM_pred <- merge(CTADGUM_pred, popLA.dat[,c("LA","Population","Density")],
                          by.x="LA Name", by.y="LA", all.x=TRUE)

    CTADGUM_pred <- merge(CTADGUM_pred, IMD.dat2010[,c("LA NAME","Average Score")], by.x="LA Name", by.y="LA NAME", all.x=TRUE)
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Average Score"] <- "IMD2010"

    houseprice_earnings.dat$"2010" <- as.numeric(as.character(houseprice_earnings.dat$"2010"))
    CTADGUM_pred <- merge(CTADGUM_pred, houseprice_earnings.dat[,c("Local authority","2010")], by.x="LA Name", by.y="Local authority", all.x=TRUE)
    names(CTADGUM_pred)[names(CTADGUM_pred)=="2010"] <- "housepriceEarnings2010"

    CTADGUM_pred <- merge(CTADGUM_pred, LAclassification.dat[,c("Name","Classification")], by.x="LA Name", by.y="Name", all.x=TRUE)


    ## ordination plots package
    # http://www.fromthebottomoftheheap.net/2013/12/31/decluttering-ordination-in-vegan-part-4-orditkplot/
    # http://blog.fellstat.com/?cat=11

    CTADGUM_pred
}


#' Calculate Output Statistics of MRP Natsal Analysis
#'
#' Calculates the residuals, indirect standardised rates,
#' mean difference, binomial threshold exceedence and quadrant.
#'
#' @param CTADGUM_pred
#'
#' @return

calcStats.CTADGUM_pred <- function(CTADGUM_pred){

    surv2011.1524_mean <- mean(CTADGUM_pred$`surv2011.1524`, na.rm=T)
    surv2012.1634_mean <- mean(CTADGUM_pred$`surv2012.1634`, na.rm=T)

    whichPredColumns <- grep("(pred$)|(pred\\.)", names(CTADGUM_pred))
    CTADGUM_pred[,whichPredColumns][CTADGUM_pred[,whichPredColumns]==0] <- NA

    binomAboveLimitProb <- function(meanVal, outcome, pop=CTADGUM_pred$Population){
        pbinom(round(meanVal*pop), size=pop, prob=outcome, lower.tail=FALSE)}

    CTADGUM_pred <- within(CTADGUM_pred, {

        resid_surv2011.1524 <- `surv2011.1524` - LApred
        resid_surv2012.1634 <- `surv2012.1634` - LApred

        ## indirect standardised ratios (obs/expd)
        ISR_surv2011.1524 <- `surv2011.1524`/LApred
        ISR_surv2012.1634 <- `surv2012.1634`/LApred

        meandiff_surv2011.1524 <- `surv2011.1524` - surv2011.1524_mean
        meandiff_surv2012.1634 <- `surv2012.1634` - surv2012.1634_mean

        meandiff_surv2011.1524_pred <- LApred - surv2011.1524_mean
        meandiff_surv2012.1634_pred <- LApred - surv2012.1634_mean

        ## Binomial threshold exceedance
        `predBINmean2011.1524`  <- binomAboveLimitProb(surv2011.1524_mean, LApred)
        `predBINmean2012.1634`  <- binomAboveLimitProb(surv2012.1634_mean, LApred)

        `predBIN0.2` <- binomAboveLimitProb(0.2, LApred)
        `predBIN0.25` <- binomAboveLimitProb(0.25, LApred)

        ## define quadrants
        quadrant_surv2011.1524 <- ifelse(meandiff_surv2011.1524>0 & meandiff_surv2011.1524_pred>0, "1",
                                             ifelse(meandiff_surv2011.1524<0 & meandiff_surv2011.1524_pred>0,"2",
                                                    ifelse(meandiff_surv2011.1524>0 & meandiff_surv2011.1524_pred<0,"3",
                                                           ifelse(meandiff_surv2011.1524<0 & meandiff_surv2011.1524_pred<0,"4",NA))))

        quadrant_surv2012.1634 <- ifelse(meandiff_surv2012.1634>0 & meandiff_surv2012.1634_pred>0, "1",
                                             ifelse(meandiff_surv2012.1634<0 & meandiff_surv2012.1634_pred>0,"2",
                                                    ifelse(meandiff_surv2012.1634>0 & meandiff_surv2012.1634_pred<0,"3",
                                                           ifelse(meandiff_surv2012.1634<0 & meandiff_surv2012.1634_pred<0,"4",NA))))
    })

    CTADGUM_pred
}


#' Calculate Output Statistics of knn and logistic Natsal Analysis
#'
#' Calculates the residuals, indirect standardised rates,
#' mean difference, binomial threshold exceedence and quadrant.
#'
#' @param CTADGUM_pred
#'
#' @return

calcStats.CTADGUM_pred_knnlog <- function(CTADGUM_pred){

    surv2011.1524_mean <- mean(CTADGUM_pred$`surv2011.1524`, na.rm=T)
    surv2012.1634_mean <- mean(CTADGUM_pred$`surv2012.1634`, na.rm=T)

    whichPredColumns <- grep("(pred$)|(pred\\.)", names(CTADGUM_pred))
    CTADGUM_pred[,whichPredColumns][CTADGUM_pred[,whichPredColumns]==0] <- NA

    binomAboveLimitProb <- function(meanVal, outcome, pop=CTADGUM_pred$Population){
        pbinom(round(meanVal*pop), size=pop, prob=outcome, lower.tail=FALSE)}

    CTADGUM_pred <- within(CTADGUM_pred, {
        resid_surv2011.1524_knn <- `surv2011.1524` - `pred.knn`
        resid_surv2012.1634_knn <- `surv2012.1634` - `pred.knn`
        resid_surv2011.1524_log <- `surv2011.1524` - `pred.log`
        resid_surv2012.1634_log <- `surv2012.1634` - `pred.log`

        ## indirect standardised ratios (obs/expd)
        ISR_surv2011.1524_knn <- `surv2011.1524`/`pred.knn`
        ISR_surv2012.1634_knn <- `surv2012.1634`/`pred.knn`
        ISR_surv2011.1524_log <- `surv2011.1524`/`pred.log`
        ISR_surv2012.1634_log <- `surv2012.1634`/`pred.log`

        meandiff_surv2011.1524 <- `surv2011.1524` - surv2011.1524_mean
        meandiff_surv2012.1634 <- `surv2012.1634` - surv2012.1634_mean

        meandiff_surv2011.1524_knn <- `pred.knn` - surv2011.1524_mean
        meandiff_surv2012.1634_knn <- `pred.knn` - surv2012.1634_mean
        meandiff_surv2011.1524_log <- `pred.log` - surv2011.1524_mean
        meandiff_surv2012.1634_log <- `pred.log` - surv2012.1634_mean

        ## Binomial threshold exceedance
        `predknnBINmean2011.1524` <- binomAboveLimitProb(surv2011.1524_mean, pred.knn)
        `predknnBINmean2012.1634`  <- binomAboveLimitProb(surv2012.1634_mean, pred.knn)
        `predlogBINmean2011.1524` <- binomAboveLimitProb(surv2011.1524_mean, pred.log)
        `predlogBINmean2012.1634` <- binomAboveLimitProb(surv2012.1634_mean, pred.log)
        #   `survBINmean2011.1524` <- binomAboveLimitProb(surv2011.1524_mean, surv2011.1524)
        #   `survBINmean2012.1634`  <- binomAboveLimitProb(surv2012.1634_mean, surv2012.1634)
        `predknnBIN0.2` <- binomAboveLimitProb(0.2, pred.knn)
        `predlogBIN0.2`  <- binomAboveLimitProb(0.2, pred.log)
        `predknnBIN0.25` <- binomAboveLimitProb(0.25, pred.knn)
        `predlogBIN0.25` <- binomAboveLimitProb(0.25, pred.log)

        ##TODO##
        ## wider (over-dispersion) Bin distn
        ## multiplicative/ additive
        ## see Spiegelhalter...

        ## define quadrants
        quadrant_surv2011.1524_knn <- ifelse(meandiff_surv2011.1524_knn>0 & meandiff_surv2011.1524>0, "1",
                                             ifelse(meandiff_surv2011.1524_knn<0 & meandiff_surv2011.1524>0,"2",
                                                    ifelse(meandiff_surv2011.1524_knn>0 & meandiff_surv2011.1524<0,"3",
                                                           ifelse(meandiff_surv2011.1524_knn<0 & meandiff_surv2011.1524<0,"4",NA))))

        quadrant_surv2012.1634_knn <- ifelse(meandiff_surv2012.1634_knn>0 & meandiff_surv2012.1634>0, "1",
                                             ifelse(meandiff_surv2012.1634_knn<0 & meandiff_surv2012.1634>0,"2",
                                                    ifelse(meandiff_surv2012.1634_knn>0 & meandiff_surv2012.1634<0,"3",
                                                           ifelse(meandiff_surv2012.1634_knn<0 & meandiff_surv2012.1634<0,"4",NA))))

        quadrant_surv2011.1524_log <- ifelse(meandiff_surv2011.1524_log>0 & meandiff_surv2011.1524>0, "1",
                                             ifelse(meandiff_surv2011.1524_log<0 & meandiff_surv2011.1524>0,"2",
                                                    ifelse(meandiff_surv2011.1524_log>0 & meandiff_surv2011.1524<0,"3",
                                                           ifelse(meandiff_surv2011.1524_log<0 & meandiff_surv2011.1524<0,"4",NA))))

        quadrant_surv2012.1634_log <- ifelse(meandiff_surv2012.1634_log>0 & meandiff_surv2012.1634>0, "1",
                                             ifelse(meandiff_surv2012.1634_log<0 & meandiff_surv2012.1634>0,"2",
                                                    ifelse(meandiff_surv2012.1634_log>0 & meandiff_surv2012.1634<0,"3",
                                                           ifelse(meandiff_surv2012.1634_log<0 & meandiff_surv2012.1634<0,"4",NA))))
    })

    CTADGUM_pred
}
