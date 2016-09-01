
#' Join All Outcome Data
#'
#' Combines area level post-stratified predictions with surveillance data.
#' Take predictions from different models.
#'
#' ##TODO##
#' I've commented-out the logisitic knn predictions and used the MRP one instead
#'
#' @param pred Prediction array. Ensure that the first column is the LA names.
#' @param adj Double counting adjustment <=1. From Sarah Woodall's paper.
#' @param survData.filename User supplied surveillance data
#' @param ...
#' @return CTADGUM_pred

joinAllOutcomeData <- function(pred, adj=0.87, survData.filename=NA, ...){

    require(scales)
    require(ggplot2)
    require(lattice)
    require(plyr)
    require(STIecoPredict)

    stopifnot(is.character(pred[,1]))
    stopifnot(ncol(pred)>1)
    stopifnot(is.numeric(adj))

    #system.file(package="STIecoPredict")

    ## surveillance data @ LA level
    data("CTADGUM_dat2012") #16-34 yr olds    #notice this has been adjusted from the standard 15 to 16 year olds in line with Natsal
    data("CTADGUM_dat2011") #16-24 yr olds
    data("CTADGUM_age1524_2012")
    data("CTADGUM_age1524_2013")
    data("CTADGUM_age1524_2014")
    data(NCSP_NNNG_GUM_age1524_2011)

    data("ONS_LA_population_2011")
    data("LA_IMD")
    data("houseprice_earnings")
    data("LA_classification")

    #########################################################################
    ## preprocess ###########################################################
    #########################################################################

    names(pred) <- sub("LA.Name", "LA Name", names(pred), ignore.case = TRUE)

    names(CTADGUM.dat2012) <- sub("LA.Name", "LA Name", names(CTADGUM.dat2012), ignore.case = TRUE)
    names(CTADGUM.dat2011) <- sub("LA.Name", "LA Name", names(CTADGUM.dat2011), ignore.case = TRUE)
    names(CTADGUM_age1524_2012) <- sub("LA.Name", "LA Name", names(CTADGUM_age1524_2012), ignore.case = TRUE)
    names(CTADGUM_age1524_2013) <- sub("LA.Name", "LA Name", names(CTADGUM_age1524_2013), ignore.case = TRUE)
    names(NCSP.NNNG.GUM_age1524_2011) <- sub("LA.Name", "LA Name", names(NCSP.NNNG.GUM_age1524_2011), ignore.case = TRUE)

    names(CTADGUM.dat2012) <- sub("Combined.Coverage", "Combined Coverage", names(CTADGUM.dat2012), ignore.case = TRUE)
    names(CTADGUM.dat2011) <- sub("Combined.Coverage", "Combined Coverage", names(CTADGUM.dat2011), ignore.case = TRUE)
    names(CTADGUM_age1524_2012) <- sub("Combined.Coverage", "Combined Coverage", names(CTADGUM_age1524_2012), ignore.case = TRUE)
    names(CTADGUM_age1524_2013) <- sub("Combined.Coverage", "Combined Coverage", names(CTADGUM_age1524_2013), ignore.case = TRUE)
    names(NCSP.NNNG.GUM_age1524_2011) <- sub("Combined.Coverage", "Combined Coverage", names(NCSP.NNNG.GUM_age1524_2011), ignore.case = TRUE)

    IMD.dat2010$`LA Name` <- gsub(" District", "", IMD.dat2010$`LA Name`, ignore.case = TRUE)

    CTADGUM.dat2012$"LA Name" <- toupper(CTADGUM.dat2012$"LA Name")
    CTADGUM.dat2012$"LA Name" <- LAnameClean(CTADGUM.dat2012$"LA Name")
    CTADGUM.dat2011$"LA Name" <- toupper(CTADGUM.dat2011$"LA Name")
    CTADGUM.dat2011$"LA Name" <- LAnameClean(CTADGUM.dat2011$"LA Name")
    CTADGUM_age1524_2012$"LA Name" <- toupper(CTADGUM_age1524_2012$"LA Name")
    CTADGUM_age1524_2012$"LA Name" <- LAnameClean(CTADGUM_age1524_2012$"LA Name")
    CTADGUM_age1524_2013$"LA Name" <- toupper(CTADGUM_age1524_2013$"LA Name")
    CTADGUM_age1524_2013$"LA Name" <- LAnameClean(CTADGUM_age1524_2013$"LA Name")
    NCSP.NNNG.GUM_age1524_2011$"LA Name" <- toupper(NCSP.NNNG.GUM_age1524_2011$"LA Name")
    NCSP.NNNG.GUM_age1524_2011$"LA Name" <- LAnameClean(NCSP.NNNG.GUM_age1524_2011$"LA Name")

    popLA.dat$`LA Name` <- toupper(popLA.dat$`LA Name`)
    popLA.dat$`LA Name` <- LAnameClean(popLA.dat$`LA Name`)
    houseprice_earnings.dat$"LA Name" <- toupper(houseprice_earnings.dat$"LA Name")
    houseprice_earnings.dat$"LA Name" <- LAnameClean(houseprice_earnings.dat$"LA Name")
    IMD.dat2010$`LA Name` <- toupper(IMD.dat2010$`LA Name`)
    IMD.dat2010$`LA Name` <- LAnameClean(IMD.dat2010$`LA Name`)
    LAclassification.dat$`LA Name` <- toupper(LAclassification.dat$`LA Name`)
    LAclassification.dat$`LA Name` <- LAnameClean(LAclassification.dat$`LA Name`)

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

    names(pred)[1] <- "LA Name"

    # pred <- merge(pred.log[,c("la","Coverage")],
    #               pred.knn[,c("la","Coverage")], by="la")

    colnames <- c("LA Name", "Combined Coverage")

    data.list <- list(CTADGUM.dat2011[, colnames],
                      CTADGUM.dat2012[, colnames],
                      CTADGUM_age1524_2012[, colnames],
                      CTADGUM_age1524_2013[, colnames],
                      NCSP.NNNG.GUM_age1524_2011[, colnames])

    CTADGUM_pred <- Reduce(function(...) merge(..., by="LA Name", all=TRUE), data.list)

    names(CTADGUM_pred)[-1] <- c("surv2011.1624", "surv2012.1634", "surv2012.1524", "surv2013.1524", "surv2011.1524")

    CTADGUM_pred[,-1] <- CTADGUM_pred[,-1]*adj      #DE-DUPLICATION for repeat tests

    # CTADGUM_pred <- within(CTADGUM_pred, {
        ##TODO##
        ##hack to remove outliers. could Winsorize instead...
        # elseif(CTADGUM_pred$surv2012.1634>upperlim, , CTADGUM_pred$surv2012.1634>upperlim)
        # `surv2012.1634`[`LA Name`=="Kettering"] <- NA#0.5221963
        # `surv2012.1634`[`LA Name`=="Dacorum"] <- NA#0.4177031
        # `surv2011.1524`[`LA Name`=="Chesterfield"] <- NA#0.6716591
    # })

    CTADGUM_pred <- merge(CTADGUM_pred, pred, by.x="LA Name", by.y="LA Name")
    # names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.x"] <- "pred.log"
    # names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.y"] <- "pred.knn"

    ## merge LA-level statistics for points

    CTADGUM_pred <- merge(CTADGUM_pred, popLA.dat[,c("LA Name","Population","Density")],
                          by.x="LA Name", by.y="LA Name", all.x=TRUE)

    CTADGUM_pred <- merge(CTADGUM_pred, IMD.dat2010[,c("LA Name","Average Score")],
                          by.x="LA Name", by.y="LA Name", all.x=TRUE)
    names(CTADGUM_pred)[names(CTADGUM_pred)=="Average Score"] <- "IMD2010"

    houseprice_earnings.dat$"2010" <- as.numeric(as.character(houseprice_earnings.dat$"2010"))
    CTADGUM_pred <- merge(CTADGUM_pred, houseprice_earnings.dat[,c("LA Name","2010")],
                          by.x="LA Name", by.y="LA Name", all.x=TRUE)
    names(CTADGUM_pred)[names(CTADGUM_pred)=="2010"] <- "housepriceEarnings2010"

    CTADGUM_pred <- merge(CTADGUM_pred, LAclassification.dat[,c("LA Name","Classification","Region")],
                          by.x="LA Name", by.y="LA Name", all.x=TRUE)

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
#' @param CTADGUM_pred Combined model LA level predictions and census/administrative data
#'
#' @return CTADGUM_pred
#' @seealso joinAllOutcomeData

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

    CTADGUM_pred$name.quadrant_surv2011.1524[CTADGUM_pred$quadrant_surv2011.1524=="1"] <- "aboveSurv_abovePred"
    CTADGUM_pred$name.quadrant_surv2011.1524[CTADGUM_pred$quadrant_surv2011.1524=="2"] <- "belowSurv_abovePred"
    CTADGUM_pred$name.quadrant_surv2011.1524[CTADGUM_pred$quadrant_surv2011.1524=="3"] <- "aboveSurv_belowPred"
    CTADGUM_pred$name.quadrant_surv2011.1524[CTADGUM_pred$quadrant_surv2011.1524=="4"] <- "belowSurv_belowPred"

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
