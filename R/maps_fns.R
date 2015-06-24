##
## ETS choropleth plots
## functions
##
## N Green
## Feb 2014




map.tb <- function(map.data, g=NA, brks=NA, title="", zoom=FALSE, value="tb", file=FALSE){
  ## plot UK choropleth maps
  ## developed using LA and ETS data but 
  ## can be applied more generally
  ## Note that the lengend title is also used for the file name
  ##
  ## map.data: map object loaded by readOGR()
  ## g: number of groups for values; max(g)=9
  ## brks: given break points to group values (vector)
  ## title: plot title (string)
  ## zoom: whether to zoom-in (for UK) (logical)
  ## value: variable name of colouring value (string)
  ## file: output to file (logical)
  
  ## save to file open device
  if(file){png(file=paste("figures/", title, ".png", sep=""), width = 600, height= 550)}
  
  ## reset plotting area
#   par(mar=c(0,0,0,0))
#   par(oma=c(0,0,0,0))
#   par(mfrow=c(1,2))
  
  zoom.coord <- c(5342,592031)
  
  if(!is.na(brks)){
    ## defined breakpoints
    cols <- brewer.pal(length(brks), "Greens")
    frac <- cut(map.data@data[,value], brks)
    gs <- cols[findInterval(map.data@data[,value], vec = brks)]
  }else if(!is.na(g)){
    ## defined number of groups
    cols <- brewer.pal(g, "Greens")
    frac <- cut2(map.data@data[,value], g=g)
    gs <- cols[as.integer(factor(frac))]
  }
  
  if(zoom){plot(map.data, col = gs, ylim=zoom.coord)}
  else{    plot(map.data, col = gs)}
  
  title(main=title)
  legend("topleft", legend = levels(frac), fill = cols, title=title, cex=0.8) 
  
  if(file){dev.off()}

}
## END FUNCTION ##



multimap <- function(map.data, datatype="all", g=NA, brks=NA, tofile=FALSE, op=NA){
  
  ## denote if using user-defined colour break points
  if(!is.na(brks)){
    datatype2 <- paste(datatype, "_brks", sep="")
  }else{datatype2 <- datatype}
  
  ## save to file open device
  if(tofile){
    png(file=paste("figures/Grid_", datatype2, ".png", sep=""), width = 3000, height= 2000)  #, width = 1500, height= 900)
  }
  
  if(!is.na(op)){par(op)}
  
  ## global within function variables
  gmap.data <<- map.data
  gg <<- g
  gbrks <<- brks
  gtofile <<- tofile
  
  ## simplify the mapping function
  map.tb.tmp <- function(title, zoom=TRUE, value){
    map.tb(gmap.data, gg, gbrks, title, zoom, value, file=FALSE)
  }  

  if(datatype!="all"){
#     par(mfrow=c(3,3))
    
    map.tb.tmp(title=paste("White.",datatype2,sep=""), value=paste("White.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-Caribbean.",datatype2,sep=""), value=paste("Black-Caribbean.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-African.",datatype2,sep=""), value=paste("Black-African.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-other.",datatype2,sep=""), value=paste("Black-other.",datatype,sep=""))
    map.tb.tmp(title=paste("Indian.",datatype2,sep=""), value=paste("Indian.",datatype,sep=""))
    map.tb.tmp(title=paste("Pakistani.",datatype2,sep=""), value=paste("Pakistani.",datatype,sep=""))
    map.tb.tmp(title=paste("Bangladeshi.",datatype2,sep=""), value=paste("Bangladeshi.",datatype,sep=""))
    map.tb.tmp(title=paste("Chinese.",datatype2,sep=""), value=paste("Chinese.",datatype,sep=""))
    
    if(!is.na(brks)){
      ## empty plot
      plot(1, type="n", axes=F, xlab="", ylab="")
      legend("topleft", legend = levels(cut(1,brks)), fill = brewer.pal(length(brks), "Greens"), cex=1.0) #, cex=1.4)
    }
  }else if(datatype=="all"){    
    ethgrp.names <- c("White", "Black-Caribbean", "Black-African", "Black-other", "Indian", "Pakistani", "Bangladeshi", "Chinese")
    
    for (i in ethgrp.names){
      map.tb.tmp(title=paste(i,".tb",sep=""), value=paste(i,".tb",sep=""))
      map.tb.tmp(title=paste(i,".prop",sep=""), value=paste(i,".prop",sep=""))
      map.tb.tmp(title=paste(i,".pop",sep=""), value=paste(i,".pop",sep=""))
    }
    
    ##TODO##
    ## different brks for tb, prop, pop
    
  } 
  
  
  if(tofile){dev.off()}

}
## END FUNCTION ##


joinAllOutcomeData <- function(pred.knn, pred.log){
  
  source("./R_code/Chlamydia_classifier/sim_logistic_regn/classifier_fns.R")
  
  require(scales)
  require(ggplot2)
  require(lattice)
  require(plyr)
  
  ########################################################################
  ## load data ###########################################################
  ########################################################################
  
  popLA.dat <- read.csv(".\\data\\ONS_popn_age&sex\\ONS_LA_population_2011.csv", check.names=FALSE)
  
  
  ## surveillance data
  ### 2012 16-34 yr olds
  CTADGUM.dat2012 <- read.csv(".\\data\\surveillance\\CTADGUM_data2012_1634years.csv", check.names=FALSE)
  ### 2011 16-24 yr olds
  # CTADGUM.dat <- read.csv(".\\data\\surveillance\\1624ageSurveillanceData2011.csv", check.names=FALSE)
  CTADGUM.dat2011 <- read.csv(".\\data\\surveillance\\1524ageSurveillanceData2011.csv", check.names=FALSE)    #newer version from Ellie
  
  IMD.dat2010 <- read.csv(".\\data\\risk_factors\\deprivation\\LA_IMD.csv", check.names=FALSE)
  IMD.dat2010$`LA NAME` <- gsub(" District", "", IMD.dat2010$`LA NAME`)
  
  houseprice_earnings.dat <- read.csv(".\\data\\houseprice-earnings\\houseprice_earnings.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")
  
  LAclassification.dat <- read.csv(".\\data\\LAclassification\\LAclassification.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")
  
  
  #########################################################################
  ## preprocess ###########################################################
  #########################################################################
  
  CTADGUM.dat2012$"LA Name" <- LAnameClean(CTADGUM.dat2012$"LA Name")
  popLA.dat$LA <- LAnameClean(popLA.dat$LA)
  houseprice_earnings.dat$"Local authority" <- LAnameClean(houseprice_earnings.dat$"Local authority")
  
  ## non-London LAs
  LAnames.nonLondon <- popLA.dat$LA[popLA.dat$Area!="LONDON"]
  LAnames.nonLondon <- LAnames.nonLondon[LAnames.nonLondon != ""]
  ## London LAs
  LAnames.London <- popLA.dat$LA[popLA.dat$Area=="LONDON"]
  LAnames.London <- LAnames.London[LAnames.London != ""]
  
  pred.knn <- cbind(pred.knn, la=rownames(pred.knn))
  pred.log <- cbind(pred.log, la=rownames(pred.log))
  
  ## this is a fudge because there are some missing areas
  ## add them in as NAs
  pred.knn <- rbind(pred.knn, data.frame(la=setdiff(CTADGUM.dat2012$`LA Name`, pred.knn$la),Coverage=NA))
  pred.log <- rbind(pred.log, data.frame(la=setdiff(CTADGUM.dat2012$`LA Name`, pred.log$la),Coverage=NA))
  
  ###########
  ## joins ##
  ###########
  
  CTADGUM_pred <- merge(CTADGUM.dat2011[,c("LA Name","Combined Coverage")], CTADGUM.dat2012[,c("LA Name", "Combined Coverage")], by="LA Name", all=TRUE)
  names(CTADGUM_pred)[names(CTADGUM_pred)=="Combined Coverage.x"] <- "surv2011.1524"
  names(CTADGUM_pred)[names(CTADGUM_pred)=="Combined Coverage.y"] <- "surv2012.1634"
  
  pred <- merge(pred.log[,c("la","Coverage")], pred.knn[,c("la","Coverage")], by="la")
  
  CTADGUM_pred <- merge(CTADGUM_pred, pred, by.x="LA Name", by.y="la")
  names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.x"] <- "pred.log"
  names(CTADGUM_pred)[names(CTADGUM_pred)=="Coverage.y"] <- "pred.knn"
  
  adj <- 0.87 # from Sarah Woodall's paper
  CTADGUM_pred <- within(CTADGUM_pred, {
    
    ## DEDUPLICATION for repeat tests
    `surv2011.1524` <- `surv2011.1524`*adj
    `surv2012.1634` <- `surv2012.1634`*adj
    
    ##TODO##
    ##hack to remove outliers. could Winsorize instead...
    # elseif(CTADGUM_pred$surv2012.1634>upperlim, , CTADGUM_pred$surv2012.1634>upperlim)
    `surv2012.1634`[`LA Name`=="Kettering"] <- NA#0.5221963
    `surv2012.1634`[`LA Name`=="Dacorum"] <- NA#0.4177031
    `surv2011.1524`[`LA Name`=="Chesterfield"] <- NA#0.6716591
  })
  
  ## Population-level statistics for points
  
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
  
  surv2011.1524_mean <- mean(CTADGUM_pred$`surv2011.1524`, na.rm=T)
  surv2012.1634_mean <- mean(CTADGUM_pred$`surv2012.1634`, na.rm=T)
  
  CTADGUM_pred$pred.knn[CTADGUM_pred$pred.knn==0] <- NA
  CTADGUM_pred$pred.log[CTADGUM_pred$pred.log==0] <- NA

binomAboveLimitProb <- function(meanVal, outcome, pop=CTADGUM_pred$Population) pbinom(round(meanVal*pop), size=pop, prob=outcome, lower.tail=FALSE)

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
  `predlogBINmean2012.1634`  <- binomAboveLimitProb(surv2012.1634_mean, pred.log)
  #   `survBINmean2011.1524` <- binomAboveLimitProb(surv2011.1524_mean, surv2011.1524)
  #   `survBINmean2012.1634`  <- binomAboveLimitProb(surv2012.1634_mean, surv2012.1634)
  `predknnBIN0.2` <- binomAboveLimitProb(0.2, pred.knn)
  `predlogBIN0.2`  <- binomAboveLimitProb(0.2, pred.log)
  `predknnBIN0.25` <- binomAboveLimitProb(0.25, pred.knn)
  `predlogBIN0.25`  <- binomAboveLimitProb(0.25, pred.log)
  
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




