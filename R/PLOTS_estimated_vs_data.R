# ##
# ## Natsal-3 classifier
# ## compare estimated LA values
# ## against surveillance figures
# ##
# ## N Green
# ## April 2014
#
#
# # CTADGUM_pred <- joinAllOutcomeData(pred.knn, pred.log)
#
# library(scales)
# library(ggplot2)
# library(lattice)
#
# # setwd("C:/Users/nathan.green/Documents/chlamydia/classifier")
#
# ## load outcome
# ## C:\Users\nathan.green\Documents\chlamydia\classifier\data\output
# ## pred.knn, pred.log
# load(file.choose())
#
#
# ########################################################################
# ## load data ###########################################################
# ########################################################################
#
# popLA.dat <- read.csv(".\\data\\ONS_popn_age&sex\\ONS_LA_population_2011.csv", check.names=FALSE)
#
#
# ## surveillance data
# ### 2012 16-34 yr olds
# CTADGUM.dat2012 <- read.csv(".\\data\\surveillance\\CTADGUM_data2012_1634years.csv", check.names=FALSE)
# ### 2011 16-24 yr olds
# # CTADGUM.dat <- read.csv(".\\data\\surveillance\\1624ageSurveillanceData2011.csv", check.names=FALSE)
# CTADGUM.dat2011 <- read.csv(".\\data\\surveillance\\1524ageSurveillanceData2011.csv", check.names=FALSE)    #newer version from Ellie
#
#
# CTADGUM.dat2012$"LA Name" <- LAnameClean(CTADGUM.dat2012$"LA Name")
#
#
# ## check consistency across years
# # CTADGUM_temp <- merge(CTADGUM.dat2011[,c("Combined Coverage","LA Name")],
# #                       CTADGUM.dat2012[,c("Combined Coverage","LA Name")],
# #                       by="LA Name")
# # plot(CTADGUM_temp[,2], CTADGUM_temp[,3])
#
#
# IMD.dat2010 <- read.csv(".\\data\\risk_factors\\deprivation\\LA_IMD.csv", check.names=FALSE)
# IMD.dat2010$`LA NAME` <- gsub(" District", "", IMD.dat2010$`LA NAME`)
#
#
# houseprice_earnings.dat <- read.csv(".\\data\\houseprice-earnings\\houseprice_earnings.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")
#
# LAclassification.dat <- read.csv(".\\data\\LAclassification\\LAclassification.csv", check.names=FALSE)  #, colClasses=c("2010"="numeric")
#
#
#
# #########################################################################
# ## preprocess ###########################################################
# #########################################################################
#
# popLA.dat$LA <- LAnameClean(popLA.dat$LA)
#
# ## non-London LAs
# LAnames.nonLondon <- popLA.dat$LA[popLA.dat$Area!="LONDON"]
# LAnames.nonLondon <- LAnames.nonLondon[LAnames.nonLondon != ""]
# ## London LAs
# LAnames.London <- popLA.dat$LA[popLA.dat$Area=="LONDON"]
# LAnames.London <- LAnames.London[LAnames.London != ""]
#
# pred.knn <- cbind(pred.knn, la=rownames(pred.knn))
# pred.log <- cbind(pred.log, la=rownames(pred.log))
#
# ## -----------
# ## select data
# ## -----------
#
# ## predicted values
# # pred <- pred.log
# pred <- pred.knn
#
# ## comparison surveillance data
# CTADGUM.dat <- CTADGUM.dat2012  #16-34
# # CTADGUM.dat <- CTADGUM.dat2011  #16-24
#
#
#
#
# ## join observed and predicted arrays
# CTADGUM_pred <- merge(CTADGUM.dat[,c("Combined Coverage","LA Name")],
#                       pred[,c("la","Coverage")],
#                       by.x="LA Name", by.y="la")
# names(CTADGUM_pred) <- c(names(CTADGUM_pred)[-3], "Coverage_knn")
#
# ## DEDUPLICATION for repeat tests
# ## from Sarah W paper
# CTADGUM_pred$`Combined Coverage` <- CTADGUM_pred$`Combined Coverage`*0.87
#
#
#
#
# ## Population-level statistics for points
#
# ## density
# ## join with total LA population sizes
# ##TODO##
# ## doesn't join lots of LAs
# ## clean data
# CTADGUM_pred <- merge(CTADGUM_pred, popLA.dat[,c("LA","Population","Density")],
#                       by.x="LA Name", by.y="LA", all.x=TRUE)
#
# CTADGUM_pred <- merge(CTADGUM_pred, IMD.dat2010[,c("LA NAME","Average Score")], by.x="LA Name", by.y="LA NAME", all.x=TRUE)
#
# houseprice_earnings.dat$"2010" <- as.numeric(as.character(houseprice_earnings.dat$"2010"))
# CTADGUM_pred <- merge(CTADGUM_pred, houseprice_earnings.dat[,c("Local authority","2010")], by.x="LA Name", by.y="Local authority", all.x=TRUE)
#
# CTADGUM_pred <- merge(CTADGUM_pred, LAclassification.dat[,c("Name","Classification")], by.x="LA Name", by.y="Name", all.x=TRUE)
#
#
#
#
#
#
# ## remove London/non-London LAs
# ## ----------------------------
# # CTADGUM_pred <- CTADGUM_pred[CTADGUM_pred$`LA Name`%in%LAnames.nonLondon,]
# # CTADGUM_pred <- CTADGUM_pred[CTADGUM_pred$`LA Name`%in%LAnames.London,]
#
#
#
# ## summary stats
# ## -------------
#
# ### national CTAD average coverage
# mean(CTADGUM_pred$`Combined Coverage`)  #0.16
#
# ### Natsal overall coverage (not covariate shifted)
# prop.table(table(data.test$cttestly))["YES"]  #0.25
#
# ### predicted overall average
# mean(CTADGUM_pred$`Coverage_knn`, na.rm=TRUE)
#
# ### predicted direct from knn
# tot.pos.pred
# ### correctly predicted direct from knn
# true.pos
#
#
#
#
#
#
# ####################################################################
# ## plots ###########################################################
# ####################################################################
#
#
# ## compare between different model output
# ### knn predicts higher coverages but correlated
# pred.pairs <- merge(merge(merge(merge(pred.knn, pred.log, by="la"),
#                     LAclassification.dat[,c("Name","Classification")], by.x="la", by.y="Name", all.x=TRUE),
#                     popLA.dat[,c("LA","Population","Density")], by.x="la", by.y="LA", all.x=TRUE),
#                     IMD.dat2010[,c("LA NAME","Average Score")], by.x="la", by.y="LA NAME", all.x=TRUE)
#
# # plot(CTADGUM_pred$"Coverage_knn", CTADGUM_pred$"Combined Coverage")
# postscript("./docs/sim_logistic_regn/figures/knn_vs_logistic_urbanrural_TEMP.eps")
# plot(pred.pairs$Coverage.x, pred.pairs$Coverage.y, type="n", xlab="knn", ylab="logistic", main="Urban rural classification")
# text(pred.pairs$Coverage.x, pred.pairs$Coverage.y, pred.pairs$Classification, cex=0.7, col=as.numeric(as.factor(pred.pairs$Classification)))
# abline(a=0, b = 1)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="LU",])
# abline(f, col=2)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="MU",])
# abline(f, col=3)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="OU",])
# abline(f, col=4)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="R50",])
# abline(f, col=5)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="R80",])
# abline(f, col=6)
# f <- lm(`Coverage.y`~`Coverage.x`, data=pred.pairs[pred.pairs$Classification=="SR",])
# abline(f, col=7)
# dev.off()
#
# plot(pred.pairs$Coverage.x, pred.pairs$Coverage.y, cex=2*sqrt(pred.pairs$Population/max(pred.pairs$Population, na.rm=T)), xlab="knn", ylab="logistic", main="Population")
# abline(a=0, b = 1)
# plot(pred.pairs$Coverage.x, pred.pairs$Coverage.y, cex=2*sqrt(pred.pairs$"Average Score"/max(pred.pairs$"Average Score", na.rm=T)), xlab="knn", ylab="logistic", main="IMD")
# abline(a=0, b = 1)
# plot(pred.pairs$Coverage.x, pred.pairs$Coverage.y, cex=2*sqrt(pred.pairs$Density/max(pred.pairs$Density, na.rm=T)), xlab="knn", ylab="logistic", main="Population density")
# abline(a=0, b = 1)
#
# ## LA-ordered coverage curves
# par(mfrow=c(1,2))
# plot(sort(CTADGUM.dat$"Combined Coverage"), ylim=c(0,0.6), type="l", xlab="Ordered LAs", ylab="Observed")
# plot(sort(pred$Coverage), ylim=c(0,0.6), type="l", xlab="Ordered LAs", ylab="Estimated")
#
#
#
# #  ------------------------------------------------------------------------
#
#
#
# ## paired scatter plots
# ## ====================
#
# ## linear regression
# par(mfrow=c(1,1))
# plot(CTADGUM_pred$`Combined Coverage`[CTADGUM_pred$`Coverage_knn`!=0],
#      CTADGUM_pred$`Coverage_knn`[CTADGUM_pred$`Coverage_knn`!=0],
#      ylab="Estimated Coverage", xlab="CTAD Observed Coverage",
#      col = rgb(1, 0, 0, 0.4), pch=16, cex=1.5, ylim=c(0,0.5), xlim=c(0,0.5))
#
# f <- lm(`Coverage_knn`~`Combined Coverage`, data=CTADGUM_pred)
# abline(f)
# # stargazer(f)    #table
#
#
# ## predict using the linear regression fit
# ComCov.pred <- predict(f, newdata=CTADGUM_pred$`Combined  Coverage`, type="response")
#
#
# ## difference between
# ## total number of positives
# ## and the number of correctly predicted positives
# a.diff <- tot.pos.obs-true.pos
#
#
# ## logical vectors identifying which LAs are
# ## outside of various model error bounds
#
# ### linear regression
# la.outside.regn <- ComCov.pred<CTADGUM_pred$`Combined Coverage`-a.diff
# ### fixed error
# la.outside.fix  <- CTADGUM_pred$`Coverage_knn`>CTADGUM_pred$`Combined Coverage`+a.diff | CTADGUM_pred$`Coverage_knn`<CTADGUM_pred$`Combined Coverage`-a.diff
# la.outside.fix  <- la.outside.fix & !la.outside.regn
# ### proportional to observed coverage
# la.outside.prop <- CTADGUM_pred$`Coverage_knn`>(CTADGUM_pred$`Combined Coverage`/prop.TP.obs) | CTADGUM_pred$`Coverage_knn`<(CTADGUM_pred$`Combined Coverage`*prop.TP.obs)
# la.outside.prop <- la.outside.prop & !la.outside.regn & !la.outside.fix
#
#
# ## rename for legend
# names(CTADGUM_pred)[names(CTADGUM_pred)=="Density"] <- "Population density" # (persons per hectare)"
# names(CTADGUM_pred)[names(CTADGUM_pred)=="Average Score"] <- "IMD"  #Index of Multiple Deprivation average score"
# names(CTADGUM_pred)[names(CTADGUM_pred)=="2010"] <- "House prices/earnings"
#
#
# #  ------------------------------------------------------------------------
#
#
# #setEPS()
# postscript("./docs/sim_logistic_regn/figures/coverage_names_TEMP.eps")
# scatterplotObsEst(CTADGUM_pred[CTADGUM_pred$`Coverage_knn`!=0,], names=T, points=F)
# dev.off()
#
# postscript("./docs/sim_logistic_regn/figures/coverage_IMD_TEMP.eps")
# scatterplotObsEst(CTADGUM_pred[CTADGUM_pred$`Coverage_knn`!=0,], names=F, points="IMD")
# dev.off()
#
# postscript("./docs/sim_logistic_regn/figures/coverage_density_TEMP.eps")
# scatterplotObsEst(CTADGUM_pred[CTADGUM_pred$`Coverage_knn`!=0,], names=F, points="density")
# dev.off()
#
# postscript("./docs/sim_logistic_regn/figures/coverage_house_TEMP.eps")
# scatterplotObsEst(CTADGUM_pred[CTADGUM_pred$`Coverage_knn`!=0,], names=F, points="house")
# dev.off()
#
# postscript("./docs/sim_logistic_regn/figures/coverage_class_TEMP.eps")
# scatterplotObsEst(CTADGUM_pred[CTADGUM_pred$`Coverage_knn`!=0,], names=F, points="class")
# dev.off()
#
#
#
# ## matched LA ordering
# plot(CTADGUM_pred$`Combined Coverage`[order(CTADGUM_pred$`Combined Coverage`)], ylim=c(0,0.6), type="l", xlab="CTAD ordered LA", ylab="")
# points(CTADGUM_pred$`Coverage_knn`[order(CTADGUM_pred$`Combined Coverage`)], ylim=c(0,0.6), col="red")
#
# plot(CTADGUM_pred$`Coverage_knn`[order(CTADGUM_pred$`Combined Coverage`)], xlab="Ordered LA", ylab="")
# # f <- lm(, data=)
# # abline(f)
#
# ## ordered list of LAs
# CTADGUM_pred$`LA Name`[order(CTADGUM_pred$`Coverage_knn`)]
# CTADGUM_pred$`LA Name`[order(CTADGUM_pred$`Combined Coverage`)]
#
#
#
# ######################################################################################
# ## residual sum of squares ###########################################################
# ######################################################################################
#
# ##TODO##
# ##errors...
#
# ## NCSP/GUMCAD vs knn predicted
# hist(CTADGUM_pred$`Combined Coverage` - CTADGUM_pred$Coverage, breaks=30, xlab="Observed - expected", main="")
# rss(data1=CTADGUM_pred$`Combined Coverage`, data2=CTADGUM_pred$Coverage)
# ## NCSP/GUMCAD mean vs knn predicted
# hist(mean(CTADGUM_pred$`Combined Coverage`) - CTADGUM_pred$Coverage, breaks=20, xlab="National average - expected", main="")
# ##
# rss(CTADGUM_pred$`Combined Coverage`, ComCov.pred)
# ##
# rss(data1=CTADGUM_pred$`Combined Coverage`+a.diff, data2=CTADGUM_pred$`Combined Coverage`-a.diff, data3=CTADGUM_pred$Coverage)
# ##
# rss(CTADGUM_pred$`Combined Coverage`+a.diff, CTADGUM_pred$`Combined Coverage`-a.diff, ComCov.pred)
#
#
#
#
# ###########################################
# ## residual vs CTAD mean difference plot ##
# ###########################################
#
# ## ordination plots package
# # http://www.fromthebottomoftheheap.net/2013/12/31/decluttering-ordination-in-vegan-part-4-orditkplot/
# # http://blog.fellstat.com/?cat=11
#
# CTADGUM_pred$Coverage_knn[CTADGUM_pred$Coverage_knn==0] <- NA
# CTADGUM_pred$resid <- CTADGUM_pred$`Combined Coverage`-CTADGUM_pred$`Coverage_knn`
# CTADGUM_pred$meandiff_obs <- CTADGUM_pred$`Combined Coverage`-mean(CTADGUM_pred$`Combined Coverage`)
# CTADGUM_pred$meandiff_pred <- CTADGUM_pred$`Coverage_knn`-mean(CTADGUM_pred$`Combined Coverage`)
#
# postscript("./docs/sim_logistic_regn/figures/meandiff_against_residuals_TEMP.eps")
#
# ## empty plots
#
# # whole region
# plot(CTADGUM_pred$meandiff_obs, CTADGUM_pred$resid,
#      xlab="Observed - mean observed coverage", ylab="Residual, observed - estimated coverage", type="n")#, xlim=c(-0.2,0.2), ylim=c(-0.1,0.2))
#
# # positive region
# plot(CTADGUM_pred$meandiff_obs[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs>0],
#      CTADGUM_pred$resid[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs>0],
#      type="n", xlim=c(0,0.2), ylim=c(0,0.2),
#      xlab="Observed - mean observed coverage", ylab="Residual, observed - estimated coverage")
#
# # negative region
# plot(CTADGUM_pred$meandiff_obs[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs<0],
#      CTADGUM_pred$resid[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs<0],
#      type="n", xlim=c(-0.1,0), ylim=c(-0.15,0),
#      xlab="Observed - mean observed coverage", ylab="Residual, observed - estimated coverage")
#
#
# text(CTADGUM_pred$meandiff_obs, CTADGUM_pred$resid, labels=CTADGUM_pred$`LA Name`)
# # text(CTADGUM_pred$resid, CTADGUM_pred$meandiff_obs, labels=CTADGUM_pred$`LA Name`)
# abline(a=0, b=1)
# abline(h = 0, col="red")
# abline(v = 0, col="red")
# dev.off()
#
#
# LA.overpred_overmean   <- na.omit(CTADGUM_pred$`LA Name`[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs>0])
# LA.underpred_overmean  <- na.omit(CTADGUM_pred$`LA Name`[CTADGUM_pred$resid<0 & CTADGUM_pred$meandiff_obs>0])
# LA.overpred_undermean  <- na.omit(CTADGUM_pred$`LA Name`[CTADGUM_pred$resid>0 & CTADGUM_pred$meandiff_obs<0])
# LA.underpred_undermean <- na.omit(CTADGUM_pred$`LA Name`[CTADGUM_pred$resid<0 & CTADGUM_pred$meandiff_obs<0])
# write.csv()
#
#
# loc <- cbind(meandiff_obs=CTADGUM_pred$meandiff_obs, resid=CTADGUM_pred$resid, scaled_resid=CTADGUM_pred$resid/CTADGUM_pred$`Combined Coverage`)
# rownames(loc) <- CTADGUM_pred$`LA Name`
#
# #########################
# ## tidied LA placement ##
# #########################
# library(wordcloud)
#
# ## total, unscaled
# error <- 0.08
# # temp <- na.omit(loc[sqrt(loc[,1]^2 + loc[,2]^2)>0.1,])
# temp <- na.omit(loc[abs(loc[,2])>error & abs(loc[,1])>0.05 | rownames(loc)=="Lincoln",c(1,2)])
# mx <- apply(temp,2,max) + 0.05
# mn <- apply(temp,2,min) - 0.05
# postscript("./docs/sim_logistic_regn/figures/meandiff_against_residuals_TEMP.eps")  # x11()
# # plot(loc, pch=19, cex=4, xlim=c(mn[1],mx[1]), ylim=c(mn[2],mx[2]), col="light blue")#col = alpha("black", 0.1))
# plot(na.omit(loc[abs(loc[,2])<error,c(1,2)]), xlim=c(mn[1],mx[1]), ylim=c(mn[2],mx[2]), cex=0.5, pch=16, col="light grey",
#      xlab="Observed - mean observed coverage", ylab="Residual, observed - estimated")#col = alpha("black", 0.1))
# textplot(temp[,1],temp[,2], rownames(temp), xlim=c(mn[1],mx[1]), ylim=c(mn[2],mx[2]), cex=0.5, show.lines=FALSE, new=FALSE)
# abline(a=0, b=1, lty=2)
# abline(h = 0, col="red")
# abline(v = 0, col="red")
# dev.off()
#
#
# ## total, scaled
# error <- 0.6
# # temp <- na.omit(loc[sqrt(loc[,1]^2 + loc[,2]^2)>0.1,])
# temp <- na.omit(loc[abs(loc[,3])>error & abs(loc[,1])>0.05 | rownames(loc)=="Lincoln",c(1,3)])
# mx <- apply(temp,2,max) + 0.05
# mn <- apply(temp,2,min) - 0.05
# postscript("./docs/sim_logistic_regn/figures/meandiff_against_residuals_TEMP.eps")
# # plot(loc, pch=19, cex=4, xlim=c(mn[1],mx[1]), ylim=c(mn[2],mx[2]), col="light blue")#col = alpha("black", 0.1))
# plot(na.omit(loc[abs(loc[,3])<error,c(1,3)]), xlim=c(-0.2,0.2), ylim=c(-3,1), cex=0.5, pch=16, col="light grey",
#      xlab="Observed - mean observed coverage", ylab="Residual, (observed - estimated)/observed")#col = alpha("black", 0.1))
# textplot(temp[,1],temp[,2], rownames(temp), new=FALSE, cex=0.5, show.lines=FALSE)#,xlim=c(mn[1],mx[1]), ylim=c(mn[2],mx[2]))
# abline(h = 0, col="red")
# abline(v = 0, col="red")
# dev.off()
#
#
#
#
#
