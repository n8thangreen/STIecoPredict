# ##
# ## Natsal-3 classifier
# ## descriptive plots
# ##
# ## N Green
# ## April 2014
# ##
#
#
# library(ggplot2)
#
# # load("./R_analyses/Chlamydia_classifier/sim_logistic_regn/workspaces/riskfactor_pop_sim_input.RData")
#
# # setwd("M:\\NGreen\\Chlamydia\\classifier\\docs\\paper_DRAFTs\\sim_logistic_regn\\figures")
# setwd("./docs/sim_logistic_regn/figures")
#
#
#
#
#
# ## logistic curve plots
# ## of data
# plot(data.train$dage+rnorm(length(data.train$dage),0,0.5), as.numeric(data.train$cttestly)-1, col=rgb(0, 0, 0, 0.003), pch=16,
#      xlab="Age (years)", ylab="Probability of testing")
# points(x=data.train$dage, y=fit.probs.train, col=rgb(0, 0, 1, 0.01), pch=16)
# abline(h=0.5, lty=2)
#
# plot(data.test$dage+rnorm(length(data.test$dage),0,0.5), as.numeric(data.test$cttestly)-1, col=rgb(0, 0, 0, 0.003), pch=16,
#      xlab="Age (years)", ylab="Probability of testing")
# points(x=data.test$dage, y=fit.probs.test, col=rgb(0, 0, 1, 0.01), pch=16)
# abline(h=0.5, lty=2)
#
#
#
#
#
#
# #############################################################################################
# ## overlapping histograms ###################################################################
# #############################################################################################
#
# ## optional:
# ## stratify by gender
# # data.temp <- data
# # data <- data.temp[data.temp$rsex=="Women",]
# # data <- data.temp
#
#
# ## empirical/data
# ## --------------
#
# datasplit <- (data$cttestly=="NO")
# # datasplit <- !data$whnchlamYr
# # datasplit <- (data$chlmtest=="NO")
#
#
# ## separate histograms for either outcome group on the same plot
# ## to see whether we can distinguish between the two
# x11()
# par(mfrow=c(3,1))
# ## ages
# hist(data[datasplit,"dage"]+16, breaks=ageRange, col=rgb(1,0,0,0.5), freq=FALSE, main="", xlab="Age (years)", ylim=c(0,0.3))
# hist(data[!datasplit,"dage"]+16, breaks=ageRange, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
# legend(title="Empirical: tested in last year", x="topright", legend=c("FALSE", "TRUE"), col=c("red","blue"), pch=c(15,15), pt.cex=2, cex=0.8)
# # legend(x="topright", legend=c("NO", "YES"), col=c("red","blue"), pch=c(15,15), pt.cex=2, cex=0.8)
# ## number of partneres not using condom in last year
# hist(data[datasplit & data$nonocon<=10,"nonocon"], breaks=-1:10, col=rgb(1,0,0,0.5), freq=FALSE, main="", ylim=c(0,1), xlab="number of partners no condom in last year", xlim=c(0,10))
# hist(data[!datasplit & data$nonocon<=10,"nonocon"], breaks=-1:10, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
# ## frequency of sex in last 4 weeks
# hist(data[datasplit & data$sex4wks<=50,"sex4wks"], breaks=0:50, col=rgb(1,0,0,0.5), freq=FALSE, main="", xlab="sex acts in last 4 weeks")
# hist(data[!datasplit & data$sex4wks<=50,"sex4wks"], breaks=0:50, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
#
#
#
#
# ## fits
# ## ----
#
# ## separate histograms for either outcome group on the same plot
# ## to see whether we can distinguish between the two
#
# datasplit.pred <- (fit.pred=="NO")
# # datasplit.pred <- (fit.pred=="FALSE")
#
# x11()
# par(mfrow=c(3,1))
# hist(data[datasplit.pred & data$dage<=max(ageRange),"dage"], breaks=ageRange, col=rgb(1,0,0,0.5), freq=FALSE, main="", xlab="Age (years)", ylim=c(0,0.2))
# hist(data[!datasplit.pred & data$dage<=max(ageRange),"dage"], breaks=ageRange, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
# legend(title="Fit: diagnosed in last year", x="topright", legend=c("NO", "YES"), col=c("red","blue"), pch=c(15,15), pt.cex=2, cex=0.8)
# hist(data[datasplit.pred & data$nonocon<=10,"nonocon"], breaks=-1:10, col=rgb(1,0,0,0.5), freq=FALSE, main="", ylim=c(0,1),xlab="number of partners no condom in last year")
# hist(data[!datasplit.pred & data$nonocon<=10,"nonocon"], breaks=-1:10, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
# hist(data[datasplit.pred & data$sex4wks<=50,"sex4wks"], breaks=0:50, col=rgb(1,0,0,0.5), freq=FALSE, main="", xlab="sex acts in last 4 weeks", ylim=c(0,0.35))
# hist(data[!datasplit.pred & data$sex4wks<=50,"sex4wks"], breaks=0:50, col=rgb(0,0,1,0.5), freq=FALSE, add=T)
# box()
#
#
#
#
#
# ##########################################################################################
# ## paired scatterplots ###################################################################
# ##########################################################################################
#
# jig <- runif(nrow(data),0,0.8)  #additive noise
#
# ### cttestly
# x11()
# par(mfcol=c(2,3))
# plot(data$dage+jig, data$nonocon+jig, col=alpha(as.numeric(as.factor(data$cttestly))+1,0.5), main="data",
#      pch=16, ylim=c(0,10), cex=2, xlab="Age (years)", ylab="number of partners no condom use in last year", xlim=c(min(ageRange),max(ageRange)))
# plot(data$dage+jig, data$nonocon+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5), main="fit",
#      pch=16, ylim=c(0,10), cex=2, xlab="Age (years)", ylab="number of partners no condom use in last year", xlim=c(min(ageRange),max(ageRange)))
#
# plot(data$dage+jig, data$het1yr+jig, col=alpha(as.numeric(as.factor(data$cttestly))+1,0.5), main="data",
#      pch=16, ylim=c(0,30), cex=2, xlab="Age (years)", ylab="number of partners in last year", xlim=c(min(ageRange),max(ageRange)))
# plot(data$dage+jig, data$het1yr+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5), main="fit",
#      pch=16, ylim=c(0,30), cex=2, xlab="Age (years)", ylab="number of partners in last year", xlim=c(min(ageRange),max(ageRange)))
#
# plot(data$dage+jig, data$sex4wks+jig), col=alpha(as.numeric(as.factor(data$cttestly))+1,0.5), main="data",
#      pch=16, ylim=c(0,50), cex=2, xlab="Age (years)", ylab="frequency of sex in last 4 weeks", xlim=c(min(ageRange),max(ageRange)))
# legend(x="topright", legend=c("NO", "YES"), col=c("red","green"), pch=c(16,16), pt.cex=2, cex=1, title="cttestly", bty="n", xjust=1)
# plot(data$dage+jig, data$sex4wks+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5), main="fit",
#      pch=16, ylim=c(0,50), cex=2, xlab="Age (years)", ylab="frequency of sex in last 4 weeks", xlim=c(min(ageRange),max(ageRange)))
#
# ## wnhchlam
# x11()
# par(mfcol=c(2,3))
# plot(data$dage+jig, data$nonocon+jig, col=alpha(as.numeric(data$whnchlamYr)+2,0.5),
#      pch=16, ylim=c(0,10), cex=2, xlab="Age (years)", ylab="number of partners no condom use in last year")
# plot(data$dage+jig, data$nonocon+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5),
#      pch=16, ylim=c(0,10), cex=2, xlab="Age (years)", ylab="number of partners no condom use in last year")
#
# plot(data$dage+jig, data$het1yr+jig, col=alpha(as.numeric(data$whnchlamYr)+2,0.5),
#      pch=16, ylim=c(0,30), cex=2, xlab="Age (years)", ylab="number of partners in last year")
# plot(data$dage+jig, data$het1yr+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5),
#      pch=16, ylim=c(0,30), cex=2, xlab="Age (years)", ylab="number of partners in last year")
#
# plot(data$dage+jig, data$sex4wks+jig, col=alpha(as.numeric(data$whnchlamYr)+2,0.5),
#      pch=16, ylim=c(0,50), cex=2, xlab="Age (years)", ylab="frequency of sex in last 4 weeks")
# legend(x="topright", legend=c("NO", "YES"), col=c("red","green"), pch=c(16,16), pt.cex=2, cex=1, title="whnchlam", bty="n", xjust=1)
# plot(data$dage+jig, data$sex4wks+jig, col=alpha(as.numeric(as.factor(fit.pred))+1,0.5),
#      pch=16, ylim=c(0,50), cex=2, xlab="Age (years)", ylab="frequency of sex in last 4 weeks")
#
#
#
# ## scatter plot matrices
# pairs(~sex4wks+het1yr+dage+nonocon+drinknum, data=data)
#
#
#
#
# ## risk factor values for subset of LAs
# ## centralised by the overall England mean values
#
# la.names <- c("Manchester", "Tower Hamlets", "Leeds", "Eden", "Newham")
# cov.names <- c("smoking", "drinking", "sex")
# ## match la name ordering to that in data
# la.names <- smokingLA.dat$Name[smokingLA.dat$Name%in%la.names]
#
# ## proportion of men in each LA and pooled
# sex <- sapply(la.names, function(la)
#                 sum(popLAagesex.dat[popLAagesex.dat$Sex=="Men"& popLAagesex.dat$Name==la,"2012"])/
#                 sum(popLAagesex.dat[popLAagesex.dat$Name==la,"2012"]))
# sexE <- sum(popLAagesex.dat[popLAagesex.dat$Sex=="Men" & popLAagesex.dat$country=="E","2012"])/sum(popLAagesex.dat[popLAagesex.dat$country=="E","2012"])  #England
#
# ## combined probability vector
# probs <- c(smokingLA.dat[smokingLA.dat$Name%in%la.names, "Indicator.value"]-smokingLA.dat[smokingLA.dat$Name=="England", "Indicator.value"],
#            drinkingLA.dat[drinkingLA.dat$Name%in%la.names, "Indicator.value"]-drinkingLA.dat[drinkingLA.dat$Name=="England", "Indicator.value"],
#            sex-sexE)
#
# df <- data.frame(LA=la.names, covariates=rep(cov.names,each=length(la.names)), probs=probs/100)
#
# ## save
# # png(file=".\\riskfactor_LA_desc.png", width = 480, height = 480)
# postscript(file=".\\riskfactor_LA_desc.png", width = 480, height = 480)
#
# ##TODO##
# ## error
# ggplot(df, aes(LA, probs, fill=LA)) + geom_bar(stat="identity") +
#    facet_grid(covariates~., scales="free_y") #+
# # facet_wrap(~ covariates, scales = "free_y") #+
#         #theme(axis.title.x = element_text(size=20),
#         #axis.text.x  = element_text(angle=45, vjust=0.5, size=20),
#         #strip.text.y  = element_text(size=20))#,
#         #text = element_text(size=20))
#
# dev.off()
#
#
#
#
#
#
# ###############################################################################################################
# ## LA-wide distibutions                     ###################################################################
# ###############################################################################################################
#
# # load("~/chlamydia/classifier/data/output/LAsim/LAsim_submodel_1634_n10000.RData")
# # load("~/chlamydia/classifier/R_analyses/Chlamydia_classifier/sim_logistic_regn/workspaces/config/riskfactor_config_output_1634_submodel.RData")
# # load("./R_analyses/Chlamydia_classifier/sim_logistic_regn/workspaces/riskfactor_pop_sim_input.RData")
# # load("./data/NATSAL/bstrap_distn_rf_1634.RData")    #risk factor bootstrap distribution samples and means
#
# ## direct from LA-level datasets
# ## -----------------------------
#
# ## LA-only proportions
# drinkingLA <- drinkingLA.dat$Indicator.value[drinkingLA.dat$ONS.Cluster!="N/A"]/100
# smokingLA  <- smokingLA.dat$Indicator.value[smokingLA.dat$ONS.Cluster!="N/A"]/100
#
#
# ## England Males only
# datM <- popLAagesex.dat[popLAagesex.dat$country=="E" & popLAagesex.dat$Sex=="Men", c("Name","2012")]
# ## England only
# dat <- popLAagesex.dat[popLAagesex.dat$country=="E",c("Name","2012")]
# ## proportion Males by LA in England
# menLA <- aggregate(datM[,"2012"], by=list(datM$Name), sum)$x/aggregate(dat[,"2012"], by=list(dat$Name), sum)$x
#
# dres <- list(menLA=menLA, smokingLA=smokingLA, drinkingLA=drinkingLA)
#
#
#
# ## from simulated LA-level data
# ## ----------------------------
# ## this allows us to estimate statistics for population subsets
# ## e.g. ages 16-24 only, which isn't directly available in the original data
#
# # load("./data/output/LAsim_(1)_1634.RData")
#
#
# aggSim <- function(LApop.list, data){
#   ## aggregate within a given LA
#   ## to get proportion of
#   ## males, smokers and drinkers
#   ## by age group of LApop.list
#
#   require(plyr)
#
#   dres <- ldply(LApop.list, data.frame) #convert from list to dataframe
#   sampleSize <- nrow(dres)
#
#   rsex <- sum(dres$rsex=="Men")/sampleSize
#   smokenow <- sum(dres$smokenow=="YES")/sampleSize
#   increasingdrinker <- sum(dres$increasingdrinker=="TRUE")/sampleSize
#
#   levels(dres$income)
#
#   return(c(rsex,
#            smokenow,
#            increasingdrinker,
#            table(factor(dres$ethnic, levels(data$ethnic)))/sampleSize,  #define factor to include empty levels from data
#            table(factor(dres$income, levels(data$income)))/sampleSize))
# }
#
#
#
#
# # dres <- lapply(res, FUN = aggSim)
#
# ##  aggregate for each LA
# dres <- NULL
# for (i in 1:length(res)){
#   dres <- rbind(dres, aggSim(res[[i]], data))
# }
# colnames(dres)[1:3] <- c("menLA", "smokingLA", "drinkingLA")
#
# # lres <- apply(dres,2,list)
# lres <- list(menLA=dres[,"menLA"],
#              smokingLA=dres[,"smokingLA"],
#              drinkingLA=dres[,"drinkingLA"])  #convert to list
#
# # save(lres, dres, file="./data/output/sim_props.RData")
# load(file="./data/output/LAsim/sim_props.RData")
#
# ## open connection to save
# # png(file=".\\acrossallLA_riskfactors.png", width = 480, height = 480)
# # postscript(file=".\\acrossallLA_riskfactors.eps")
#
# ## additionally
# ## add associated Natsal-3 points to plots
#
#
# breaks <- 20
#
# par(mfrow=c(4,1))
# par(mar=c(6,6,4.1,2.1))
#
# # postscript(file=".\\acrossallLA_sex.eps")#, width=4.0, height=3.0)
# hist(lres$menLA, freq=FALSE, breaks, main="(a)", cex.lab=1.3,
#      xlim=c(0.1,0.55), xlab="Proportion male")
# # lines(density(lres$menLA, bw=0.005), col="red")
# # abline(v=sum(data$rsex=="Men")/nrow(data), col="blue")  #Natsal-3
# # rug(male[1:200], col = rgb(0.5,0.5,0.5,alpha=0.5))
# rug(male[1:100])
# abline(v=male_bar, col="blue", lwd=2)
# # dev.off()
#
# # postscript(file=".\\acrossallLA_drink.eps")
# hist(lres$drinkingLA, freq=FALSE, breaks, main="(b)", cex.lab=1.3,
#      xlim=c(0.1,0.55), xlab="Proportion increasing or higher risk drinker")
# # lines(density(lres$drinkingLA, bw=0.005), col="red")
# # abline(v=sum(data$increasingdrinker)/nrow(data), col="blue")  #Natsal-3
# # lines(density(drink, bw=0.005), col="red")
# # rug(drink[1:200], col = rgb(0.5,0.5,0.5,alpha=0.5))#, ticksize = 1)
# rug(drink[1:100])
# abline(v=drink_bar, col="blue", lwd=2)
# # dev.off()
#
# # postscript(file=".\\acrossallLA_smoke.eps")
# hist(lres$smokingLA, freq=FALSE, breaks, main="(c)", cex.lab=1.3,
#      xlim=c(0.1,0.55), xlab="Proportion smoker")
# # lines(density(lres$smokingLA, bw=0.01), col="red")
# # abline(v=sum(data$smokenow=="YES")/nrow(data), col="blue")  #Natsal-3
# # rug(smoke[1:200], col = rgb(0.5,0.5,0.5,alpha=0.5))
# rug(smoke[1:100])
# abline(v=smoke_bar, col="blue", lwd=2)
# # dev.off()
#
#
# ## White British only
# # postscript(file=".\\acrossallLA_white.eps")
# hist(dres[,"WHITE BRITISH"], freq=TRUE, breaks=40, main="(d)", cex.lab=1.3,
#      xlab=paste("Proportion White British"))#, xlim=c(0.1,0.55))
# #        lines(density(dres[,ethgrp], bw=0.005), col="red")
# # abline(v=sum(data$ethnic=="WHITE BRITISH")/nrow(data), col="blue")  #Natsal-3
# # rug(whitebrit[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# rug(whitebrit[1:100])
# abline(v=whitebrit_bar, col="blue", lwd=2)
# # dev.off()
#
#
# ## close connection
# # dev.off()
#
#
# ### ethnicity
# par(mfrow=c(3,3))
# par(mar=c(6,5,4.1,2.1))
# nms <- c("WHITE BRITISH", "ASIAN/ASIAN BRITISH: BANGLADESHI", "ASIAN/ASIAN BRITISH: INDIAN", "ASIAN/ASIAN BRITISH: OTHER",
#          "ASIAN/ASIAN BRITISH: PAKISTANI", "BLACK/BLACK BRITISH: AFRICAN",
#          "BLACK/BLACK BRITISH: CARIBBEAN", "CHINESE", "OTHER ETHNIC GROUP", "OTHER ETHNIC GROUP", "WHITE IRISH", "WHITE OTHER")
# for (ethgrp in nms){
#   hist(dres[,ethgrp], freq=FALSE, breaks, main="", cex.lab=2,
#        xlab=paste("Proportion", ethgrp), title=paste("(",letters[which(nms==ethgrp)],")"))#, xlim=c(0.1,0.55))
# #        lines(density(dres[,ethgrp], bw=0.005), col="red")
#        abline(v=sum(data$ethnic==ethgrp)/nrow(data), col="blue")  #Natsal-3
# }
#
#
#
#
# ### income
# par(mfrow=c(2,3))
# par(mar=c(6,5,4.1,2.1))
# for (incomegrp in levels(data$income)[4:9]){
#   hist(dres[dres[,incomegrp]!=0,incomegrp], freq=TRUE, breaks, main="", cex.lab=2,
#        xlab=paste("Proportion", incomegrp), xlim=c(0,1))
#   #        lines(density(dres[incomegrp], bw=0.005), col="red")
#   abline(v=sum(data$income==incomegrp)/nrow(data), col="blue")  #Natsal-3
# }
#
#
# ## idividually plot each income band
# par(mfrow=c(2,3))
# par(mar=c(6,5,4.1,2.1))
# hist(dres[dres[,levels(data$income)[4]]!=0, levels(data$income)[4]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[4]), xlim=c(0,1), main="(a)")
# rug(income5000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2)))
# abline(v=income5000_bar, col="blue", lwd=2)
#
# hist(dres[dres[,levels(data$income)[5]]!=0, levels(data$income)[5]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[5]), xlim=c(0,1), main="(b)")
# rug(income10000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# abline(v=income10000_bar, col="blue", lwd=2)
#
# hist(dres[dres[,levels(data$income)[6]]!=0, levels(data$income)[6]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[6]), xlim=c(0,1), main="(c)")
# rug(income20000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# abline(v=income20000_bar, col="blue", lwd=2)
#
# hist(dres[dres[,levels(data$income)[7]]!=0, levels(data$income)[7]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[7]), xlim=c(0,1), main="(d)")
# rug(income30000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# abline(v=income30000_bar, col="blue", lwd=2)
#
# hist(dres[dres[,levels(data$income)[8]]!=0, levels(data$income)[8]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[8]), xlim=c(0,1), main="(e)")
# rug(income40000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# abline(v=income40000_bar, col="blue", lwd=2)
#
# hist(dres[dres[,levels(data$income)[9]]!=0, levels(data$income)[9]], freq=TRUE, breaks, cex.lab=2,
#      xlab=paste("Proportion",  levels(data$income)[9]), xlim=c(0,1), main="(f)")
# rug(income50000[1:200], col = rgb(0.5,0.5,0.5,alpha=0.2))
# abline(v=income50000_bar, col="blue", lwd=2)
#
