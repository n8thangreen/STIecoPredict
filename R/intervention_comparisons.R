# ##
# ## interventions
# ## comparisons
# ##
# ## N Green
# ## June 2014
#
#
# ##
# ## need to run classPredict() with
# ## modified risk factor values first
# ## e.g. res.df$increasingdrinker <- FALSE
#
# ## propensity scores
# ## http://www4.stat.ncsu.edu/~davidian/double.pdf
#
#
# setwd("./data/output/predictions")
#
# load("knn/pred_knn_agesexsmokedrinkethnic_drink0_1634.RData")
# load("logistic/pred_logistic_agesexsmokedrinkethnic_drink0_1634.RData")
#
# # load("knn/pred_knn_submodel_nonocon0_1634.RData")
# # load("logistic/pred_logistic_submodel_nonocon0_1634.RData")
#
# pred.knn.interv <- pred.knn
# pred.log.interv <- pred.log
#
#
#
# load("knn/pred_knn_agesexsmokedrinkethnic_1634.RData")
# load("logistic/pred_logistic_agesexsmokedrinkethnic_1634.RData")
#
# pred.knn.orig <- pred.knn
# pred.log.orig <- pred.log
#
# setwd("../../..")
#
#
# ## KNN
# postscript("./docs/sim_logistic_regn/figures/knn/knn_drink0_comparison.eps")
# plot(pred.knn.orig$Coverage[pred.knn.orig$Coverage!=0], pred.knn.interv$Coverage[pred.knn.orig$Coverage!=0], xlab="Simulated coverage before intervention", ylab="Simulated coverage after intervention", xlim=c(0,0.35), ylim=c(0,0.35))
# abline(a=0,b=1,col=4,lwd=3)
# # text(pred.knn.orig$Coverage, pred.knn.interv$Coverage, rownames(pred.knn.orig))
# dev.off()
#
# ## logistic
# postscript("./docs/sim_logistic_regn/figures/logistic/logistic_drink0_comparison.eps")
# plot(pred.log.orig$Coverage, pred.log.interv$Coverage, xlab="Simulated coverage before intervention", ylab="Simulated coverage after intervention")#, xlim=c(0,0.26), ylim=c(0,0.25))
# abline(a=0,b=1,col=4,lwd=3)
# # text(pred.log.orig$Coverage, pred.log.interv$Coverage, rownames(pred.log.orig))
# dev.off()
#
