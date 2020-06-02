##
## data preprocessing
## and cleaning functions
## classifier_fns.R
##
## N Green
## June 2014




#' Fit GLM on Survey (Natsal) data
#'
#' Create regression variables and
#' binds them together into a list
#'
#' @param data.train Training/fitting data
#' @param data.test Test data
#' @param riskfac Risk factors
#' @param depvar Dependant variable name
#'
#' @return list of fits
#' @export
#'
#' @examples
#'
fitModel <- function(data.train, data.test, riskfac, depvar){

  depvar <- depvar[1]

  formula <- as.formula(paste(depvar, " ~ ", paste(riskfac, collapse = "+"), sep = ""))

  data.train <- data.train[ ,c(depvar, riskfac)]
  data.test  <- data.test[ ,c(depvar, riskfac)]

  data.train <- subset(data.train, !depvar %in% c("NOT ANSWERED","NOT APPLICABLE"))
  data.test  <- subset(data.test, !depvar %in% c("NOT ANSWERED","NOT APPLICABLE"))

  ndata <- min(nrow(data.train), nrow(data.test), 50000)

  data.train <- data.train[1:ndata,]
  data.test  <- data.test[1:ndata,]

  data.train <- droplevels(data.train)
  data.test  <- droplevels(data.test)

  train.out <- data.train[,depvar]
  test.out  <- data.test[,depvar]

  ## LA-level estimates to simulate with
  ### "sparsification" with dummy()
  train.datmat <- knnDatamatrix(data.train)
  test.datmat  <- knnDatamatrix(data.test)

  ## clean column names created in dummy()
  colnames(train.datmat) <- cleanDummyNames(train.datmat)
  colnames(test.datmat)  <- cleanDummyNames(test.datmat)

  ##TODO##
  ## could include the Natsal weights directly in likelihood
  fit <- tryCatch(glm(formula, family=binomial, data=data.train), error = function(e){})

list(logisticfit=fit, data.train=data.train, data.test=data.test,
     train.datmat=train.datmat, test.datmat=test.datmat,
     train.out=train.out, test.out=test.out, formula=formula)
}


#' Distribution Table
#'
#' frequency table as dataframe
#' of discrete joint distribution
#'
#' @param data
#' @param riskfac
#'
#' @return array
#' @export
#'
#' @examples
#'
DistnTable <- function(data, riskfac){

  require(reshape)

  data$Freq <- 1
  formula <- paste(paste(riskfac, collapse = "+"), "~.", sep="")
  out <- cast(data, formula, fun=sum, value="Freq")
  out[,"(all)"] <- out[,"(all)"]/sum(out[,"(all)"])
  out
}


#' Predict Logistic
#'
#' predict coverage from
#' logistic classifier model
#'
#'
#' @param fit
#' @param data
#' @param ADD add to existing plot
#'
#' @return
#' @export
#'
#' @examples
#'
predict.lgstc <- function(fit, data, ADD=F){


  require(Hmisc)
  require(ROCR)
  require(caret)

  fit.probs <- predict(fit, newdata = data, type = "response")

  fit.pred  <- rep("NO", nrow(data))
  fit.pred[fit.probs > 0.5] <- "YES"
  print(paste("mean coverage ", mean(fit.probs)))

  ## contingency tables
  print(Hmisc::latexTabular(
    ctable <- addmargins(table(pred=fit.pred, data=data$cttestly), FUN=list(Total=sum), quiet=TRUE)))
#   print(ctable)
  print(confusionMatrix(fit.pred, data$cttestly))

  ## ROC curve
  pred <- prediction(fit.probs, data$cttestly)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, colorize=F, add=ADD)
}


#' changeToArrayIndiv
#'
#' Warning: slow
#'
#' @param res
#'
#' @return res
#' @export
#'
#' @examples
#'
changeToArrayIndiv <- function(res){
  require(plyr)
  res.df <- list()
  for (la in names(res)){
    res.df[[la]] <- ldply(res[[la]], data.frame)
  }
  res.df
}


#' scatterplotObsEst
#'
#' @param CTADGUM_pred
#' @param names
#' @param points
#'
#' @return plot
#' @export
#'
#' @examples
#'
scatterplotObsEst <- function(CTADGUM_pred, names, points){

  out <- ggplot(CTADGUM_pred, aes(x=`Combined Coverage`, y=`Coverage_knn`)) +
    #   ggplot(CTADGUM_pred, aes(x=`Combined Coverage`, y=Coverage*prop.TP.pred)) +
    #   geom_point(aes(size = Population)) +

    #   scale_size_area() +
    #   geom_point(shape=1) +    # Use hollow circles

    ylab("Estimated Coverage") + xlab("Observed Coverage") +
    #xlim(0.1, 0.4) +
    #ylim(0.1, 0.2) +
    geom_abline(linetype="dashed", size=1) + # line y=x

    theme(text = element_text(size=20)) +  #font size

    ## Natsal-3 fit point
    #   geom_point(aes(y=true.pos, x=tot.pos.obs), shape=17, size=4, col="blue") +
    #   geom_point(aes(y=tot.pos.pred, x=tot.pos.obs), shape=17, size=4, col="red") +

    theme(panel.background = element_rect(fill='white', colour='black')) +


    ## from surveillance data
    geom_abline(intercept=mean(CTADGUM_pred$`Combined Coverage`, na.rm=TRUE), slope=0, linetype="dotted", col="darkgreen", lwd=1.2) +
    geom_abline(intercept=mean(CTADGUM_pred$`Coverage_knn`, na.rm=TRUE), slope=0,  linetype="dotdash", col="darkgreen", lwd=1.2)


  ## error bounds
  #   geom_abline(intercept=a.diff, linetype="dotted") +
  #   geom_abline(intercept=-a.diff, linetype="dotted") +
  #
  #   geom_abline(slope=1/prop.TP.obs , linetype="dotdash") +
  #   geom_abline(slope=prop.TP.obs , linetype="dotdash") +
  #

  #   geom_text(data=CTADGUM_pred[la.outside.fix,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.fix]) +
  #   geom_text(data=CTADGUM_pred[la.outside.regn,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.regn], colour="red", fontface="bold") +
  #   geom_text(data=CTADGUM_pred[la.outside.prop,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.prop], colour="blue", fontface="bold")

  # geom_point(aes(x=CTADGUM_pred$`Combined Coverage`, y=ComCov.pred))

  ## text point annotation
  if(names==TRUE){out <- out + geom_text(data=CTADGUM_pred, label=CTADGUM_pred$`LA Name`, size=3) }  #all LAs

  if(points=="IMD"){out <- out +
                      #geom_smooth(method=lm) +
                      geom_point(aes(size = `IMD`))
  }
  if(points=="density"){out <- out +
                          #geom_smooth(method=lm) +
                          geom_point(aes(size = `Population density`))
  }
  if(points=="house"){out <- out +
                        #geom_smooth(method=lm) +
                        geom_point(aes(size = `House prices/earnings`))
  }
  if(points=="class"){out <- out +
                        aes(shape=Classification, col=Classification) +
                        geom_point(size=3)
                      #geom_text(data=CTADGUM_pred, label=CTADGUM_pred$`Classification`)
  }

  out
}


#' Residual Sum of Squares
#'
#' @param data1
#' @param data2
#' @param data3
#'
#' @return rss
#' @export
#'
#' @examples
#'
rss <- function(data1,
                data2,
                data3=NULL){

  if(is.null(data3)) rss <- sum((data1-data2)^2)
  else{
    diff <- 1:length(data1)
    for (i in 1:length(data1)){
      if(data3[i]>data1[i]) diff[i] <- data3[i]-data1[i]
      else if(data3[i]<data2[i]) diff[i] <- data2[i]-data3[i]
      else diff[i] <- 0
    }
    rss <- sum(diff^2)
  }

  rss
}



