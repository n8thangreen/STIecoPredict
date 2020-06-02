
#' Predict with Classifier LA reponse variable values
#'
#' Classification prediction using
#' LA individual simulated data and fitted models
#'
#' @param fit.model Output from \code{modelfit()}
#' @param drink0 Intervention hypothetical impact of no alcohol drinking
#'
#' @return list
#' @export
#'
#' @examples
#'
classPredict <- function(fit.model, drink0=FALSE){

  require(plyr, quietly = TRUE)

  set.seed(1234)

  ## initialise logistic & k-nearest neighbours prediction arrays

  res <- fit.model$res
  lenres   <- length(res)
  laNames  <- names(res)
  pred.log <- pred.knn <- data.frame(matrix(nrow=lenres, ncol=1))
  colnames(pred.log) <- colnames(pred.knn) <- "Coverage"
  rownames(pred.log) <- rownames(pred.knn) <- names(res)
  fit.probs <- fit.knn <- cumfreq <- vector("list", lenres)
  cum.seq <- seq(0, 1, by=0.01)

  vars <- attr(fit.model[[1]]$logisticfit$terms,"term.labels")

  for (la in laNames){

    if(la%in%names(fit.model)){
      fit <- fit.model[[la]]
    }else{
      fit <- fit.model}

    print(la)
    print(paste(which(la==laNames), "of", length(laNames)))

    ## convert from list to dataframe
    res.df <- ldply(res[[la]], data.frame)

    ## interventions
    if(drink0==TRUE){
      res.df$increasingdrinker <- FALSE
#       res.df$nonocon <- 0
    }

    ## match factor levels with training data/logistic fit
#     res.df$income <- levelsMatch(fit$data.train$income, res.df$income)
    res.df$ethnic <- levelsMatch(fit$data.train$ethnic, res.df$ethnic)
    res.df$rsex   <- levelsMatch(fit$data.train$rsex, res.df$rsex)
    res.df$smokenow <- levelsMatch(fit$data.train$smokenow, res.df$smokenow)


    ##############
    ## logistic ##
    ##############

    fit.probs[[la]] <- tryCatch(predict(fit$logisticfit, newdata=res.df, type="response"), error=function(e){NA})

    ## cumulative probabilities
    fit.cut  <- cut(fit.probs[[la]], cum.seq)
    fit.freq <- table(fit.cut)
    cumfreq[[la]] <- c(0, cumsum(fit.freq))/nrow(res.df)

    #pred.log[la, "Coverage"] <- as.numeric(sum(fit.probs[[la]]>=0.5)/length(fit.probs[[la]]))      #rounding before averaging
    pred.log[la, "Coverage"] <- mean(fit.probs[[la]], na.rm=TRUE)     #no rounding


    #########
    ## KNN ##
    #########

    ## convert to numeric array
    ## including nominal "sparsification"

    res.datmat <- knnDatamatrix(res.df, vars)

    colnames(res.datmat) <- cleanDummyNames(res.datmat)
    colnames(fit$train.datmat) <- cleanDummyNames(fit$train.datmat)

    fit.knn[[la]] <- tryCatch(knn(fit$train.datmat[,colnames(res.datmat)], res.datmat, fit$train.out, k=5), error=function(e){})

    pred.knn[la,"Coverage"] <- as.numeric(sum(fit.knn[[la]]=="YES")/nrow(res.df))

  }


  save(fit.probs,cumfreq,cum.seq,pred.log, file="C:/Users/nathan.green/Documents/chlamydia/classifier/data/output/predictions/pred_logistic_TEMP.RData")
  save(fit.knn,pred.knn, file="C:/Users/nathan.green/Documents/chlamydia/classifier/data/output/predictions/pred_knn_TEMP.RData")

list(fit.probs=fit.probs, cumfreq=cumfreq, cum.seq=cum.seq, pred.log=pred.log,
     fit.knn=fit.knn, pred.knn=pred.knn)
}

