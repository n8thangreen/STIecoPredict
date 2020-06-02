

#' predict K-Nearest Neighbour
#'
#' k-nerarest neighbours classifier prediction
#' NB only works for numeric covariates
#' careful when some of these are categorical imposing a (numerical) order
#'
#' @param fit
#' @param test
#'
#' @return list

predictKNN <- function(fit, test=FALSE){

    require(caret)
    require(ROCR)

    ## number of neighbours for majority vote
    k <- 1

    set.seed(1)

    ## NB for 16-24 years old knn may not work because the
    ## data is discrete and only takes a few values

    if (test==TRUE){
        dat <- fit$test.datmat
        out <- fit$test.out
    }else{
        dat <- fit$train.datmat
        out <- fit$train.out
    }

    knn.Natsal.pred <- failknn(fit$train.datmat, dat, fit$train.out, k=k, use.all=FALSE, prob=TRUE)

    ## contingency table
    print(ctable <- addmargins(table(knn.Natsal.pred, out), FUN=list(Total=sum), quiet=TRUE))
    # Hmisc::latexTabular(ctable)
    print(confusionMatrix(knn.Natsal.pred, out))

    ## ROC curve
    ##TODO## check
    label <- 1-(2*(as.numeric(fit$train.out)-1))  #rescale to [-1,1]

    # prob <- 1-attr(knn.Natsal.pred, "prob")
    prob <- 1-(as.numeric(knn.Natsal.pred)-1)

    pred_knn <- prediction(prob, label)
    pred_knn <- performance(pred_knn, "tpr", "fpr")

    list(ctable=ctable, pred_knn=pred_knn, knn.Natsal.pred=knn.Natsal.pred)
}




#' scaleknn
#'
#' @param var
#' @param c
#'
#' @return val

scaleknn <- function(var, c){
    ## min-max standardisation

    c*(var - min(var))/(max(var) - min(var))
}



require(plyr)
require(class)
failknn <- failwith(NA, knn)



#' knnDatamatrix
#'
#'  create KNN input matrix
#'  comment-out unwanted variables
#'
#'  LA-level estimates to simulate with
#'  "sparsification" with dummy()
#'
#' @param df
#' @param vars
#' @param c age scaling
#'
#' @return data.matrix

knnDatamatrix <- function(df, vars = "smokenow", c = 0.5){

    require(dummies)

    if("het1yr" %in% vars) {           #full model
        return(data.matrix(with(df,
                                cbind(
                                    #london
                                    #scaledAge=scaleknn(dage, c),
                                    dage,
                                    nonocon,
                                    sex4wks,
                                    het1yr,
                                    smokenow,
                                    rsex,
                                    increasingdrinker,
                                    dummy(ethnic, drop=FALSE)
                                    #income#, #dummy(income),
                                    #scaledIncome=scaleknn(as.numeric(income), c)
                                ))))
    }
    else if ("smokenow" %in% vars) {   #submodel
        return(data.matrix(with(df,
                                cbind(
                                    #london
                                    #scaledAge=scaleknn(dage, c),
                                    dage,
                                    smokenow,
                                    rsex,
                                    increasingdrinker,
                                    dummy(ethnic, drop=FALSE)
                                    #income#, #dummy(income),
                                    #scaledIncome=scaleknn(as.numeric(income), c)
                                ))))
    }else{                          #minimal model
        return(data.matrix(with(df,
                                cbind(
                                    #london
                                    #scaledAge=scaleknn(dage, c),
                                    dage,
                                    rsex
                                ))))
    }
}


