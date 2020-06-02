


#' Covariate Shift
#'
#' Importance sampling approach
#' when different distributions for the
#' training and test data
#'
#' @param data
#' @param resla
#' @param riskfac
#' @param ssize
#'
#' @return data

covariateShift <- function(data, resla, riskfac, ssize=10000){

    require(plyr)

    Natsal.riskfac.table <- DistnTable(data, riskfac)
    Natsal.riskfac.table <- colNameReplace(Natsal.riskfac.table, "(all)", "Natsalfreq")

    res.df <- ldply(resla, data.frame)
    LA.riskfac.table <- DistnTable(res.df, riskfac)
    LA.riskfac.table <- colNameReplace(LA.riskfac.table, "(all)", "LAfreq")

    data.freq <- merge(LA.riskfac.table, Natsal.riskfac.table, by=riskfac)
    data.freq <- transform(data.freq, ratio = LAfreq/Natsalfreq)
    data.freq$ratio[is.na(data.freq$ratio)] <- 0

    datat <- merge(data, data.freq, by=riskfac)

    set.seed(1968)
    sampleRows <- sample(1:nrow(datat), prob=datat$ratio, replace=TRUE, size=ssize)
    data.adj <- datat[sampleRows,]
    rownames(data.adj) <- NULL

    data.adj
}


#' Covariate Shift GLM
#'
#'  alternative model-based approach:
#'  could fit a logistic regression to estimate the ratio of probabilities of each data set
#'  and then predict for (all) permutations
#'  http://blog.smola.org/post/4110255196/real-simple-covariate-shift-correction
#'
#' @param data
#' @param resla
#' @param riskfac
#' @param ssize
#'
#' @return data

covariateShift.glm <- function(data, resla, riskfac, ssize=10000){

    require(plyr)

    res.df <- ldply(resla, data.frame)
    res.df <- cbind(res.df, out=TRUE)
    comb.df <- cbind(data, out=FALSE)
    comb.df <- rbind(comb.df[,c(riskfac,"out")], res.df[,c(riskfac,"out")])

    formula <- as.formula(paste("out ~ ", paste(riskfac, collapse="+"), sep=""))

    wt <- c(rep(1/nrow(data), nrow(data)), rep(1/nrow(res.df), nrow(res.df)))
    fit <- glm(formula, family=binomial, data=comb.df, weight=wt)

    #   grid <- expand.grid(apply(rdata[,riskfac], 2, unique))
    data$odds <- exp(predict(fit, newdata=data, type="link"))

    set.seed(1968)
    sampleRows <- sample(1:nrow(data), prob=data$odds, replace=TRUE, size=ssize)
    data.adj <- data[sampleRows,]
    rownames(data.adj) <- NULL

    data.adj
}
