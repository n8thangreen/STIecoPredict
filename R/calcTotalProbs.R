
#' Check Simulated Input Proportions Data
#'
#' Tests the integrity of the input data list for combining into the conditional joint data set.
#'
#' @param input
#'
#' @return prop_props class object

SimInputPropData <- function(input, formula=as.formula(cttestly ~ 1+Sex+Age+drink+smoke+ethnic2+student)){

    stopifnot(as.list(input))
    stopifnot(is.formula(formula))

    var.names <- attr(terms(formula),"term.labels")

    for(i in 1:length(input)){

        if(length(input[[i]])==1){
            stopifnot("value"%in%names(input[[i]]))
            stopifnot(names(input[[i]])%in%var.names)
            var.names <- var.names[!var.names%in%names(input[[i]])]

        }else if(length(input[[i]])==2){
            stopifnot(names(input[[i]][[1]])%in%var.names)
            stopifnot("value"%in%names(input[[i]][[1]]))
            stopifnot(names(input[[i]][[2]])%in%var.names)
            stopifnot("value"%in%names(input[[i]][[2]]))

            var.names <- var.names[!var.names%in%names(input[[i]])]
        }else{
            stop("unexpected list structure")
        }

    }

    stopifnot(length(var.names)==0)

    class(input) <- "pop_props"
}


#' Calculate the Combined (Conditional) Probabilities
#'
#' Assuming risk factors are conditionally independent given age and sex.
#'
#' @param formula Formula object
#' @param data Proportions in categories (array)
#' @param extracols Columns to retain but do nothing with
#'
#' @return data
#'
#' @examples
calcTotalProbs <- function(formula, data=sim_prop_la,
                           extracols = c("LAname","la_code","region_code","region_name","gor")){

    stopifnot(is.formula(formula))

    colnames <- names(data)
    TERMS <- attr(terms(formula),"term.labels")
    p.TERMS <- paste("p.", TERMS, sep="")
    p.TERMS <- unique(gsub("(p.age)|(p.sex)", "p.agesex", p.TERMS)) #combine agesex probability

    stopifnot(all(p.TERMS%in%colnames))

    newcolnames <- c(TERMS, p.TERMS, extracols)
    dropl <- !colnames%in%newcolnames
    p.todrop <- dropl & grepl("^p\\.", names(data))
    factors.todrop <- gsub("p.", "", colnames[p.todrop])

    ## identify first level of each variable to drop because repetition
    # singlelevels <- apply(data[ ,factors.todrop, drop=FALSE], 2, function(x) levels(as.factor(x))[1]) ##TODO## why adds extra leading space??
    singlelevels <- data[1, factors.todrop, drop=FALSE]

    len.todrop <- length(factors.todrop)
    if(len.todrop>0){
        data <- merge(data, singlelevels, al.x=FALSE, all.y=FALSE)
    }

    data$totalprob <- apply(data[ ,p.TERMS, drop=FALSE], 1, prod)
    # stopifnot(aggregate(totalprob, probs_levels_array$LAname, sum))

    data <- data[ ,c(newcolnames, "totalprob")]

    return(data)
}

