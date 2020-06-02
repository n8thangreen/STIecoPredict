
#' Convert Age to Age Group
#'
#' @param age (integer)
#'
#' @return character strings of categories
#' @export
#'
#' @examples
#'
convertAge2ageGroup <- function(age){

    ONSagegrps <- c("15", "16 to 17", "18 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39")
    age.breaks <- c(14,15,17,19,24,29,34,39)
    names(ONSagegrps) <- levels(cut(age.breaks+1, age.breaks, right=TRUE))

    agegrp <- as.character(sapply(age, function(x)
        ONSagegrps[cut(x, breaks=age.breaks, right=TRUE)]))
    agegrp
}

#' Convert Age to Age Group Breaks 16-19, 20-21, 22-24
#'
#' @param age (integer)
#'
#' @return character strings of categories
#' @export
#'
#' @examples
#'
convertAge2ageGroup2 <- function(age){

    ONSagegrps <- c("16 to 19", "20 to 21", "22 to 24")
    age.breaks <- c(15,19,21,24)
    names(ONSagegrps) <- levels(cut(age.breaks+1, age.breaks, right=TRUE))

    agegrp <- as.character(sapply(age, function(x)
        ONSagegrps[cut(x, breaks=age.breaks, right=TRUE)]))
    agegrp
}

#' Convert Income to Income Group
#'
#' @param income (integer)
#'
#' @return character strings of categories
#' @export
#'
#' @examples
#'
convertIncome2incomeGroup <- function(income){

    NATSALincome.names <- c("<2,500","2,500-4,999","5,000-9,999","10,000-19,999","20,000-29,999","30,000-39,999","40,000-49,999","50,000+")
    income.breaks <- c(-1000, 2500, 5000, 10000, 20000, 30000, 40000, 50000, 1e+06)
    names(NATSALincome.names) <- levels(cut(income.breaks+1, income.breaks, right = FALSE))

    incomegrp <- cut(income, breaks=income.breaks, right=FALSE)
    incomegrp <- as.character(NATSALincome.names[incomegrp])
}

#' Generic Convert to Group
#'
#' @param ctsdata
#' @param groups
#' @param breaks
#'
#' @return character strings of categories
#' @export
#'
#' @examples
#'
convert2Groups <- function(ctsdata, groups, breaks){

    names(groups) <- levels(cut(breaks+1, breaks, right=TRUE))
    res <- as.character(sapply(ctsdata, function(x) groups[cut(x, breaks=breaks, right=TRUE)]))
    res
}

