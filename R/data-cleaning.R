#' Clean Dummy Names
#'
#' clean names after expanding to
#' dummy variable format
#'
#' @param res
#'
#' @return names
#' @export
#'
#' @examples
#'
cleanDummyNames <- function(res){

    resNames <- unlist(lapply(strsplit(split = "))",colnames(res)), tail, 1))
    resNames <- unlist(lapply(strsplit(split = "train",resNames), tail, 1))
    resNames <- unlist(lapply(strsplit(split = "res.df",resNames), tail, 1))
    resNames <- unlist(lapply(strsplit(split = "data.covshift",resNames), tail, 1))
    resNames <- unlist(lapply(strsplit(split = "fit.model",resNames), tail, 1))
    unlist(lapply(strsplit(split = "test",resNames), tail, 1))
}


#' toupper.fac
#'
#' Coerce dataframe multiple factors to upper case
#'
#' Column-wise operation
#' .
#'
#' @param data dataframe
#' @param names factors (char)
#'
#' @return data dataframe
#' @export
#'
#' @examples
#'
toupper.fac <- function(data, names)
{
    for (i in names) {
        data[,i] <- as.factor(toupper(data[,i]))}
    data
}


#' LA name Clean
#'
#' Remove extra characters
#' and other consistencies
#' from Local Authority names
#'
#' @param datName Vector of LA names
#'
#' @return datName Vector of cleaned LA names
#' @export
#'
#' @examples
#'
LAnameClean <- function(datName){

    datName <- sub("^\\s+", "", datName) #trim leading space
    datName <- sub("\\s+$", "", datName) #trim trailing space
    datName <- gsub(" UA", "", datName, ignore.case = TRUE)
    datName <- gsub(" CC", "", datName, ignore.case = TRUE)
    datName <- gsub(" CD", "", datName, ignore.case = TRUE)
    datName <- gsub(" LB", "", datName, ignore.case = TRUE)
    datName <- gsub(" MCD", "", datName, ignore.case = TRUE)
    datName <- gsub("COUNTY DURHAM", "DURHAM", datName, ignore.case = TRUE)
    datName <- gsub("DURHAM", "COUNTY DURHAM", datName, ignore.case = TRUE)
    datName <- gsub("BRISTOL,( CITY OF)+", "BRISTOL", datName, ignore.case = TRUE)
    datName <- gsub("Bristol", "BRISTOL, CITY OF", datName, ignore.case = TRUE)
    # datName <- gsub("City of Westminster", "City of London", datName) ## TODO ## check same
    datName <- gsub("St Edmundsbury", "ST. EDMUNDSBURY", datName, ignore.case = TRUE)
    datName <- gsub("St Helens", "ST. HELENS", datName, ignore.case = TRUE)
    datName <- gsub("St Albans", "ST. ALBANS", datName, ignore.case = TRUE)
    datName <- gsub("BRIGHTON & HOVE", "BRIGHTON AND HOVE", datName, ignore.case = TRUE)
    datName <- gsub("HEREFORDSHIRE COUNTY OF", "HEREFORDSHIRE, COUNTY OF", datName, ignore.case = TRUE)
    datName <- gsub("Kingston upon Hull,( City of)+", "KINGSTON UPON HULL", datName, ignore.case = TRUE)
    datName <- gsub("KINGSTON UPON HULL", "KINGSTON UPON HULL, CITY OF", datName, ignore.case = TRUE)
    datName <- gsub("Southend on Sea", "SOUTHEND-ON-SEA", datName, ignore.case = TRUE)
    datName <- gsub("City of Westminster", "WESTMINSTER", datName, ignore.case = TRUE)
    datName <- gsub("King\\?s Lynn and West Norfolk", "KING'S LYNN AND WEST NORFOLK", datName, ignore.case = TRUE)
    datName <- gsub("KING`S LYNN AND WEST NORFOLK", "KING'S LYNN AND WEST NORFOLK", datName, ignore.case = TRUE)

    datName
}



#' Fill NA Elements With 1
#'
#' @param x
#'
#' @return x
#' @export
#'
#' @examples
#'
fillEmpty <- function(x){
    fill <- function(y) if(is.na(y)){y <- 1}else{y}
    if(is.list(x)){
        out <- lapply(x, fill)
    }else{out <- fill(x)}
    out
}

#' Replace One Set of Column Names With Another
#'
#' @param array
#' @param name.before
#' @param name.after
#'
#' @return array
#' @export
#'
#' @examples
#'
colNameReplace <- function(array, name.before, name.after){
    stopifnot(is.character(name.before))
    names(array)[names(array)==name.before] <- name.after
    array
}

#' levelsMatch
#'
#'  match-up number and order of levels
#'  in two vectors
#'
#' @param target
#' @param modify
#'
#' @return factor
#' @export
#'
#' @examples
#'
levelsMatch <- function(target, modify)
{
    require(gdata, quietly = TRUE)

    ### add extra levels to
    ### subset variable
    modify <- factor(modify, levels=
                         c(levels(modify), levels(target)[!levels(target)%in%levels(modify)]))
    ### reorder levels
    modify <- reorder.factor(modify, new.order=as.character(levels(target)))

    modify
}
