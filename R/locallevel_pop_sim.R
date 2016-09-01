

#   NATSAL.dat <- read.csv("C:/Users/nathan.green/Documents/Chlamydia/data/NATSAL/Natsal-3_extract_2April2014.csv")

#' Local Level Population Simulation
#'
#' Combined LA level data for single risk factors and non-LA but multiple risk factor data sets.
#' Simulate a sample, for each LA using the reference tables.
#' Makes relative (to national average) LA-level prevalence adjustments
#'
#' @param NATSAL.dat NATSAL study data extract
#' @param n Number of individuals per LA to simulate
#' @return list by each LA
#'
#' @examples
#'
locallevel_pop_sim <- function(NATSAL.dat=NA, n=100){

  require(plyr, quietly = TRUE)
  require(class, quietly = TRUE)

  if(!exists("ageRange")){ageRange <- 16:24}

  ind <- res <- list()

  ## match LA names in LA-level and national data sets
  # laNames <- sort(unique(popLAagesex.dat$Name[popLAagesex.dat$country=="E"]))
  # laNames <- sort(unique(popLAagesex.dat$lad2012_code[popLAagesex.dat$country=="E"]))
  laNames <- toupper(smokingLA.dat$Name)[toupper(smokingLA.dat$Name)%in%toupper(unique(popLAagesex.dat$Name))]
  warning("Places dropped because names don't match:", smokingLA.dat$Name[!smokingLA.dat$Name%in%unique(popLAagesex.dat$Name)], noBreaks. = FALSE)

  ## match Natsal-3 and reference table ethnic group names
  NATSALethnic.names <- toupper(levels(NATSAL.dat$ethnic)[levels(NATSAL.dat$ethnic)!="Not answered"])

  ## ONS table ethnic groups
  # unlist(lapply(strsplit(unique(
  #   unlist(lapply(strsplit(names(ethnicityLA.dat[["male"]]), "Ethnic Group: "), function(x) x[2]))), ";"), function(x) x[1]))

  ## income group relabelling lookup table
  NATSALincome.names <- c("<2,500","2,500-4,999","5,000-9,999","10,000-19,999","20,000-29,999","30,000-39,999","40,000-49,999","50,000+")
  income.breaks <- c(-1000, 2500, 5000, 10000, 20000, 30000, 40000, 50000, 1e+06)
  names(NATSALincome.names) <- cut(income.breaks+1, income.breaks, right = FALSE)[-(length(NATSALincome.names)+1)]

  set.seed(1968)

  ################
  ## simulation ##
  ################

  for (la in laNames){

    print(la)
    print(paste(which(la==laNames), "of", length(laNames)))

    ## stratified population
    ## by la and age
    subpop <- (popLAagesex.dat$Name==la) & (popLAagesex.dat$Age%in%ageRange)
    size.subpop <- sum(subpop)

    ## relative (to national average) LA-level prevalence adjustments
    ## --------------------------------------------------------------
    ## NB >=18 proxy for all ages of interest

    adj.smoke  <- with(smokingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
    adj.drink  <- with(drinkingLA.dat, Indicator.value[Name==la]/Indicator.value[Name=="ENGLAND"])
    adj.income <- lapply(incomeLA.dat, function(x) x$Mean[x$LA_Name==la]/x$Mean[x$LA_Name=="ENGLAND"])  #Men, Women

    ## if data not available
    adj.smoke  <- fillEmpty(adj.smoke)
    adj.drink  <- fillEmpty(adj.drink)
    adj.income <- fillEmpty(adj.income)

    ## sample AGE & SEX
    ## ----------------
    popsample <- sample(1:size.subpop, size=n, prob=popLAagesex.dat[subpop, "2012"], replace=TRUE)
    age <- popLAagesex.dat[subpop,"Age"][popsample]
    sex <- popLAagesex.dat[subpop,"Sex"][popsample]

    ## change from factor
    age.char <- as.character(age)
    sex.char <- as.character(sex)

    agegrp <- convertAge2ageGroup(age)

    rv.drink <- runif(n)
    covariate_names <- c("dage", "rsex", "smokenow", "increasingdrinker", "ethnic", "income")
    ind <- data.frame(matrix(NA, nrow = n, ncol = length(covariate_names)))
    names(ind) <- covariate_names

    for (i in 1:n){

      ## sample SMOKING status
      ## ---------------------
      psmoke <- adj.smoke * smokingage.dat[smokingage.dat$Age==age.char[i] &
                                             smokingage.dat$Sex==sex.char[i], "2010"]/100

      ## assign smoking status for England-wide age groups
      smoke <- sample(c("YES", "NO"), 1, prob=c(psmoke,1-psmoke))
      # drink <- rv.smoke[i]<=psmoke

      ## sample DRINKING status
      ## ----------------------
      pdrink <- adj.drink * drinkingage.dat[drinkingage.dat$Age==age.char[i] &
                                              # drinkingage.dat$Freq=="one or more" &
                                              drinkingage.dat$Units%in%c("more than 4 per day", "more than 3 per day") &
                                              drinkingage.dat$Sex==sex.char[i], "2011"]/100

      ## assign drinking status for England-wide age groups
      drink <- rv.drink[i]<=pdrink


      ## sample ETHNICITY
      ## ----------------

      ## sample ethnicity from new table
      sset <- ethnicityLA.melt[[sex[i]]][ethnicityLA.melt[[sex[i]]]$LA_Names==la &
                                           ethnicityLA.melt[[sex[i]]]$age==agegrp[i],]
      pethnic <- sset$value
      #     nethnic <- sset$NATSAL  #original ethnic groups
      nethnic <- sset$ethnic2   #amalgamated ethnic groups

      ethnic <- sample(nethnic, 1, prob=pethnic)

      ## sample INCOME
      ## -------------
      #     income <- adj.income[[sex[i] ]] * incomeage.dat$median.income[incomeage.dat$Age==age.char[i] &
      income <- adj.income[[sex[i] ]] * incomeage.dat$mean.income[incomeage.dat$Age==age.char[i] &
                                                                    incomeage.dat$Sex==sex.char[i]]

      ##TODO## could add error to the estimate...
      #     income <- sample(levels(NATSAL.dat$income), 1, prob=)

      ## convert to income interval and consistent format
      incomegrp <- cut(income, breaks=income.breaks, right=FALSE)
      incomegrp <- as.character(NATSALincome.names[incomegrp])


      ind[i,] <- c(max(ageRange)-age[i],
                   sex[i],
                   smoke,
                   drink,
                   ethnic,
                   incomegrp)
    }

    res[[as.character(la)]] <- ind
  }

  invisible(res)
}
