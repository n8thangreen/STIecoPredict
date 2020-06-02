##
## Natsal chlamydia regression and post-stratification
## following Gelman and MRP approach
## Feb 2016

## data preparation


library(mrp)
library(STIecoPredict)
# library(mrpdata)
library(stats)
library(plyr)
library(stringr)
library(reshape2)
library(Hmisc)

### PHE
## load in LA level data (i.e. census and administrative data)
# LApop <- read.csv("H:/old pc/input_data/ONS/pop_age_sex_LA.csv", check.names = FALSE)
# LAclass <- read.csv("H:/old pc/input_data/LAclassification/LAClassification.csv", check.names = FALSE)
# LAsmoke <- read.csv("H:/old pc/input_data/risk_factors/smoking/raw_data/Adultssmoking_LA.csv", check.names = FALSE)
# student <- read.csv("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/STIecoPredict/data/student-pop-201314.csv") #https://www.hesa.ac.uk/

## load in survey data (i.e. individual Natsal)
# Natsal <- read.csv("H:/old pc/input_data/NATSAL/Natsal-3_extract_2April2014.csv", check.names = FALSE)
# load("H:/old pc/input_data/NATSAL/gor_strata/sin2-gors.RData")

## load estimated risk factor cross-tabulation probabilities
# load("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/packages/STIecoPredict/data/sim_prop.RData")

# load("C:/Users/nathan.green/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/raw-input-mrpNatsal.RData")

# LA_ethnic2.male <- read.csv("C:/Users/nathan.green.PHE/Dropbox/small-area & chlamydia/R_code/packages/STIecoPredict/raw-data/CT0122 - Age by ethnic group male.csv", check.names = F)
# LA_ethnic2.female <- read.csv("C:/Users/nathan.green.PHE/Dropbox/small-area & chlamydia/R_code/packages/STIecoPredict/raw-data/CT0122 - Age by ethnic group female.csv", check.names = F)

# GUMclinic_LAs <- read.csv("data/GUMclinic_LAs.csv")


devtools::load_all(".")
setwd(system.file(package = "STIecoPredict"))

# NAtsal data set
## UK data archive download
load(file = "../data/eul_natsal_2010_for_archive.RData")
load("data/raw-data-mrpNatsal.RData")


###################
## harmonisation ##
###################

## ------
## Natsal
## ------

if (names(Natsal)[1] == "X") names(Natsal)[1] <- "id"

Natsal <- merge(Natsal, sin2.gors, by = "sin2", all.x = TRUE)
Natsal <- merge(Natsal, sin2.LA, by = "sin2", all.x = TRUE)
Natsal <- merge(Natsal, sin2_household, by = "sin2", all.x = TRUE)
# Natsal <- merge(Natsal, eul_natsal_2010_for_archive[ ,c("sin2","chtstwh1","chltstwh")], by="sin2", all.x=TRUE)


Natsal$laname <- LAnameClean(Natsal$laname)
Natsal$laname <- toupper(Natsal$laname)

Natsal <- within(Natsal, {
  sex <- factor(rsex, exclude = NA, labels = c("Women","Men"))
  sex <- relevel(sex, "Men")    #Men as baseline

  age <- as.numeric(dage)

  smokenow <- factor(smokenow, exclude = NA)
  smokenow <- relevel(smokenow, "No")

  student <- rnssecgp_4 == "Student in full-time education"

  ## age >44 or totlife=0 (virgin)
  cttestly.notapplicable <- (cttestly == "not applicable")

  cttestly[cttestly == "not applicable"] <- "no"
  cttestly <- as.character(cttestly)
  cttestly[cttestly == "no"]  <- 0
  cttestly[cttestly == "yes"] <- 1
  cttestly <- as.integer(cttestly)

  # cttestly <- droplevels(cttestly)
  # cttestly <- factor(cttestly, exclude=NA,
  #                    labels=c(FALSE, TRUE))

  drinkoft[drinkoft == "not applicable"] <- "Not at all in the last 12 months"

  increasingdrinker <- (manyalc2 %in% c("3 or 4","5 or 6", ">6") & rsex == "Female" &
                         drinkoft %in% c("Once/twice a week", "3/4 days a week", "5+ days a week")) |
                       (manyalc2 %in% c("3 or 4","5 or 6", ">6") & rsex == "Male" &
                         drinkoft %in% c("Once/twice a week", "3/4 days a week", "5+ days a week"))

  ethnic2 <- factor(ethnicgrp, levels = c("White", "Asian/Asian British", "Black/Black British", "Chinese", "Mixed", "Other", "Not answered"))
  levels(ethnic2) <- toupper(levels(ethnic2))

  manyalc2 <- factor(manyalc2, levels = c("1 or 2", "3 or 4", "5 or 6", ">6", "Other"))

  region_names <- plyr::revalue(as.character(gor), c(`1` = "North East",
                                                     `2` = "North West",
                                                     # `3`="notused",
                                                     `4` = "Yorkshire and The Humber",
                                                     `5` = "East Midlands",
                                                     `6` = "West Midlands",
                                                     `7` = "South West",
                                                     `8` = "East",
                                                     `9` = "London",
                                                     `10` = "South East",
                                                     `11` = "Wales",
                                                     `12` = "Scotland"))

  sex.age <- interaction(sex, age)
  age.scaled <- scale(as.numeric(dage))
  total_wt.int <- round(total_wt*1000)
})

Natsal$smokenow <- as.logical(plyr::revalue(Natsal$smokenow, replace = c("No" = FALSE, "Yes" = TRUE)))

## only remove if including in analysis
## going to assume that this is MAR for the moment
Natsal <- subset(Natsal, cttestly != "not answered")
# Natsal <- subset(Natsal, drinkoft!="Not answered")
Natsal <- subset(Natsal, gor != 11 & gor != 12) #England only
Natsal <- subset(Natsal, ethnic2 != "NOT ANSWERED")
# Natsal <- subset(Natsal, age>15 & age<25)


## group-level covariates ##

## ONS urban-rural
Natsal <- merge(Natsal, LAclassification.dat[,c("LA Name", "Numerical classification")],
                by.x = "laname", by.y = "LA Name", all.x = TRUE)
## density
Natsal <- merge(Natsal, popLA.dat[,c("LA Name", "Density")],
                by.x = "laname", by.y = "LA Name", all.x = TRUE)
## IMD
Natsal <- merge(Natsal, IMD.dat2010[,c("LA Name", "Average Score")],
                by.x = "laname", by.y = "LA Name", all.x = TRUE)
## metcounty_UA
Natsal <- merge(Natsal, laregionlookup2013[,c("la_name","metcounty_UA")],
                by.x = "laname", by.y = "la_name", all.x = TRUE)

Natsal$metcounty_UA <- toupper(Natsal$metcounty_UA)
Natsal$metcounty_UA.val <- as.numeric(as.factor(Natsal$metcounty_UA))

## GUM clinic
GUMclinic_LAs.freq <- data.frame(table(GUMclinic_LAs$LA.Name))
names(GUMclinic_LAs.freq) <- c("LA.Name", "FreqGUM")
GUMclinic_LAs.freq$LA.Name <- toupper(GUMclinic_LAs.freq$LA.Name)
GUMclinic_LAs.freq$LA.Name <- LAnameClean(GUMclinic_LAs.freq$LA.Name)

nonGUM_LAnames <- unique(Natsal$laname[!Natsal$laname %in% GUMclinic_LAs.freq$LA.Name])
GUMclinic_LAs.freq <- rbind(GUMclinic_LAs.freq,
                            data.frame(LA.Name = nonGUM_LAnames, FreqGUM = 0))

Natsal <- merge(Natsal, GUMclinic_LAs.freq,
                by.x = "laname", by.y = "LA.Name", all.x = TRUE)


## ---------------------------------------
## if only want to predict same areas
# LApop <- na.omit(LApop[LApop$gor_l%in%Natsal$gor_l,])

LApop <- subset(LApop, Age > 15 & Age < 75)
LApop <- subset(LApop, country == "E")

LApop <- within(LApop,{
  sex <- Sex
  sex <- relevel(sex, "Men")

  age <- Age
  pop <- `2012`

  sex.age <- interaction(sex, age)
})
LApop <- na.omit(LApop)


#  census data ------------------------------------------------------------------

## create new binary valued columns from success probabilities
## for consistent format across variables

##TODO## or if use joint_covariate_input_data list output, rewrite sim_pop (sim_prop,class) so this is already done

odd_rows <- function(sim) seq(1, nrow(sim), by = 2)
duplicate_rows <- function(sim) rep(1:nrow(sim), each = 2)

for (i in 1:length(sim_prop)) {

  sim <- sim_prop[[i]]

  sim <- sim[duplicate_rows(sim), ]
  
  sim[odd_rows(sim), "p.increasingdrinker"] <- 1 - sim[odd_rows, "p.increasingdrinker"] #subtract prob from one
  
  sim$increasingdrinker <- rep(c(FALSE, TRUE), length = nrow(sim))  #create factor level column

  sim <- sim[duplicate_rows(sim), ]
  
  sim[odd_rows(sim),"p.smokenow"] <- 1 - sim[odd_rows(sim), "p.smokenow"]
  
  sim$smokenow <- rep(c(FALSE, TRUE), length = nrow(sim))

  sim <- sim[duplicate_rows(sim), ]
  
  sim[odd_rows(sim),"p.student"] <- 1 - sim[odd_rows(sim), "p.student"]
  
  sim$student <- rep(c(FALSE, TRUE), length = nrow(sim))

  sim <- sim[duplicate_rows(sim),]
  
  sim[odd_rows(sim),"p.livealone"] <- 1 - sim[odd_rows(sim), "p.livealone"]
  
  sim$livealone <- rep(c(FALSE, TRUE), length = nrow(sim))

  sim_prop[[i]] <- sim
}


## convert to long format
sim_prop_la <- plyr::ldply(sim_prop, .id = "LAname")

sim_prop_la <- within(sim_prop_la, {
  age <- as.numeric(as.character(dage))
  sex <- rsex
})

## compare
str(Natsal[,c("sex","age","ethnic2","smokenow","hhsize","increasingdrinker","student")])
str(sim_prop_la[,c("sex","age","ethnic2","livealone","smokenow","increasingdrinker","student")])


#  age-sex only population data --------------------------------------------------

LApop <- subset(LApop, age > 15 & age < 25)

# rescale each LA to sum probabilities to 1 for new age range
LApop$pop.adj <- LApop$pop/sapply(LApop$Name, function(name) sum(LApop[LApop$Name == name,"pop"]))
sapply(unique(LApop[,"Name"])[1:30], function(name) sum(LApop[LApop$Name == name,"pop.adj"])) #LA population totals adjusted test


#  include LA area level variables -----------------------------------------------------

laregionlookup2013$region_name <- toupper(laregionlookup2013$region_name)
laregionlookup2013$region_name <- factor(laregionlookup2013$region_name,
                                         levels = c("NORTH EAST", #1
                                                    "NORTH WEST", #2
                                                    "NOTUSED", #3
                                                    "YORKSHIRE AND THE HUMBER", #4
                                                    "EAST MIDLANDS", #5
                                                    "WEST MIDLANDS", #6
                                                    "SOUTH WEST", #7
                                                    "EAST", #8
                                                    "LONDON", #9
                                                    "SOUTH EAST", #10
                                                    "WALES", #11
                                                    "SCOTLAND")) #12

laregionlookup2013$gor <- as.numeric(laregionlookup2013$region_name)

laregionlookup2013$la_name <- toupper(laregionlookup2013$la_name)

sim_prop_la <- merge(sim_prop_la, laregionlookup2013,
                     by.x = "LAname", by.y = "la_name")
# table(sim_prop_la$gor, useNA = "always") #test

sim_prop_la$metcounty_UA <- toupper(sim_prop_la$metcounty_UA)

sim_prop_la$LAname <- LAnameClean(sim_prop_la$LAname)

LAclassification.dat$`LA Name` <- LAnameClean(LAclassification.dat$`LA Name`)
sim_prop_la <- merge(sim_prop_la, LAclassification.dat[,c("LA Name", "Numerical classification")],
                     by.x = "LAname", by.y = "LA Name", all.x = TRUE)

IMD.dat2010[ ,"LA Name"] <- LAnameClean(IMD.dat2010[ ,"LA Name"])
sim_prop_la <- merge(sim_prop_la, IMD.dat2010[ ,c("LA Name","Average Score")],
                     by.x = "LAname", by.y = "LA Name", all.x = TRUE)

popLA.dat[ ,"LA Name"] <- LAnameClean(popLA.dat[ ,"LA Name"])
sim_prop_la <- merge(sim_prop_la, popLA.dat[,c("LA Name", "Density")],
                     by.x = "LAname", by.y = "LA Name", all.x = TRUE)

## GUM clinics
nonGUM_LAnames <- unique(sim_prop_la$LAname[!sim_prop_la$LAname %in% GUMclinic_LAs.freq$LA.Name]) #don't strictly need to do this because only post-stratify LAs in Natsal anyway
GUMclinic_LAs.freq <- rbind(GUMclinic_LAs.freq, data.frame(LA.Name = nonGUM_LAnames, FreqGUM = 0))
sim_prop_la <- merge(sim_prop_la, GUMclinic_LAs.freq,
                     by.x = "LAname", by.y = "LA.Name", all.x = TRUE)


## <=18 years old conception rate
conception.under18[ ,"Name"] <- LAnameClean(conception.under18[ ,"Name"])
conception.under18[ ,"Name"] <- toupper(conception.under18[ ,"Name"])
conception.under18 <- conception.under18[!duplicated(conception.under18), ]

sim_prop_la <- merge(sim_prop_la, conception.under18[ ,c("Name", "Conception.rate.per.1.000.women.in.age.group")],
                     by.x = "LAname", by.y = "Name", all.x = TRUE)

sim_prop_la$Conception.deciles <- cut2(sim_prop_la$Conception.rate.per.1.000.women.in.age.group, g = 10)

interiorcuts <- cut2(sim_prop_la$Conception.rate.per.1.000.women.in.age.group, g = 10, onlycuts = TRUE)

Natsal <- merge(Natsal, conception.under18[ ,c("Name", "Conception.rate.per.1.000.women.in.age.group")],
                by.x = "laname", by.y = "Name", all.x = TRUE)

Natsal$Conception.decile <- cut2(Natsal$Conception.rate.per.1.000.women.in.age.group,
                                 cuts = interiorcuts)

sim_prop_la$ethnic2 <- as.factor(sim_prop_la$ethnic2)
sim_prop_la$sex <- as.factor(sim_prop_la$sex)
sim_prop_la$age <- as.factor(sim_prop_la$age)
sim_prop_la$LAname <- as.factor(sim_prop_la$LAname)



## London ethnicity by individual ages ##

# source('../../packages/STIecoPredict/R/classifier_fns.R')
## men
laname.col <- grepl("laname", gsub(" ", "", tolower(names(LA_ethnic2.male))))
LA_ethnic2.male[ ,laname.col] <- LAnameClean(LA_ethnic2.male[ ,laname.col])
LA_ethnic2.male <- cbind(str_split_fixed(LA_ethnic2.male$`LA Name`, " ", 2), LA_ethnic2.male[ ,-1])
names(LA_ethnic2.male)[1:2] <- c("lacode", "LAname")
LA_ethnic2.male$LAname <- toupper(LA_ethnic2.male$LAname)

cityoflondon <- LA_ethnic2.male[LA_ethnic2.male$LAname == "CITY OF LONDON,WESTMINSTER", ]
cityoflondon$LAname <- "CITY OF LONDON"
LA_ethnic2.male <- rbind(LA_ethnic2.male, cityoflondon)
LA_ethnic2.male$LAname <- sub(pattern = "CITY OF LONDON,WESTMINSTER",
                              replacement = "WESTMINSTER", LA_ethnic2.male$LAname)

LA_ethnic2.male <- cbind(LA_ethnic2.male[ ,1:3], LA_ethnic2.male[ ,-(1:3)]/LA_ethnic2.male$total)

LA_ethnic2.male <- reshape2::melt(LA_ethnic2.male, id.vars = c("lacode","LAname","age"))
names(LA_ethnic2.male)[names(LA_ethnic2.male) == "variable"] <- "ethnic2"
names(LA_ethnic2.male)[names(LA_ethnic2.male) == "value"] <- "p.ethnic2"
LA_ethnic2.male$sex <- "Men"


## women
LA_ethnic2.female$`LA Name` <- LAnameClean(LA_ethnic2.female$`LA Name`)
LA_ethnic2.female <- cbind(str_split_fixed(LA_ethnic2.female$`LA Name`, " ", 2), LA_ethnic2.female[ ,-1])
names(LA_ethnic2.female)[1:2] <- c("lacode", "LAname")
LA_ethnic2.female$LAname <- toupper(LA_ethnic2.female$LAname)

cityoflondon <- LA_ethnic2.female[LA_ethnic2.female$LAname == "CITY OF LONDON,WESTMINSTER", ]
cityoflondon$LAname <- "CITY OF LONDON"
LA_ethnic2.female <- rbind(LA_ethnic2.female, cityoflondon)
LA_ethnic2.female$LAname <- sub(pattern = "CITY OF LONDON,WESTMINSTER",
                                replacement = "WESTMINSTER", LA_ethnic2.female$LAname)

LA_ethnic2.female <- cbind(LA_ethnic2.female[ ,1:3],
                           LA_ethnic2.female[ ,-(1:3)]/LA_ethnic2.female$total)

LA_ethnic2.female <- reshape2::melt(LA_ethnic2.female, id.vars = c("lacode","LAname","age"))
names(LA_ethnic2.female)[names(LA_ethnic2.female) == "variable"] <- "ethnic2"
names(LA_ethnic2.female)[names(LA_ethnic2.female) == "value"] <- "p.ethnic2"
LA_ethnic2.female$sex <- "Women"

LA_ethnic2_london <- rbind(LA_ethnic2.female, LA_ethnic2.male)

LA_ethnic2_london$ethnic2 <- factor(LA_ethnic2_london$ethnic2, levels = levels(sim_prop_la$ethnic2))
LA_ethnic2_london$sex <- factor(LA_ethnic2_london$sex, levels = levels(sim_prop_la$sex))
LA_ethnic2_london$age <- factor(LA_ethnic2_london$age, levels = levels(sim_prop_la$age))
LA_ethnic2_london$LAname <- factor(LA_ethnic2_london$LAname, levels = levels(sim_prop_la$LAname))

LA_ethnic2_london <- na.omit(LA_ethnic2_london)


colnames(madjacency) <- LAnameClean(colnames(madjacency))

rm(sin2.gors, sin2.LA, sin2_household,sim, i, laname.col)

# save.image("C:/Users/ngreen1/Dropbox/small-area & chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

