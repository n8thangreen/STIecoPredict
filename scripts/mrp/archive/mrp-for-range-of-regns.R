##
## Natsal chlamydia regression and post-stratification
## following Gelman and MRP approach and mrp package
##


library(mrp)
library(mrpdata)
library(stats)

## load in LA level data (i.e. census and administrative data)
LApop <- read.csv("H:/old pc/input_data/ONS/pop_age_sex_LA.csv", check.names = FALSE)
LAclass <- read.csv("H:/old pc/input_data/LAclassification/LAClassification.csv", check.names = FALSE)
LAsmoke <- read.csv("H:/old pc/input_data/risk_factors/smoking/raw_data/Adultssmoking_LA.csv", check.names = FALSE)

## load in survey data (i.e. individual Natsal)
Natsal <- read.csv("H:/old pc/input_data/NATSAL/Natsal-3_extract_2April2014.csv", check.names = FALSE)
load("H:/old pc/input_data/NATSAL/gor_strata/sin2-gors.RData")


## harmonise
names(Natsal)[1] <- "id"
Natsal <- subset(Natsal, cttestly!="not answered")  #only neeed to remove if including in analysis
Natsal <- subset(Natsal, drinkoft!="Not answered")

Natsal <- within(Natsal, {
  sex <- factor(rsex, exclude=NA, labels=c("Women","Men"))
  sex <- relevel(sex, "Men")
  
  age <- dage
  smoke <- factor(smokenow, exclude=NA)

  cttestly[cttestly=="not applicable"] <- "no"
  cttestly <- as.character(cttestly)
  cttestly[cttestly=="no"]  <- 0
  cttestly[cttestly=="yes"] <- 1
  cttestly <- as.integer(cttestly)
  
  # cttestly <- droplevels(cttestly)
  # cttestly <- factor(cttestly, exclude=NA,
  #                    labels=c(FALSE, TRUE))

  drinkoft[drinkoft=="not applicable"] <- "Not at all in the last 12 months"

  #income
  #ethnic/ethnicgrp
  #gor_l

  sex.age <- interaction(sex, age)
})



## if only want to predict same areas
# LApop <- na.omit(LApop[LApop$gor_l%in%Natsal$gor_l,])

LApop <- subset(LApop, Age>15 & Age<75)

LApop <- within(LApop,{
  sex <- Sex
  sex <- relevel(sex, "Men")
  
  age <- Age
  pop <- `2012`
  
  sex;.age <- interaction(sex, age)
})
LApop <- na.omit(LApop)


## intercept for each group

i <- 1; out <- c(NULL,NULL)
for (name in unique(LApop$Name)){
  mrp.simple <- mrp(cttestly ~ sex+age,
                    data=Natsal,
                    population=LApop[LApop$Name==name,],
                    pop.weights="pop")
  out <- rbind(out, c(name, round(poststratify(object=mrp.simple), digits=2)))
  i <- i+1
}
# xtable::xtable(100*poststratify(mrp.simple, ~ age+sex), digits=0)



### without group level predictors

## baseline regression
age + sex + ethnicity + smoking + drinking

## paired interactions

## geographic interaction


### with group level predictors
### la|region

urbanrural + averageincome + popdensity + transport + govbudget + unemployment + univ

## no individual level variables (area only)




