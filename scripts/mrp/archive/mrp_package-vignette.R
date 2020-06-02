
library(mrp)
library(mrpdata)
library(haven)

# marriage.data <- read_stata("C:\\Users\\ngreen1\\Dropbox\\R\\mrp\\gay_marriage_megapoll.dta")
# write.csv(marriage.data, file="marriage_data.csv")

## Preparing the data
## ------------------
### changed from . to _ to match downloaded data

marriage.data <- within(marriage_data, {
  state <- factor(state, exclude=NA)
  poll <- factor(poll, exclude=NA)
  age <- factor(age_cat, exclude=NA,
                labels=c("18-29","30-44","45-64","65+"))
  edu <- factor(edu_cat,exclude=NA,labels=c("< High School",
                                             "High School",
                                             "Some College",
                                             "Graduated College"))
  ## Code interaction here, first fixing levels
  female <- factor(female,levels=c(0,1),
                   labels=c("Male","Female"))
  race_wbh <- factor(race_wbh)
  levels(race_wbh) <- c("White","Black","Hispanic")
  f.race <- interaction(female, race_wbh)
})

## Remove empty "" state and drop it from levels.
marriage.data <- subset(marriage.data, !is.na(state) & state!="" )
marriage.data$state <- factor(marriage.data$state)



mrp.census <- na.omit(mrp.census[mrp.census$state %in% marriage.data$state,])
mrp.census <- within(mrp.census,{
  age <- factor(age,exclude=NA,labels=c("18-29","30-44","45-64","65+"))
  education[education=="postgraduate"] <- "college graduate"
  edu <- factor(education,exclude=NA,labels=c("< High School",
                                               "High School",
                                               "Some College",
                                               "Graduated College"))
  state <- factor(state,exclude=NA)
  
  race[race=="Other"] <- NA
  race <- factor(race,exclude=NA)
  f.race <- interaction(sex,race)
})
mrp.census <- na.omit(mrp.census)


## Calling mrp() for the first time
## --------------------------------


## intercept for each group
mrp.simple <- mrp(yes_of_all ~ state+age+edu,
                  data=marriage.data,
                  population=mrp.census,
                  pop.weights="weighted2004")

# tables
print(100*poststratify(object=mrp.simple, ~ edu+age), digits=2)
print(100*poststratify(object=mrp.simple, ~ state), digits=2)
# xtable::xtable(100*poststratify(mrp.simple, ~ edu+age), digits=0)

## maps
print(spplot(mrp.simple, ~state))





## The Full Model
## --------------

# Joining Predictors and Transforming Data
Regions <- mrp.regions  #state to region lookup

add = list(
    Statelevel,
    Regions,
    expression(z.age <- rescale(age)),
    expression(age.edu <- interaction(age,edu)))

mr.formula= .~.+ (1|region) + (1|age.edu) + z.age + p.relig.full + p.kerry.full

mrp.statelevel <- mrp(yes.of.all~
                        state+f.race+age+edu, #+poll,,
                      data = marriage.data,
                      population = mrp.census, pop.weights="weighted2008",
                      # formula.pop.update=.~.-poll,
                      grouplevel.data.frames = list(Statelevel,
                                                  mrp.regions),
                      grouplevel.expressions = list(expression(age.edu <- interaction(age,edu)),
                                                  expression(z.age <- rescale(age))),
                      formula.model.update = .~.+(1|region)+ (1|age.edu)+z.age+p.relig.full+p.kerry.full)


# tables
print(100*poststratify(object=mrp.statelevel, ~ edu+age), digits=2)
print(100*poststratify(object=mrp.statelevel, ~ state), digits=2)
xtable::xtable(100*poststratify(mrp.statelevel, ~ edu+age), digits=0)

## maps
print(spplot(mrp.statelevel, ~state))


