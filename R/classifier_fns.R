##
## data preprocessing
## and cleaning functions
## classifier_fns.R
##
## N Green
## June 2014


fillEmpty <- function(x){
  fill <- function(y) if(is.na(y)){y <- 1}else{y}
  if(is.list(x)){
    out <- lapply(x, fill)
  }else{out <- fill(x)}
out  
}

colNameReplace <- function(array, name.before, name.after){
  names(array)[names(array)==name.before] <- name.after
  array
}


covariateShift <- function(data, resla, riskfac, ssize=10000){
  ## importance sampling approach
  ## when different distributions for the
  ## training and test data
  
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


covariateShift.glm <- function(data, resla, riskfac, ssize=10000){
  ## alternative model-based approach:
  ## could fit a logistic regression to estimate the ratio of probabilities of each data set
  ## and then predict for (all) permutations
  ## http://blog.smola.org/post/4110255196/real-simple-covariate-shift-correction

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


fitModel <- function(data.train, data.test, riskfac, depvar){
  ## create regression variables
  ## binds them together into a list
  
  formula <- as.formula(paste(depvar, " ~ ", paste(riskfac, collapse="+"), sep=""))
  
  data.train <- data.train[!data.train[,depvar]%in%c("NOT ANSWERED","NOT APPLICABLE"), c(depvar, riskfac)]
  data.test  <- data.test[!data.test[,depvar]%in%c("NOT ANSWERED","NOT APPLICABLE"), c(depvar, riskfac)]
  
  ndata <- min(nrow(data.train),nrow(data.test),50000)
  
  data.train <- data.train[1:ndata,]
  data.test  <- data.test[1:ndata,]
  
  data.train <- droplevels(data.train)
  data.test  <- droplevels(data.test)
  
  train.out <- data.train[,depvar]
  test.out  <- data.test[,depvar]
  
  ## LA-level estimates to simulate with
  ### "sparsification" with dummy()
  train.datmat <- knnDatamatrix(data.train)
  test.datmat  <- knnDatamatrix(data.test)
  
  ## clean column names created in dummy()
  colnames(train.datmat) <- cleanDummyNames(train.datmat)
  colnames(test.datmat)  <- cleanDummyNames(test.datmat)
  
  
  ##TODO##
  ## could include the Natsal weights directly in likelihood
  fit <- tryCatch(glm(formula, family=binomial, data=data.train), error=function(e){})
  
list(logisticfit=fit, data.train=data.train, data.test=data.test,
     train.datmat=train.datmat, test.datmat=test.datmat,
     train.out=train.out, test.out=test.out, formula=formula)
}


predictKNN <- function(fit, test=FALSE){
  ## k-nerarest neighbours classifier prediction
  ## NB only works for numeric covariates
  ## careful when some of these are categorical imposing a (numerical) order
  
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



DistnTable <- function(data, riskfac){
  ## frequency table as dataframe
  ## of discrete joint distribution
  
  require(reshape)
  
  data$Freq <- 1
  formula <- paste(paste(riskfac, collapse = "+"), "~.", sep="")
  out <- cast(data, formula, fun=sum, value="Freq")
  out[,"(all)"] <- out[,"(all)"]/sum(out[,"(all)"])
  out
}


predict.lgstc <- function(fit, data, ADD=F){
  ## predict coverage from
  ## logistic classifier model
  ## ADD: add to existing plot
  
  require(Hmisc)
  require(ROCR)
  require(caret)
  
  fit.probs <- predict(fit, newdata=data, type="response")
  
  fit.pred  <- rep("NO", nrow(data))
  fit.pred[fit.probs>0.5] <- "YES"
  print(paste("mean coverage ", mean(fit.probs)))
  
  ## contingency tables
  print(Hmisc::latexTabular(
    ctable <- addmargins(table(pred=fit.pred, data=data$cttestly), FUN=list(Total=sum), quiet=TRUE)))
#   print(ctable)
  print(confusionMatrix(fit.pred, data$cttestly))
  
  ## ROC curve
  pred <- prediction(fit.probs, data$cttestly)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, colorize=F, add=ADD)
}



scaleknn <- function(var, c){
  ## min-max standardisation
  
  c*(var-min(var))/(max(var)-min(var))
}



require(plyr)
require(class)
failknn <- failwith(NA, knn)


cleanDummyNames <- function(res){
  ## clean names after expanding to
  ## dummy variable format
  
  resNames <- unlist(lapply(strsplit(split = "))",colnames(res)), tail, 1))
  resNames <- unlist(lapply(strsplit(split = "train",resNames), tail, 1))
  resNames <- unlist(lapply(strsplit(split = "res.df",resNames), tail, 1))
  resNames <- unlist(lapply(strsplit(split = "data.covshift",resNames), tail, 1))
  resNames <- unlist(lapply(strsplit(split = "fit.model",resNames), tail, 1))
  unlist(lapply(strsplit(split = "test",resNames), tail, 1))
}


toupper.fac <- function(data,
                        names)  #factors (char)
{ ## coerce multiple factors to upper case

  for (i in names){
    data[,i] <- as.factor(toupper(data[,i]))}
  data
}


LAnameClean <- function(datName){
  ## remove extra characters
  ## and other consistencies
  ## from ONS tables
  
  datName <- gsub(" UA", "", datName)
  datName <- gsub(" CC", "", datName)
  datName <- gsub(" CD", "", datName)
  datName <- gsub(" LB", "", datName)
  datName <- gsub(" MCD", "", datName)
#   datName <- gsub("Bristol", "Bristol, City of", datName)
  #   datName <- gsub("City of Westminster", "City of London", datName) ## TODO ## check same
  datName <- gsub("St Helens", "St. Helens", datName)
  datName <- gsub("Kingston upon Hull, City of", "Kingston upon Hull", datName, fixed=TRUE) 
  datName <- gsub("Kingston upon Hull", "Kingston upon Hull, City of", datName, fixed=TRUE)
  datName <- gsub("City of Westminster", "Westminster", datName)
  
  datName
}


levelsMatch <- function(target, modify)
{ ## match-up number and order of levels 
  ## in two vectors
  
  require(gdata, quietly = TRUE)
  
  ### add extra levels to
  ### subset variable
  modify <- factor(modify, levels=
                     c(levels(modify), levels(target)[!levels(target)%in%levels(modify)]))
  ### reorder levels
  modify <- reorder.factor(modify, new.order=as.character(levels(target)))
  
  modify
}




knnDatamatrix <- function(df, vars="smokenow", c=0.5){
  ## create KNN input matrix
  ## comment-out unwanted variables
  ## c: age scaling
  ##  
  ## LA-level estimates to simulate with
  ### "sparsification" with dummy()
    ##TODO##
  ## have similar ethnic groups `closer' to one another
  
  
  require(dummies)
  
  if("het1yr"%in%vars){           #full model
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
  else if ("smokenow"%in%vars){   #submodel
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


##SLOW
changeToArrayIndiv <- function(res){
  require(plyr)
  res.df <- list()
  for (la in names(res)){
    res.df[[la]] <- ldply(res[[la]], data.frame)
  }
  res.df
}


scatterplotObsEst <- function(CTADGUM_pred, names, points){
  
  out <- ggplot(CTADGUM_pred, aes(x=`Combined Coverage`, y=`Coverage_knn`)) +
    #   ggplot(CTADGUM_pred, aes(x=`Combined Coverage`, y=Coverage*prop.TP.pred)) +
    #   geom_point(aes(size = Population)) +
    
    #   scale_size_area() +
    #   geom_point(shape=1) +    # Use hollow circles
    
    ylab("Estimated Coverage") + xlab("Observed Coverage") +
    #xlim(0.1, 0.4) +
    #ylim(0.1, 0.2) +
    geom_abline(linetype="dashed", size=1) + # line y=x
    
    theme(text = element_text(size=20)) +  #font size
    
    ## Natsal-3 fit point
    #   geom_point(aes(y=true.pos, x=tot.pos.obs), shape=17, size=4, col="blue") +
    #   geom_point(aes(y=tot.pos.pred, x=tot.pos.obs), shape=17, size=4, col="red") +
    
    theme(panel.background = element_rect(fill='white', colour='black')) +
    
    
    ## from surveillance data
    geom_abline(intercept=mean(CTADGUM_pred$`Combined Coverage`, na.rm=TRUE), slope=0, linetype="dotted", col="darkgreen", lwd=1.2) +
    geom_abline(intercept=mean(CTADGUM_pred$`Coverage_knn`, na.rm=TRUE), slope=0,  linetype="dotdash", col="darkgreen", lwd=1.2)
  
  
  ## error bounds
  #   geom_abline(intercept=a.diff, linetype="dotted") + 
  #   geom_abline(intercept=-a.diff, linetype="dotted") +
  #   
  #   geom_abline(slope=1/prop.TP.obs , linetype="dotdash") + 
  #   geom_abline(slope=prop.TP.obs , linetype="dotdash") +
  #     
  
  #   geom_text(data=CTADGUM_pred[la.outside.fix,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.fix]) +
  #   geom_text(data=CTADGUM_pred[la.outside.regn,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.regn], colour="red", fontface="bold") +
  #   geom_text(data=CTADGUM_pred[la.outside.prop,],
  #             label=CTADGUM_pred$`LA Name`[la.outside.prop], colour="blue", fontface="bold")
  
  # geom_point(aes(x=CTADGUM_pred$`Combined Coverage`, y=ComCov.pred))
  
  ## text point annotation
  if(names==TRUE){out <- out + geom_text(data=CTADGUM_pred, label=CTADGUM_pred$`LA Name`, size=3) }  #all LAs
  
  if(points=="IMD"){out <- out + 
                      #geom_smooth(method=lm) + 
                      geom_point(aes(size = `IMD`))
  }
  if(points=="density"){out <- out + 
                          #geom_smooth(method=lm) + 
                          geom_point(aes(size = `Population density`))               
  }
  if(points=="house"){out <- out + 
                        #geom_smooth(method=lm) + 
                        geom_point(aes(size = `House prices/earnings`))
  }
  if(points=="class"){out <- out + 
                        aes(shape=Classification, col=Classification) +
                        geom_point(size=3)
                      #geom_text(data=CTADGUM_pred, label=CTADGUM_pred$`Classification`)
  }
  
  out
}


rss <- function(data1,
                data2,
                data3=NULL){
  ## residual sum of squares
  ## also for region
  
  if(is.null(data3)) rss <- sum((data1-data2)^2)
  else{
    diff <- 1:length(data1)
    for (i in 1:length(data1)){
      if(data3[i]>data1[i]) diff[i] <- data3[i]-data1[i]
      else if(data3[i]<data2[i]) diff[i] <- data2[i]-data3[i]
      else diff[i] <- 0
    }
    rss <- sum(diff^2)
  }
  
  rss
}



