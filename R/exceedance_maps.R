# ##
# ## chlamydia spatial analysis
# ## convolution of demographic strata
# ##
# ## N Green
# ## Feb 2015
#
# ##TODO##
# ## 1)generalise for more variables other than male & female
# ##    eg age groups
# ## 2) Normal approximation
#
#
# pm <<- 0.5   #proportion males
# pf <<- 1- pm
# E_f <<- 0.5  #expected outcome 'success' probability from logistic model for females
# E_m <<- 0.5
#
# K <<- 0.2  #integration variable
# lim <<- 0.5
#
# size <<- 100   #should use the actual population sizes instead
#
# # http://stackoverflow.com/questions/23569133/adding-two-random-variables-via-convolution-in-r
#
# ##TODO##
# # not sure how this works...
# fX <- function(x) dbinom(round(x/pm*size), size=size, prob=E_f)
# FY <- function(y) pbinom(round(y/pf*size), size=size, prob=E_m, lower.tail=FALSE)
#
# intl <- function(z) integrate(function(x, z){fX(x)*FY(z-x)}, lower=0, upper=1, z)$value
# intl <- Vectorize(intl)
#
# intl(0.5)
# intl(0.9)
#
#
# ## check
# set.seed(1)
# X <- rbinom(n=1000, size=size, prob=E_f)
# Y <- rbinom(n=1000, size=size, prob=E_m)
# plot(X,Y, col=1+(Y>=(lim-(X/size*pm))/pf*size))
#
# xseq <- seq(0, 1, by=0.001)
# # xseq/pm*size
# # lines(xseq/pm*size, (lim-xseq)/pf*size)
# # hist(X)
# # plot(table(X))
# # lines(table(Y), col="red")
#
# sum(Y>=(lim-(X/size*pm))/pf*size)/1000
#
#
#
# plot(xseq, FY(lim-xseq), type="l")
# lines(xseq, fX(xseq), type="l", col="red")
# lines(xseq, fX(xseq)*FY(lim-xseq), type="l")
# abline(v=E_f)
# abline(v=E_m, col="red")
#
# sum(fX(xseq)*FY(lim-xseq))/1000  #fX(xseq)%*%FY(lim-xseq)
# ## close to integrate() solution
#
#
# # http://r.789695.n4.nabble.com/Convolution-confusion-td3532943.html
# convolve(fX(xseq), rev(FY(lim-xseq)), type="o")
#
#
# # Normal approximation ----------------------------------------------------
#
#
#
#
# # n-fold convolution ------------------------------------------------------
#
#
