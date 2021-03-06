% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classifier_fns.R
\name{fitModel}
\alias{fitModel}
\title{Fit GLM on Survey (Natsal) data}
\usage{
fitModel(data.train, data.test, riskfac, depvar)
}
\arguments{
\item{data.train}{Training/fitting data}

\item{data.test}{Test data}

\item{riskfac}{Risk factors}

\item{depvar}{Dependant variable name}
}
\value{
list of fits
}
\description{
Create regression variables and
binds them together into a list
}
