% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcTotalProbs.R
\name{calcTotalProbs}
\alias{calcTotalProbs}
\title{Calculate the Combined (Conditional) Probabilities}
\usage{
calcTotalProbs(formula, data = sim_prop_la, extracols = c("LAname",
  "la_code", "region_code", "region_name", "gor"))
}
\arguments{
\item{formula}{Formula object}

\item{data}{Proportions in categories (array)}

\item{extracols}{Columns to retain but do nothing with}
}
\value{
data
}
\description{
Assuming risk factors are conditionally independent given age and sex.
}
