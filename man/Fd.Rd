% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fd.R
\name{Fd}
\alias{Fd}
\title{Cumulative distribution function for a subset of the dataset}
\usage{
Fd(y, data, group = "qtyvar", newwts = "newwts")
}
\arguments{
\item{y}{cumulative distribution quantile}

\item{data}{dataset with defined group and newwts columns}

\item{group}{variable of interest}

\item{newwts}{sampling weights}
}
\value{
Cumulative distribution percentile for quantile y
}
\description{
Cumulative distribution function for a subset of the dataset
}
\examples{

df_samp <- data.frame(qtyvar = rnorm(1000, mean = 5, sd = 2),newwts = rep(1, 1000))
Fd(3.5,df_samp)
# Finds the percentile for 3.5 in simulated data.
}
