% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kappaCalc.R
\name{calcKappaValues}
\alias{calcKappaValues}
\title{Calculating Kappa Metrics from a Confusion/Agreement Matrix}
\usage{
calcKappaValues(excelfilepath)
}
\arguments{
\item{excelfilepath}{This is the filepath to the excel sheet containing the matrix of agreement values (excluding the row and column totals)}
}
\value{
This function returns a list of all calculations.
}
\description{
This function returns all of the individual kappa values for each state/event along with Cohen's kappa, percent agreement, percent by chance, and weighted/unweighted kappa metrics. It invocates the calcCohensKappa function.
}
\examples{
calcKappaValues(excelfilpath)
}
