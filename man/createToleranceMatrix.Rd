% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reliabilityFunctions.R
\name{createToleranceMatrix}
\alias{createToleranceMatrix}
\title{Creating Agreement/Confusion Matrix between 2 coders with tolerance of n}
\usage{
createToleranceMatrix(
  filepath1,
  filepath2,
  eventlist,
  tolerance = 0,
  file_seconds = 300
)
}
\arguments{
\item{filepath1}{Filepath to first excel file.}

\item{filepath2}{Filepath to second excel file.}

\item{eventlist}{List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.}

\item{tolerance}{The tolerance value for increasing the buffer of comparison (0 means that the codes need to be exactly the same at each given second with no leniency, a tolerance of 1 means that we will count the codes as equal as long as they were recorded within 1 second of each other)}

\item{file_seconds}{The total number of seconds coded for in each file.}
}
\value{
We return a list of two entries: the first being the confusion matrix and the second being the percent agreement metric.
}
\description{
This function takes in the 2 filepaths and the events that we are looking at for coder similarity. We create an agreement/confusion matrix comparing matches between codes in both files. This is the same as the buildMatrix function except that we can custommize the tolerance values.
}
\examples{
createToleranceMatrix(filepath1 = './6008BECEU.xlsx', filepath2 = './6008BECNR.xlsx', eventlist = list(c("positive","neutral","negative","CantTellAffect"), c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")), 300)
}
