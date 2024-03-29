% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropyFunction.R
\name{CalcTransitionMatrix}
\alias{CalcTransitionMatrix}
\title{Calculate the Transition Matrix of a First-Order Markov Chain}
\usage{
CalcTransitionMatrix(trans_counts)
}
\arguments{
\item{trans_counts}{Matrix of transition counts}
}
\value{
Row-stochastic transition matrix of a first-order Markov chain.
}
\description{
Calculate the Transition Matrix of a First-Order Markov Chain
}
\examples{
t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
tc <- CalcTransitionCounts(sim_mc)
tm <- CalcTransitionMatrix(tc)
}
