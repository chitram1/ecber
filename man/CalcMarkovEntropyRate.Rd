% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropyFunction.R
\name{CalcMarkovEntropyRate}
\alias{CalcMarkovEntropyRate}
\title{Calculate an estimate of the Entropy Rate of a finite Markov Chain}
\usage{
CalcMarkovEntropyRate(trans_mat, stat_dist)
}
\arguments{
\item{stat_dist}{Vector of the stationary distribution of the Markov chain}

\item{trans_mar}{Transition Matrix of a finite Markov chain}
}
\value{
Estimate of the Entropy Rate
}
\description{
Calculate an estimate of the Entropy Rate of a finite Markov Chain
}
\examples{
t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
sim_mc <- SimulateMarkovChain(t_mat, n_sims = 10000)
tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, 2)
tm2 <- CalcTransitionMatrix(tc2)
eig_sm <- CalcEigenStationary(tm2)
emp_sm <- CalcEmpStat_Mth_Order(sim_mc, 1:2, 2)
CalcMarkovEntropyRate(t_mat, CalcEigenStationary(t_mat))
CalcMarkovEntropyRate(tm2, eig_sm)
CalcMarkovEntropyRate(tm2, emp_sm)
}
