% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropyFunction.R
\name{ber_analyze_file_affect}
\alias{ber_analyze_file_affect}
\title{Estimate Behavioral Entropy Rate based upon Video Data with Sensory Signal Data Affect and Tactile/Auditory/Visual}
\usage{
ber_analyze_file_affect(
  f_loc,
  plot_all = F,
  plots_to_file = F,
  tactile_padding = 1,
  auditory_padding = 1,
  behavior_types = list(mom_auditory_types = c("Vocal"), mom_tactile_types =
    c("TouchBaby", "HoldingBaby"), mom_visual_types = c("ManipulatingObject"),
    baby_visual_types = c("LookAtMomActivity"), positive = c("positive"), negative =
    c("negative"), neutral = c("neutral"), missing_types = c("CantTellHolding",
    "ActivityNotVisible", "CantTellLooking", "CantTellAffect")),
  missing_threshold = 0.1,
  order = 1
)
}
\arguments{
\item{f_loc}{file location}

\item{plot_all}{logical: Plot the data to observe the sequence of behaviors}

\item{plots_to_file}{logical: send all plots to a file}

\item{tactile_padding}{right padding adjustment to tactile events}

\item{auditory_padding}{right padding adjustment to auditory events}

\item{behavior_types}{dictionary of behavior types.
Required sections: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, positive, negative, neutral, missing_types}

\item{missing_threshold}{proportion of acceptable missing time}

\item{order}{Markov model order}
}
\value{
Entropy rate estimates of an individual
}
\description{
Estimate Behavioral Entropy Rate based upon Video Data with Sensory Signal Data Affect and Tactile/Auditory/Visual
}
