% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropyFunction.R
\name{ber_analyze_file_affect_and_autonomy}
\alias{ber_analyze_file_affect_and_autonomy}
\title{Estimate Behavioral Entropy Rate based upon Video Data}
\usage{
ber_analyze_file_affect_and_autonomy(
  f_loc,
  plot_all = F,
  plots_to_file = F,
  behavior_types = list(positive = c("positive"), negative = c("negative"), neutral =
    c("neutral"), autonomy_support = c("AutonomySupport"), intrusiveness =
    c("Intrusiveness"), neither = c("Neither"), missing_types = c("CantTellAffect",
    "CantTellBehavior")),
  missing_threshold = 0.1,
  order = 1
)
}
\arguments{
\item{f_loc}{file location}

\item{plot_all}{logical: Plot the data to observe the sequence of behaviors}

\item{plots_to_file}{logical: send all plots to a file}

\item{behavior_types}{dictionary of behavior types.
Required sections: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, positive, negative, neutral, missing_types}

\item{missing_threshold}{proportion of acceptable missing time}

\item{tactile_padding}{right padding adjustment to tactile events}

\item{auditory_padding}{right padding adjustment to auditory events}
}
\value{
Entropy rate estimates of an individual
}
\description{
Estimate Behavioral Entropy Rate based upon Video Data
}