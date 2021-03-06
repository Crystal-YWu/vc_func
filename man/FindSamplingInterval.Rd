% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{FindSamplingInterval}
\alias{FindSamplingInterval}
\title{FindSamplingInterval finds a suitable intervals for sampling current and voltage data of the loaded abf file}
\usage{
FindSamplingInterval(abf, current_chan_id = 0, voltage_chan_id = 0,
  interval_size = 0, max_interval_expansion_rate = 1,
  allowed_voltage_delta = 0, epoch_name = "auto", backward_seach = FALSE,
  backward_search_ratio = 3)
}
\arguments{
\item{abf}{data loaded from abf file}

\item{current_chan_id}{OPTIONAL, current channel id, usually can be identified automatically}

\item{voltage_chan_id}{OPTIONAL, voltage channel id, usually can be identified automatically}

\item{interval_size}{OPTIONAL, prefered minimum interval size.}

\item{max_interval_expansion_rate}{OPTIONAL, determine if the function returns variable interval size.}

\item{allowed_voltage_delta}{OPTIONAL, allowed maximum deviation of voltage W.R.T protocol setting in the sampling interval.}

\item{epoch_name}{OPTIONAL, determines which in which epoch to find the interval.}

\item{backward_seach}{OPTIONAL, perform interval search along negative direction}

\item{backward_search_ratio}{OPTIONAL, determines how aggressive the backward search is}
}
\value{
a vector of 2 numeric describes an interval
}
\description{
FindSamplingInterval finds a suitable intervals for sampling current and voltage data of the loaded abf file
}
\examples{
Usually this function does all the work automatically and no extra parameters are needed: intv <- FindSamplingInterval(abf)
}
