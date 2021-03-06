% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episodic.R
\name{AvailEpisodeIdx}
\alias{AvailEpisodeIdx}
\title{AvailEpisodeIdx returns available episode indices}
\usage{
AvailEpisodeIdx(ByChannel)
}
\arguments{
\item{ByChannel}{data.frame of a channel}
}
\value{
a vector of available episode indices
}
\description{
AvailEpisodeIdx returns available episode indices
}
\examples{
idx <- AvailEpisodeIdx(abf$ByChannel[[1]])
}
