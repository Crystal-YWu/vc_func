% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episodic.R
\name{AvailEpisodeName}
\alias{AvailEpisodeName}
\title{AvailEpisodeName returns available episode names}
\usage{
AvailEpisodeName(ByChannel)
}
\arguments{
\item{ByChannel}{data.frame of a channel}
}
\value{
a vector of available episode names
}
\description{
AvailEpisodeName returns available episode names
}
\examples{
epi_names <- AvailEpisodeName(abf$ByChannel[[1]])
}
