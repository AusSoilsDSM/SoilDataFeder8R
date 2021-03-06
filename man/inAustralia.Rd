% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataMassage.R
\name{inAustralia}
\alias{inAustralia}
\title{Return locations only within the bounding box od Australia}
\usage{
inAustralia(soilDF)
}
\arguments{
\item{soilDF}{Dataframe to filter}
}
\description{
Takes the standard SoilDataFederator dataframe returned by getSoilData function and filters the locations to the extent of Australia .
}
\examples{
inAustralia(soilDF=DF)
}
\keyword{dataframe}
