% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keydur.R
\name{wg}
\alias{wg}
\title{wg helper function}
\usage{
wg(keyrates, tt)
}
\arguments{
\item{keyrates}{vector of keyrates}

\item{tt}{time of cash flow to attribute to keyrates}
}
\value{
a list of weights with length of keyrates, sum = 1
}
\description{
wg helper function
}
\examples{
\dontrun{
wg(c(2,5,10),6.2)
}
}
