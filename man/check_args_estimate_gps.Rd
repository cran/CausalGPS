% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_arguments.R
\name{check_args_estimate_gps}
\alias{check_args_estimate_gps}
\title{Check estimate_gps function arguments}
\usage{
check_args_estimate_gps(gps_density, ...)
}
\arguments{
\item{gps_density}{Model type which is used for estimating GPS value, including
\code{normal} and \code{kernel}.}

\item{...}{Additional arguments to successfully run the process.}
}
\value{
Returns True if passes all checks, successfully. Otherwise raises ERROR.
}
\description{
Checks estimate_gps function arguments to make sure that the required
additional arguments are provided.
}
\keyword{internal}
