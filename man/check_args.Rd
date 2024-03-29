% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_arguments.R
\name{check_args}
\alias{check_args}
\title{Check additional arguments}
\usage{
check_args(
  ci_appr,
  use_cov_transform,
  transformers,
  gps_density,
  trim_quantiles,
  ...
)
}
\arguments{
\item{ci_appr}{The causal inference approach.}

\item{use_cov_transform}{A logical value (TRUE/FALSE) to use covariate
balance transforming.}

\item{transformers}{A list of transformers.}

\item{...}{Additional named arguments passed.}
}
\value{
TRUE if requirements are met. Raises error otherwise.
}
\description{
Checks additional arguments that user needs to provide for different
prediction models.
}
\keyword{internal}
