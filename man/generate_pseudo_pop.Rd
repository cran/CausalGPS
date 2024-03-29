% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_pseudo_pop.R
\name{generate_pseudo_pop}
\alias{generate_pseudo_pop}
\title{Generate pseudo population}
\usage{
generate_pseudo_pop(
  w,
  c,
  ci_appr,
  gps_density = "normal",
  use_cov_transform = FALSE,
  transformers = list("pow2", "pow3"),
  bin_seq = NULL,
  exposure_trim_qtls = c(0.01, 0.99),
  gps_trim_qtls = c(0, 1),
  params = list(),
  sl_lib = c("m_xgboost"),
  nthread = 1,
  include_original_data = FALSE,
  gps_obj = NULL,
  ...
)
}
\arguments{
\item{w}{A data.frame comprised of two columns: one contains the observed
exposure variable, and the other is labeled as 'id'. The column for the
outcome variable can be assigned any name as per your requirements.}

\item{c}{A data.frame of includes observed covariate variables. It should
also consist of a column named 'id'.}

\item{ci_appr}{The causal inference approach. Possible values are:
\itemize{
\item "matching": Matching by GPS
\item "weighting": Weighting by GPS
}}

\item{gps_density}{Model type which is used for estimating GPS value,
including \code{normal} (default) and \code{kernel}.}

\item{use_cov_transform}{If TRUE, the function uses transformer to meet the
covariate balance.}

\item{transformers}{A list of transformers. Each transformer should be a
unary function. You can pass name of customized function in the quotes.
Available transformers:
\itemize{
\item pow2: to the power of 2
\item pow3: to the power of 3
}}

\item{bin_seq}{Sequence of w (treatment) to generate pseudo population. If
NULL is passed the default value will be used, which is
\code{seq(min(w)+delta_n/2,max(w), by=delta_n)}.}

\item{exposure_trim_qtls}{A numerical vector of two. Represents the trim quantile
level for exposure values. Both numbers should be in the range of [0,1] and
in increasing order (default: c(0.01, 0.99)).}

\item{gps_trim_qtls}{A numerical vector of two. Represents the trim quantile
level for the gps values. Both numbers should be in the range of [0,1] and
in increasing order (default: c(0.0, 1.0)).}

\item{params}{Includes list of params that is used internally. Unrelated
parameters will be ignored.}

\item{sl_lib}{A vector of prediction algorithms.}

\item{nthread}{An integer value that represents the number of threads to be
used by internal packages.}

\item{include_original_data}{If TRUE, includes the original data in the
outcome.}

\item{gps_obj}{A gps object that is generated with \code{estimate_gps} function.
If it is provided, the number of iteration will forced to 1 (Default: NULL).}

\item{...}{Additional arguments passed to different models.}
}
\value{
Returns a pseudo population (gpsm_pspop) object that is generated
or augmented based on the selected causal inference approach (ci_appr). The
object includes the following objects:
\itemize{
\item params
\itemize{
\item ci_appr
\item params
}
\item pseudo_pop
\item adjusted_corr_results
\item original_corr_results
\item best_gps_used_params
\item effect size of generated pseudo population
}
}
\description{
Generates pseudo population data set based on user-defined causal inference
approach. The function uses an adaptive approach to satisfies covariate
balance requirements. The function terminates either by satisfying covariate
balance or completing the requested number of iteration, whichever comes
first.
}
\details{
\subsection{Additional parameters}{
\subsection{Causal Inference Approach (ci_appr)}{
\itemize{
\item if ci_appr = 'matching':
\itemize{
\item \emph{dist_measure}: Matching function. Available options:
\itemize{
\item l1: Manhattan distance matching
}
\item \emph{delta_n}: caliper parameter.
\item \emph{scale}: a specified scale parameter to control the relative weight that
is attributed to the distance measures of the exposure versus the GPS.
\item \emph{covar_bl_method}: covariate balance method. Available options:
\itemize{
\item 'absolute'
}
\item \emph{covar_bl_trs}: covariate balance threshold
\item \emph{covar_bl_trs_type}: covariate balance type (mean, median, maximal)
\item \emph{max_attempt}: maximum number of attempt to satisfy covariate balance.
\item See \code{\link[=create_matching]{create_matching()}} for more details about the parameters and default
values.
}
\item if ci_appr = 'weighting':
\itemize{
\item \emph{covar_bl_method}: Covariate balance method.
\item \emph{covar_bl_trs}: Covariate balance threshold
\item \emph{max_attempt}: Maximum number of attempt to satisfy covariate balance.
}
}
}

}
}
\examples{
\donttest{
m_d <- generate_syn_data(sample_size = 100)
pseuoo_pop <- generate_pseudo_pop(m_d[, c("id", "w")],
                                  m_d[, c("id", "cf1","cf2","cf3","cf4","cf5","cf6")],
                                  ci_appr = "matching",
                                  gps_density = "normal",
                                  bin_seq = NULL,
                                  expos_trim_qlts = c(0.01,0.99),
                                  gps_trim_qlts = c(0.01,0.99),
                                  use_cov_transform = FALSE,
                                  transformers = list(),
                                  params = list(xgb_nrounds=c(10,20,30),
                                                xgb_eta=c(0.1,0.2,0.3)),
                                  sl_lib = c("m_xgboost"),
                                  nthread = 1,
                                  covar_bl_method = "absolute",
                                  covar_bl_trs = 0.1,
                                  covar_bl_trs_type= "mean",
                                  max_attempt = 1,
                                  dist_measure = "l1",
                                  delta_n = 1,
                                  scale = 0.5)
}
}
