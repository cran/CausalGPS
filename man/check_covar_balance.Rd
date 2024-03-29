% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_covar_balance.R
\name{check_covar_balance}
\alias{check_covar_balance}
\title{Check covariate balance}
\usage{
check_covar_balance(w, c, ci_appr, counter_weight = NULL, nthread = 1, ...)
}
\arguments{
\item{w}{A vector of observed continuous exposure variable.}

\item{c}{A data.frame of observed covariates variable.}

\item{ci_appr}{The causal inference approach.}

\item{counter_weight}{A weight vector in different situations. If the
matching approach is selected, it is an integer data.table of counters.
In the case of the weighting approach, it is weight data.table.}

\item{nthread}{The number of available threads.}

\item{...}{Additional arguments passed to different models.}
}
\value{
output object:
\itemize{
\item corr_results
\itemize{
\item absolute_corr
\item mean_absolute_corr
}
\item pass (TRUE,FALSE)
}
}
\description{
Checks the covariate balance of original population or pseudo population.
}
\details{
\subsection{Additional parameters}{
\itemize{
\item For ci_appr == matching:
\itemize{
\item covar_bl_method
\item covar_bl_trs
}
}
}
}
\examples{
\donttest{
set.seed(422)
n <- 100
mydata <- generate_syn_data(sample_size=100)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n,
              replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n,
                replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)



pseudo_pop <- generate_pseudo_pop(mydata[, c("id", "w")],
                                 mydata[, c("id", "cf1", "cf2", "cf3",
                                            "cf4","cf5", "cf6", "year",
                                            "region")],
                                 ci_appr = "matching",
                                 gps_density = "kernel",
                                 exposure_trim_qtls = c(0.01,0.99),
                                 sl_lib = c("m_xgboost"),
                                 covar_bl_method = "absolute",
                                 covar_bl_trs = 0.1,
                                 covar_bl_trs_type = "mean",
                                 max_attempt = 1,
                                 dist_measure = "l1",
                                 delta_n = 1,
                                 scale = 0.5,
                                 nthread = 1)

adjusted_corr_obj <- check_covar_balance(w = pseudo_pop$pseudo_pop[, c("w")],
                                        c = pseudo_pop$pseudo_pop[ ,
                                        pseudo_pop$covariate_cols_name],
                                        counter = pseudo_pop$pseudo_pop[,
                                                    c("counter_weight")],
                                        ci_appr = "matching",
                                        nthread = 1,
                                        covar_bl_method = "absolute",
                                        covar_bl_trs = 0.1,
                                        covar_bl_trs_type = "mean")
}
}
