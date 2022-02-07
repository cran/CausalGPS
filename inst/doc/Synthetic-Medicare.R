## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(CausalGPS)
library(ggplot2)

## -----------------------------------------------------------------------------
data("synthetic_us_2010")
data <- synthetic_us_2010
knitr::kable(head((data)))

## -----------------------------------------------------------------------------
# transformers
pow2 <- function(x) {x^2}
pow3 <- function(x) {x^3}
clog <- function(x) log(x+0.001)

confounders <- names(data)
confounders <- confounders[!(confounders %in% c("FIPS","Name","STATE", 
                                                "STATE_CODE","cms_mortality_pct",
                                                "qd_mean_pm25"))]

## -----------------------------------------------------------------------------

confounders_s1 <- c("cs_poverty","cs_hispanic",
                   "cs_black",
                   "cs_ed_below_highschool",
                   "cs_median_house_value",
                   "cs_population_density",
                   "cdc_mean_bmi","cdc_pct_nvsmoker",
                   "gmet_mean_summer_tmmx",
                   "gmet_mean_summer_rmx",
                   "gmet_mean_summer_sph",
                   "cms_female_pct", "region"
)

study_data <- data[, c("qd_mean_pm25", confounders, "cms_mortality_pct")]
study_data$region <- as.factor(study_data$region)
study_data$cs_PIR <- study_data$cs_median_house_value/study_data$cs_household_income

# Choose subset of data
q1 <- stats::quantile(study_data$qd_mean_pm25,0.25)
q2 <- stats::quantile(study_data$qd_mean_pm25,0.99)
trimmed_data <- subset(study_data[stats::complete.cases(study_data) ,],
                       qd_mean_pm25 <= q2  & qd_mean_pm25 >= q1)

trimmed_data$gmet_mean_summer_sph <- pow2(trimmed_data$gmet_mean_summer_sph)

set.seed(172)
pseudo_pop_1 <- generate_pseudo_pop(trimmed_data$cms_mortality_pct,
                                    trimmed_data$qd_mean_pm25,
                                    data.frame(trimmed_data[, confounders_s1,
                                                            drop=FALSE]),
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.0 ,
                                                       1.0),
                                    optimized_compile = TRUE,
                                    use_cov_transform = TRUE,
                                    transformers = list("pow2","pow3","clog"),
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=c(17),
                                                  xgb_eta=c(0.28)),
                                    nthread = 1,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

plot(pseudo_pop_1)

## -----------------------------------------------------------------------------
set.seed(172)
pseudo_pop_2 <- generate_pseudo_pop(trimmed_data$cms_mortality_pct,
                                    trimmed_data$qd_mean_pm25,
                                    data.frame(trimmed_data[, confounders_s1,
                                                            drop=FALSE]),
                                    ci_appr = "matching",
                                    pred_model = "sl",
                                    gps_model = "parametric",
                                    bin_seq = NULL,
                                    trim_quantiles = c(0.0 ,
                                                       1.0),
                                    optimized_compile = FALSE,
                                    use_cov_transform = TRUE,
                                    transformers = list("pow2","pow3","clog"),
                                    sl_lib = c("m_xgboost"),
                                    params = list(xgb_nrounds=c(17),
                                                  xgb_eta=c(0.28)),
                                    nthread = 1,
                                    covar_bl_method = "absolute",
                                    covar_bl_trs = 0.1,
                                    covar_bl_trs_type = "mean",
                                    max_attempt = 1,
                                    matching_fun = "matching_l1",
                                    delta_n = 0.1,
                                    scale = 1)

plot(pseudo_pop_2)


## -----------------------------------------------------------------------------
optimized_data_1 <- pseudo_pop_1$pseudo_pop[,c("w","gps","counter")]
nonoptimized_data_2 <- pseudo_pop_2$pseudo_pop[,c("w","gps","counter")]


print(paste("Number of rows of data in the optimized approach: ",
            nrow(optimized_data_1)))

print(paste("Number of rows of data in the non-optimized approach: ",
            nrow(nonoptimized_data_2)))

print(paste("Sum of data samples in the optimized approach: ",
            sum(optimized_data_1$counter)))
print(paste("Number of data in the non-optimized approach: ",
            length(nonoptimized_data_2$w)))

# Replicate gps values of optimized approach
expanded_opt_data_1 <- optimized_data_1[rep(seq_len(nrow(optimized_data_1)),
                                            optimized_data_1$counter), 1:3]

exp_gps_a_1 <- expanded_opt_data_1$gps
gps_b_1 <- nonoptimized_data_2$gps

differences <- sort(gps_b_1) - sort(exp_gps_a_1)

print(paste("Sum of differences in gps values between optimized and ", 
            "non-optimized approaches is: ",
            sum(differences)))

## -----------------------------------------------------------------------------
trimmed_data <- subset(study_data[stats::complete.cases(study_data) ,],
                       qd_mean_pm25 <= q2  & qd_mean_pm25 >= q1)

set.seed(8967)
pseudo_pop_3 <- generate_pseudo_pop(trimmed_data$cms_mortality_pct,
                                      trimmed_data$qd_mean_pm25,
                                      data.frame(trimmed_data[, confounders_s1,
                                                              drop=FALSE]),
                                      ci_appr = "matching",
                                      pred_model = "sl",
                                      gps_model = "non-parametric",
                                      bin_seq = NULL,
                                      trim_quantiles = c(0.0 ,
                                                         1.0),
                                      optimized_compile = TRUE,
                                      use_cov_transform = TRUE,
                                      transformers = list("pow2","pow3","clog"),
                                      sl_lib = c("m_xgboost"),
                                      params = list(xgb_nrounds=c(12),
                                                    xgb_eta=c(0.1)),
                                      nthread = 1,
                                      covar_bl_method = "absolute",
                                      covar_bl_trs = 0.1,
                                      covar_bl_trs_type = "mean",
                                      max_attempt = 1,
                                      matching_fun = "matching_l1",
                                      delta_n = 0.1,
                                      scale = 1)

plot(pseudo_pop_3)


## -----------------------------------------------------------------------------
trimmed_data <- subset(study_data[stats::complete.cases(study_data) ,],
                       qd_mean_pm25 <= q2  & qd_mean_pm25 >= q1)

trimmed_data$cs_poverty <- pow2(trimmed_data$cs_poverty)

set.seed(672)
pseudo_pop_4 <- generate_pseudo_pop(trimmed_data$cms_mortality_pct,
                                      trimmed_data$qd_mean_pm25,
                                      data.frame(trimmed_data[, confounders_s1,
                                                              drop=FALSE]),
                                      ci_appr = "weighting",
                                      pred_model = "sl",
                                      gps_model = "parametric",
                                      bin_seq = NULL,
                                      trim_quantiles = c(0.0 ,
                                                         1.0),
                                      optimized_compile = TRUE,
                                      use_cov_transform = TRUE,
                                      transformers = list("pow2","pow3","clog"),
                                      sl_lib = c("m_xgboost"),
                                      params = list(xgb_nrounds=c(35),
                                                    xgb_eta=c(0.14)),
                                      nthread = 1,
                                      covar_bl_method = "absolute",
                                      covar_bl_trs = 0.1,
                                      covar_bl_trs_type = "mean",
                                      max_attempt = 1,
                                      matching_fun = "matching_l1",
                                      delta_n = 0.1,
                                      scale = 1)

plot(pseudo_pop_4)

## -----------------------------------------------------------------------------
trimmed_data <- subset(study_data[stats::complete.cases(study_data) ,],
                       qd_mean_pm25 <= q2  & qd_mean_pm25 >= q1)

set.seed(328)
pseudo_pop_5 <- generate_pseudo_pop(trimmed_data$cms_mortality_pct,
                                      trimmed_data$qd_mean_pm25,
                                      data.frame(trimmed_data[, confounders_s1,
                                                              drop=FALSE]),
                                      ci_appr = "matching",
                                      pred_model = "sl",
                                      gps_model = "non-parametric",
                                      bin_seq = NULL,
                                      trim_quantiles = c(0.0 ,
                                                         1.0),
                                      optimized_compile = TRUE,
                                      use_cov_transform = TRUE,
                                      transformers = list("pow2","pow3","clog"),
                                      sl_lib = c("m_xgboost"),
                                      params = list(xgb_nrounds=seq(10, 100, 1),
                                                    xgb_eta=seq(0.1,0.5,0.01)),
                                      nthread = 1,
                                      covar_bl_method = "absolute",
                                      covar_bl_trs = 0.08,
                                      covar_bl_trs_type = "mean",
                                      max_attempt = 5,
                                      matching_fun = "matching_l1",
                                      delta_n = 0.1,
                                      scale = 1)

plot(pseudo_pop_5)

