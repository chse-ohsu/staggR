library(tidytable)
library(janitor)
library(devtools)
devtools::load_all()
data(hosp)


#' ****************************************************************************************************
#' # Fit model

hosp <- hosp %>% mutate(policy_yr = lubridate::year(intervention_dt))

foo <- sdid(df = hosp,
            y <- "y",
            cohort_var = "cohort",
            cohort_ref = "0",
            cohort_time_refs = list(`5` = "2014",
                                    `6` = "2015",
                                    `7` = "2016",
                                    `8` = "2017"),
            time_var = "yr",
            time_ref = "2010",
            intervention_var = "policy_yr",
            covariates = c("age", "sex", "comorb"))
ave_coeff(sdid = foo,
          coefs = select_period(mdl = foo,
                                period = "pre"))
ave_coeff(sdid = foo,
          coefs = select_period(mdl = foo,
                                period = "post"))


foo <- sdid("y ~ cohort + yr + age + sex + comorb",
            df = hosp,
            intervention_var = "policy_yr")
ave_coeff(sdid = foo,
          coefs = select_period(mdl = foo,
                                period = "pre"))
ave_coeff(sdid = foo,
          coefs = select_period(mdl = foo,
                                period = "post"))

#' ****************************************************************************************************
#' # Experiment with aggregated data

hosp_agg <- hosp %>%
  summarise(pct_y = mean(y),
            n_enr = n_distinct(guid),
            mean_age = mean(age),
            pct_fem = mean(sex == "F"),
            pct_cmb = mean(comorb),
            .by = c("yr", "grp", "cohort", "policy_yr"))

fooagg <- sdid("pct_y ~ cohort + yr + mean_age + pct_fem + pct_cmb",
               df = hosp_agg,
               weights = "n_enr",
               intervention_var = "policy_yr")
ave_coeff(sdid = fooagg,
          coefs = select_period(mdl = fooagg,
                                period = "pre"))
ave_coeff(sdid = fooagg,
          coefs = select_period(mdl = fooagg,
                                period = "post"))



### Argument assignments for troubleshooting

formula <- "y ~ cohort + yr + age + sex + comorb"
df = hosp
weights = NULL
intervention_var = "policy_yr"
cohort_var = NULL; cohort_ref = NULL; cohort_time_refs = NULL;
time_var = NULL; time_ref = NULL;
.vcov = stats::vcov

formula <- "pct_y ~ cohort + yr + mean_age + pct_fem + pct_cmb"
weights = "n_enr"
df = hosp_agg
intervention_var = "policy_yr"
cohort_var = NULL; cohort_ref = NULL; cohort_time_refs = NULL;
time_var = NULL; time_ref = NULL;
covariates = NULL; .vcov = stats::vcov



#' ****************************************************************************************************
#' # Experiment with qt_pos data
load("dev/c1d0.RData")
fml_covariates1_detrend0
df_qtpos <- pq %>%
  select(hash_id, state_abbr,
         residential,
         waiver_dt,
         cohort, study_qtr, yr, qtr,
         age_grp, sex, elig_grp, mh, sud) %>%
  # Study quarter corresponding to waiver_dt takes same value as cohort
  mutate(waiver_qtr = cohort)


qtpos <- sdid("residential ~ cohort + study_qtr + age_grp + sex + elig_grp + mh + sud",
              df = df_qtpos,
              intervention_var = "waiver_qtr",
              .vcov = sandwich::vcovCL,
              cluster = df_qtpos$state_abbr)

ave_coeff(sdid = qtpos,
          coefs = select_period(mdl = qtpos,
                                period = "pre"))
ave_coeff(sdid = qtpos,
          coefs = select_period(mdl = qtpos,
                                period = "post"))
