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
