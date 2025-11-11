library(tidytable)
library(janitor)
library(devtools)
devtools::load_all()
data(hosp)


#' ****************************************************************************************************
#' # Fit model

sdid_hosp <- sdid("hospitalized ~ cohort + yr + age + sex + comorb",
                  df = hosp,
                  intervention_var = "intervention_yr",
                  .vcov = sandwich::vcovCL,
                  cluster = hosp$county)
ave_coeff(sdid = sdid_hosp,
          coefs = select_period(sdid = sdid_hosp,
                                period = "pre"))
ave_coeff(sdid = sdid_hosp,
          coefs = select_period(sdid = sdid_hosp,
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
intervention_var = "intervention_yr"
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



#' ****************************************************************************************************
#' # Plotting function
library(ggplot2)

df <- hosp
y <- "hospitalized"
group <- "county"
time_var <- "yr"
intervention_var <- "intervention_yr"
ncol <- 3

# Calculate time, in years, relative to the intervention year
tsi_df <- tsi(df = hosp, cohort_var = cohort_var, time_var = time_var, intervention_var = intervention_var)
# df$tsi[is.na(df$tsi)] <- 0 ## <- Might need this


# Replace missing intervention years for comparison counties
# hosp$intervention_yr[is.na(hosp$intervention_yr)] <- ""

# Plot time series for all counties
df[[intervention_var]] <- ifelse(is.na(df[[intervention_var]]),
                                 yes = "", no = df[[intervention_var]])


agg <- aggregate(as.formula(paste0(y, " ~ ", group, " + ", time_var, " + ", intervention_var)),
          data = df,
          FUN = mean)

agg[[time_var]] <- as.character(agg[[time_var]])
agg[[intervention_var]] <- as.character(agg[[intervention_var]])

ggplot(data = agg,
       aes(x = .data[[time_var]], y = .data[[y]], group = .data[[group]])) +
  facet_wrap(~ .data[[group]], ncol = ncol) +
  geom_point(stat = "identity", size = 1) +
  geom_line(stat = "identity", linewidth = 1.0, alpha = 0.8) +
  geom_vline(aes(xintercept = .data[[intervention_var]]),
             color = "blue") +
  theme_light()



ggplot()
