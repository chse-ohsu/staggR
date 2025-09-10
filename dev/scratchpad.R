library(tidytable)
library(janitor)
library(devtools)
devtools::load_all()
data(hosp)


#' ****************************************************************************************************
#' # Fit model

hosp <- hosp %>% mutate(policy_yr = lubridate::year(intervention_dt))
#
# df <- hosp; y <- "y"; cohort_var = "cohort"; time_var = "yr"
# intervention_var <- "policy_yr"; covariates = c("age", "sex", "comorb")
# time_refs = as.character(2014:2017);
# time_refs = list(`5` = "2014",
#                  `6` = "2015",
#                  `7` = "2016",
#                  `8` = "2017")
# cohort_ref = "0"; .vcov = stats::vcov

df = hosp
y <- "y"
cohort_var = "cohort"
cohort_ref = "0"
cohort_time_refs = list(`5` = "2014",
                        `6` = "2015",
                        `7` = "2016",
                        `8` = "2017")
time_var = "yr"
time_ref = "2010"
intervention_var = "policy_yr"
covariates = c("age", "sex", "comorb")
.vcov = stats::vcov

sdid <- function(df, y, cohort_var, cohort_ref = NULL, cohort_time_refs = NULL,
                 time_var, time_ref,
                 intervention_var, covariates = NULL, .vcov = stats::vcov, ...) {
  # Make sure df is a data.frame
  df <- as.data.frame(df)

  # Validate cohort_var, time_var, intervention_var, and covariates
  if(!all(c(cohort_var, time_var, intervention_var, covariates) %in% names(df))) {
    stop("cohort_var, time_var, intervention_var, and all elements of covariates must match column names in df.")
  }

  # Prepare data by creating dummy variables
  df_prepped <- prep_data(df = df,
                          cohort_var = cohort_var,
                          time_var = time_var)

  # Define dummy variables
  cohort_dummies <- grep(paste0(cohort_var, "_"), names(df_prepped), value = TRUE)
  cohort_lvls <- sub(paste0(cohort_var, "_"), "", cohort_dummies)
  time_dummies <- grep(paste0(time_var, "_"), names(df_prepped), value = TRUE)
  time_lvls <- sub(paste0(time_var, "_"), "", time_dummies)

  # Define time period referents, if not specified in arguments
  if(is.null(cohort_time_refs)) {
    if(is.null(cohort_ref)) stop("cohort_time_refs or cohort_ref must be specified")
    cohort_time_refs <- pick_time_refs(df = df,
                                cohort_var = cohort_var,
                                cohort_ref = cohort_ref,
                                time_var = time_var)
  }

  # Check that cohort_time_refs is a list object corresponding to cohort levels
  if(class(cohort_time_refs) != "list" | any(sort(names(cohort_time_refs)) != sort(cohort_lvls))) {
    stop("cohort_time_refs must be a list object with elements named to match the levels of cohort_var.")
  }

  # Define the regression formula
  fml <- stats::formula(
    paste0(y, " ~ ",
           paste(
             c(cohort_dummies, # Fixed effects for cohorts,
               time_dummies[time_dummies != paste0(time_var, "_", time_ref)], # Fixed effects for time periods, exclude referent
               unlist(lapply(1:length(cohort_dummies), function(x) { # Fixed effects for cohort-time interactions
                 paste(cohort_dummies[[x]],
                       time_dummies[time_dummies != paste0(time_var, "_",
                                                           cohort_time_refs[names(cohort_time_refs) == cohort_lvls[[x]]])],
                       sep = ":")
               })),
               covariates),
             collapse = " + "))
  )

  # Fit model
  mdl <- lm(formula = fml,
            data = df_prepped)
  # vcv <- .vcov(mdl)
  vcv <- .vcov(mdl, ...)

  # Identify time since intervention
  # Create a dataset that contains all the unique values of cohort_var,
  # time_var, and tsi_var.
  tsi <- unique(df[order(df[[cohort_var]], df[[time_var]]),
                                   c(cohort_var, time_var, intervention_var)])

  # Change column names so we don't have to keep referencing them dynamically
  colnames(tsi) <- c("cohort", "time", "intervention_time")

  # Number the rows within each cohort
  tsi$rn <- ave(
    seq_len(nrow(tsi)), # the sequence to number
    tsi$cohort,         # grouping variable
    FUN = seq_along                     # restart sequence for each group
  )

  # Identify the value of time_var that matches the value of tsi_var for
  # each cohort
  tsi$tsi <-
    with(tsi,
         rn[match(paste0(cohort, "_", intervention_time),
                  paste0(cohort, "_", time))])

  # To calculate time since intervention, subtract tsi from rn and add 1
  tsi$tsi <- tsi$rn - tsi$tsi

  # Create return object
  rslts <- new_sdid(mdl = mdl,
                    vcov = vcov,
                    tsi = tsi,
                    cohort = list(var = cohort_var,
                                  ref = cohort_ref,
                                  time_refs = cohort_time_refs),
                    time = list(var = time_var,
                                ref = time_ref),
                    intervention_var = intervention_var,
                    covariates = covariates)
  return(rslts)
}

#' Prep the dataset for modeling
# newhosp <-
#   hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
#                                                   TRUE ~ paste0("201", cohort)),
#                   policy_implement_yr = as.character(lubridate::year(intervention_dt)))

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

# ave_coeff(sdid = foo,
#           df = newhosp,
#           select_vars = NULL,
#           cohort_numbers = 5:8,
#           tsi_numbers = 0:5,
#           cohort_name = "cohort")


