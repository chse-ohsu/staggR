library(tidytable)
library(janitor)
library(devtools)
devtools::load_all()
data(hosp)

#' ****************************************************************************************************
#' Define ave_coeff() function


#' ****************************************************************************************************
#' # Fit model

df <- hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
                                              TRUE ~ paste0("201", cohort)))
y <- "y"
cohort_var = "new_cohort"
time_var = "yr"
covariates = c("age", "sex", "comorb")
# time_refs <- list("5" = "2014",
#                   "6" = "2015",
#                   "7" = "2016",
#                   "8" = "2017")
time_refs = NULL
cohort_ref <- "0"
.vcov = stats::vcov
#' TODO: Here I'm building the modeling function, which takes a simple data set,
#' preps it for modeling, runs the model & the vcov, and returns a summary object.

sdid <- function(df, y, cohort_var, time_var, covariates = NULL, time_refs = NULL, cohort_ref = NULL, .vcov = stats::vcov, ...) {
  # Prepare data by creating dummy variables
  df_prepped <- prep_data(df = df,
                          cohort_var = cohort_var,
                          time_var = time_var)

  # Define dummy variables
  cohort_dummies = grep(paste0(cohort_var, "_"), names(df_prepped), value = TRUE)
  cohort_lvls <- sub(paste0(cohort_var, "_"), "", cohort_dummies)
  time_dummies = grep(paste0(time_var, "_"), names(df_prepped), value = TRUE)

  # Define time period referents, if not specified in arguments
  if(is.null(time_refs)) {
    if(is.null(cohort_ref)) stop("time_refs or cohort_ref must be specified")
    time_refs <- pick_time_refs(df = df,
                                cohort_var = cohort_var,
                                cohort_ref = cohort_ref,
                                time_var = time_var)
  }

  # Define the regression formula
  fml <- stats::formula(
    paste0(y, " ~ ",
           paste(
             c(cohort_dummies, # Fixed effects for cohorts,
               time_dummies, # Fixed effects for time periods
               unlist(lapply(1:length(cohort_dummies), function(x) { # Fixed effects for cohort-time interactions
                 paste(cohort_dummies[[x]],
                       time_dummies[time_dummies != paste0(time_var, "_", time_refs[names(time_refs) == cohort_lvls[[x]]])],
                       sep = ":")
               })),
               covariates),
             collapse = " + "))
  )

  # Fit model
  mdl <- lm(formula = fml,
            data = df_prepped)
  vcv <- .vcov(mdl, ...)

  rslts <- new_sdid(mdl = mdl,
                    vcov = vcov)
  return(rslts)
}

#' Prep the dataset for modeling
newhosp <- hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
                                                  TRUE ~ paste0("201", cohort)))

foo <- sdid(df = newhosp,
            y <- "y",
            cohort_var = "new_cohort",
            time_var = "yr",
            covariates = c("age", "sex", "comorb"),
            cohort_ref = "0")


class(foo)
names(foo)

ave_coeff(sdid = foo,
          df = newhosp,
          select_vars = NULL,
          cohort_numbers = 5:8,
          tsi_numbers = 0:5,
          cohort_name = "cohort")

sdid = foo
df = newhosp
select_vars = NULL
cohort_numbers = 5:8
tsi_numbers = 0:5
cohort_name = "cohort"
