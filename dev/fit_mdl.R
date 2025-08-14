#' ********************
#' ## Load Required Packages

library(tidytable)
library(magrittr)
library(janitor)
# library(sandwich)
# library(lmtest)
devtools::load_all()
data(hosp)

#' ****************************************************************************************************
#' Define regression functions
ave_coeff <- function(reg_model, reg_data, reg_vcov, select_vars=NULL, cohort_name, cohort_numbers, tsi_numbers, pop_var=NULL){
  ## Function to calculate a population-weighted average of regression coefficients
  ## reg_model:           regression model
  ## reg_data:            dataset used in regression
  ## reg_vcov:            variance-covariance from regression
  ## select_vars:         list of variables selected for averaging; if null, then select_cohorts, time_intervention and name_cohort needs to be specified
  ## cohort_name:         character string for how cohorts are named (e.g., wedge).  Needs to be specificed if select_vars is null.
  ## cohort_numbers:      list of cohorts numbers selected for averaging.  Needs to be specificed if select_vars is null.
  ## tsi_numbers:         list of time since intervention periods selected for averaging.  Needs to be specificed if select_vars is null.
  ## pop_var:             Variable showing population numbers (aggregate data only); if null, use nrow (i.e., number of rows = number of observations)
  ## rates_as_percentage  whether rates should be multiplied by 100 before returning results


  ## Step 1: Translate cohort / time since intervention information to variables to use
  if (is.null(select_vars)){

    ## For each cohort and time since intervention, translate into cohort-study_quarter interaction term
    select_vars <- unlist(lapply(cohort_numbers, function(cohort_number)  paste0(cohort_name, "_", sprintf("%01.0f",cohort_number), ":yr_", sprintf("%02.0f", 2010 + cohort_number + tsi_numbers))))

    ## Select those included in the regression (some may not due to left / right censoring)
    select_vars <- select_vars[select_vars %in% names(reg_model$coef)]

  }

  # Only run the rest of the function if there are select vars left after filtering by those in the model.
  # This is necessary because we sometimes omit terms in the model when there are no observations corresponding
  # to the interaction.
  if(length(select_vars) > 0) {

    ## Step 2: Calculate population fractions and extract estimates

    ## Number of observations
    if (is.null(pop_var)) {
      ave_num <- sapply(select_vars, function(select_var) nrow(reg_data[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1", sep="", collapse=" & "))),]))
    } else {
      ave_num <- sapply(select_vars, function(select_var)      reg_data[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1", sep="", collapse=" & "))),sum(get(pop_var))])
    }

    ## Pct of population for each coefficient
    ave_pct <- ave_num/sum(ave_num)


    ## Extract estimates selected for averaging
    select_est  <- sapply(select_vars, function(x) reg_model$coef[names(reg_model$coef)== x])


    ## Step 3: Calculate average estimate and corresponding SE, p-value, CI

    ## Calculcate weighted estimate
    ave_est <- sum(ave_pct * select_est) #One weighted estimate


    ## Extract relevant part of variance-covariance matric
    select_vcv <- reg_vcov[select_vars, select_vars]


    ## Get SE of average estimate and convert from a matrix to a vector
    ave_se <- as.vector(sqrt(t(as.matrix(ave_pct)) %*% select_vcv %*% as.matrix(ave_pct)))

    ## Extract degrees of freedom, which we use for our P value
    df <- reg_model$df.residual

    ## Calculate p-value, assuming normal distribution
    ave_pval <-  2*pt(abs(ave_est/ave_se), df, lower=FALSE)

    ## Make into a reg_data table
    ave_res <- data.table(est  = ave_est,
                          se   = ave_se ,
                          pval = ave_pval,
                          sign = data.table::fcase(ave_pval < .01                  , "***",
                                                   ave_pval >= .01 & ave_pval < .05, "**" ,
                                                   ave_pval >  .05 & ave_pval < .1 , "*"  ,
                                                   ave_pval >= .1                  , ""   ),
                          lb   = (ave_est - 1.96*ave_se),
                          ub   = (ave_est + 1.96*ave_se),
                          n    = sum(ave_num))
  } else {
    ave_res <- data.table(est  = NA,
                          se   = NA ,
                          pval = NA,
                          sign = "",
                          lb   = NA,
                          ub   = NA,
                          n    = 0)
  }
  return(ave_res)
}


#' Function to define FEs
define_fes <- function(y, cohorts, times, covariates = NULL, omit_ident = TRUE) {
  # Omit lowest value for cohorts and times for regression identification
  if(omit_ident == TRUE) {
    cohorts <- as.list(sort(unlist(cohorts))[2:length(cohorts)])
    times <- as.list(sort(unlist(times))[2:length(times)])
  }

  return(
    stats::formula(
      paste0(y, " ~ ",
             paste(
               c(names(cohorts), names(times),
                 unlist(lapply(names(cohorts), function(x) {
                   paste(x,
                         names(Filter(function(y) y != cohorts[[x]]-1, times)),
                         sep = ":")
                 })),
                 covariates),
               collapse = " + "))
    )
  )
}

define_fes(y = "y",
           cohorts = purrr::set_names(as.list(c(0, 5:8)), paste0("cohort_", c(0, 5:8))),
           times = purrr::set_names(as.list(0:10), paste0("yr_", 2010:2020)),
           covariates = c("age", "sex", "comorb"))

#' Function to convert categorical cohort & time to dummy variables


#' ****************************************************************************************************
#' # Fit model

#' ******************************
#' Prep data
foo <- prep_data(df = hosp,
                 cohort_var = "cohort",
                 time_var = "yr")


#' ******************************
#' Define regression formula

fml_hosp <- define_fes(y = "y",
                       cohorts = purrr::set_names(as.list(c(0, 5:8)), paste0("cohort_", c(0, 5:8))),
                       times = purrr::set_names(as.list(0:10), paste0("yr_", 2010:2020)),
                       covariates = c("age", "sex", "comorb"))


#' The function call I'd like to end up with:
#' mdl_foobar <- sdid(y = "y",
#'                       cohort = "cohort",
#'                       time = "yr",
#'                       covariates = c("age", "sex", "comorb"),
#'                       data = foo)




hosp %>% tabyl(cohort)
data <- hosp %>%
  mutate(new_cohort = case_when(cohort == "0" ~ "0",
                                TRUE ~ paste0("201", cohort)))

cohort_var = "new_cohort",
cohort_ref = "0",
time_var = "yr"

#' Function to identify time period referents if the user doesn't specify but instead
#' names cohorts with suffixes corresponding to intervention time periods.
find_time_refs <- function(data, cohort_var, cohort_ref, time_var, time_offset = -1) {
  if(!cohort_var %in% names(data)) stop("Invalid cohort_var")
  if(!time_var %in% names(data)) stop("Invalid time_var")

  cohort_lvls <- sort(unique(data[[cohort_var]][data[[cohort_var]] != cohort_ref]))
  time_lvls <- unique(data[[time_var]])

  time_refs <- lapply(1:length(cohort_lvls), function(i) {
    time_ref <- as.character(as.integer(cohort_lvls[[i]]) + time_offset)
    if(time_ref %in% time_lvls) {
      return(time_ref)
    } else {
      stop("No corresponding time period level for cohort index ", cohort_lvls[[i]])
    }
  })
  names(time_refs) <- cohort_lvls

  return(time_refs)
}

find_time_refs(data = hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
                                                             TRUE ~ paste0("201", cohort))),
               cohort_var = "new_cohort",
               cohort_ref = "0",
               time_var = "yr")

data = hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
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
#' TODO: Here I'm building the modeling function, which takes a simple data set,
#' preps it for modeling, runs the model & the vcov, and returns a summary object.

sdid <- function(data, y, cohort_var, time_var, covariates = NULL, time_refs = NULL, cohort_ref = NULL, .vcov = stats::vcov, ...) {
  # Prepare data by creating dummy variables
  df <- prep_data(df = data,
                  cohort_var = cohort_var,
                  time_var = time_var)

  # Define dummy variables
  cohort_dummies = grep(paste0(cohort_var, "_"), names(df), value = TRUE)
  cohort_lvls <- sub(paste0(cohort_var, "_"), "", cohort_dummies)
  time_dummies = grep(paste0(time_var, "_"), names(df), value = TRUE)

  # Define time period referents, if not specified in arguments
  if(is.null(time_refs)) {
    if(is.null(cohort_ref)) stop("time_refs or cohort_ref must be specified")
    time_refs <- find_time_refs(data = data,
                                cohort_var = cohort_var,
                                cohort_ref = cohort_ref,
                                time_var = time_var)
  }
  ## TODO: This needs to be revised to work with vectors instead of lists. This effort
  ##       also highlights that we need the user to specify what the reference period is
  ##       for each cohort, probably by creating a new column with that info and passing
  ##       the name of the column as a param in this function.
  fml <- stats::formula(
    paste0(y, " ~ ",
           paste(
             c(cohort_dummies, time_dummies,
               unlist(lapply(1:length(cohort_dummies), function(x) {
                 paste(cohort_dummies[[x]],
                       time_dummies[time_dummies != paste0(time_var, "_", time_refs[names(time_refs) == cohort_lvls[[x]]])],
                       sep = ":")
               })),
               covariates),
             collapse = " + "))
  )

  # Fit model
  mdl <- lm(formula = fml,
            data = df)
  vcv <- .vcov(mdl, ...)

  rslts <- list()
  return(vcv)
}

sdid(data = hosp %>% mutate(new_cohort = case_when(cohort == "0" ~ "0",
                                                   TRUE ~ paste0("201", cohort))),
     y <- "y",
     cohort_var = "new_cohort",
     time_var = "yr",
     covariates = c("age", "sex", "comorb"),
     cohort_ref = "0")

#' ****************************************************************************************************
#' # Fit models

#' ******************************
#' ## Model: covariates0, detrend0
tic("Regression: No detrending")

#' Estimate regression
mdl_detrend0 <- lm(formula = fml_hosp,
                   data = foo)

#' Estimate clustered variance-covariance
vcv_detrend0 <- sandwich::vcovCL(x = mdl_detrend0,
                                 cluster = foo$grp)

ave_coeff(reg_model = mdl_detrend0,
          reg_vcov = vcv_detrend0,
          reg_data = foo,
          select_vars = NULL,
          cohort_numbers = 5:8,
          tsi_numbers = 0:5,
          cohort_name = "cohort")




#' Calculate average estimates
est_covariates0_detrend0 <- list_ave_coeff(reg_dd = mdl_covariates0_detrend0,
                                           vcv_dd = vcv_covariates0_detrend0,
                                           dx = pq,
                                           pre_detrend = pre_detrend_qtrs)
toc()

#' ******************************
#' ## Model: covariates1, detrend0
tic("Regression: Adjust for covariates, do not detrend (covariates1, detrend0)\n")

#' Estimate regression
mdl_covariates1_detrend0 <- lm(formula = fml_covariates1_detrend0, data = pq)

#' Estimate clustered variance-covariance
vcv_covariates1_detrend0 <- vcovCL(x = mdl_covariates1_detrend0,
                                   cluster = pq$state_abbr)

#' Calculate average estimates
est_covariates1_detrend0 <- list_ave_coeff(reg_dd = mdl_covariates1_detrend0,
                                           vcv_dd = vcv_covariates1_detrend0,
                                           dx = pq,
                                           pre_detrend = pre_detrend_qtrs)
toc()
