#' Fit a staggered difference-in-differences model
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under 'Details'.
#' @param df data frame containing the variables in the model.
#' @param weights an optional vector of weights to be passed to `lm()` to be used in the fitting process. Should be NULL or a numeric vector.
#' @param cohort_var name of the variable in `df` that contains cohort assignments. If NULL, this is assumed to be the first column named in the right hand side of `formula`.
#' @param cohort_ref value of `cohort_var` that serves as the referent for main effects for cohorts. If NULL, this is assumed to the be the first value in the set of values for `cohort_var`.
#' @param cohort_time_refs a list, whose elements are named to match levels of `cohort_var`, specifying the value of `time_var` that serves as the referent for each time interaction with values of `cohort_var`. See 'Details.'
#' @param time_var name of the variable in `df` that contains time periods. If NULL, this is assumed to be the second column named in the right hand side of `formula`.
#' @param time_ref value of `time_var` that serves as the referent for main effects for time periods. If NULL, this is assumed to the be the first value in the set of values for `time_var`.
#' @param intervention_var name of the cohort-level variable in `df` that specifies which values in `time_var` correspond to the first post-intervention time period for each cohort.
#' @param .vcov Function to be used to estimate the variance-covariance matrix. Defaults to stats::vcov
#' @param ... additional arguments to be passed to `.vcov`.
#'
#' @return sdid
#' @export sdid

sdid <- function(formula,
                 df,
                 weights = NULL,
                 cohort_var = NULL, cohort_ref = NULL, cohort_time_refs = NULL,
                 time_var = NULL, time_ref = NULL,
                 intervention_var, .vcov = stats::vcov, ...) {

  # Make sure df is a data.frame
  df <- as.data.frame(df)

  # Define dependent variable from formula
  formula <- stats::formula(formula)
  y <- formula[[2]]

  # Retrieve RHS terms from formula
  trm <- terms(formula)

  ## If cohort_var is not specified, pull it from the first RHS term in the formula
  if(is.null(cohort_var)) cohort_var <- attr(trm,  "term.labels")[[1]]

  ## If time_var is not specified, pull it from the second RHS term in the formula
  if(is.null(time_var)) time_var <- attr(trm, "term.labels")[[2]]

  ## If covariates is not specified, pull it from the remaining RHS terms in the formula
  if(is.null(covariates)) covariates <-
    attr(trm, "term.labels")[!(attr(trm, "term.labels") %in% c(cohort_var, time_var))]

  # Validate cohort_var, time_var, intervention_var, and covariates
  if(!all(c(cohort_var, time_var, intervention_var, covariates) %in% names(df))) {
    stop("cohort_var, time_var, intervention_var, and all elements of covariates must match column names in df.")
  }
  # Make sure intervention_var is consistent within each cohort
  if(nrow(unique(df[, c(cohort_var, intervention_var)])) != length(unique(df[[cohort_var]]))) {
    stop("Values of `intervention_var` are not consistent within each cohort.")
  }

  # If cohort_ref is not specified, choose the first level
  if(is.null(cohort_ref)) cohort_ref <- levels(factor(df[[cohort_var]]))[[1]]

  # If time_ref is not specified, choose the first level
  if(is.null(time_ref)) time_ref <- levels(factor(df[[time_var]]))[[1]]

  # Prepare data by creating dummy variables
  df_prepped <- prep_data(df = df,
                          cohort_var = cohort_var,
                          time_var = time_var)

  # Define dummy variables
  cohort_dummies <- grep(paste0(cohort_var, "_"), names(df_prepped), value = TRUE)
  cohort_lvls <- sub(paste0(cohort_var, "_"), "", cohort_dummies)
  time_dummies <- grep(paste0(time_var, "_"), names(df_prepped), value = TRUE)
  time_lvls <- sub(paste0(time_var, "_"), "", time_dummies)

  # Gather counts of observations for all possible cohort-time interactions
  obs_cnt <- expand.grid(cohort_dummies, time_dummies)
  colnames(obs_cnt) <- c("cohort", "time")
  obs_cnt$n_obs <- mapply(function(cohort, time) {
    nrow(df_prepped[eval(parse(text = paste0("df_prepped$", cohort, "==1 & ",
                                             "df_prepped$", time, "==1"))), ])
  },
  obs_cnt$cohort, obs_cnt$time)

  # Define time period referents, if not specified in arguments
  if(is.null(cohort_time_refs)) {
    cohort_time_refs <- pick_time_refs(df = df,
                                       cohort_var = cohort_var,
                                       cohort_ref = cohort_ref,
                                       time_var = time_var,
                                       intervention_var = intervention_var)
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
  ## Unweighted
  if(is.null(weights)) {
  mdl <- lm(formula = fml,
            data = df_prepped)
  } else {
    if(!(weights %in% colnames(df_prepped))) {
      stop("Supplied `weights` column `",
           weights,
           "` does not exist in ",
           deparse(substitute(df)), ".")
    } else{
    mdl <- lm(formula = fml,
              data = df_prepped,
              weights = df_prepped[[weights]])
    }
  }
  ## Weighted
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
    FUN = seq_along     # restart sequence for each group
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
                    formula = list(supplied = formula,
                               generated = fml),
                    vcov = vcv,
                    tsi = tsi,
                    obs_cnt = obs_cnt,
                    cohort = list(var = cohort_var,
                                  ref = cohort_ref,
                                  time_refs = cohort_time_refs),
                    time = list(var = time_var,
                                ref = time_ref),
                    intervention_var = intervention_var,
                    covariates = covariates)
  return(rslts)
}
