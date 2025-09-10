#' Constructor for a sdid_mdl object
#'
#' @param model A lm object
#' @param vcov A variance-covariance matrix
#'
#' @return An object of class "sdid_mdl"
#' @export

new_sdid <- function(mdl, vcov = NULL, tsi, cohort, time, intervention_var, covariates) {
  if(!all(!vapply(cohort[c("var", "ref", "time_refs")], is.null, logical(1)))) {
    stop("cohort must contain a list with elements var, ref, and time_refs.")
  }

  if(!all(!vapply(time[c("var", "ref")], is.null, logical(1)))) {
    stop("time must contain a list with elements var and ref.")
  }

  structure(
    list(
      mdl = mdl,
      vcov = vcov,
      tsi = tsi,
      cohort = cohort,
      time = time,
      intervention_var = intervention_var,
      covariates = covariates
    ),
    class = "sdid_mdl"
  )
}
