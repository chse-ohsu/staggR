#' Identify time period referents for cases when the user doesn't specify but instead names cohorts with suffixes corresponding to intervention time periods.
#'
#' @param df data frame containing the variables in the model.
#' @param cohort_var string specifying the name of the column in `df` that defines the intervention cohorts. If using this function, each cohort must be named to match the value of `time_var` that corresponds to the intervention period.
#' @param cohort_ref an optional string specifying the value of `cohort_var` to be used as the referent in the model. If not specified, the value is taken from the first observed value in `cohort_var`.
#' @param time_var string specifying the name of the column in `df` that defines time periods over the study.
#' @param time_offset integer specifying which time period relative to the intervention time period should be used as the referent for each cohort. Defaults to -1, which corresponds to the time period immediately preceding intervention.
#'
#' @return list
#' @export prep_data

pick_time_refs <- function(df, cohort_var, cohort_ref, time_var, time_offset = -1) {
  if(!cohort_var %in% names(df)) stop("Invalid cohort_var")
  if(!time_var %in% names(df)) stop("Invalid time_var")

  cohort_lvls <- sort(unique(df[[cohort_var]][df[[cohort_var]] != cohort_ref]))
  time_lvls <- sort(unique(df[[time_var]]))

  time_refs <- lapply(1:length(cohort_lvls), function(i) {
    time_ref <- as.character(as.integer(cohort_lvls[[i]]) + time_offset)
    if(time_ref %in% time_lvls) {
      return(time_ref)
    } else {
      stop("No time period level '", time_ref,
           "' corresponding to cohort index ", cohort_lvls[[i]], "\n",
           "Either pass a named list to parameter `time_refs` or ",
           "make sure cohort levels match time period levels.")
    }
  })
  names(time_refs) <- cohort_lvls

  return(time_refs)
}
