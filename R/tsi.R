#' Identify time-since-intervention
#'
#' @description
#' `id_tsi()` uses a supplied data set to identify the number of time periods away from the intervention each observation is. This is important for plotting and for aggregating model coefficients in `ave_coeff()`.
#'
#' @param df data frame containing the variables in the model.
#' @param cohort_var name of the variable in `df` that contains cohort assignments.
#' @param time_var name of the variable in `df` that contains time periods.
#' @param intervention_var name of the cohort-level variable in `df` that specifies which values in `time_var` correspond to the first post-intervention time period for each cohort.
#'
#' @export id_tsi

id_tsi <- function(df,
                   cohort_var,
                   time_var,
                   intervention_var) {

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

  # To calculate time since intervention, subtract tsi from rn
  tsi$tsi <- tsi$rn - tsi$tsi

  return(new_tsi(tsi))
}
