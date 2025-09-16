#' Retrieve a list of interaction terms from a sdid model representing the pre-intervention period
#'
#' @param mdl sdid object
#' @param cohorts character vector containing cohort levels to include in the term selection. If `cohorts` is omitted, all available cohorts will be selected.
#'
#' @return character vector
#' @export select_period

select_period <- function(mdl, period = "post", cohorts = NULL) {
  # Validate that period is "pre" or "post"
  if(!period %in% c("pre", "post")) {
    stop("`period` must be set to \"pre\" or \"post\".")
  }

  # Validate that cohorts contains valid cohort levels
  if(!all(cohorts %in% unique(mdl$tsi$cohort))) {
    stop(paste0("One or more supplied values for cohorts (",
                paste(cohorts, collapse = ", "),
                ") are invalid cohort levels.\n",
                "Must match one or more of {",
                paste(unique(mdl$tsi$cohort), collapse = ", "),
                "}."))
  }

  # If the user didn't specify cohorts, set it to include all cohorts except
  # the referent
  if(is.null(cohorts)) {
    cohorts <- unique(mdl$tsi[mdl$tsi[["cohort"]] != mdl$cohort$ref, "cohort"])
  }

  # Restrict tsi dataset to the specified cohorts
  tsi <- mdl$tsi[mdl$tsi$cohort %in% cohorts, ]

  # Restrict tsi dataset to just the pre-intervention period
  if(period == "pre") {
    tsi <- tsi[tsi$tsi < 0, ]
  } else if(period == "post") {
    tsi <- tsi[tsi$tsi >= 0,]
  }

  # Exclude referent time periods
  for(cohort_lvl in unique(tsi$cohort)) {
    tsi$time_ref[tsi$cohort == cohort_lvl] <- mdl$cohort$time_refs[[cohort_lvl]]
  }
  tsi <- tsi[tsi$time != tsi$time_ref,]

  # Now retrieve the values of the relevant interaction terms
  tsi$coefs <- with(tsi,
                    paste0(mdl$cohort$var, "_", cohort,
                           ":",
                           mdl$time$var, "_", time))

  coefs <- tsi[, "coefs"]

  # Check that all terms appear in the list of coefficients
  if(!all(coefs %in% names(mdl$mdl$coefficients))) {
    stop("Selected terms (",
         paste(coefs, collapse = ", "),
         ") do not all appear in the model's list of coefficients.")
  } else return(coefs)
}
