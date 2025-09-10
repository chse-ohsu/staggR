#' Retrieve a list of interaction terms from a sdid model to be passed on for aggregation
#'
#' @param mdl sdid object
#' @param coefs optional list of specific terms from `mdl` to be selected
#' @param selection list object containing values for named elements `cohorts`, `times`, and `tsi`. `cohorts` contains a character vector of cohort levels to include in the term selection; `times` contains a character vector of time period levels to include in the term selection; and `tsi` contains a vector of integers representing the number of units of time relative to each cohort's intervention to include in the term selection. If `cohorts` is omitted, all available cohorts will be selected. One of `times` or `tsi` must be specified. If both are specified, `times` is ignored.
#'
#' @return character vector
#' @export select_terms

select_terms <- function(mdl, coefs = NULL, selection = NULL) {
  # Validate that selection$cohorts contains valid cohort levels
  if(is.null(coefs) & !all(selection$cohorts %in% unique(mdl$tsi$cohort))) {
    stop(paste0("One or more supplied values for cohorts (",
                paste(selection$cohorts, collapse = ", "),
                ") are invalid cohort levels.\n",
                "Must match one or more of {",
                paste(unique(mdl$tsi$cohort), collapse = ", "),
                "}."))
  }

  # If the user specified coefs, check that all coefs appear in the model
  if(!is.null(coefs)) {
    if(!all(coefs %in% names(mdl$mdl$coefficients))) {
      stop(paste0("Specified coefs must be contained in sdid model coefficients."))
    }
  }

  # If the user didn't specify selection$cohorts, set it to include all cohorts except
  # the referent
  if(is.null(selection$cohorts)) {
    selection$cohorts <-
      unique(mdl$tsi[mdl$tsi[["cohort"]] != mdl$cohort$ref, "cohort"])
  }

  # The user specified cohorts and times, but not tsi
  if(is.null(coefs) &
     !is.null(selection$times) & is.null(selection$tsi)) {
    prelim_coefs <- with(mdl$cohort, paste0(
      var, "_", rep(selection$cohorts[selection$cohorts != ref],
                       each = length(selection$times)),
      ":", mdl$time$var, "_", rep(selection$times,
                          times = length(selection$cohorts))))
    # Check that all terms appear in the list of coefficients
    if(!all(prelim_coefs %in% names(mdl$mdl$coefficients))) {
      stop("This generates named interaction terms that do not appear in the model's coefficients.")
    } else coefs <- prelim_coefs

    # The user specified cohorts and tsi
  } else if(is.null(coefs) &
            !is.null(selection$tsi)) {

    # If selection$times is not null, display a warning
    if(!is.null(selection$times)) {
      warning(paste0("Because `tsi` is specified with values {",
                     paste(selection$tsi, collapse = ", "),
                     "}, all values specified for `times` {",
                     paste(selection$times, collapse = ", "),
                     "} will be ignored."))
    }

    # Restrict tsi dataset to the specified cohorts
    tsi <- mdl$tsi[mdl$tsi$cohort %in% selection$cohorts, ]

    # Restrict tsi dataset to just the requested tsis
    tsi <- tsi[tsi$tsi %in% selection$tsi, ]

    # Exclude referent time periods
    for(cohort_lvl in unique(tsi$cohort)) {
      tsi$time_ref[tsi$cohort == cohort_lvl] <- mdl$cohort$time_refs[[cohort_lvl]]
    }
    tsi <- tsi[tsi$time != tsi$time_ref,]

    # If there are invalid tsis passed through the selection list, display a warning
    # invalid_tsis <-
    for(cohort in unique(tsi$cohort)) {
      warning(paste0("Supplied `tsi` value(s) (",
                     paste(selection$tsi[!(selection$tsi %in% tsi[tsi$cohort == cohort, "tsi"])],
                           collapse = ", "),
                     ") are invalid for cohort ", cohort, "."))
    }

    # Now retrieve the values of the relevant interaction terms
    tsi$coefs <- with(tsi,
                       paste0(mdl$cohort$var, "_", cohort,
                              ":",
                              mdl$time$var, "_", time))

    prelim_coefs <- tsi[, "coefs"]

    # Check that all terms appear in the list of coefficients
    if(!all(prelim_coefs %in% names(mdl$mdl$coefficients))) {
      stop("This generates named interaction terms that do not appear in the model's coefficients.")
    } else coefs <- prelim_coefs

  }

  # The user didn't specify enough values in selection list
  else if(is.null(coefs) &
          is.null(selection$times) &
          is.null(selection$tsi)) {
    stop("Must specify one of `coefs`, `times`, or `tsi`.")
  }

  # Check that all terms appear in the list of coefficients
  if(!all(coefs %in% names(mdl$mdl$coefficients))) {
    stop("Selected terms (",
         paste(coefs, collapse = ", "),
         ") do not all appear in the model's list of coefficients.")
  } else return(coefs)
}
