#' Aggregate a specified set of terms and corresponding standard errors from an sdid model object
#'
#' @param sdid sdid object containing the model to summarize
#' @param coefs Character vector containing the names of coefficients to
#' aggregate. Can be specified using `select_period()` or `select_terms()`.
#' @return data.frame
#' @export ave_coeff
#' @examples
#' # First fit a model to generate a sdid object
#' sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
#'                   df = hosp,
#'                   intervention_var  = "intervention_yr")
#'
#' # Then request an average of a specified set of coefficients. Here we use the
#' # select_period() convenience function to automatically select all
#' # coefficients representing the post-intervention period.
#' ave_coeff(sdid_hosp, coefs = select_period(sdid_hosp, period = "post"))
#'
#' # We could also specify the coefficients manually. Here we request the
#' # average effect for Cohort 5 in the post-intervention period.
#' ave_coeff(sdid_hosp, coefs = c("cohort_5:yr_2015", "cohort_5:yr_2016",
#'                                "cohort_5:yr_2017", "cohort_5:yr_2018",
#'                                "cohort_5:yr_2019", "cohort_5:yr_2020"))

ave_coeff <- function(sdid, coefs, name = "", type = NULL, times = NULL) {
  if(is.null(type)) {
    return(ave_coeff_main(sdid, coefs, name))
  }
  type <- match.arg(type, c("es", "calendar"))
  switch(type,
         es = ave_coeff_es(sdid, times),
         calendar = ave_coeff_calendar(sdid, times))
}

ave_coeff_main <- function(sdid, coefs, name) {
  # Make sure coefs is not null
  if(is.null(coefs)) {
    stop("Must specify `coefs`.")
  }

  # Make sure the specified coefs exist in the model
  if(!all(coefs %in% names(sdid$mdl$coefficients))) {
    stop("One or more specified coefs (",
         paste(coefs, collapse = ", "),
         ") do not exist in the supplied sdid model object.")
  }

  # Step 1: Calculate population fractions and extract estimates

  ## Number of observations for each specified coeff
  n_obs <- sapply(coefs, function(coeff) {
    if(grepl(pattern = ":", x = coeff)) {
      coeff_parts <- unlist(strsplit(coeff, ":"))
      rtn_cnt <- sdid$obs_cnt[sdid$obs_cnt$cohort == coeff_parts[[1]] &
                                sdid$obs_cnt$time == coeff_parts[[2]], "n_obs"]
    } else {
      rtn_cnt <- sum(sdid$obs_cnt[sdid$obs_cnt$cohort == coeff |
                                    sdid$obs_cnt$time == coeff,
                                  "n_obs"])
    }
    return(rtn_cnt)
  })

  ## Pct of population for each coefficient
  ave_pct <- n_obs/sum(n_obs)

  ## Extract estimates selected for averaging
  select_est <- sapply(coefs, function(x) sdid$mdl$coef[names(sdid$mdl$coef)== x])


  # Step 2: Calculate average estimate and corresponding SE, p-value, CI

  ## Calculate weighted estimate
  ave_est <- sum(ave_pct * select_est) #One weighted estimate

  ## Extract relevant part of variance-covariance matrix
  select_vcv <- sdid$vcov[coefs, coefs]

  ## Get SE of average estimate and convert from a matrix to a vector
  ave_se <- as.vector(sqrt(t(as.matrix(ave_pct)) %*% select_vcv %*% as.matrix(ave_pct)))

  ## Extract degrees of freedom, which we use for our P value
  df <- sdid$mdl$df.residual

  ## Calculate p-value, assuming t distribution
  ave_pval <- stats::qt(0.975, df = df)*stats::pt(abs(ave_est/ave_se), df, lower=FALSE)

  ## Make into a df table
  ave_res <- data.frame(term = name,
                        est  = ave_est,
                        se   = ave_se ,
                        pval = ave_pval,
                        sign = ifelse(ave_pval < 0.001, "***",
                                      ifelse(ave_pval < 0.010, "**",
                                             ifelse(ave_pval < 0.050,  "*", ""))),
                        lb   = (ave_est - 1.96*ave_se),
                        ub   = (ave_est + 1.96*ave_se),
                        n    = sum(n_obs))
  return(ave_res)
}

ave_coeff_es <- function(sdid, times) {
  # Exclude comparison groups from the TSI data frame
  valid_tsi <- sdid$tsi[!is.na(sdid$tsi$tsi),]

  # Choose all TSIs if times is not specified
  if(is.null(times)) {
    times <- c(min(valid_tsi$tsi),
               max(valid_tsi$tsi))
  }

  # Validate that times represents valid beginning and ending event times
  if(times[1] < min(valid_tsi$tsi) |
     times[2] > max(valid_tsi$tsi) |
     times[1] > times[2]) {
    stop("Invalid values specified for event-study TSI limits: (",
         paste(times, collapse = ","), ")\n",
         "Must supply to the `times` parameter a 2-element vector of TSIs, ",
         "both observed in the study and with the first element representing ",
         "the earliest TSI and the second element representing the latest TSI.")
  }

  # Restrict to the specified time window
  valid_tsi <- valid_tsi[valid_tsi$tsi >= times[1] & valid_tsi$tsi <= times[2],]

  # Exclude referent time periods from TSI data frame
  for(coh in names(sdid$cohort$time_refs)) {
    valid_tsi <- valid_tsi[!(valid_tsi$cohort == coh &
                               valid_tsi$time == sdid$cohort$time_refs[coh]),]
  }

  atts <- data.frame()
  for(i in sort(unique(valid_tsi$tsi))) {
    atts <- rbind(atts,
                  ave_coeff_main(sdid = sdid,
                                 coefs = select_tsi(sdid = sdid,
                                                    tsi = i),
                                 name = paste0("TSI ", i)))
  }
  return(atts)
}


ave_coeff_calendar <- function(sdid, times) {
  # Exclude comparison groups from the TSI data frame
  valid_tsi <- sdid$tsi[!is.na(sdid$tsi$tsi),]

  # Choose all available years if times is not specified
  if(is.null(times)) {
    times <- unique(valid_tsi$time)
  }

  # Validate that times represents valid event times
  if(!all(times %in% valid_tsi$time)) {
    stop("Invalid values specified for calendar years: \n",
         "Time period(s) (",
         paste(times[!(times %in% valid_tsi$time)], collapse = ", "),
         ") do not appear in the study")
  }

  # Restrict to the specified time window
  valid_tsi <- valid_tsi[valid_tsi$time %in% times,]

  # Exclude referent time periods from TSI data frame
  for(coh in names(sdid$cohort$time_refs)) {
    valid_tsi <- valid_tsi[!(valid_tsi$cohort == coh &
                               valid_tsi$time == sdid$cohort$time_refs[coh]),]
  }

  # Restrict to cohort-time period combinations for the specified time periods
  valid_tsi <- valid_tsi[valid_tsi$time %in% times,]

  # Throw an error if there are no remaining rows in valid_tsi
  if(nrow(valid_tsi) == 0) {
    stop("There are no valid coefficients for the specified time periods (",
         paste(times, collapse = ", "),  ".\n")
  }

  # Now retrieve the values of the relevant interaction terms
  valid_tsi$coefs <- with(valid_tsi,
                          paste0(sdid$cohort$var, "_", cohort,
                                 ":",
                                 sdid$time$var, "_", time))


  atts <- data.frame()
  for(i in sort(unique(valid_tsi$time))) {
    atts <- rbind(atts,
                  ave_coeff_main(sdid = sdid,
                                 coefs = valid_tsi[valid_tsi$time == i, "coefs"],
                                 name = paste0("Time period ", i)))
  }
  return(atts)
}
