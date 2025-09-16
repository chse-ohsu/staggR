#' Aggregate a specified set of terms and corresponding standard errors from an sdid model object
#'
#' @param mdl sdid object containing the model to summarize
#' @param df data frame containing the variables in the model
#' @param vcv variance-covariance corresponding to `mdl`
#' @param select_vars list of variables selected for averaging; if NULL, then [SOMETHING] must be specified
#' @param cohort_name character string for how cohorts are named. Must be specified if `select_vars` is NULL.
#' @param cohort_numbers list of cohort numbers selected for averaging. Must be specified if `select_vars` is NULL.
#' @param tsi_numbers list of time-since-intervention periods selected for averaging. Must be specified if `select_vars` is NULL.
#' @param pop_var name of variable showing population numbers (aggregate data only); if NULL, use nrow (i.e., number of rows = number of observations)
#'
#' @return data.frame
#' @export ave_coeff

ave_coeff <- function(sdid, coefs) {

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

  ## Number of observations for each specified interaction coeff
  n_obs <- sapply(coefs, function(coeff) {
    coeff_parts <- unlist(strsplit(coeff, ":"))
    return(sdid$obs_cnt[sdid$obs_cnt$cohort == coeff_parts[[1]] &
                          sdid$obs_cnt$time == coeff_parts[[2]], "n_obs"])
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

  ## Calculate p-value, assuming normal distribution
  ave_pval <- stats::qnorm(0.975)*pt(abs(ave_est/ave_se), df, lower=FALSE)

  ## Make into a df table
  ave_res <- data.table(est  = ave_est,
                        se   = ave_se ,
                        pval = ave_pval,
                        sign = data.table::fcase(ave_pval < .01                  , "***",
                                                 ave_pval >= .01 & ave_pval < .05, "**" ,
                                                 ave_pval >  .05 & ave_pval < .1 , "*"  ,
                                                 ave_pval >= .1                  , ""   ),
                        lb   = (ave_est - 1.96*ave_se),
                        ub   = (ave_est + 1.96*ave_se),
                        n    = sum(n_obs))
  return(ave_res)
}
