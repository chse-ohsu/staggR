#' Aggregate a specified set of terms and corresponding standard errors from an sdid model object
#'
#' @param mdl
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

sdid = foo
df = hosp
# coefs = NULL
coefs <- c("cohort_5:yr_2013", "cohort_6:yr_2014", "cohort_7:yr_2015")
# cohorts = NULL
cohorts <- c("5", "6")
times = NULL
# tsi <- NULL
tsi = -7:-2

ave_coeff <- function(sdid, df, coefs = NULL, cohorts = NULL, times = NULL, tsi = NULL) {

  # Step 1: Identify cohort / time-since-intervention / individual time periods to summarize
  ## Case A: The user did not specify coefs
  if(is.null(coefs)) {
    coefs <- select_terms(mdl = sdid, selection = list(cohorts = cohorts,
                                                        times = times,
                                                        tsi = tsi))
  }

  ## Case B: The user specified coefs
  if(!is.null(coefs)) {
    # If times or tsi is specified, issue a warning that it will be ignored.
    if(!is.null(times) | !is.null(tsi)) warning("If `coefs` is specified, supplied values for `times` and `tsi` will be ignored.")

    # Check whether the user simply asked for the entire pre- or post-intervention period
    prepost <- all(grepl(pattern = "^pre|^post", x = coefs))
    if(prepost) {
      requested_period <- regmatches(coefs, regexpr("^pre|^post", coefs))
      coefs <- select_period(mdl = sdid, period = requested_period)
    } else coefs <- select_terms(mdl = sdid, coefs = coefs)
  }

  # Only run the rest of the function if there are select vars left after the filtering
  # and checks in calls to select_terms() above.
  if(length(coefs) > 0) {

    ## Step 2: Calculate population fractions and extract estimates

    ## Number of observations
    if (is.null(pop_var)) {
      ave_num <- sapply(select_vars, function(select_var) {
        nrow(df[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1",
                                      sep="", collapse=" & "))),])
        })
    } else {
      ave_num <- sapply(select_vars, function(select_var) {
        df[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1",
                                 sep="", collapse=" & "))),sum(get(pop_var))]
        })
    }

    ## Pct of population for each coefficient
    ave_pct <- ave_num/sum(ave_num)


    ## Extract estimates selected for averaging
    select_est  <- sapply(select_vars, function(x) sdid$mdl$coef[names(sdid$mdl$coef)== x])


    ## Step 3: Calculate average estimate and corresponding SE, p-value, CI

    ## Calculcate weighted estimate
    ave_est <- sum(ave_pct * select_est) #One weighted estimate


    ## Extract relevant part of variance-covariance matric
    select_vcv <- vcv[select_vars, select_vars]


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
