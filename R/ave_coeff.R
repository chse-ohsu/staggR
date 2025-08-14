#' Prepare a data frame to work with sdid() function
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

ave_coeff <- function(sdid, df, select_vars=NULL, cohort_name, cohort_numbers, tsi_numbers, pop_var=NULL){

  ## Step 1: Translate cohort / time since intervention information to variables to use
  if (is.null(select_vars)){

    ## For each cohort and time since intervention, translate into cohort-study_quarter interaction term
    select_vars <- unlist(lapply(cohort_numbers, function(cohort_number)  paste0(cohort_name, "_", sprintf("%01.0f",cohort_number), ":yr_", sprintf("%02.0f", 2010 + cohort_number + tsi_numbers))))

    ## Select those included in the regression (some may not due to left / right censoring)
    select_vars <- select_vars[select_vars %in% names(sdid$mdl$coef)]

  }

  # Only run the rest of the function if there are select vars left after filtering by those in the model.
  # This is necessary because we sometimes omit terms in the model when there are no observations corresponding
  # to the interaction.
  if(length(select_vars) > 0) {

    ## Step 2: Calculate population fractions and extract estimates

    ## Number of observations
    if (is.null(pop_var)) {
      ave_num <- sapply(select_vars, function(select_var) nrow(df[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1", sep="", collapse=" & "))),]))
    } else {
      ave_num <- sapply(select_vars, function(select_var)      df[eval(parse(text=paste(unlist(strsplit(select_var, ":")), "==1", sep="", collapse=" & "))),sum(get(pop_var))])
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
    ave_pval <-  2*pt(abs(ave_est/ave_se), df, lower=FALSE)

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
