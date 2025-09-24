#' Hospitalization data
#'
#' A simulated data set of 15 counties, 11 of which implemented a policy intervention during 2015 - 2018 to reduce hospitalizations. The data set is longitudinal, with each row corresponding to an individual-year.
#'
#' Report ...
#'
#' @format ## `hosp`
#' A data frame with 31,040 rows and 10 columns:
#' \describe{
#'   \item{guid}{Globally unique identifier for individuals living in the 15 counties}
#'   \item{county}{County names}
#'   \item{intervention_dt}{Date on which each county implemented their policy intervention to reduce hospitalizations}
#'   \item{intervention_yr}{Year during which `intervention_dt` takes place}
#'   \item{age}{Individual's age, time-varying by year}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"hosp"
