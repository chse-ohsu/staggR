#' Constructor for a sdid_mdl object
#'
#' @param model A lm object
#' @param vcov A variance-covariance matrix
#'
#' @return An object of class "sdid_mdl"
#' @export
new_sdid <- function(model, vcov = NULL) {
  structure(
    list(
      mdl = mdl,
      vcov = vcov
    ),
    class = "sdid_mdl"
  )
}
